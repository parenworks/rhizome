;;; Prefix key engine for Stumpway — the C-t StumpWM experience
;;;
;;; River's xkb bindings are single-key+modifier registrations, so a
;;; keychain (prefix then follow-up key) cannot be one binding. The
;;; prefix (default Ctrl+T) is a normal binding; pressing it opens an
;;; invisible 1x1 keyboard-exclusive layer surface, so follow-up keys
;;; flow to the WM's wl_keyboard and dispatch through a client-side
;;; keymap state machine — StumpWM's architecture, ported to Wayland.
;;;
;;;   (define-prefix-key +xkb-key-W+ (lambda () (windowlist)))
;;;   (define-prefix-key +xkb-key-H+ *help-map*)   ; nested maps
;;;
;;; Escape aborts, an unmapped key aborts (with a message), and the
;;; mode auto-expires after 5 seconds.

(in-package #:stumpway)

(defparameter *prefix-mode-active* nil
  "Whether a prefix chain is in progress.")
(defparameter *prefix-state* nil
  "Surface state hash for the prefix-mode key capture surface.")
(defparameter *prefix-current-map* nil
  "The keymap currently being matched in the chain.")
(defparameter *prefix-expiry* 0
  "Universal time when prefix mode auto-aborts.")
(defparameter *prefix-size* 1
  "Width/height of the capture surface (invisible 1x1).")

;;; --- Keymaps ---

(defun make-keymap ()
  "A prefix keymap: maps keysyms to actions (thunks) or nested keymaps."
  (make-hash-table))

(defvar *prefix-map* (make-keymap)
  "Root prefix keymap, matched after the prefix key is pressed.")

(defun define-prefix-key (keysym action)
  "Bind KEYSYM in the root prefix keymap to ACTION — a function, or a
nested keymap for multi-key chains."
  (setf (gethash keysym *prefix-map*) action))

(defun define-keymap-key (keymap keysym action)
  "Bind KEYSYM in KEYMAP to ACTION (function or nested keymap)."
  (setf (gethash keysym keymap) action))

;;; --- Mode lifecycle ---

(defun prefix-capture-configure (state layer-surface serial width height)
  "Acknowledge the capture surface's configure; attach a 1x1 buffer so
the surface maps and receives keyboard focus."
  (declare (ignore width height))
  (setf (gethash :width state) *prefix-size*
        (gethash :height state) *prefix-size*)
  (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
  (bar-ensure-shm state)
  ;; Attach whatever buffer exists; contents are irrelevant (1px)
  (let ((ptr (gethash :shm-ptr state)))
    (when (and ptr (not (cffi:null-pointer-p ptr)))
      (wl:wl-surface.attach (gethash :surface state)
                            (gethash :wl-buffer state) 0 0)
      (wl:wl-surface.damage (gethash :surface state) 0 0 1 1)
      (wl:wl-surface.commit (gethash :surface state)))))

(defun enter-prefix-mode ()
  "Begin a prefix key chain (from the prefix xkb binding)."
  ;; Don't fight with an open prompt for the keyboard
  (when (or *prompt-active* *eval-prompt-active* *launcher-active*)
    (return-from enter-prefix-mode))
  (when *prefix-mode-active*
    ;; Double prefix: treat as abort-and-restart
    (exit-prefix-mode))
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Stumpway: cannot enter prefix mode (missing globals)~%")
    (return-from enter-prefix-mode))
  (setf *prefix-current-map* *prefix-map*
        *prefix-expiry* (+ (get-universal-time) 5)
        *prefix-state* (make-hash-table))
  (let ((state *prefix-state*))
    ;; The render helpers expect the global wl_shm in the state hash
    (setf (gethash :shm state) *shm*)
    (let ((surface (wl:wl-compositor.create-surface *compositor*)))
      (setf (gethash :surface state) surface)
      (let ((layer-surface
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface nil :top "stumpway-prefix")))
        (setf (gethash :layer-surface state) layer-surface)
        (push (lambda (event-name &rest args)
                (case event-name
                  (:configure
                   (destructuring-bind (serial width height) args
                     (prefix-capture-configure
                       state layer-surface serial width height)))
                  (:closed
                   (setf *prefix-mode-active* nil))))
              (wl:wl-proxy-hooks layer-surface))
        ;; Anchor top-left, 1x1, no exclusive zone: invisible
        (rwm:zwlr-layer-surface-v1.set-anchor
          layer-surface '(:top :left))
        (rwm:zwlr-layer-surface-v1.set-size layer-surface
          *prefix-size* *prefix-size*)
        (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
          layer-surface :exclusive)
        (setf *prefix-mode-active* t)
        (show-message "C-t -" 5)
        (wl:wl-surface.commit surface)))))

(defun exit-prefix-mode ()
  "End the prefix chain and destroy the capture surface."
  (setf *prefix-mode-active* nil
        *prefix-current-map* nil
        *prefix-expiry* 0)
  (message-hide)
  (let ((state *prefix-state*))
    (when state
      (handler-case
          (progn
            (when (gethash :layer-surface state)
              (rwm:zwlr-layer-surface-v1.destroy
                (gethash :layer-surface state)))
            (when (gethash :surface state)
              (wl:wl-surface.destroy (gethash :surface state)))
            (bar-destroy-shm state))
        (error (e)
          (format t "~&; Stumpway: error exiting prefix mode: ~A~%" e))))
    (setf *prefix-state* nil)))

(defun prefix-drain ()
  "Abort prefix mode when its expiry passes. Called from the main loop."
  (when (and *prefix-mode-active*
             (plusp *prefix-expiry*)
             (>= (get-universal-time) *prefix-expiry*))
    (format t "~&; Stumpway: prefix mode timed out~%")
    (exit-prefix-mode)))

;;; --- Key dispatch ---

(defun prefix-handle-key (keysym)
  "Dispatch one keysym within a prefix chain. Returns T."
  (cond
    ;; Escape aborts
    ((= keysym 65307)
     (message "aborted")
     (exit-prefix-mode))
    (t
     (let ((entry (gethash keysym *prefix-current-map*)))
       (cond
         ;; Nested keymap: descend, wait for the next key
         ((hash-table-p entry)
          (setf *prefix-current-map* entry
                *prefix-expiry* (+ (get-universal-time) 5)))
         ;; Action: run it, then end the chain
         ((and entry (functionp entry))
          (exit-prefix-mode)
          (handler-case
              (funcall entry)
            (error (e)
              (format t "~&; Stumpway: prefix action error: ~A~%" e)
              (message "error: ~A" e))))
         ;; Unmapped key: abort with feedback
         (t
          (message "unbound key")
          (exit-prefix-mode))))))
  t)

;;; --- Default prefix bindings (StumpWM-flavored) ---

(defmacro with-frame-context (&body body)
  "Run BODY with OUT and WS bound to the output and workspace frame
commands act on. Skips when no output exists."
  `(let* ((out* (first (all-outputs))))
     (when out*
       (let ((out out*) (ws (current-workspace out*)))
         ,@body))))

(defun switch-workspace-relative (delta)
  "Switch to the workspace DELTA steps away (clamped to the range)."
  (let ((out (first (all-outputs))))
    (when out
      (let ((target (+ (current-workspace out) delta)))
        (when (<= 1 target *workspace-count*)
          (switch-workspace target out))))))

(defun focus-frame-direction* (direction)
  "Prefix-chain wrapper: move frame focus in DIRECTION."
  (with-frame-context
    (focus-frame-direction out ws direction)))

(defun setup-default-prefix-bindings ()
  "The default C-t keymap, mapped onto stumpway's features."
  ;; C-t w — windowlist (StumpWM: windowlist)
  (define-prefix-key 119 (lambda () (windowlist)))        ; w
  ;; C-t g — workspace picker (StumpWM: group commands)
  (define-prefix-key 103 (lambda () (workspace-list)))   ; g
  ;; C-t x — command palette
  (define-prefix-key 120 (lambda () (command-palette)))   ; x
  ;; C-t : — eval prompt (StumpWM: eval)
  (define-prefix-key 58 (lambda () (show-eval-prompt)))   ; :
  ;; C-t ! — shell command prompt (StumpWM: run shell command)
  (define-prefix-key 33 (lambda () (run-shell-command-prompt))) ; !
  ;; C-t b — toggle the bar
  (define-prefix-key 98 (lambda () (toggle-bar)))        ; b
  ;; C-t f — float/tile the focused window (GIMP-style floats)
  (define-prefix-key 102 (lambda () (toggle-float-focused))) ; f
  ;; C-t n / C-t p — cycle the windows in the current frame
  (define-prefix-key 110 (lambda () (with-frame-context
                                       (cycle-frame-window out ws 1))))  ; n
  (define-prefix-key 112 (lambda () (with-frame-context
                                       (cycle-frame-window out ws -1)))) ; p
  ;; C-t N / C-t P — next/previous workspace
  (define-prefix-key 78 (lambda () (switch-workspace-relative 1)))   ; N
  (define-prefix-key 80 (lambda () (switch-workspace-relative -1)))   ; P
  ;; C-t s / C-t S — split the current frame (side-by-side / stacked)
  (define-prefix-key 115 (lambda () (with-frame-context
                                       (split-current-frame out ws :horizontal)))) ; s
  (define-prefix-key 83 (lambda () (with-frame-context
                                       (split-current-frame out ws :vertical))))    ; S
  ;; C-t R — remove the current split (merge into sibling)
  (define-prefix-key 82 (lambda () (with-frame-context
                                       (remove-current-split out ws))))  ; R
  ;; C-t arrows — move focus between frames
  (define-prefix-key 65361 (lambda () (focus-frame-direction* :left)))
  (define-prefix-key 65362 (lambda () (focus-frame-direction* :up)))
  (define-prefix-key 65363 (lambda () (focus-frame-direction* :right)))
  (define-prefix-key 65364 (lambda () (focus-frame-direction* :down)))
  ;; C-t q — quit stumpway (StumpWM: quit)
  (define-prefix-key 113 (lambda () (message "stopping stumpway") (stop))))
