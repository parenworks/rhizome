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
(defparameter *prefix-command-mode* nil
  "Command mode: the prefix stays armed for successive commands until
Escape (StumpWM's command-mode / push-top-map).")
(defparameter *iresize-active* nil
  "Interactive resize mode (StumpWM's iresize): arrows move the
current split's dividing line until Enter commits or Escape aborts.")

;;; --- Keymaps ---

(defun make-keymap ()
  "A prefix keymap: maps keysyms to actions (thunks) or nested keymaps."
  (make-hash-table))

(defvar *prefix-map* (make-keymap)
  "Root prefix keymap, matched after the prefix key is pressed.")

(defvar *prefix-key-docs* (make-hash-table)
  "Maps root prefix keysyms to their binding's documentation.")

(defparameter *prefix-describe-key-pending* nil
  "Set by C-t h k: the next keypress is described, not run.")

(defun define-prefix-key (keysym action &optional doc)
  "Bind KEYSYM in the root prefix keymap to ACTION — a function, or a
nested keymap for multi-key chains. DOC describes the binding for
describe-key and describe-bindings."
  (setf (gethash keysym *prefix-map*) action)
  (if doc
      (setf (gethash keysym *prefix-key-docs*) doc)
      (remhash keysym *prefix-key-docs*)))

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
        ;; which-key: show the available follow-up keys
        (show-message (which-key-text "C-t" *prefix-map*)
                      (prefix-indicator-seconds))
        (wl:wl-surface.commit surface)))))

(defun exit-prefix-mode ()
  "End the prefix chain and destroy the capture surface. Clears all
prefix modes (command mode, iresize)."
  (setf *prefix-mode-active* nil
        *prefix-current-map* nil
        *prefix-expiry* 0
        *prefix-command-mode* nil
        *iresize-active* nil)
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
    ;; Escape aborts whatever prefix mode is running
    ((= keysym 65307)
     (message "aborted")
     (exit-prefix-mode))
    ;; iresize: arrows adjust the split until Enter/Esc
    (*iresize-active*
     (iresize-handle-key keysym))
    ;; describe-key pending: describe, don't run
    (*prefix-describe-key-pending*
     (setf *prefix-describe-key-pending* nil)
     (describe-prefix-key keysym)
     (exit-prefix-mode))
    (t
     (let ((entry (gethash keysym *prefix-current-map*)))
       (cond
         ;; Nested keymap: descend, wait for the next key
         ((hash-table-p entry)
          (setf *prefix-current-map* entry
                *prefix-expiry* (+ (get-universal-time)
                                   (prefix-indicator-seconds)))
          ;; which-key: the available follow-up keys
          (show-message (which-key-text
                          (format nil "C-t ~A" (keysym-name keysym))
                          entry)
                        (prefix-indicator-seconds)))
         ;; Action: run it, then end the chain — unless command mode
         ;; keeps the prefix armed for the next command
         ((and entry (functionp entry))
          (if *prefix-command-mode*
              (progn
                (handler-case
                    (funcall entry)
                  (error (e)
                    (format t "~&; Stumpway: prefix action error: ~A~%" e)
                    (message "error: ~A" e)))
                (setf *prefix-current-map* *prefix-map*
                      *prefix-expiry* (+ (get-universal-time) 60))
                (show-message (which-key-text "C-t" *prefix-map*) 60))
              (progn
                (exit-prefix-mode)
                (handler-case
                    (funcall entry)
                  (error (e)
                    (format t "~&; Stumpway: prefix action error: ~A~%" e)
                    (message "error: ~A" e))))))
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

(defun move-window-direction* (direction)
  "Prefix-chain wrapper: move the focused window one frame over."
  (with-frame-context
    (move-window-to-frame-direction out ws direction)))

(defun select-window-by-number-prompt ()
  "Prompt for a window number and focus that window (StumpWM's C-t ')."
  (prompt-open "Window number:"
              :candidates
              (sort (loop for n being the hash-values of *window-number-map*
                          collect (format nil "~D" n))
                    #'string<)
              :on-complete
              (lambda (choice)
                (let ((n (parse-integer choice :junk-allowed t)))
                  (when n
                    (select-window-by-number n))))))

(defparameter *quit-armed-at* 0
  "Universal time C-t q was last armed (quit-confirm).")

(defun quit-confirm ()
  "Quit only on a second C-t q within 3 seconds (StumpWM's
quit-confirm)."
  (let ((now (get-universal-time)))
    (if (> *quit-armed-at* (- now 3))
        (progn
          (setf *quit-armed-at* 0)
          (message "stopping stumpway")
          (stop))
        (progn
          (setf *quit-armed-at* now)
          (message "Press C-t q again to quit")))))

;;; --- Help system (StumpWM's C-t h chains) ---

(defun keysym-name (keysym)
  "A display name for a keysym."
  (case keysym
    (65307 "Esc") (65288 "Backspace") (65289 "Tab") (65293 "Return")
    (65361 "Left") (65362 "Up") (65363 "Right") (65364 "Down")
    (t (let ((c (code-char keysym)))
         (if (graphic-char-p c)
             (string c)
             (format nil "#~D" keysym))))))

(defun describe-prefix-key (keysym)
  "Show what KEYSYM is bound to in the current prefix map."
  (let ((entry (gethash keysym *prefix-current-map*)))
    (cond ((hash-table-p entry)
           (message "C-t ~A: keymap — ~{~A~^ ~}" (keysym-name keysym)
                    (loop for k being the hash-keys of entry
                          collect (keysym-name k))))
          (entry
           (message "C-t ~A: ~A" (keysym-name keysym)
                    (or (gethash keysym *prefix-key-docs*) "a command")))
          (t
           (message "C-t ~A: unbound" (keysym-name keysym))))))

(defun describe-key-prompt ()
  "Wait for the next key in a fresh C-t chain and describe it."
  (message "describe-key: press C-t then a key")
  (setf *prefix-describe-key-pending* t)
  (enter-prefix-mode))

(defun describe-prefix-bindings ()
  "List the C-t bindings in the message window."
  (message "C-t: ~{~A~^, ~}"
           (sort (loop for k being the hash-keys of *prefix-map*
                       for doc = (gethash k *prefix-key-docs*)
                       collect (if doc
                                   (format nil "~A (~A)" (keysym-name k) doc)
                                   (keysym-name k)))
                 #'string<)))

(defun describe-function-prompt ()
  "Prompt for a stumpway function name and show its docstring."
  (prompt-open "Describe function"
              :candidates
              (sort (loop for sym being the external-symbols
                              of (find-package '#:stumpway)
                          when (fboundp sym)
                          collect (string-downcase (string sym)))
                    #'string<)
              :on-complete
              (lambda (name)
                (let ((sym (find-symbol (string-upcase name) '#:stumpway)))
                  (cond ((null sym) (message "no such function: ~A" name))
                        ((documentation sym 'function)
                         (message "~A: ~A" sym (documentation sym 'function)))
                        (t (message "~A: no documentation" sym)))))))

(defun describe-variable-prompt ()
  "Prompt for a stumpway variable name and show its docstring."
  (prompt-open "Describe variable"
              :candidates
              (sort (loop for sym being the external-symbols
                              of (find-package '#:stumpway)
                          when (boundp sym)
                          collect (string-downcase (string sym)))
                    #'string<)
              :on-complete
              (lambda (name)
                (let ((sym (find-symbol (string-upcase name) '#:stumpway)))
                  (cond ((null sym) (message "no such variable: ~A" name))
                        ((documentation sym 'variable)
                         (message "~A: ~A" sym (documentation sym 'variable)))
                        (t (message "~A: no documentation" sym)))))))

(defun describe-focused-window ()
  "Show the focused window's details in the message window."
  (let ((win (find-if #'window-focused-p (all-windows))))
    (if win
        (message "~A ~Ax~A at (~A,~A)~A"
                 (or (window-title win) (window-app-id win) "window")
                 (window-width win) (window-height win)
                 (window-x win) (window-y win)
                 (if (window-floating-p win) " [float]" ""))
        (message "no focused window"))))

;;; --- Which-key: show the available follow-up keys ---

(defun prefix-indicator-seconds ()
  "How long prefix-mode displays stay up (longer in command mode)."
  (if *prefix-command-mode* 60 5))

(defun which-key-text (title map)
  "Display text listing MAP's bindings titled with the chain so far
(TITLE), four entries per line so it fits the message window."
  (let* ((entries (sort
                    (loop for k being the hash-keys of map
                          collect (string-right-trim
                                    " "
                                    (format nil "~A ~A"
                                            (keysym-name k)
                                            (or (gethash k *prefix-key-docs*)
                                                ""))))
                    #'string<))
         (lines nil) (cur nil) (n 0))
    (dolist (e entries)
      (push e cur)
      (incf n)
      (when (= n 4)
        (push (format nil "~{~A~^   ~}" (reverse cur)) lines)
        (setf cur nil n 0)))
    (when cur
      (push (format nil "~{~A~^   ~}" (reverse cur)) lines))
    (format nil "~A:~%~{~A~^~%~}" title (reverse lines))))

;;; --- Command mode: sticky prefix ---

(defun enter-command-mode ()
  "Keep the prefix armed for successive commands until Escape
(StumpWM's command-mode / push-top-map). The which-key display shows
the available commands while it lasts."
  (enter-prefix-mode)
  (when *prefix-mode-active*
    (setf *prefix-command-mode* t
          *prefix-expiry* (+ (get-universal-time) 60))
    (show-message (which-key-text "C-t" *prefix-map*) 60)))

;;; --- Interactive resize (StumpWM's iresize) ---

(defun enter-iresize-mode ()
  "Resize interactively (StumpWM's iresize, C-t r): arrows move the
current split's dividing line until Enter commits or Escape aborts."
  (enter-prefix-mode)
  (when *prefix-mode-active*
    (setf *iresize-active* t
          *prefix-expiry* (+ (get-universal-time) 60))
    (show-message "resize: arrows, Enter, Esc" 60)))

(defun exit-iresize-mode (&optional (feedback "resized"))
  "Leave iresize, keeping the resized geometry."
  (exit-prefix-mode)
  (message "~A" feedback))

(defun iresize-adjust (delta)
  "Move the current split's dividing line by DELTA along its axis."
  (with-frame-context
    (resize-current-split out ws delta)))

(defun iresize-handle-key (keysym)
  "Handle one keysym inside iresize."
  (case keysym
    (65393 (exit-iresize-mode))
    (65361 (iresize-adjust -30))     ; Left
    (65363 (iresize-adjust 30))      ; Right
    (65362 (iresize-adjust -30))     ; Up
    (65364 (iresize-adjust 30))      ; Down
    (t (message "arrows, Enter, Esc")))
  ;; Keep the mode alive while the user works
  (setf *prefix-expiry* (+ (get-universal-time) 60))
  t)

(defun setup-default-prefix-bindings ()
  "The default C-t keymap, mapped onto stumpway's features."
  ;; C-t w — windowlist (StumpWM: windowlist)
  (define-prefix-key 119 (lambda () (windowlist)) "windowlist")   ; w
  ;; C-t g — workspace picker (StumpWM: group commands)
  (define-prefix-key 103 (lambda () (workspace-list)) "workspace picker") ; g
  ;; C-t x — command palette
  (define-prefix-key 120 (lambda () (command-palette)) "command palette") ; x
  ;; C-t : — eval prompt (StumpWM: eval)
  (define-prefix-key 58 (lambda () (show-eval-prompt)) "eval prompt") ; :
  ;; C-t ! — shell command prompt (StumpWM: run shell command)
  (define-prefix-key 33 (lambda () (run-shell-command-prompt)) "shell command") ; !
  ;; C-t b — toggle the bar
  (define-prefix-key 98 (lambda () (toggle-bar)) "toggle bar")   ; b
  ;; C-t f — float/tile the focused window (GIMP-style floats)
  (define-prefix-key 102 (lambda () (toggle-float-focused)) "float/tile window") ; f
  ;; C-t n / C-t p — pull the next/previous hidden window into the
  ;; current frame (StumpWM's pull-hidden-next/previous)
  (define-prefix-key 110 (lambda () (with-frame-context
                                       (pull-hidden-next out ws 1)))
                         "pull next hidden window")              ; n
  (define-prefix-key 112 (lambda () (with-frame-context
                                       (pull-hidden-next out ws -1)))
                         "pull previous hidden window")          ; p
  ;; C-t N / C-t P — next/previous workspace
  (define-prefix-key 78 (lambda () (switch-workspace-relative 1))
                         "next workspace")                        ; N
  (define-prefix-key 80 (lambda () (switch-workspace-relative -1))
                         "previous workspace")                    ; P
  ;; C-t 1..9 — switch workspace directly (groups-map parity)
  (dolist (n '(1 2 3 4 5 6 7 8 9))
    (define-prefix-key (+ 48 n)
                       (lambda () (switch-workspace n (first (all-outputs))))
                       (format nil "workspace ~D" n)))
  ;; C-t ' — select window by number (StumpWM's select)
  (define-prefix-key 39 (lambda () (select-window-by-number-prompt))
                         "select window by number")               ; '
  ;; C-t a — show the time (StumpWM's time)
  (define-prefix-key 97 (lambda () (time-message)) "time")       ; a
  ;; C-t m — redisplay the last message (StumpWM's lastmsg)
  (define-prefix-key 109 (lambda () (lastmsg)) "last message")   ; m
  ;; C-t s / C-t S — split the current frame (side-by-side / stacked)
  (define-prefix-key 115 (lambda () (with-frame-context
                                       (split-current-frame out ws :horizontal)))
                         "split frame side-by-side")              ; s
  (define-prefix-key 83 (lambda () (with-frame-context
                                       (split-current-frame out ws :vertical)))
                         "split frame stacked")                   ; S
  ;; C-t R — remove the current split (merge into sibling)
  (define-prefix-key 82 (lambda () (with-frame-context
                                       (remove-current-split out ws)))
                         "remove split")                          ; R
  ;; C-t + / C-t - — grow/shrink the current split
  (define-prefix-key 43 (lambda () (with-frame-context
                                      (resize-current-split out ws 30)))
                         "grow split (+30)")                      ; +
  (define-prefix-key 45 (lambda () (with-frame-context
                                      (resize-current-split out ws -30)))
                         "shrink split (-30)")                    ; -
  ;; C-t arrows — move focus between frames
  (define-prefix-key 65361 (lambda () (focus-frame-direction* :left)) "focus frame left")
  (define-prefix-key 65362 (lambda () (focus-frame-direction* :up)) "focus frame up")
  (define-prefix-key 65363 (lambda () (focus-frame-direction* :right)) "focus frame right")
  (define-prefix-key 65364 (lambda () (focus-frame-direction* :down)) "focus frame down")
  ;; C-t M <arrow> — move the focused window to the frame in that
  ;; direction (StumpWM: move-window; StumpWM's C-t m is lastmsg)
  (let ((move-map (make-keymap)))
    (define-keymap-key move-map 65361 (lambda () (move-window-direction* :left)))
    (define-keymap-key move-map 65362 (lambda () (move-window-direction* :up)))
    (define-keymap-key move-map 65363 (lambda () (move-window-direction* :right)))
    (define-keymap-key move-map 65364 (lambda () (move-window-direction* :down)))
    (define-prefix-key 77 move-map "move window to frame (then arrows)")) ; M
  ;; C-t e <arrow> — swap the current frame's window with the frame
  ;; in that direction (StumpWM: exchange-direction, C-t x there;
  ;; stumpway keeps C-t x for the command palette)
  (let ((exchange-map (make-keymap)))
    (define-keymap-key exchange-map 65361
                       (lambda () (with-frame-context
                                    (exchange-window-direction out ws :left))))
    (define-keymap-key exchange-map 65362
                       (lambda () (with-frame-context
                                    (exchange-window-direction out ws :up))))
    (define-keymap-key exchange-map 65363
                       (lambda () (with-frame-context
                                    (exchange-window-direction out ws :right))))
    (define-keymap-key exchange-map 65364
                       (lambda () (with-frame-context
                                    (exchange-window-direction out ws :down))))
    (define-prefix-key 101 exchange-map "exchange windows (then arrows)")) ; e
  ;; C-t Q — collapse the workspace to one frame (StumpWM's only)
  (define-prefix-key 81 (lambda () (with-frame-context
                                      (frame-only out ws)))
                         "collapse to one frame")                  ; Q
  ;; C-t B — equalize all splits (StumpWM's balance-frames)
  (define-prefix-key 66 (lambda () (with-frame-context
                                      (frame-balance out ws)))
                         "balance frames")                         ; B
  ;; C-t F11 — toggle fullscreen (StumpWM's fullscreen)
  (define-prefix-key 65480 (lambda () (toggle-fullscreen
                                        (find-if #'window-focused-p
                                                 (all-windows))))
                         "toggle fullscreen")                      ; F11
  ;; C-t h — help map (StumpWM: *help-map*)
  (let ((help-map (make-keymap)))
    ;; b — describe-bindings
    (define-keymap-key help-map 98 (lambda () (describe-prefix-bindings)))
    ;; c — describe-command
    (define-keymap-key help-map 99 (lambda () (describe-command-prompt)))
    ;; f — describe-function
    (define-keymap-key help-map 102 (lambda () (describe-function-prompt)))
    ;; v — describe-variable
    (define-keymap-key help-map 118 (lambda () (describe-variable-prompt)))
    ;; k — describe-key (waits for the next key)
    (define-keymap-key help-map 107 (lambda () (describe-key-prompt)))
    ;; w — describe the focused window
    (define-keymap-key help-map 119 (lambda () (describe-focused-window)))
    (define-prefix-key 104 help-map "help (then b c f v k w)"))  ; h
  ;; C-t # — mark the focused window for mark commands
  (define-prefix-key 35 (lambda () (toggle-mark-focused))
                         "mark/unmark window")                    ; #
  ;; C-t r — interactive resize mode: arrows until Enter/Esc
  (define-prefix-key 114 (lambda () (enter-iresize-mode))
                         "interactive resize (arrows, Enter)")
  ;; C-t C — command mode: sticky prefix until Escape
  (define-prefix-key 67 (lambda () (enter-command-mode))
                         "command mode (sticky prefix)")
  ;; C-t q — quit, with confirm (StumpWM's quit-confirm)
  (define-prefix-key 113 (lambda () (quit-confirm))
                         "quit stumpway (press twice)"))           ; q
