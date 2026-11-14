;;; Prefix key engine for Rhizome — the C-t StumpWM experience
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

(in-package #:rhizome)

(defparameter *prefix-mode-active* nil
  "Whether a prefix chain is in progress.")
(defparameter *prefix-state* nil
  "Surface state hash for the prefix-mode key capture surface.")
(defparameter *prefix-current-map* nil
  "The keymap currently being matched in the chain.")

(defparameter *prefix-root-map* nil
  "The keymap the current key sequence was entered on; command mode
returns to it after each command.")

(defparameter *prefix-label* nil
  "The which-key title of the current key sequence's root, such as
\"C-t\".")

(defparameter *prefix-entry-keys* nil
  "The keys that entered the current key sequence (the direct key
bound to its root keymap), or NIL.")

(defparameter *prefix-keys* nil
  "The keys typed since the current key sequence was entered.")

(defparameter *prefix-seat* nil
  "The seat whose key press entered the current key sequence, or NIL.")

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
(defparameter *expose-active* nil
  "Expose mode (StumpWM's C-t Return): the workspace's windows float
into a grid; pick with digits, arrows+Enter, a click, or Escape.
Layout restoration lives in expose.lisp (stub-redefined here).")

;;; --- Keymaps ---

(defvar *default-prefix-map*
  (make-keymap :name :default-prefix
               :doc "Rhizome's prefix chain bindings.")
  "The default layer of the root prefix keymap: key -> binding. Only setup-default-prefix-bindings writes it.")

(defvar *prefix-map*
  (make-keymap :name :prefix
               :doc "User prefix chain bindings."
               :parents (list *default-prefix-map*))
  "Root prefix keymap. *default-top-map* binds the prefix key (C-t by
default) to this symbol, so a press enters a key sequence on it. It is
the user layer: define-prefix-key writes it, and *default-prefix-map*
is its parent, so a user binding shadows a default on the same key.")

(pushnew (cons '*prefix-map* '*default-prefix-map*) *default-layers*
         :test #'equal)

(defparameter *prefix-describe-key-pending* nil
  "Set by C-t h k: the next keypress is described, not run.")

(defun define-prefix-key (key action &optional doc)
  "Bind KEY in the user root prefix keymap *prefix-map* to ACTION, as
define-key does. KEY is a bare keysym (no modifiers), a key, or a key
spec string; a sequence binds through nested keymaps. ACTION is a
function, an event-binding, or a keymap for multi-key chains. DOC
describes the binding for describe-key and which-key. While the
defaults are built, write *default-prefix-map* instead. A NIL ACTION
removes the user binding."
  (bind-key-sequence (default-layer-target *prefix-map*)
                     (ensure-key-sequence key) action doc))

(defun define-keymap-key (keymap key action &optional doc)
  "Bind KEY (a bare keysym, a key, or a key spec string) in KEYMAP to
ACTION (function, event-binding, or nested keymap), documented by DOC."
  (bind-key-sequence keymap (ensure-key-sequence key) action doc))

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

(defun prefix-capture-available-p ()
  "True when the globals the capture surface needs are bound."
  (and *compositor* *shm* *layer-shell* t))

(defun open-prefix-capture-surface ()
  "Create the invisible keyboard-exclusive layer surface that routes
keys to the WM's wl_keyboard, and set *prefix-mode-active*."
  (setf *prefix-state* (make-hash-table))
  (let ((state *prefix-state*))
    ;; The render helpers expect the global wl_shm in the state hash
    (setf (gethash :shm state) *shm*)
    (let ((surface (wl:wl-compositor.create-surface *compositor*)))
      (setf (gethash :surface state) surface)
      (let ((layer-surface
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface nil :top "rhizome-prefix")))
        (setf (gethash :layer-surface state) layer-surface)
        (push (lambda (event-name &rest args)
                (case event-name
                  (:configure
                   (destructuring-bind (serial width height) args
                     (prefix-capture-configure
                       state layer-surface serial width height)))
                  (:closed
                   (setf *prefix-mode-active* nil)
                   (reassert-seat-focus))))
              (wl:wl-proxy-hooks layer-surface))
        ;; Anchor top-left, 1x1, no exclusive zone: invisible
        (rwm:zwlr-layer-surface-v1.set-anchor
          layer-surface '(:top :left))
        (rwm:zwlr-layer-surface-v1.set-size layer-surface
          *prefix-size* *prefix-size*)
        (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
          layer-surface :exclusive)
        (setf *prefix-mode-active* t)
        (wl:wl-surface.commit surface)))))

(defun merged-keymap (maps)
  "Return the keymap a key sequence on MAPS matches against: the keymap
MAPS denotes, or for a list of several, a new keymap whose parents are
those keymaps in order."
  (let ((resolved (mapcar (lambda (m)
                            (or (binding-keymap m)
                                (error "~S is not a keymap" m)))
                          (if (listp maps) maps (list maps)))))
    (if (rest resolved)
        (make-keymap :name :merged :parents resolved)
        (first resolved))))

(defun prefix-key-label ()
  "The description of the key *default-top-map* binds to *prefix-map*."
  (key-description (make-key (car *prefix-key-binding*)
                             (cdr *prefix-key-binding*))))

(defun enter-prefix-mode (&optional (maps '*prefix-map*) label entry-keys seat)
  "Begin a key sequence on MAPS: a keymap, a symbol naming one, or a
list of them merged in order (an earlier map wins a key). Following
keys arrive through the capture surface and dispatch in
prefix-handle-key. LABEL titles the which-key display; it defaults to
the description of ENTRY-KEYS, the keys that entered the sequence, or
of the prefix key. SEAT is the seat those keys came from. Does nothing
while a prompt owns the keyboard or the capture surface cannot be
created."
  ;; Don't fight with an open prompt for the keyboard
  (when (or *prompt-active* *eval-prompt-active* *launcher-active*)
    (return-from enter-prefix-mode))
  (when *prefix-mode-active*
    ;; Double prefix: treat as abort-and-restart
    (exit-prefix-mode))
  (unless (prefix-capture-available-p)
    (format t "~&; Rhizome: cannot enter prefix mode (missing globals)~%")
    (return-from enter-prefix-mode))
  (let ((map (merged-keymap maps)))
    (setf *prefix-current-map* map
          *prefix-root-map* map
          *prefix-label* (or label
                             (and entry-keys (sequence-description entry-keys))
                             (prefix-key-label))
          *prefix-entry-keys* entry-keys
          *prefix-keys* nil
          *prefix-seat* seat
          *prefix-expiry* (+ (get-universal-time) 5))
    (open-prefix-capture-surface)
    (when *prefix-mode-active*
      ;; which-key: show the available follow-up keys
      (show-message (which-key-text *prefix-label* map)
                    (prefix-indicator-seconds)))))

(defun exit-prefix-mode ()
  "End the prefix chain and destroy the capture surface. Clears all
prefix modes (command mode, iresize, expose)."
  (expose-restore-if-active)
  (setf *prefix-mode-active* nil
        *prefix-current-map* nil
        *prefix-root-map* nil
        *prefix-keys* nil
        *prefix-expiry* 0
        *prefix-command-mode* nil
        *iresize-active* nil
        *expose-active* nil)
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
          (format t "~&; Rhizome: error exiting prefix mode: ~A~%" e)))
      (reassert-seat-focus))
    (setf *prefix-state* nil)))

(defun prefix-drain ()
  "Abort prefix mode when its expiry passes. Called from the main loop."
  (when (and *prefix-mode-active*
             (plusp *prefix-expiry*)
             (>= (get-universal-time) *prefix-expiry*))
    (format t "~&; Rhizome: prefix mode timed out~%")
    (exit-prefix-mode)))

;;; --- Key dispatch ---

(defun prefix-title (keys)
  "The current sequence's label followed by KEYS, for which-key titles
and messages, e.g. \"C-t h\"."
  (format nil "~A~{ ~A~}"
          (or *prefix-label* (prefix-key-label))
          (mapcar #'key-description keys)))

(defun prefix-lookup (key)
  "Look KEY up in the enabled minor modes, then *prefix-current-map*.
When nothing binds KEY and it carries modifiers of the key that
entered the sequence (still held from that chord), look up KEY without
them. Returns the binding (or NIL) and the key it was found under."
  (flet ((lookup (k)
           (or (minor-mode-lookup k)
               (keymap-lookup *prefix-current-map* k))))
    (let ((entry (lookup key)))
      (if entry
          (values entry key)
          (let* ((held (and *prefix-entry-keys*
                            (key-modifiers (car (last *prefix-entry-keys*)))))
                 (bare (and held
                            (intersection held (key-modifiers key))
                            (make-key (key-keysym key)
                                      (set-difference (key-modifiers key) held)))))
            (let ((found (and bare (lookup bare))))
              (if found
                  (values found bare)
                  (values nil key))))))))

(defun prefix-handle-key (keysym &optional modifiers)
  "Dispatch one key, KEYSYM with MODIFIERS (logical or river keywords),
within a key sequence. Returns T."
  (let ((key (make-key keysym modifiers)))
    (cond
      ;; Escape aborts whatever prefix mode is running
      ((= keysym 65307)
       (message "aborted")
       (exit-prefix-mode))
      ;; iresize: arrows adjust the split until Enter/Esc
      (*iresize-active*
       (iresize-handle-key keysym))
      ;; expose: digits/arrows pick a window, Enter picks the focused
      (*expose-active*
       (expose-handle-key keysym))
      ;; describe-key pending: describe, don't run
      (*prefix-describe-key-pending*
       (setf *prefix-describe-key-pending* nil)
       (describe-prefix-key key)
       (exit-prefix-mode))
      (t
       (multiple-value-bind (entry key) (prefix-lookup key)
         (let ((map (binding-keymap entry))
               (typed (append *prefix-keys* (list key))))
           (cond
             ;; Undefined key: unbound, runs nothing
             ((undefined-binding-p entry)
              (message "~A is unbound" (prefix-title typed))
              (exit-prefix-mode))
             ;; Nested keymap: descend, wait for the next key
             (map
              (setf *prefix-current-map* map
                    *prefix-keys* typed
                    *prefix-expiry* (+ (get-universal-time)
                                       (prefix-indicator-seconds)))
              ;; which-key: the available follow-up keys
              (show-message (which-key-text (prefix-title typed) map)
                            (prefix-indicator-seconds)))
             ;; Action: run it, then end the chain, unless command mode
             ;; keeps the sequence armed for the next command
             (entry
              (let ((event (make-key-event
                            key :sequence (append *prefix-entry-keys* typed)
                                :seat *prefix-seat*)))
                (cond (*prefix-command-mode*
                       (call-binding entry event)
                       (let ((root (or *prefix-root-map* *prefix-map*)))
                         (setf *prefix-current-map* root
                               *prefix-keys* nil
                               *prefix-expiry* (+ (get-universal-time) 60))
                         (show-message (which-key-text (prefix-title nil) root)
                                       60)))
                      (t
                       (exit-prefix-mode)
                       (call-binding entry event)))))
             ;; Unmapped key: abort with feedback
             (t
              (message "~A is unbound" (prefix-title typed))
              (exit-prefix-mode))))))))
  t)

;;; --- Minor modes (StumpWM's keymap overlays, Wayland-adapted) ---
;;;
;;; StumpWM minor modes bind keys globally and re-send the unclaimed
;;; ones to the focused window. Wayland forbids key injection, so
;;; rhizome's minor modes scope to the prefix chain instead: while a
;;; mode is enabled, ITS keymap is consulted before the prefix map
;;; (most recently enabled mode first). Everything else about C-t
;;; chains is unchanged.

(defvar *minor-mode-keymaps* (make-hash-table)
  "Maps minor mode names to their keymaps (all defined modes).")
(defparameter *minor-mode-stack* nil
  "Enabled minor modes, most recently enabled first. Their keymaps
take precedence over the prefix map, in this order.")
(defparameter *minor-mode-docs* (make-hash-table)
  "Maps minor mode names to their docstrings.")

(defmacro define-minor-mode (name &optional doc)
  "Define a minor mode named NAME (any designator; keywords read
best). Its bindings live in its own keymap, consulted before the
prefix map while the mode is enabled:
\(define-minor-mode :presentation \"bindings for talks\")"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (unless (gethash ,name *minor-mode-keymaps*)
       (setf (gethash ,name *minor-mode-keymaps*) (make-keymap :name ,name)))
     (setf (gethash ,name *minor-mode-docs*) ,doc)
     ',name))

(defun minor-mode-keymap (name)
  "NAME's keymap (defining it if needed)."
  (or (gethash name *minor-mode-keymaps*)
      (setf (gethash name *minor-mode-keymaps*) (make-keymap :name name))))

(defun define-minor-mode-key (name key action)
  "Bind KEY (a bare keysym, a key, or a key spec string) in minor mode
NAME to ACTION."
  (bind-key-sequence (minor-mode-keymap name) (ensure-key-sequence key)
                     action))

(defun enable-minor-mode (name)
  "Enable NAME: its keymap takes precedence over the prefix map."
  (minor-mode-keymap name)
  (unless (member name *minor-mode-stack*)
    (push name *minor-mode-stack*))
  (message "minor mode enabled: ~A" name))

(defun disable-minor-mode (name)
  "Disable NAME."
  (setf *minor-mode-stack* (remove name *minor-mode-stack*))
  (message "minor mode disabled: ~A" name))

(defun enabled-minor-modes ()
  "The enabled minor modes, most recently enabled first."
  (copy-list *minor-mode-stack*))

(defun minor-mode-lookup (key)
  "The first binding for KEY (a key designator) among the enabled minor
modes (most recently enabled first), if any."
  (let ((key (ensure-key key)))
    (loop for name in *minor-mode-stack*
          for entry = (keymap-lookup (minor-mode-keymap name) key)
          when entry
            return entry)))

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
          (message "stopping rhizome")
          (stop))
        (progn
          (setf *quit-armed-at* now)
          (message "Press C-t q again to quit")))))

;;; --- Help system (StumpWM's C-t h chains) ---

(defun describe-prefix-key (key)
  "Show what KEY (a key designator) is bound to in the current prefix
map, naming it in key spec syntax."
  (let* ((key (ensure-key key))
         (entry (keymap-lookup *prefix-current-map* key))
         (sub (binding-keymap entry))
         (name (prefix-title (append *prefix-keys* (list key)))))
    (cond ((undefined-binding-p entry)
           (message "~A: unbound" name))
          (sub
           (message "~A: keymap: ~{~A~^ ~}" name
                    (mapcar #'key-description (keymap-keys sub))))
          (entry
           (message "~A: ~A" name
                    (or (keymap-binding-doc *prefix-current-map* key)
                        "a command")))
          (t
           (message "~A: unbound" name)))))

(defun describe-key-prompt ()
  "Wait for the next key in a fresh prefix chain and describe it."
  (message "describe-key: press a key in ~A" (prefix-key-label))
  (setf *prefix-describe-key-pending* t)
  (enter-prefix-mode))

(defun describe-prefix-bindings ()
  "List the prefix map's bindings, in key spec syntax, in the message
window."
  (message "~A:~A ~{~A~^, ~}"
           (prefix-key-label)
           (if *minor-mode-stack*
               (format nil " minor modes: ~{~A~^ ~};" *minor-mode-stack*)
               "")
           (sort (loop for k in (keymap-keys *prefix-map*)
                       for doc = (keymap-binding-doc *prefix-map* k)
                       collect (if doc
                                   (format nil "~A (~A)" (key-description k) doc)
                                   (key-description k)))
                 #'string<)))

(defun describe-function-prompt ()
  "Prompt for a rhizome function name and show its docstring."
  (prompt-open "Describe function"
              :candidates
              (sort (loop for sym being the external-symbols
                              of (find-package '#:rhizome)
                          when (fboundp sym)
                          collect (string-downcase (string sym)))
                    #'string<)
              :on-complete
              (lambda (name)
                (let ((sym (find-symbol (string-upcase name) '#:rhizome)))
                  (cond ((null sym) (message "no such function: ~A" name))
                        ((documentation sym 'function)
                         (message "~A: ~A" sym (documentation sym 'function)))
                        (t (message "~A: no documentation" sym)))))))

(defun describe-variable-prompt ()
  "Prompt for a rhizome variable name and show its docstring."
  (prompt-open "Describe variable"
              :candidates
              (sort (loop for sym being the external-symbols
                              of (find-package '#:rhizome)
                          when (boundp sym)
                          collect (string-downcase (string sym)))
                    #'string<)
              :on-complete
              (lambda (name)
                (let ((sym (find-symbol (string-upcase name) '#:rhizome)))
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
                    (loop for k in (keymap-keys map)
                          collect (string-right-trim
                                    " "
                                    (format nil "~A ~A"
                                            (key-description k)
                                            (or (keymap-binding-doc map k)
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

(defun enter-sticky-keymap (maps &optional label)
  "Begin a key sequence on MAPS (as enter-prefix-mode takes them) that
stays armed after each command, returning to MAPS, until Escape or a
minute without a key."
  (enter-prefix-mode maps label)
  (when *prefix-mode-active*
    (setf *prefix-command-mode* t
          *prefix-expiry* (+ (get-universal-time) 60))
    (show-message (which-key-text *prefix-label* *prefix-root-map*) 60)))

(defun sticky-keymap (maps &optional label)
  "Return a binding that enters MAPS as a sticky key sequence (see
enter-sticky-keymap), e.g.
  (define-key *top-map* \"s-r\" (sticky-keymap *resize-map* \"resize\"))"
  (lambda () (enter-sticky-keymap maps label)))

(defun enter-command-mode ()
  "Keep the prefix armed for successive commands until Escape
(StumpWM's command-mode / push-top-map). The which-key display shows
the available commands while it lasts."
  (enter-sticky-keymap '*prefix-map*))

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

;;; --- Expose stubs (redefined by expose.lisp, which loads later) ---

(defun enter-expose ()
  "Float every window on the workspace into a pick grid (StumpWM's
expose, C-t Return). Placeholder: redefined by expose.lisp."
  (message "expose unavailable"))

(defun expose-handle-key (keysym)
  "Placeholder: redefined by expose.lisp."
  (declare (ignore keysym))
  nil)

(defun expose-restore-if-active ()
  "Placeholder: redefined by expose.lisp."
  nil)

(defun bind-default-prefix-keys ()
  "Bind the default C-t keymap, mapped onto rhizome's features. Called
by setup-default-prefix-bindings, which directs the writes to
*default-prefix-map*."
  ;; C-t ; toggles the scratchpad (StumpWM classic)
  (define-prefix-key 59 (lambda () (toggle-scratchpad)) "toggle the scratchpad")
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
  ;; rhizome keeps C-t x for the command palette)
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
  ;; C-t l — cycle the workspace group type (tile/scroll/floating)
  (define-prefix-key 108
                     (lambda () (cycle-workspace-type))
                     "cycle group type (tile/scroll/floating)")  ; l
  ;; C-t v — toggle the niri-style scroll ribbon
  (define-prefix-key 118
                     (lambda () (scroll-toggle (first (all-outputs))))
                     "toggle scroll layout (niri ribbon)")     ; v
  ;; C-t , / C-t . — focus column left / right (viewport follows)
  (define-prefix-key 44 (lambda () (scroll-focus-left))
                     "focus column left")                      ; ,
  (define-prefix-key 46 (lambda () (scroll-focus-right))
                     "focus column right")                      ; .
  ;; C-t Return — expose: float everything into a pick grid
  (define-prefix-key 65293 (lambda () (enter-expose))
                         "expose window grid (pick: digits/arrows/click)") ; Return
  ;; C-t q — quit, with confirm (StumpWM's quit-confirm)
  (define-prefix-key 113 (lambda () (quit-confirm))
                         "quit rhizome (press twice)"))

(defun setup-default-prefix-bindings ()
  "Rebuild *default-prefix-map*, the default C-t keymap. The user
layer *prefix-map* is untouched. Returns *default-prefix-map*."
  (clear-keymap *default-prefix-map*)
  (let ((*defining-default-bindings* t))
    (bind-default-prefix-keys))
  *default-prefix-map*)

(defun clear-user-keymaps ()
  "Remove every user binding from *top-map* and *prefix-map*, leaving
the defaults in force. The next manage sequence releases the grabs of
the bindings that are gone."
  (clear-keymap *top-map*)
  (clear-keymap *prefix-map*)
  (mark-key-grabs-dirty))
