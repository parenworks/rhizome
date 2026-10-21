;;; Seat (input) management for Rhizome
;;;
;;; Each river_seat_v1 is wrapped in a rhizome-seat CLOS instance
;;; that tracks keyboard focus, pointer state, and pointer bindings.

(in-package #:rhizome)

(defvar *keyboard-repeat-rate* 25
  "Key repeat rate in keys per second (from compositor repeat-info).")
(defvar *keyboard-repeat-delay* 300
  "Key repeat delay in milliseconds (from compositor repeat-info).")
(defvar *keyboard-repeat-thread* nil
  "Thread handling key repeat.")
(defvar *keyboard-repeat-key* nil
  "Currently repeating keysym, or nil.")
(defvar *keyboard-repeat-stop* nil
  "Flag to stop the repeat thread.")
(defparameter *keyboard-shift-p* nil
  "Whether shift is currently held (from wl_keyboard modifiers).")

;;; --- Pointer bindings (MOD+drag move/resize) ---

(defvar *pointer-bindings* nil
  "Active river_pointer_binding_v1 objects (MOD+drag bindings).")
(defvar *pointer-bindings-enabled* nil
  "Whether the MOD+drag bindings have been enabled (they may only be
enabled during a manage sequence).")
(defvar *seats-with-pointer-bindings* (make-hash-table)
  "Seats that already have MOD+drag bindings (skip re-creation).")

(defun setup-pointer-bindings (seat)
  "Create the StumpWM-style MOD+drag bindings on SEAT: MOD+Left-drag
moves the window under the pointer (floating it first if tiled),
MOD+Right-drag resizes it. The modifier is the user's *mod*."
  (dolist (spec '((:move 272) (:resize 273)))   ; BTN_LEFT / BTN_RIGHT
    (let* ((type (first spec))
           (button (second spec))
           (binding (rwm:river-seat-v1.get-pointer-binding
                      (seat-proxy seat) button *mod*)))
      (push (lambda (event-name &rest args)
              (declare (ignore args))
              (when (eq event-name :pressed)
                (pointer-drag-start seat type)))
            (wl:wl-proxy-hooks binding))
      (push binding *pointer-bindings*))))

(defun enable-pointer-bindings-if-needed ()
  "Give every seat its MOD+drag bindings and enable them. Pointer
bindings may only be enabled during a manage sequence, so this runs
from on-manage-sequence; it also covers seats that registered before
a hot reload added the feature."
  (dolist (seat (all-seats))
    (unless (gethash seat *seats-with-pointer-bindings*)
      (handler-case
          (progn
            (setup-pointer-bindings seat)
            (setf (gethash seat *seats-with-pointer-bindings*) t))
        (error (e)
          (format t "~&; Rhizome: error creating pointer bindings: ~A~%" e)))))
  (unless *pointer-bindings-enabled*
    (dolist (binding *pointer-bindings*)
      (handler-case
          (rwm:river-pointer-binding-v1.enable binding)
        (error (e)
          (format t "~&; Rhizome: error enabling pointer binding: ~A~%" e))))
    (setf *pointer-bindings-enabled* t)))

(defun pointer-drag-start (seat type)
  "An MOD+drag began. Placeholder: redefined by floating.lisp."
  (declare (ignore seat type))
  nil)

(defun expose-clicked (win)
  "A window was clicked (placeholder: redefined by expose.lisp —
during the expose overview a click picks the window)."
  (declare (ignore win))
  nil)

(defclass rhizome-seat ()
  ((proxy          :initarg :proxy          :accessor seat-proxy)
   (wl-seat-name   :initarg :wl-seat-name   :accessor seat-wl-seat-name :initform nil)
   (wl-seat-proxy  :initarg :wl-seat-proxy  :accessor seat-wl-seat-proxy :initform nil)
   (wl-keyboard    :initarg :wl-keyboard    :accessor seat-wl-keyboard :initform nil)
   (focused-window :initarg :focused-window :accessor seat-focused-window :initform nil)
   (pointer-x      :initarg :pointer-x      :accessor seat-pointer-x :initform 0)
   (pointer-y      :initarg :pointer-y      :accessor seat-pointer-y :initform 0)
   (op-pending     :initarg :op-pending     :accessor seat-op-pending :initform nil)
   (op-state       :initarg :op-state       :accessor seat-op-state :initform nil))
  (:documentation "Represents an input seat managed by rhizome."))

(defun register-seat (proxy)
  "Register a new river_seat_v1 proxy and set up event handlers."
  (let ((seat (make-instance 'rhizome-seat :proxy proxy)))
    (setf (gethash proxy *seats*) seat)
    (push (lambda (event-name &rest args)
            (apply #'handle-seat-event seat event-name args))
          (wl:wl-proxy-hooks proxy))
    (format t "~&; Rhizome: new seat registered~%")
    ;; xcursor theme through the protocol (river_seat_v1 v2+): the
    ;; compositor-rendered cursors follow the config (or the
    ;; XCURSOR_THEME/SIZE environment the session exports).
    (let* ((env-theme (uiop:getenv "XCURSOR_THEME"))
           (env-size (ignore-errors
                       (parse-integer
                         (or (uiop:getenv "XCURSOR_SIZE") "")
                         :junk-allowed t)))
           (theme (config-getf '(:pointer) :xcursor-theme
                               :default (or env-theme "")))
           (size (config-getf '(:pointer) :xcursor-size
                              :default (or env-size 24))))
      (when (and (stringp theme) (plusp (length theme)))
        (handler-case
            (rwm:river-seat-v1.set-xcursor-theme proxy theme size)
          (error ()
            ;; Pre-v2 river: cursors fall back to river's default
            nil))))
    ;; MOD+drag move/resize bindings (enabled during manage sequences)
    (handler-case
        (progn
          (setup-pointer-bindings seat)
          (setf (gethash seat *seats-with-pointer-bindings*) t))
      (error (e)
        (format t "~&; Rhizome: error creating pointer bindings: ~A~%" e)))
    ;; Register with river_layer_shell_v1 if available
    (when *river-layer-shell*
      (handler-case
          (let ((ls-seat (rwm:river-layer-shell-v1.get-seat
                           *river-layer-shell* proxy)))
            (push (lambda (event-name &rest args)
                    (case event-name
                      (:focus_exclusive
                       (format t "~&; Rhizome: layer shell exclusive focus~%"))
                      (:focus_non_exclusive
                       (format t "~&; Rhizome: layer shell non-exclusive focus~%"))
                      (:focus_none
                       (format t "~&; Rhizome: layer shell focus none~%"))))
                  (wl:wl-proxy-hooks ls-seat))
            (format t "~&; Rhizome: registered seat with river_layer_shell_v1~%"))
        (error (e)
          (format t "~&; Rhizome: failed to register seat with layer shell: ~A~%" e))))))

(defun handle-seat-event (seat event-name &rest args)
  "Dispatch river_seat_v1 events for SEAT."
  (case event-name
    (:wl-seat
     (destructuring-bind (name) args
       (setf (seat-wl-seat-name seat) name)
       ;; Bind to the wl_seat global and get a keyboard
       (setup-wl-keyboard seat name)))

    (:pointer-position
     (destructuring-bind (x y) args
       (setf (seat-pointer-x seat) x
             (seat-pointer-y seat) y)))

    (:pointer-enter
     (destructuring-bind (window-proxy) args
       (declare (ignore window-proxy))
       nil))

    (:pointer-leave
     nil)

    (:window-interaction
     (destructuring-bind (window-proxy) args
       (let ((win (gethash window-proxy *windows*)))
         (when win
           ;; River reports a pointer press / touch interaction with
           ;; this window so the WM can give it keyboard focus.
           ;; Actually send the focus request (previously only the
           ;; internal slot was updated, so clicks never moved focus).
           (focus-window win seat)
           ;; Expose: a click during the overview picks the window
           ;; (redefined by expose.lisp, which loads later)
           (expose-clicked win)
           (request-manage-dirty)))))

    (:op-delta
     ;; Interactive float move/resize: river reports the TOTAL
     ;; cumulative pointer motion since the op started. Applied by
     ;; float-op-manage-pass in the manage sequence that follows.
     (destructuring-bind (dx dy) args
       (when (seat-op-state seat)
         (setf (getf (seat-op-state seat) :dx) dx
               (getf (seat-op-state seat) :dy) dy))))

    (:op-release
     ;; Buttons released: the op ends in the next manage sequence.
     (when (seat-op-state seat)
       (setf (getf (seat-op-state seat) :released) t)))

    (:removed
     (format t "~&; Rhizome: seat removed~%")
     (remhash (seat-proxy seat) *seats*))

    (t
     (format t "~&; Rhizome: unhandled seat event ~A~%" event-name))))

(defun setup-wl-keyboard (seat wl-seat-name)
  "Bind to wl_seat global and get a wl_keyboard for keyboard events."
  (let ((iface-class (wl:find-interface-named "wl_seat")))
    (unless iface-class
      (format t "~&; Rhizome: wl_seat interface not found~%")
      (return-from setup-wl-keyboard))
    (unless *registry*
      (format t "~&; Rhizome: no registry, cannot bind wl_seat~%")
      (return-from setup-wl-keyboard))
    (let ((wl-seat (wl:wl-registry.bind *registry* wl-seat-name iface-class 7)))
      (setf (seat-wl-seat-proxy seat) wl-seat)
      ;; Set up event handler for wl_seat events
      (push (lambda (event-name &rest args)
              (case event-name
                (:capabilities
                 (destructuring-bind (caps) args
                   ;; Wayflan decodes bitfield as list of keywords
                   ;; Check if :keyboard is in the capabilities list
                   (when (member :keyboard caps :test #'eq)
                     (unless (seat-wl-keyboard seat)
                       (let ((kb (wl:wl-seat.get-keyboard wl-seat)))
                         (setf (seat-wl-keyboard seat) kb)
                         (setup-keyboard-handler seat kb))))))
                (:name
                 nil)))
            (wl:wl-proxy-hooks wl-seat))
      ;; Do a roundtrip to get capabilities
      (wl:wl-display-roundtrip *display*))))

(defun setup-keyboard-handler (seat keyboard)
  "Set up event handler for wl_keyboard key events."
  (declare (ignore seat))
  (push (lambda (event-name &rest args)
          (case event-name
            (:keymap nil)
            (:enter
             (format t "~&; Rhizome [debug]: keyboard enter surface~%"))
            (:leave
             (format t "~&; Rhizome [debug]: keyboard leave surface~%"))
            (:key
             (destructuring-bind (serial time key state) args
               (declare (ignore serial time))
               (let ((keysym (evdev-to-xkb key *keyboard-shift-p*)))
                 (when *prompt-active*
                   (format t "~&; Rhizome [debug]: prompt key keysym=~A state=~A~%"
                           keysym state))
                 (cond
                   ((eq state :pressed)
                    (keyboard-stop-repeat)
                    (cond
                      ;; Prefix key chains (C-t ...) consume keys first
                      ((and *prefix-mode-active* keysym)
                       (prefix-handle-key keysym))
                      ;; Interactive prompt next
                      ((and *prompt-active* keysym)
                       (prompt-handle-key keysym))
                      ;; Live eval prompt next
                      ((and *eval-prompt-active* keysym)
                       (eval-prompt-handle-key keysym))
                      ((and *launcher-active* keysym)
                       (launcher-handle-key keysym nil)
                       (when (member keysym '(65362 65364 65288))
                         (keyboard-start-repeat keysym)))))
                   ((eq state :released)
                    (keyboard-stop-repeat))))))
            (:modifiers
             (destructuring-bind (serial depressed latched locked group) args
               (declare (ignore serial latched locked group))
               ;; XKB shift is mask bit 0
               (setf *keyboard-shift-p* (logbitp 0 depressed))))
            (:repeat-info
             (destructuring-bind (rate delay) args
               (setf *keyboard-repeat-rate* rate
                     *keyboard-repeat-delay* delay)
               (format t "~&; Rhizome: keyboard repeat rate=~A delay=~A~%"
                       rate delay)))))
        (wl:wl-proxy-hooks keyboard)))

(defun keyboard-start-repeat (keysym)
  "Start a repeat thread for KEYSYM."
  (keyboard-stop-repeat)
  (setf *keyboard-repeat-key* keysym
        *keyboard-repeat-stop* nil)
  (setf *keyboard-repeat-thread*
        (bt:make-thread
          (lambda ()
            (sleep (/ *keyboard-repeat-delay* 1000.0))
            (loop while (not *keyboard-repeat-stop*) do
              (when *launcher-active*
                (launcher-handle-key *keyboard-repeat-key* nil))
              (sleep (/ 1.0 *keyboard-repeat-rate*))))
          :name "rhizome-key-repeat")))

(defun keyboard-stop-repeat ()
  "Stop the current repeat thread."
  (setf *keyboard-repeat-stop* t)
  (when *keyboard-repeat-thread*
    (bt:destroy-thread *keyboard-repeat-thread*)
    (setf *keyboard-repeat-thread* nil))
  (setf *keyboard-repeat-key* nil))

(defun evdev-to-xkb (keycode &optional (shift-p nil))
  "Convert a Linux evdev keycode to an xkb keysym, considering shift.
QWERTY layout assumed. Supports the full printable ASCII set (with
shift) so that Lisp expressions can be typed in the eval prompt."
  (let ((row1 "qwertyuiop")
        (row2 "asdfghjkl")
        (row3 "zxcvbnm")
        (digits "1234567890")
        (shifted "!@#$%^&*()"))
    (cond
      ;; Letter rows; shift produces uppercase
      ((<= 16 keycode 25)
       (let ((c (char row1 (- keycode 16))))
         (char-code (if shift-p (char-upcase c) c))))
      ((<= 30 keycode 38)
       (let ((c (char row2 (- keycode 30))))
         (char-code (if shift-p (char-upcase c) c))))
      ((<= 44 keycode 50)
       (let ((c (char row3 (- keycode 44))))
         (char-code (if shift-p (char-upcase c) c))))
      ;; Digit row
      ((<= 2 keycode 11)
       (char-code (if shift-p
                      (char shifted (- keycode 2))
                      (char digits (- keycode 2)))))
      ;; Punctuation with shift
      (shift-p
       (case keycode
         (12 (char-code #\_)) (13 (char-code #\+))
         (39 (char-code #\")) (40 (char-code #\{))
         (41 (char-code #\~)) (43 (char-code #\|))
         (51 (char-code #\<)) (52 (char-code #\>))
         (53 (char-code #\?))
         (t nil)))
      ;; Unshifted
      (t
       (case keycode
         (1 65307)         ; ESC
         (14 65288)        ; Backspace
         (15 65289)        ; Tab
         (28 65293)        ; Enter
         (103 65362)       ; Up
         (108 65364)       ; Down
         (105 65361)       ; Left
         (106 65363)       ; Right
         (57 32)           ; Space
         ;; Function keys F1-F12
         (59 65470) (60 65471) (61 65472) (62 65473)
         (63 65474) (64 65475) (65 65476) (66 65477)
         (67 65478) (68 65479) (87 65480) (88 65481)
         (12 (char-code #\-))
         (13 (char-code #\=))
         (39 (char-code #\'))
         (40 (char-code #\[))
         (41 (char-code #\`))
         (43 (char-code #\\))
         (51 (char-code #\,))
         (52 (char-code #\.))
         (53 (char-code #\/))
         (t nil))))))

(defun all-seats ()
  "Return a list of all registered seats."
  (loop for seat being the hash-values of *seats* collect seat))

(defun clear-focus (seat)
  "Clear keyboard focus on SEAT."
  (rwm:river-seat-v1.clear-focus (seat-proxy seat))
  (setf (seat-focused-window seat) nil))
