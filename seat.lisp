;;; Seat (input) management for Stumpway
;;;
;;; Each river_seat_v1 is wrapped in a stumpway-seat CLOS instance
;;; that tracks keyboard focus, pointer state, and pointer bindings.

(in-package #:stumpway)

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

(defclass stumpway-seat ()
  ((proxy          :initarg :proxy          :accessor seat-proxy)
   (wl-seat-name   :initarg :wl-seat-name   :accessor seat-wl-seat-name :initform nil)
   (wl-seat-proxy  :initarg :wl-seat-proxy  :accessor seat-wl-seat-proxy :initform nil)
   (wl-keyboard    :initarg :wl-keyboard    :accessor seat-wl-keyboard :initform nil)
   (focused-window :initarg :focused-window :accessor seat-focused-window :initform nil)
   (pointer-x      :initarg :pointer-x      :accessor seat-pointer-x :initform 0)
   (pointer-y      :initarg :pointer-y      :accessor seat-pointer-y :initform 0))
  (:documentation "Represents an input seat managed by stumpway."))

(defun register-seat (proxy)
  "Register a new river_seat_v1 proxy and set up event handlers."
  (let ((seat (make-instance 'stumpway-seat :proxy proxy)))
    (setf (gethash proxy *seats*) seat)
    (push (lambda (event-name &rest args)
            (apply #'handle-seat-event seat event-name args))
          (wl:wl-proxy-hooks proxy))
    (format t "~&; Stumpway: new seat registered~%")
    ;; Register with river_layer_shell_v1 if available
    (when *river-layer-shell*
      (handler-case
          (let ((ls-seat (rwm:river-layer-shell-v1.get-seat
                           *river-layer-shell* proxy)))
            (push (lambda (event-name &rest args)
                    (case event-name
                      (:focus_exclusive
                       (format t "~&; Stumpway: layer shell exclusive focus~%"))
                      (:focus_non_exclusive
                       (format t "~&; Stumpway: layer shell non-exclusive focus~%"))
                      (:focus_none
                       (format t "~&; Stumpway: layer shell focus none~%"))))
                  (wl:wl-proxy-hooks ls-seat))
            (format t "~&; Stumpway: registered seat with river_layer_shell_v1~%"))
        (error (e)
          (format t "~&; Stumpway: failed to register seat with layer shell: ~A~%" e))))))

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
           (request-manage-dirty)))))

    (:removed
     (format t "~&; Stumpway: seat removed~%")
     (remhash (seat-proxy seat) *seats*))

    (t
     (format t "~&; Stumpway: unhandled seat event ~A~%" event-name))))

(defun setup-wl-keyboard (seat wl-seat-name)
  "Bind to wl_seat global and get a wl_keyboard for keyboard events."
  (let ((iface-class (wl:find-interface-named "wl_seat")))
    (unless iface-class
      (format t "~&; Stumpway: wl_seat interface not found~%")
      (return-from setup-wl-keyboard))
    (unless *registry*
      (format t "~&; Stumpway: no registry, cannot bind wl_seat~%")
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
  (push (lambda (event-name &rest args)
          (case event-name
            (:keymap nil)
            (:enter nil)
            (:leave nil)
            (:key
             (destructuring-bind (serial time key state) args
               (declare (ignore serial time))
               (let ((keysym (evdev-to-xkb key)))
                 (cond
                   ((eq state :pressed)
                    (keyboard-stop-repeat)
                    (when (and *launcher-active* keysym)
                      (launcher-handle-key keysym nil)
                      (when (member keysym '(65362 65364 65288))
                        (keyboard-start-repeat keysym))))
                   ((eq state :released)
                    (keyboard-stop-repeat))))))
            (:modifiers nil)
            (:repeat-info
             (destructuring-bind (rate delay) args
               (setf *keyboard-repeat-rate* rate
                     *keyboard-repeat-delay* delay)
               (format t "~&; Stumpway: keyboard repeat rate=~A delay=~A~%" rate delay)))))
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
          :name "stumpway-key-repeat")))

(defun keyboard-stop-repeat ()
  "Stop the current repeat thread."
  (setf *keyboard-repeat-stop* t)
  (when *keyboard-repeat-thread*
    (bt:destroy-thread *keyboard-repeat-thread*)
    (setf *keyboard-repeat-thread* nil))
  (setf *keyboard-repeat-key* nil))

(defun evdev-to-xkb (keycode)
  "Convert a Linux evdev keycode to an xkb keysym.
This is a simplified mapping for basic ASCII and special keys.
Evdev keycodes are offset by 8 from xkb keycodes."
  (let ((xkb-keycode (+ keycode 8)))
    ;; Common evdev keycodes (from linux/input-event-codes.h):
    ;; 1=ESC, 15=Tab, 28=Enter, 14=Backspace
    ;; 103=Up, 108=Down, 105=Left, 106=Right
    ;; 11=0, 2-10=1-9, 16-25=Q-P, 30-38=A-L, 39=;, 40=', 41=`, 
    ;; 42=LeftShift, 43=#, 44-50=Z-M, 51=,, 52=., 53=/
    (case keycode
      (1 65307)         ; ESC -> XK_Escape
      (14 65288)        ; Backspace -> XK_BackSpace
      (15 65289)        ; Tab -> XK_Tab
      (28 65293)        ; Enter -> XK_Return
      (103 65362)       ; Up -> XK_Up
      (108 65364)       ; Down -> XK_Down
      (105 65361)       ; Left -> XK_Left
      (106 65363)       ; Right -> XK_Right
      (57 32)           ; Space -> XK_space
      ;; Letters (evdev 16-50 map to a-z, but layout-dependent)
      ;; For QWERTY: 16=q, 17=w, 18=e, 19=r, 20=t, 21=y, 22=u, 23=i, 24=o, 25=p
      ;; 30=a, 31=s, 32=d, 33=f, 34=g, 35=h, 36=j, 37=k, 38=l
      ;; 44=z, 45=x, 46=c, 47=v, 48=b, 49=n, 50=m
      (16 (char-code #\q)) (17 (char-code #\w)) (18 (char-code #\e))
      (19 (char-code #\r)) (20 (char-code #\t)) (21 (char-code #\y))
      (22 (char-code #\u)) (23 (char-code #\i)) (24 (char-code #\o))
      (25 (char-code #\p))
      (30 (char-code #\a)) (31 (char-code #\s)) (32 (char-code #\d))
      (33 (char-code #\f)) (34 (char-code #\g)) (35 (char-code #\h))
      (36 (char-code #\j)) (37 (char-code #\k)) (38 (char-code #\l))
      (44 (char-code #\z)) (45 (char-code #\x)) (46 (char-code #\c))
      (47 (char-code #\v)) (48 (char-code #\b)) (49 (char-code #\n))
      (50 (char-code #\m))
      ;; Numbers (evdev 2-11 map to 1-0)
      (2 (char-code #\1)) (3 (char-code #\2)) (4 (char-code #\3))
      (5 (char-code #\4)) (6 (char-code #\5)) (7 (char-code #\6))
      (8 (char-code #\7)) (9 (char-code #\8)) (10 (char-code #\9))
      (11 (char-code #\0))
      (t nil))))

(defun all-seats ()
  "Return a list of all registered seats."
  (loop for seat being the hash-values of *seats* collect seat))

(defun clear-focus (seat)
  "Clear keyboard focus on SEAT."
  (rwm:river-seat-v1.clear-focus (seat-proxy seat))
  (setf (seat-focused-window seat) nil))
