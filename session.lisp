;;; Session management for Rhizome: lock, idle lock, logout
;;;
;;; The locker is a configurable external program (swaylock by
;;; default — `sudo pacman -S swaylock`; hyprlock, waylock or any
;;; ext-session-lock-v1 client work too), configured under (:lock ...):
;;;
;;;   (rhizome:configure
;;;     '(:lock (:command "swaylock"                  ; the locker
;;;              :args ("--daemonize" "--effect-blur" "10x3")
;;;              :idle-seconds 600)))                  ; swayidle auto-lock
;;;
;;; Logout ends the river session and returns to the display
;;; manager's greeter. River 0.4.x ships no riverctl, so rhizome
;;; sends the compositor SIGTERM — river then sends the WM its
;;; finished event and both shut down cleanly.

(in-package #:rhizome)

(defparameter *idle-locker-process* nil
  "The running swayidle process, if idle locking is configured.")

(defun lock-command ()
  "The configured locker command line (program + args)."
  (let ((program (config-getf '(:lock) :command :default "swaylock"))
        (args (config-getf '(:lock) :args :default nil)))
    (if (listp program) program (list* program args))))

(defun lock-screen ()
  "Lock the session with the configured locker. Nothing happens until
a locker is installed (sudo pacman -S swaylock)."
  (message "locking")
  (ignore-errors
    (uiop:launch-program
      (list "bash" "-c"
            (format nil "~{~A~^ ~}" (lock-command)))))
  t)

(defun start-idle-locker ()
  "Spawn swayidle to lock the session after (:lock :idle-seconds)
of inactivity. Called at WM startup; config changes need a restart."
  (let ((secs (config-getf '(:lock) :idle-seconds :default 0)))
    (when (and (plusp secs)
               (null *idle-locker-process*))
      (setf *idle-locker-process*
            (ignore-errors
              (uiop:launch-program
                (list "swayidle" "-w"
                      "timeout" (format nil "~A" secs)
                      (format nil "~{~A~^ ~}" (lock-command))))))
      (format t "~&; Rhizome: idle locker armed (~As)~%" secs))))

(defparameter *logout-armed-at* 0
  "Universal time logout was last armed (logout-confirm).")

(defun logout-session ()
  "End the river session: the display manager's greeter returns.
Prefers riverctl exit (river < 0.4 ships it); river 0.4.x sends the
compositor SIGTERM, which triggers its clean shutdown path (the WM
receives the finished event and exits with it)."
  (message "logging out")
  (if (uiop:launch-program (list "bash" "-c"
                                 "command -v riverctl >/dev/null \
                                  && riverctl exit \
                                  || pkill -TERM -x river"))
      t
      (progn (message "cannot find a way to log out") nil)))

(defun logout-confirm ()
  "Log out only on a second press within 3 seconds."
  (let ((now (get-universal-time)))
    (if (> *logout-armed-at* (- now 3))
        (progn
          (setf *logout-armed-at* 0)
          (logout-session))
        (progn
          (setf *logout-armed-at* now)
          (message "Press again to log out")))))
