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
;;; manager's greeter via river_window_manager_v1.exit_session
;;; (protocol >= 4) — the compositor's own session-exit path:
;;; river disconnects all clients, sends the WM its finished event,
;;; and shuts down cleanly. Rivers bound below v4 fall back to
;;; SIGTERM, which river treats as a clean shutdown request.

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

(defun logout-method ()
  "The logout mechanism available on this compositor:
:exit-session (river_window_manager_v1 v4+) or :sigterm."
  (if (and *wm-proxy* (>= *wm-proxy-version* 4))
      :exit-session
      :sigterm))

(defun logout-session ()
  "End the river session: the display manager's greeter returns.
exit_session is the protocol's own way out (the XML: 'window
managers should only make this request if the user explicitly asks
to exit the Wayland session' — which is exactly what the
logout-confirm double-press guards). Pre-v4 rivers get SIGTERM,
river's clean shutdown signal; the WM exits on the finished event
either way."
  (message "logging out")
  (case (logout-method)
    (:exit-session
     (rwm:river-window-manager-v1.exit-session *wm-proxy*)
     t)
    (t
     (if (uiop:launch-program (list "pkill" "-TERM" "-x" "river"))
         t
         (progn (message "cannot find a way to log out") nil)))))

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
