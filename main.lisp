;;; Main entry point for Stumpway
;;;
;;; Usage:
;;;   1. Start river (nested in WSLg or on a TTY)
;;;   2. Run: sbcl --load stumpway.asd --eval '(stumpway:run)'
;;;
;;; Or as a script:
;;;   ./stumpway.lisp
;;;
;;; Configure river to send commands:
;;;   riverctl send-layout-cmd stumpway "switch-layout :even"
;;;   riverctl send-layout-cmd stumpway "split-horizontal"

(in-package #:stumpway)

(defun main ()
  "Entry point for standalone execution."
  (format t "~&; Stumpway: River Wayland layout generator~%")
  (format t "~&; Stumpway: Connecting to river compositor...~%")
  (handler-case
      (run)
    (error (e)
      (format t "~&; Stumpway: fatal error: ~A~%" e)
      (uiop:quit 1)))
  (uiop:quit 0))

;;; Allow running as a script with: sbcl --script stumpway.lisp
#+sbcl
(when (eq (sb-ext:posix-getenv "STUMPWAY_SCRIPT") "1")
  (main))
