;;; Main entry point for Rhizome
;;;
;;; Usage:
;;;   1. Start river (nested in WSLg or on a TTY)
;;;   2. Run: sbcl --load rhizome.asd --eval '(rhizome:run)'
;;;
;;; Or as a script:
;;;   ./rhizome.lisp
;;;
;;; Configure river to send commands:
;;;   riverctl send-layout-cmd rhizome "switch-layout :even"
;;;   riverctl send-layout-cmd rhizome "split-horizontal"

(in-package #:rhizome)

(defun main ()
  "Entry point for standalone execution."
  (format t "~&; Rhizome: River Wayland layout generator~%")
  (format t "~&; Rhizome: Connecting to river compositor...~%")
  (handler-case
      (run)
    (error (e)
      (format t "~&; Rhizome: fatal error: ~A~%" e)
      (uiop:quit 1)))
  (uiop:quit 0))

;;; Allow running as a script with: sbcl --script rhizome.lisp
#+sbcl
(when (eq (sb-ext:posix-getenv "RHIZOME_SCRIPT") "1")
  (main))
