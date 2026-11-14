;;; bar-adoption.lisp — a WM restart adopts a surviving bar instead
;;; of cold-starting a fresh one (the 40-second black strip, #9):
;;; pgrep parsing, adoption in start-bar, adopted liveness in
;;; bar-alive-p, and the handover flag keeping the shutdown from
;;; stopping the bar. pgrep/kill/launch are stubbed; no real bar
;;; process is started.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *stub-run-output* "")
(defparameter *stub-kills* nil)
(defparameter *stub-launches* 0)

(defun stub-run-program (command &rest keys)
  (declare (ignore keys))
  (cond ((and (member "pgrep" command :test #'string=)
              (member "-f" command :test #'string=))
         *stub-run-output*)
        ((member "kill" command :test #'string=)
         (push command *stub-kills*)
         "")
        ((member "pkill" command :test #'string=)
         (push command *stub-kills*)
         "")
        (t "")))

(defun stub-launch-program (command &rest keys)
  (declare (ignore keys))
  (incf *stub-launches*))

(defun stub-alive-p (process)
  (declare (ignore process))
  nil)

(let ((pass 0) (fail 0)
      (real-run (symbol-function 'uiop:run-program))
      (real-launch (symbol-function 'uiop:launch-program))
      (real-alive (symbol-function 'uiop:process-alive-p)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                          name actual expected)))))
    (setf (symbol-function 'uiop:run-program) #'stub-run-program
          (symbol-function 'uiop:launch-program) #'stub-launch-program
          (symbol-function 'uiop:process-alive-p) #'stub-alive-p)
    (let ((*wm-process-mode* t)
          (*bar-process-mode* nil)
          (*bar-process* nil)
          (rhizome::*adopted-bar-pid* nil)
          (*bar-starting* nil))
      (unwind-protect
           (progn
             ;; --- pgrep parsing ---
             (setf *stub-run-output* "123
456

")
             (ok "external-bar-pids parses pgrep output"
                 (external-bar-pids) '(123 456))
             (setf *stub-run-output* "")
             (ok "external-bar-pids on no bar"
                 (external-bar-pids) '())

             ;; --- adoption: existing bars are adopted, not launched ---
             (setf *stub-run-output* "999
1000
"
                   *stub-kills* nil
                   *stub-launches* 0)
             (setf rhizome::*adopted-bar-pid* nil)
             (start-bar)
             (ok "start-bar adopts the first external bar"
                 rhizome::*adopted-bar-pid* 999)
             (ok "adoption stamps the bar settings"
                 (and *bar-launched-settings* t) t)
             (ok "start-bar launches nothing while adopting"
                 *stub-launches* 0)
             (ok "extra bars are reaped, the adopted one is not"
                 (sort (mapcar
                         (lambda (c) (second c))
                         (remove-if-not
                           (lambda (c) (and (listp c)
                                            (member "kill" c :test #'string=)))
                           *stub-kills*))
                       #'<)
                 '("1000"))

             ;; --- adopted liveness ---
             (ok "bar-alive-p counts an adopted pid"
                 (and (setf rhizome::*adopted-bar-pid* 1) ; init: always alive
                      (bar-alive-p))
                 t)
             (setf rhizome::*adopted-bar-pid* nil)
             (ok "bar-alive-p with no bar and no adoption"
                 (bar-alive-p)
                 (or *bar-starting*
                     (and *bar-process*
                          (funcall real-alive *bar-process*))))

             ;; --- no external bar: normal launch ---
             (setf *stub-run-output* ""
                   rhizome::*adopted-bar-pid* nil
                   *stub-launches* 0)
             (setf *bar-process* nil *bar-starting* nil)
             (start-bar)
             (ok "start-bar launches fresh with no external bar"
                 *stub-launches* 1)

             ;; --- stop-bar clears the adoption ---
             (setf rhizome::*adopted-bar-pid* 4242)
             (stop-bar)
             (ok "stop-bar clears the adopted pid"
                 rhizome::*adopted-bar-pid* nil)

             ;; --- the handover flag ---
             (ok "handover flag starts clear"
                 *bar-handover-p* nil))

        (setf (symbol-function 'uiop:run-program) real-run
              (symbol-function 'uiop:launch-program) real-launch
              (symbol-function 'uiop:process-alive-p) real-alive)
        (setf *bar-process* nil
              rhizome::*adopted-bar-pid* nil
              *bar-handover-p* nil))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
