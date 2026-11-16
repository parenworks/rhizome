;;; output-hotplug.lisp - P4: when an output goes away, its windows
;;; migrate to a survivor (workspace number and float geometry kept,
;;; frame trees of the dead output dropped); with no survivor, the
;;; windows are let go.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *oh-pass* 0)
(defparameter *oh-fail* 0)

(defun oh-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *oh-pass*)
             (format t "~&PASS: ~A~%" name))
      (progn (incf *oh-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                     name actual expected))))

(let ((a (make-instance 'rhizome-output :proxy :oh-a))
      (b (make-instance 'rhizome-output :proxy :oh-b)))
  (setf (output-wl-output-name a) "eDP-2"
        (output-wl-output-name b) "DP-1")
  ;; two outputs registered
  (clrhash *outputs*)
  (setf (gethash :oh-a *outputs*) a
        (gethash :oh-b *outputs*) b)
  ;; two windows on output A
  (let ((w1 (make-instance 'rhizome-window))
        (w2 (make-instance 'rhizome-window)))
    (setf (window-output w1) a
          (window-output w2) a)
    (assign-window-workspace w1 2)
    (assign-window-workspace w2 3)
    (clrhash *windows*)
    (setf (gethash :oh-w1 *windows*) w1
          (gethash :oh-w2 *windows*) w2)
    (let ((dirty 0))
      (setf (symbol-function 'request-manage-dirty)
            (lambda (&rest r) (declare (ignore r)) (incf dirty)))
      (unwind-protect
           (progn
             ;; simulate the :removed event for output A
             (apply #'handle-output-event a :removed nil)
             (oh-ok "windows migrated to the survivor"
                    (list (window-output w1) (window-output w2))
                    (list b b))
             (oh-ok "workspace numbers kept through migration"
                    (list (window-workspace w1) (window-workspace w2))
                    '(2 3))
             (oh-ok "the dead output is unregistered"
                    (gethash :oh-a *outputs*) nil)
             (oh-ok "the survivor stays"
                    (and (gethash :oh-b *outputs*) t) t)
             ;; removing the LAST output: nothing to migrate to,
             ;; no crash
             (apply #'handle-output-event b :removed nil)
             (oh-ok "last output removal: no crash"
                    (gethash :oh-b *outputs*) nil))
        (setf (symbol-function 'request-manage-dirty) #'request-manage-dirty)
        (clrhash *windows*)
        (clrhash *outputs*)))))

(format t "~&=== ~A passed, ~A failed ===~%" *oh-pass* *oh-fail*)
(when (plusp *oh-fail*)
  (uiop:quit 1))
