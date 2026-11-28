(ql:quickload :rhizome)
(in-package #:rhizome)
(configure '(:bar (:enabled nil)))

(defparameter *fow-pass* 0)
(defparameter *fow-fail* 0)
(defun fow-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *fow-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *fow-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(let ((a (make-instance 'rhizome-output :proxy :fow-a))
      (b (make-instance 'rhizome-output :proxy :fow-b)))
  (setf (output-x a) 0 (output-y a) 0
        (output-width a) 1920 (output-height a) 1080
        (output-x b) 1920 (output-y b) 0
        (output-width b) 2560 (output-height b) 1440)
  (clrhash *outputs*)
  (setf (gethash :fow-a *outputs*) a
        (gethash :fow-b *outputs*) b)
  (let ((seat (make-instance 'rhizome-seat :proxy :fow-seat)))
    (setf (seat-pointer-x seat) 500
          (seat-pointer-y seat) 400)
    (clrhash rhizome::*seats*)
    (setf (gethash :fow-seat rhizome::*seats*) seat)
    ;; pointer on output A
    (fow-ok "pointer on A: window goes to A"
            (eq (find-output-for-window nil) a) t)
    ;; pointer on output B
    (setf (seat-pointer-x seat) 2500)
    (fow-ok "pointer on B: window goes to B"
            (eq (find-output-for-window nil) b) t)
    ;; pointer between outputs
    (setf (seat-pointer-x seat) 1900)
    (fow-ok "pointer on boundary: goes to A"
            (eq (find-output-for-window nil) a) t))
  (clrhash *outputs*))

(format t "~&=== ~A passed, ~A failed ===~%" *fow-pass* *fow-fail*)
(when (plusp *fow-fail*) (uiop:quit 1))
