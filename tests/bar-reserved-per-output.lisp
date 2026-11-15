;;; bar-reserved-per-output.lisp — the layout reserves bar space
;;; PER OUTPUT: an output whose zone shows the bar reserves it, one
;;; without (hidden, no report) does not, and the no-argument form
;;; keeps the any-output semantics. Two fake outputs, stubbed zone
;;; table.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled t :margin 4 :height 28 :corner-radius 10)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                          name actual expected)))))
    (let ((a (make-instance 'rhizome-output :proxy :fake-a))
          (b (make-instance 'rhizome-output :proxy :fake-b)))
      ;; both 1080 tall
      (setf (slot-value a 'height) 1080
            (slot-value b 'height) 1080)
      ;; fresh zone table
      (clrhash *bar-non-exclusive-heights*)

      ;; no reports at all: config fallback, reserved on both
      (ok "no zone reports: output A reserves"
          (bar-reserved-height a) 36)
      (ok "no zone reports: output B reserves"
          (bar-reserved-height b) 36)
      (ok "no zone reports: any-output form reserves"
          (bar-reserved-height) 36)

      ;; A reports a full exclusive zone (non-exclusive height 0);
      ;; B reports no zone (bar hidden there: non-exclusive = full)
      (setf (gethash a *bar-non-exclusive-heights*) 0
            (gethash b *bar-non-exclusive-heights*) 1080)
      (ok "zone on A: A reserves"
          (bar-reserved-height a) 36)
      (ok "bar hidden on B: B reserves nothing"
          (bar-reserved-height b) 0)
      (ok "any-output form: some output shows it"
          (bar-reserved-height) 36)

      ;; B's bar appears (zone at 0): both reserve now
      (setf (gethash b *bar-non-exclusive-heights*) 0)
      (ok "zone on both: B reserves"
          (bar-reserved-height b) 36)

      ;; bar disabled entirely: nothing anywhere
      (configure '(:bar (:enabled nil)))
      (ok "disabled: A reserves nothing"
          (bar-reserved-height a) 0)
      (configure '(:bar (:enabled t :margin 4 :height 28)))

      ;; cleanup
      (clrhash *bar-non-exclusive-heights*))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (when (plusp fail)
      (uiop:quit 1))))
