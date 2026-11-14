;;; Tests for pull-hidden-next: hidden windows cycle in window-number
;;; order whatever order the workspace lists them in. River-touching
;;; functions are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (messages '()) (listing '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    (dolist (fn '(request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (push (apply #'format nil fmt args) messages)
            nil))
    ;; The workspace listing order is the fixture: never number order
    (setf (symbol-function 'windows-on-workspace)
          (lambda (output workspace)
            (declare (ignore output workspace))
            listing))

    (setf *seats* (make-hash-table)
          (gethash :ph-seat *seats*)
          (make-instance 'rhizome-seat :proxy :ph-seat))

    (labels ((fresh ()
               (setf *windows* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *window-number-map* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     messages '()))
             (setup (out specs order focused)
               ;; SPECS: (name number) in frame push order; the last
               ;; pushed is the frame's front window. ORDER: names in
               ;; the order windows-on-workspace returns them.
               (fresh)
               (setf (gethash out *output-workspace*) 1)
               (let ((frame (current-frame-for out 1))
                     (wins '()))
                 (dolist (spec specs)
                   (let ((w (make-instance 'rhizome-window
                                           :proxy (first spec))))
                     (setf (gethash (first spec) *windows*) w
                           (window-output w) out)
                     (assign-window-workspace w 1)
                     (when (second spec)
                       (setf (gethash w *window-number-map*) (second spec)))
                     (frame-push-window w frame)
                     (push (cons (first spec) w) wins)))
                 (setf listing (mapcar (lambda (n) (cdr (assoc n wins)))
                                       order))
                 (dolist (seat (all-seats))
                   (focus-window (cdr (assoc focused wins)) seat))))
             (shown ()
               (let ((w (find-if #'window-focused-p (all-windows))))
                 (and w (window-proxy w))))
             (walk (out delta steps)
               (loop repeat steps
                     collect (progn (pull-hidden-next out 1 delta)
                                    (shown)))))

      (let ((out (make-instance 'rhizome-output :proxy :ph-out
                                :width 1920 :height 1080)))

        ;; fa(1) fb(2) fc(3) in one frame, listed (fb fa fc), fc shown
        (setup out '((:fa 1) (:fb 2) (:fc 3)) '(:fb :fa :fc) :fc)
        (ok "next from fc walks fa, fb, fc"
            (walk out 1 3) '(:fa :fb :fc))
        (setup out '((:fa 1) (:fb 2) (:fc 3)) '(:fb :fa :fc) :fc)
        (ok "previous from fc walks fb, fa, fc"
            (walk out -1 3) '(:fb :fa :fc))
        (setup out '((:fa 1) (:fb 2) (:fc 3)) '(:fc :fb :fa) :fa)
        (ok "next from fa walks fb, fc, fa (listing reversed)"
            (walk out 1 3) '(:fb :fc :fa))
        (setup out '((:fa 1) (:fb 2) (:fc 3)) '(:fc :fa :fb) :fb)
        (ok "previous from fb walks fa, fc, fb"
            (walk out -1 3) '(:fa :fc :fb))

        ;; One hidden window: the two alternate in both directions
        (setup out '((:fa 1) (:fb 2)) '(:fb :fa) :fb)
        (ok "single hidden: next alternates" (walk out 1 2) '(:fa :fb))
        (setup out '((:fa 1) (:fb 2)) '(:fb :fa) :fb)
        (ok "single hidden: previous alternates" (walk out -1 2) '(:fa :fb))

        ;; No hidden window: a message, focus unchanged
        (setup out '((:fa 1)) '(:fa) :fa)
        (pull-hidden-next out 1 1)
        (ok "none hidden: says so" (first messages) "no hidden windows")
        (ok "none hidden: focus stays" (shown) :fa)

        ;; Unnumbered windows follow the numbered ones, in listing order
        (setup out '((:fx nil) (:fa 1) (:fy nil) (:fc 3))
               '(:fy :fc :fx :fa) :fc)
        (ok "unnumbered: next from fc walks fy, fx, fa, fc"
            (handler-case (walk out 1 4)
              (error (e) (princ-to-string e)))
            '(:fy :fx :fa :fc))
        (setup out '((:fx nil) (:fa 1) (:fy nil) (:fc 3))
               '(:fy :fc :fx :fa) :fc)
        (ok "unnumbered: previous from fc walks fa, fx, fy, fc"
            (handler-case (walk out -1 4)
              (error (e) (princ-to-string e)))
            '(:fa :fx :fy :fc))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
