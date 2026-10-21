;;; Tests for the animation engine (animate.lisp): easing curves,
;;; position interpolation in both modes, state lifecycle, the
;;; frame pump, and the render/glide integration points.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))
(configure '(:animations (:enabled t :duration-ms 200
                         :easing :ease-out-cubic)))

(let ((pass 0) (fail 0) (dirty 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args) (declare (ignore args)) (incf dirty)))

    ;; --- easing curves ---

    (ok "linear easing"
        (mapcar (lambda (k) (ease :linear k)) '(0 0.5 1))
        '(0.0 0.5 1.0))
    (ok "ease-out endpoints"
        (mapcar (lambda (k) (round (* 1000 (ease :ease-out-cubic k))))
                '(0 1))
        '(0 1000))
    (ok "ease-out is fast early"
        (> (ease :ease-out-cubic 0.25) 0.5) t)
    (ok "ease-in-out midpoint"
        (round (* 1000 (ease :ease-in-out 0.5))) 500)
    (ok "easing clamps beyond 1"
        (ease :ease-out-cubic 5) 1.0)

    ;; --- wire safety: positions must be whole pixels ---
    ;; SBCL's internal-real-time is exact rational arithmetic; a
    ;; rational easing leaks fractional positions into river's
    ;; int32 set_position args, which type-errors wayflan MID-REQUEST,
    ;; corrupts the wire, and gets the client terminated.
    (ok "easing never returns rationals"
        (loop for k in (mapcar (lambda (n) (ease :linear (/ n 7)))
                               '(0 1 2 3 5 7))
              always (typep k 'single-float))
        t)
    (let ((win (make-instance 'rhizome-window :proxy :anim-int))
          (*animations* (make-hash-table)))
      (setf (window-x win) 0 (window-y win) 0)
      (animation-start win :from-x 0 :from-y 0
                            :duration 7 :easing :linear)
      (multiple-value-bind (x y)
          (animation-position win 1376 0)
        (ok "interpolated positions are integers"
            (list (typep x 'integer) (typep y 'integer))
            '(t t))))

    ;; --- position interpolation, absolute mode ---

    (let ((win (make-instance 'rhizome-window :proxy :anim-1))
          (*animations* (make-hash-table)))
      (setf (window-x win) 100 (window-y win) 200)
      (animation-start win :duration 1000 :easing :linear)
      (ok "absolute animation is active"
          (animation-active-p win) t)
      (multiple-value-bind (x y)
          (animation-position win 300 400)
        (ok "absolute interpolates from the recorded start"
            (list (round x) (round y)) '(100 200)))
      ;; Time-travel: force t=1 by faking the start into the past
      (let ((anim (gethash win *animations*)))
        ;; internal-real-time counts from PROCESS start — travel
        ;; back by one duration plus a frame, not to epoch zero
        (setf (getf anim :start)
              (- (get-internal-real-time) (getf anim :duration) 10000))
        (setf (gethash win *animations*) anim))
      (multiple-value-bind (x y)
          (animation-position win 300 400)
        (ok "finished animation returns the target"
            (list (round x) (round y)) '(300 400)))
      (ok "finished animation clears its state"
          (animation-active-p win) nil))

    ;; --- relative mode: offset decays to zero ---

    (let ((win (make-instance 'rhizome-window :proxy :anim-2))
          (*animations* (make-hash-table)))
      (setf (window-x win) 500 (window-y win) 50)
      (animation-start win :dx -500 :duration 1000 :easing :linear)
      (multiple-value-bind (x y)
          (animation-position win 500 50)
        (ok "relative starts at the full offset"
            (list (round x) (round y)) '(0 50)))
      (let ((anim (gethash win *animations*)))
        (setf (getf anim :start)
              (- (get-internal-real-time) (getf anim :duration) 10000))
        (setf (gethash win *animations*) anim))
      (multiple-value-bind (x y)
          (animation-position win 500 50)
        (ok "relative offset decays to the exact target"
            (list (round x) (round y)) '(500 50))))

    ;; --- config gates ---

    (ok "default duration 200ms"
        (animation-duration) 200)
    (let ((*config* '(:animations (:enabled nil :duration-ms 200)))
          (win (make-instance 'rhizome-window :proxy :anim-off))
          (*animations* (make-hash-table)))
      (animation-start win :duration 200)
      (ok "disabled animations never start"
          (hash-table-count *animations*) 0))
    (let ((*config* '(:animations (:enabled t :duration-ms 0)))
          (win (make-instance 'rhizome-window :proxy :anim-zero))
          (*animations* (make-hash-table)))
      (animation-start win :duration 0)
      (ok "zero-duration animations never start"
          (hash-table-count *animations*) 0))

    ;; --- the frame pump ---

    (setf dirty 0)
    (let ((*wm-proxy* nil))
      (ok "drain is idle without a compositor"
          (animation-drain) nil))
    (let* ((*wm-proxy* :fake)
           (*animations* (make-hash-table))
           (win (make-instance 'rhizome-window :proxy :anim-3)))
      (ok "drain is idle with nothing animating"
          (animation-drain) nil)
      (animation-start win :duration 500)
      (ok "drain pumps while animating"
          (animation-drain) t)
      (ok "the pump requests fresh sequences"
          (> dirty 0) t))

    ;; --- render integration: float glide captures position ---

    (let* ((*animations* (make-hash-table))
           (*config* '(:animations (:enabled t :duration-ms 200)))
           (win (make-instance 'rhizome-window :proxy :anim-4)))
      (setf (window-x win) 10 (window-y win) 20
            (window-width win) 100 (window-height win) 80)
      (float-window win :x 300 :y 100)
      (ok "float to an explicit position starts an animation"
          (animation-active-p win) t)
      (multiple-value-bind (x y)
          (animation-position win 300 100)
        (ok "float glide interpolates from the tiling position"
            (list (round x) (round y)) '(10 20)))
      (unfloat-window win)
      (ok "unfloat glides back"
          (animation-active-p win) t))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
