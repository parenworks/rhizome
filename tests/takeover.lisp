;;; Tests for WM hot-swap takeover state (takeover.lisp): the
;;; serialize/restore round-trip, per-window placement restore,
;;; number translation for the frame trees, settle detection, and
;;; consumption. River-touching functions are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls nil))
  (labels ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (stub (fn &optional (record t))
           (setf (symbol-function fn)
                 (lambda (&rest args)
                   (when record (push (cons fn args) calls))
                   (case fn
                     (show-window (setf (window-hidden-p (first args)) nil))
                     (hide-window (setf (window-hidden-p (first args)) t))
                     (float-window (setf (window-floating-p (first args)) t))
                     (otherwise nil)))))
         (reset ()
           (setf calls nil
                 *windows* (make-hash-table)
                 *output-workspace* (make-hash-table)
                 *window-workspace* (make-hash-table)
                 *minimized-windows* (make-hash-table)
                 *window-number-map* (make-hash-table)
                 *takeover-state* nil
                 *takeover-manage-passes* 0)
           (stub 'float-window)
           (stub 'request-manage-dirty)
           (stub 'focus-window)
           (stub 'assign-window-workspace nil)
           ;; assign-window-workspace must actually stamp
           (setf (symbol-function 'assign-window-workspace)
                 (lambda (win ws)
                   (setf (gethash win *window-workspace*) ws)))))

    ;; --- round-trip: write then read ---
    (let* ((*takeover-state-path* #P"/tmp/rhizome-test-takeover.sexp")
           (*workspace-count* 4)
           (out (make-instance 'rhizome-output :proxy :tk-out
                               :width 800 :height 600)))
      (setf (output-wl-output-name out) "DP-1")
      (reset)
      (setf (gethash :tk-out *outputs*) out
            (gethash out *output-workspace*) 3)
      ;; two windows with identifiers, numbers, geometry
      (let ((w1 (make-instance 'rhizome-window :proxy :tk-w1))
            (w2 (make-instance 'rhizome-window :proxy :tk-w2)))
        (setf (gethash :tk-w1 *windows*) w1
              (gethash :tk-w2 *windows*) w2
              (window-identifier w1) "app-one"
              (window-identifier w2) "app-two"
              (gethash w1 *window-number-map*) 1
              (gethash w2 *window-number-map*) 2
              (gethash w1 *window-workspace*) 2
              (gethash w2 *window-workspace*) 3
              (window-floating-p w2) t
              (window-float-x w2) 40 (window-float-y w2) 50
              (window-float-width w2) 200 (window-float-height w2) 100)
        (ignore-errors (delete-file *takeover-state-path*))
        ;; trees: write-takeover-state needs frame trees — with no
        ;; frame trees it serializes none (tree-for may return nil)
        (write-takeover-state)
        (ok "takeover file written"
            (and (probe-file *takeover-state-path*) t) t)
        (load-takeover-state)
        (ok "state loaded from disk"
            (and *takeover-state* t) t)
        (ok "output workspace round-trips"
            (cdr (assoc "DP-1" (getf *takeover-state* :outputs)
                        :test #'equal))
            3)
        (ok "window entries round-trip (identifier-keyed)"
            (sort (mapcar #'car (getf *takeover-state* :windows))
                  #'string<)
            '("app-one" "app-two"))
        (ok "float geometry round-trips"
            (getf (cdr (assoc "app-two"
                              (getf *takeover-state* :windows)
                              :test #'equal))
                  :float-x)
            40))

      ;; --- per-window placement restore (the successor WM) ---
      (let ((win (make-instance 'rhizome-window :proxy :tk-w3)))
        (setf (gethash :tk-w3 *windows*) win
              (window-identifier win) "app-two")
        (restore-takeover-placement win)
        (ok "takeover placement restores workspace"
            (window-workspace win) 3)
        (ok "takeover placement floats with geometry"
            (list (window-floating-p win)
                  (window-float-x win) (window-float-width win))
            '(t 40 200))
        (ok "placement stamped the output"
            (and (window-output win) t) t))

      ;; --- number translation for the trees ---
      (let ((number-map (make-hash-table)))
        (setf (gethash 1 number-map) 7   ; old 1 → new 7
              (gethash 2 number-map) 9)  ; old 2 → new 9
        (let ((tree '(:split :horizontal (:rect 0 0 800 300)
                       (:frame (:rect 0 0 400 300) 1 5)
                       (:frame (:rect 400 0 400 300) 2))))
          (let ((translated (translate-tree-numbers tree number-map)))
            (ok "tree leaves translate through the map"
                (fourth translated) '(:frame (:rect 0 0 400 300) 7))
            (ok "mapped numbers translate on the right too"
                (fifth translated) '(:frame (:rect 400 0 400 300) 9))
            (ok "unknown numbers vanish from frames"
                (translate-tree-numbers
                  '(:frame (:rect 0 0 100 100) 1 42) number-map)
                '(:frame (:rect 0 0 100 100) 7)))))

      ;; --- settle detection ---
      (setf *takeover-manage-passes* 0
            *takeover-state* (list :windows '((("app-one") . nil))))
      (ok "not settled while the window is missing"
          (takeover-settled-p) nil)
      (ok "pass counter advances"
          *takeover-manage-passes* 1)

      ;; --- consumption ---
      (consume-takeover-state)
      (ok "consume clears the state and deletes the file"
          (list *takeover-state* (probe-file *takeover-state-path*))
          '(nil nil)))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
