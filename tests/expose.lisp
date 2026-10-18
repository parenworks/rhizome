;;; Tests for expose (C-t Return): grid geometry, exact layout
;;; restoration, digit/arrow/Return/click picking.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; River-touching stubs
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'show-message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'focus-window)
          (lambda (win &rest seats)
            (declare (ignore seats))
            ;; mirror the real focus semantics enough for expose
            (dolist (w (all-windows))
              (unless (eq w win)
                (setf (window-focused-p w) nil)))
            (setf (window-focused-p win) t)))
    (setf (symbol-function 'exit-prefix-mode)
          (lambda ()
            (expose-restore-if-active)
            (setf *prefix-mode-active* nil
                  *prefix-command-mode* nil
                  *iresize-active* nil
                  *expose-active* nil)))

    ;; Fresh state (and one seat, so focused-state changes apply)
    (setf *seats* (make-hash-table)
          (gethash :expose-seat *seats*)
          (make-instance 'rhizome-seat :proxy :expose-seat))
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *marked-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *expose-active* nil
          *expose-saved* nil
          *expose-order* nil
          *expose-prev-focus* nil)

    (let* ((out (make-instance 'rhizome-output :proxy :exp-out
                              :width 1920 :height 1080))
           (wins (loop for i below 4
                       for proxy = (intern (format nil "E~D" i) :keyword)
                       collect (make-instance 'rhizome-window
                                               :proxy proxy))))
      (setf (gethash out *output-workspace*) 3)
      (dolist (win wins)
        (setf (gethash (window-proxy win) *windows*) win
              (window-output win) out
              (window-width win) 800
              (window-height win) 600)
        (assign-window-workspace win 3))
      ;; Two frames: w0 w1 in left, w2 in right (w1 hidden, monocle)
      (set-workspace-tree out 3
                          '((0 0 960 1060) (960 0 960 1060))
                          (list (first wins) (second wins) (third wins)))
      (let ((leaves (frame-leaves (frame-tree-for out 3))))
        (setf (gethash (frame-key out 3) *current-frames*) (first leaves))
        (frame-remove-window (second wins))
        (frame-push-window (second wins) (first leaves))  ; hidden: second
        ;; w3 is a pre-existing float
        (float-window (fourth wins) :x 100 :y 200 :width 300 :height 200)
        (setf (window-focused-p (first wins)) t))

      ;; --- Grid geometry ---

      (let ((placements (expose-grid out 3)))
        (ok "grid covers every workspace window"
            (sort (mapcar (lambda (p) (window-proxy (first p))) placements)
                  #'string<)
            '(:E0 :E1 :E2 :E3))
        ;; 4 windows -> 2x2 cells of 960x540 (bar disabled in tests);
        ;; 800x600 aspect-fit: scale = min(952/800, 532/600) -> 709x532
        (let ((p0 (first placements))
              (p2 (third placements)))
          (ok "aspect-fitted cell size"
              (list (fourth p0) (fifth p0)) '(709 532))
          (ok "fitted window centered in its cell"
              (second p0) 125)
          (ok "row 2 starts below row 1"
              (third p2) 544)))

      ;; --- Enter: everything floats into the grid ---

      (ok "expose-begin succeeds"
          (expose-begin out 3) t)
      (ok "expose floats every window"
          (count-if #'window-floating-p wins) 4)
      (ok "expose order is row-major"
          (mapcar #'window-proxy *expose-order*)
          '(:E0 :E1 :E2 :E3))
      (ok "expose remembers the pre-expose focus"
          (window-proxy *expose-prev-focus*) :E0)
      ;; grid float coords: first cell, second row x offset present
      (ok "floats placed inside the content area"
          (and (>= (window-float-x (first wins)) 0)
               (>= (window-float-y (first wins)) 0)
               (< (+ (window-float-y (fourth wins))
                     (window-float-height (fourth wins)))
                  1080))
          t)

      ;; --- Digit pick ---

      ;; pick '2' = second window in grid order
      (setf *expose-active* t
            *prefix-mode-active* t
            *prefix-expiry* (+ (get-universal-time) 60))
      (expose-handle-key 50)                       ; keysym '2'
      (ok "digit pick focuses the picked window"
          (and (window-focused-p (second wins)) t) t)
      (ok "digit pick restores float flags"
          (mapcar #'window-floating-p wins)
          '(nil nil nil t))
      (ok "digit pick ends expose"
          (list *expose-active* *expose-order*) '(nil nil))
      ;; frame membership restored exactly, including the hidden one
      (let ((leaves (frame-leaves (frame-tree-for out 3))))
        (ok "frame order restored exactly"
            (list (mapcar #'window-proxy (frame-windows (first leaves)))
                  (mapcar #'window-proxy (frame-windows (second leaves))))
            '((:E1 :E0) (:E2)))
        (ok "hidden window back in its frame"
            (window-frame (second wins)) (first leaves))
        (ok "picked tiled window back in its frame"
            (window-frame (second wins)) (first leaves)))
      ;; float geometry restored for the pre-existing float
      (ok "float geometry restored"
          (list (window-float-x (fourth wins))
                (window-float-y (fourth wins))
                (window-float-width (fourth wins))
                (window-float-height (fourth wins)))
          '(100 200 300 200))

      ;; --- Arrow + Return picking ---

      (expose-begin out 3)
      (setf *expose-active* t
            *prefix-mode-active* t
            *prefix-expiry* (+ (get-universal-time) 60))
      ;; baseline focus after the digit-pick section is E1 (grid pos 1)
      (expose-handle-key 65363)          ; Right -> pos 2 (E2)
      (ok "arrow moves focus through the grid"
          (and (window-focused-p (third wins)) t) t)
      (expose-handle-key 65293)          ; Return
      (ok "Return picks the focused window"
          (and (window-focused-p (third wins))
               (null *expose-active*))
          t)

      ;; --- Click pick ---

      (expose-begin out 3)
      (setf *expose-active* t
            *prefix-mode-active* t
            *prefix-expiry* (+ (get-universal-time) 60))
      (expose-clicked (fourth wins))
      (ok "click picks the window"
          (and (window-focused-p (fourth wins))
               (null *expose-active*))
          t)

      ;; --- Escape restores everything including focus ---

      (expose-begin out 3)
      (setf *expose-active* t
            *prefix-mode-active* t
            *prefix-expiry* (+ (get-universal-time) 60))
      (expose-restore-if-active)
      ;; baseline: the click-pick section left focus on E3
      (ok "escape-level restore returns pre-expose focus"
          (and (window-focused-p (fourth wins)) t) t)
      (ok "restore puts the float back where it was"
          (list (window-float-x (fourth wins))
                (window-floating-p (fourth wins)))
          '(100 t))

      ;; --- Too few windows ---

      ;; --- Expose guards the frame tree against pruning ---
      ;; A manage pass can arrive mid-expose (request-manage-dirty
      ;; from expose-begin): every window is floated off its frame,
      ;; and pruning the now-empty tree would destroy the layout
      ;; expose-restore is about to put back.
      (expose-begin out 3)
      (setf *expose-active* t)
      (frames-manage-decisions out 3 '())
      (ok "manage pass mid-expose does not collapse the tree"
          (>= (length (frame-leaves (frame-tree-for out 3))) 2)
          t)
      (expose-restore (first wins))
      (ok "layout survives an interleaved manage pass"
          (mapcar #'window-proxy
                  (sort (copy-list (all-windows)) #'string<
                        :key #'window-proxy))
          '(:E0 :E1 :E2 :E3))

      (remhash :E1 *windows*)
      (remhash :E2 *windows*)
      (remhash :E3 *windows*)
      (ok "single window does not expose"
          (expose-begin out 3) nil)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
