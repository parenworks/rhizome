;;; Tests for ribbon columns at the edge of their output: a column
;;; wholly outside the output is not rendered (the render pass hides
;;; it), a column partly outside is clipped to the output's usable
;;; area, and a column back on screen loses its clip. Two fake
;;; outputs side by side, the second carrying the ribbon; river
;;; requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; keep geometry exact
(configure '(:bar (:enabled nil)
             :animations (:enabled nil)
             :pointer (:warp-on-switch nil)))

(let ((pass 0) (fail 0) (calls '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record calls instead of touching river
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders set-window-position
                  message show-message request-manage-dirty
                  recommend-dimension-bounds))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'show-window)
          (lambda (win)
            (push (list :show win) calls)
            (setf (window-hidden-p win) nil))
          (symbol-function 'hide-window)
          (lambda (win)
            (push (list :hide win) calls)
            (setf (window-hidden-p win) t))
          (symbol-function 'set-window-clip-box)
          (lambda (win x y w h)
            (push (list :clip win (list x y w h)) calls)))

    ;; Fresh global state
    (setf *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *seats* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *scroll-states* (make-hash-table :test 'equal)
          *clipped-windows* (make-hash-table :test #'eq)
          *animations* (make-hash-table))

    ;; Left output 1280x800 at x=0; right output 1024x768 at x=1280,
    ;; showing workspace 2 as a ribbon of three 512-wide columns.
    (let* ((left (make-instance 'rhizome-output :proxy :left-proxy
                                :x 0 :y 0 :width 1280 :height 800))
           (right (make-instance 'rhizome-output :proxy :right-proxy
                                 :x 1280 :y 0 :width 1024 :height 768))
           (cols (loop for proxy in '(:c0-proxy :c1-proxy :c2-proxy)
                       collect (make-instance 'rhizome-window
                                              :proxy proxy
                                              :width 512 :height 768)))
           (c0 (first cols)) (c1 (second cols)) (c2 (third cols))
           (state (list :columns cols :scroll-x 0 :column-width 512
                        :prev-layout nil)))
      (setf (gethash :left-proxy *outputs*) left
            (gethash :right-proxy *outputs*) right
            (gethash left *output-workspace*) 1
            (gethash right *output-workspace*) 2)
      (dolist (win cols)
        (setf (gethash (window-proxy win) *windows*) win
              (window-output win) right)
        (assign-window-workspace win 2))
      (setf (gethash (frame-key right 2) *scroll-states*) state)
      (labels ((render-at (scroll-x)
                 ;; STATE already holds :scroll-x, so SETF GETF
                 ;; updates the ribbon's own plist in place
                 (setf calls nil
                       (getf state :scroll-x) scroll-x)
                 (render-layout-for-output right))
               (calls-for (kind win)
                 (loop for c in (reverse calls)
                       when (and (eq (first c) kind) (eq (second c) win))
                         collect (or (third c) t)))
               (clipped-p (win)
                 (and (gethash win *clipped-windows*) t)))

        ;; Viewport at the ribbon's start: c0 and c1 on screen, c2
        ;; wholly past the right edge
        (render-at 0)
        (ok "an on-screen column is shown and unclipped"
            (list (window-hidden-p c0) (calls-for :clip c0) (clipped-p c0))
            '(nil nil nil))

        ;; Scrolled by one column: c0 sits wholly on the left output
        (render-at 512)
        (ok "a column wholly outside its output is hidden"
            (list (window-hidden-p c0) (calls-for :hide c0))
            '(t (t)))
        (ok "columns inside the output stay shown and unclipped"
            (list (window-hidden-p c1) (window-hidden-p c2)
                  (calls-for :clip c1) (calls-for :clip c2))
            '(nil nil nil nil))

        ;; Scrolled by half a column: c0 and c2 straddle the edges
        (render-at 256)
        (ok "a column partly outside comes back into view"
            (list (window-hidden-p c0) (calls-for :show c0))
            '(nil (t)))
        (ok "a column across the left edge is clipped to its output"
            (list (calls-for :clip c0) (clipped-p c0))
            '(((256 0 256 768)) t))
        (ok "a column across the right edge is clipped to its output"
            (list (calls-for :clip c2) (clipped-p c2))
            '(((0 0 256 768)) t))
        (ok "a column wholly inside its output takes no clip"
            (list (calls-for :clip c1) (clipped-p c1))
            '(nil nil))

        (render-at 256)
        (ok "an unchanged render sends no clip, show, or hide requests"
            calls nil)

        ;; Back to the start: c0 is whole again, c2 leaves the output
        (render-at 0)
        (ok "scrolling a clipped column back on screen clears its clip"
            (list (calls-for :clip c0) (clipped-p c0))
            '(((0 0 0 0)) nil))
        (ok "a clipped column scrolled wholly off is hidden"
            (list (window-hidden-p c2) (calls-for :hide c2))
            '(t (t)))
        (render-at 512)
        (ok "a hidden column that returns whole is shown with its clip cleared"
            (list (window-hidden-p c2) (calls-for :clip c2) (clipped-p c2))
            '(nil ((0 0 0 0)) nil))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
