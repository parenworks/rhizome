;;; Tests for where a floating window lands when it returns to the
;;; frame tiling (floating.lisp unfloat-window). River requests are
;;; stubbed.
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

    ;; Record nothing; keep river out of the run. show/hide update state.
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders message request-manage-dirty
                  recommend-dimension-bounds))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'show-window)
          (lambda (win &rest args)
            (declare (ignore args))
            (setf (window-hidden-p win) nil)))
    (setf (symbol-function 'hide-window)
          (lambda (win &rest args)
            (declare (ignore args))
            (setf (window-hidden-p win) t)))

    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal))

    (labels ((mk-out (proxy &optional (x 0))
               ;; A 1920x1080 output at X showing workspace 1
               (let ((out (make-instance 'rhizome-output :proxy proxy :x x
                                         :width 1920 :height 1080)))
                 (setf (gethash out *output-workspace*) 1)
                 out))
             (mk-win (proxy out)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win 1)
                 win))
             (root-win (proxy out)
               (let ((win (mk-win proxy out)))
                 (frame-push-window win (frame-tree-for out 1))
                 win))
             (float-at (proxy out x y)
               ;; A focused 400x300 float with its top-left at X Y
               (let ((win (mk-win proxy out)))
                 (float-window win :x x :y y :width 400 :height 300
                                   :animate nil)
                 win))
             (focus-only (win)
               (maphash (lambda (k w)
                          (declare (ignore k))
                          (setf (window-focused-p w) (eq w win)))
                        *windows*))
             (leaves (out)
               (frame-leaves (frame-tree-for out 1)))
             (grid (out a)
               ;; Split A's root into four quarters, tree order
               ;; top-left (A), bottom-left, top-right, bottom-right
               (focus-only a)
               (split-current-frame out 1 :horizontal)
               (split-current-frame out 1 :vertical)
               (setf (gethash (frame-key out 1) *current-frames*)
                     (third (leaves out)))
               (focus-only nil)
               (split-current-frame out 1 :vertical)
               (apply-layout-for-output out))
             (forget-size (win)
               ;; No committed or float size: the centre is unknown
               (setf (window-width win) 0
                     (window-height win) 0
                     (window-float-width win) nil
                     (window-float-height win) nil))
             (siblings-p (w1 w2)
               (eq (frame-parent (window-frame w1))
                   (frame-parent (window-frame w2))))
             (leaf-index (out win)
               (position (window-frame win) (leaves out)))
             (retile (out win)
               (focus-only win)
               (unfloat-window win)
               (apply-layout-for-output out)))

      ;; --- A: the float sits over an empty frame ---

      (let* ((out (mk-out :up-a-out))
             (a (root-win :up-a1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal)
        (apply-layout-for-output out)
        (let ((b (float-at :up-a2 out 1300 300)))
          (retile out b)
          (ok "a float over an empty frame tiles into that frame"
              (list (length (leaves out)) (leaf-index out a)
                    (leaf-index out b))
              '(2 0 1))))

      ;; --- B: the float sits over the occupied frame ---

      (let* ((out (mk-out :up-b-out))
             (a (root-win :up-b1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal)
        (apply-layout-for-output out)
        (let ((b (float-at :up-b2 out 200 300)))
          (retile out b)
          (ok "a float over an occupied frame still fills the empty one"
              (list (length (leaves out)) (leaf-index out b))
              '(2 1))))

      ;; --- C: several empty frames, none under the float ---

      (let* ((out (mk-out :up-c-out))
             (a (root-win :up-c1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal)
        ;; Split the empty right half: top-right and bottom-right
        (setf (gethash (frame-key out 1) *current-frames*)
              (second (leaves out)))
        (focus-only nil)
        (split-current-frame out 1 :vertical)
        (apply-layout-for-output out)
        (ok "the setup has one occupied and two empty frames"
            (mapcar (lambda (f) (length (frame-windows f))) (leaves out))
            '(1 0 0))
        ;; Centre (700, 950): over the left frame, nearest bottom-right
        (let ((b (float-at :up-c2 out 500 800)))
          (retile out b)
          (ok "the nearest empty frame to the float's centre wins"
              (list (length (leaves out)) (leaf-index out b))
              '(3 2))))

      ;; --- D: no empty frame keeps the focused-frame split ---

      (let* ((out (mk-out :up-d-out))
             (a (root-win :up-d1 out)))
        (focus-only a)
        (apply-layout-for-output out)
        (let ((b (float-at :up-d2 out 1300 300)))
          (retile out b)
          (ok "with no empty frame the only frame splits"
              (list (length (leaves out)) (leaf-index out a)
                    (leaf-index out b))
              '(2 0 1))))

      ;; --- F: two empty frames equally near the centre ---

      (let* ((out (mk-out :up-f-out))
             (a (root-win :up-f1 out)))
        (grid out a)
        (let ((d (mk-win :up-f2 out)))
          (frame-push-window d (fourth (leaves out))))
        (apply-layout-for-output out)
        ;; Centre (760, 340): 200 px from bottom-left and top-right
        (let ((b (float-at :up-f3 out 560 190)))
          (retile out b)
          (ok "a tie on distance goes to the first empty frame in tree order"
              (list (length (leaves out)) (leaf-index out b))
              '(4 1))))

      ;; --- G: no known centre ---

      (let* ((out (mk-out :up-g-out))
             (a (root-win :up-g1 out)))
        (grid out a)
        ;; Its last position sits over the bottom-right frame
        (let ((b (float-at :up-g2 out 1500 800)))
          (forget-size b)
          (retile out b)
          (ok "a float with no known centre takes the first empty frame"
              (list (length (leaves out)) (leaf-index out b))
              '(4 1))))

      ;; --- H: no empty frame splits the largest, not the focused ---

      (let* ((out (mk-out :up-h-out))
             (a (root-win :up-h1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal 1/4)
        (let ((c (mk-win :up-h2 out)))
          (frame-push-window c (second (leaves out)))
          (apply-layout-for-output out)
          (focus-only a)
          ;; Centre (300, 450): over A's narrow frame
          (let ((b (float-at :up-h3 out 100 300)))
            (retile out b)
            (ok "with no empty frame the largest frame splits"
                (list (length (leaves out)) (frame-width (window-frame a))
                      (siblings-p b c) (leaf-index out b))
                '(3 480 t 2)))))

      ;; --- I: equal largest frames, the one nearest the centre ---

      (let* ((out (mk-out :up-i-out))
             (a (root-win :up-i1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal)
        (let ((c (mk-win :up-i2 out)))
          (frame-push-window c (second (leaves out)))
          (apply-layout-for-output out)
          (focus-only a)
          ;; Centre (1500, 450): over C's frame
          (let ((b (float-at :up-i3 out 1300 300)))
            (retile out b)
            (ok "of equal largest frames the one nearest the centre splits"
                (list (length (leaves out)) (siblings-p b c)
                      (leaf-index out b))
                '(3 t 2)))))

      ;; --- J: equal largest frames, no known centre ---

      (let* ((out (mk-out :up-j-out))
             (a (root-win :up-j1 out)))
        (focus-only a)
        (split-current-frame out 1 :horizontal)
        (let ((c (mk-win :up-j2 out)))
          (frame-push-window c (second (leaves out)))
          (apply-layout-for-output out)
          ;; C's frame is the command frame
          (focus-only c)
          (setf (gethash (frame-key out 1) *current-frames*)
                (window-frame c))
          (let ((b (float-at :up-j3 out 1300 300)))
            (forget-size b)
            (retile out b)
            (ok "of equal largest frames with no known centre the first splits"
                (list (length (leaves out)) (siblings-p b a)
                      (leaf-index out b))
                '(3 t 1)))))

      ;; --- E: the float's own output decides ---

      (let* ((left (mk-out :up-e-left))
             (right (mk-out :up-e-right 1920))
             (a (root-win :up-e1 left))
             (c (root-win :up-e2 right)))
        (focus-only a)
        (split-current-frame left 1 :horizontal)
        (focus-only c)
        (split-current-frame right 1 :horizontal)
        (apply-layout-for-output left)
        (apply-layout-for-output right)
        ;; Each output's command frame is its occupied half
        (setf (gethash (frame-key left 1) *current-frames*) (window-frame a)
              (gethash (frame-key right 1) *current-frames*) (window-frame c))
        ;; Centre (3220, 450): right output, its right half is empty
        (let ((b (float-at :up-e3 right 3020 300)))
          (retile right b)
          (ok "a float on the second output tiles into that output's empty frame"
              (list (window-output b) (leaf-index right b)
                    (mapcar (lambda (f) (length (frame-windows f)))
                            (leaves left)))
              (list right 1 '(1 0))))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
