;;; Tests for the frame tree engine (frames.lisp) and its tiling
;;; integration (tiling.lisp). River-touching functions are stubbed.
(ql:quickload :stumpway)

(in-package #:stumpway)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (called? (win tag)
           (and (member (list tag win) calls :test #'equal) t))
         (tag-called? (tag)
           (and (find tag calls :key #'car) t)))

    ;; Record calls instead of touching river; show/hide update state
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  message request-manage-dirty))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (if (and args (typep (first args) 'stumpway-window))
                    (progn
                      (push (list (intern (string fn-sym) :keyword)
                                  (first args))
                            calls)
                      (when (eq fn-sym 'show-window)
                        (setf (window-hidden-p (first args)) nil))
                      (when (eq fn-sym 'hide-window)
                        (setf (window-hidden-p (first args)) t)))
                    (push (list (intern (string fn-sym) :keyword) args)
                          calls))
                nil))))

    ;; Fresh global state (frame hashes need :test 'equal for the
    ;; (output . workspace) cons keys)
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal))

    (let ((out (make-instance 'stumpway-output :proxy :out-proxy
                              :width 1920 :height 1080)))
      (setf (gethash out *output-workspace*) 1)

      ;; --- Lazy tree creation: one full-screen frame ---

      (let ((root (frame-tree-for out 1)))
        (ok "new workspace gets a single frame"
            (list (frame-width root) (frame-height root)) '(1920 1080))
        (ok "a lone frame is its only leaf"
            (length (frame-leaves root)) 1))

      ;; --- Splitting ---

      (let* ((root (frame-tree-for out 2))
             (a (make-instance 'stumpway-window :proxy :a-proxy))
             (b (make-instance 'stumpway-window :proxy :b-proxy))
             (c (make-instance 'stumpway-window :proxy :c-proxy)))
        (setf (gethash :a-proxy *windows*) a
              (gethash :b-proxy *windows*) b
              (gethash :c-proxy *windows*) c)

        ;; Split the root side-by-side; window stays in the first half
        (frame-push-window a root)
        (setf (window-output a) out)
        (assign-window-workspace a 2)
        (setf calls '())
        (split-current-frame out 2 :horizontal)
        (let ((left (frame-first-child root))
              (right (frame-second-child root)))
          (ok "split halves the frame"
              (list (frame-width left) (frame-width right)) '(960 960))
          (ok "window stays in the first child"
              (window-frame a) left)
          (ok "focus moves to the new frame"
              (current-frame-for out 2) right)
          (ok "split has two leaves" (length (frame-leaves root)) 2))

        ;; Split the right frame stacked
        (split-current-frame out 2 :vertical)
        (ok "nested split gives three leaves"
            (length (frame-leaves root)) 3)

        ;; --- Resizing the split ---

        (let ((frame (current-frame-for out 2))
              (parent (frame-parent (current-frame-for out 2))))
          (declare (ignore frame))
          (let* ((first (frame-first-child parent))
                 (second (frame-second-child parent)))
            ;; line at y 540; move it down 40 → 580
            (setf calls '())
            (resize-current-split out 2 40)
            (ok "resize moves the dividing line"
                (list (frame-height first) (frame-y second)) '(580 580))
            ;; clamped at the minimum on the parent's edge
            (resize-current-split out 2 100000)
            (ok "resize clamps at the far edge"
                (frame-height first) (- 1080 *frame-min-size*))))

        ;; --- Removing a split ---

        (let ((frame (current-frame-for out 2)))
          ;; frame is second child of a :vertical split of the right half
          (let* ((parent (frame-parent frame))
                 (sibling (frame-sibling frame))
                 (grand (frame-parent parent)))
            (frame-push-window b frame)
            (frame-push-window c sibling)
            (setf calls '())
            (remove-current-split out 2)
            (ok "windows merge into the sibling"
                (sort (mapcar #'window-proxy (frame-windows sibling))
                      #'string<)
                '(:b-proxy :c-proxy))
            (ok "sibling takes over the parent rectangle"
                (list (frame-x sibling) (frame-width sibling))
                (list (frame-x parent) (frame-width parent)))
            (ok "sibling relinked to the grandparent"
                (frame-parent sibling) grand)
            (ok "tree back to two leaves"
                (length (frame-leaves root)) 2)
            (ok "focus lands on the merged frame"
                (current-frame-for out 2) sibling)))

        ;; Removing a lone root frame's split is refused: no message
        ;; relayout, no relayout request
        (let ((lonely (frame-tree-for out 5)))
          (setf (gethash (frame-key out 5) *current-frames*) lonely)
          (setf calls '())
          (remove-current-split out 5)
          (ok "root removal is refused"
              (tag-called? :request-manage-dirty) nil))

        ;; --- Directional focus (2x2 grid) ---

        (set-workspace-tree out 3
                             '((0 0 960 540) (960 0 960 540)
                               (0 540 960 540) (960 540 960 540))
                             nil)
        (let* ((root2 (frame-tree-for out 3))
               (leaves (frame-leaves root2)))
          (flet ((at (x y)
                   (find-if (lambda (f)
                              (and (= (frame-x f) x)
                                   (= (frame-y f) y)))
                            leaves)))
            (let ((tl (at 0 0)) (tr (at 960 0))
                  (bl (at 0 540)) (br (at 960 540)))
              (ok "grid template builds four leaves"
                  (length leaves) 4)
              (ok "from top-left, right goes to top-right"
                  (frame-at-direction tl :right leaves) tr)
              (ok "from top-left, down goes to bottom-left"
                  (frame-at-direction tl :down leaves) bl)
              (ok "from top-right, left goes to top-left"
                  (frame-at-direction tr :left leaves) tl)
              (ok "from bottom-right, up goes to top-right"
                  (frame-at-direction br :up leaves) tr)
              (ok "off-screen direction finds nothing"
                  (frame-at-direction tl :up leaves) nil))))))

    ;; --- Tree construction from rectangles ---

    (let ((tree (build-tree-from-rects '((0 0 960 1080) (960 0 960 1080)))))
      (ok "master-stack rects build a side-by-side tree"
          (frame-split-direction tree) :horizontal))
    (let ((tree (build-tree-from-rects
                  '((0 0 100 100) (100 0 50 100) (150 0 50 100)))))
      (ok "three-column rects build a valid tree"
          (mapcar #'frame-width (frame-leaves tree)) '(100 50 50)))
    (ok "overlapping rects are not a partition"
        (build-tree-from-rects '((0 0 800 600) (400 300 800 600))) nil)
    (ok "a lone rect is a lone frame"
        (length (frame-leaves (build-tree-from-rects '((0 0 800 600))))) 1)

    ;; --- set-workspace-tree distribution ---

    (let* ((out2 (make-instance 'stumpway-output :proxy :out2-proxy
                               :width 1920 :height 1080))
           (w1 (make-instance 'stumpway-window :proxy :w1))
           (w2 (make-instance 'stumpway-window :proxy :w2))
           (w3 (make-instance 'stumpway-window :proxy :w3)))
      (setf (gethash :w1 *windows*) w1
            (gethash :w2 *windows*) w2
            (gethash :w3 *windows*) w3
            (window-output w1) out2 (window-output w2) out2
            (window-output w3) out2)
      (assign-window-workspace w1 1)
      (assign-window-workspace w2 1)
      (assign-window-workspace w3 1)
      (setf (gethash out2 *output-workspace*) 1)

      (set-workspace-tree out2 1
                          '((0 0 960 1080) (960 0 960 1080))
                          (list w1 w2 w3))
      (let ((leaves (frame-leaves (frame-tree-for out2 1))))
        (ok "first window in the master frame"
            (window-frame w1) (first leaves))
        (ok "second window in the stack frame"
            (window-frame w2) (second leaves))
        (ok "extra windows pile onto the last frame"
            (window-frame w3) (second leaves))
        (ok "both windows present in the stack frame"
            (sort (mapcar #'window-proxy (frame-windows (second leaves)))
                  #'string<)
            '(:w2 :w3)))

      ;; Invalid partition → single-frame fallback
      (set-workspace-tree out2 1
                          '((0 0 500 500) (400 400 100 100))
                          (list w1 w2 w3))
      (ok "invalid rects fall back to one frame"
          (list (length (frame-leaves (frame-tree-for out2 1)))
                (sort (mapcar #'window-proxy
                              (frame-windows (frame-tree-for out2 1)))
                      #'string<))
          '(1 (:w1 :w2 :w3))))

    ;; --- Cycling windows within a frame ---

    (let* ((out3 (make-instance 'stumpway-output :proxy :out3-proxy
                               :width 1920 :height 1080))
           (x (make-instance 'stumpway-window :proxy :x))
           (y (make-instance 'stumpway-window :proxy :y))
           (z (make-instance 'stumpway-window :proxy :z)))
      (setf (gethash :x *windows*) x
            (gethash :y *windows*) y
            (gethash :z *windows*) z
            (gethash out3 *output-workspace*) 1)
      (setf (window-output x) out3 (window-output y) out3
            (window-output z) out3)
      (assign-window-workspace x 1)
      (assign-window-workspace y 1)
      (assign-window-workspace z 1)
      (let ((root (frame-tree-for out3 1)))
        (frame-push-window x root)
        (frame-push-window y root)
        (frame-push-window z root)
        (setf (window-focused-p z) t)          ; z visible/focused
        (setf calls '())
        (cycle-frame-window out3 1 1)
        (ok "cycle rotates the frame's windows"
            (mapcar #'window-proxy (frame-windows root)) '(:y :z :x))
        ;; focus moves with the cycle (frame-current-window prefers
        ;; the focused window)
        (setf (window-focused-p z) nil (window-focused-p y) t)
        (cycle-frame-window out3 1 -1)
        (ok "reverse cycle rotates the other way"
            (mapcar #'window-proxy (frame-windows root)) '(:x :y :z))))

    ;; --- Manage/render integration ---

    (let* ((out4 (make-instance 'stumpway-output :proxy :out4-proxy
                                :width 1920 :height 1080))
           (t1 (make-instance 'stumpway-window :proxy :t1))
           (t2 (make-instance 'stumpway-window :proxy :t2))
           (t3 (make-instance 'stumpway-window :proxy :t3)))
      (setf (gethash :t1 *windows*) t1
            (gethash :t2 *windows*) t2
            (gethash :t3 *windows*) t3
            (window-output t1) out4 (window-output t2) out4
            (window-output t3) out4
            (window-hidden-p t1) nil (window-hidden-p t2) nil
            (window-hidden-p t3) t
            (gethash out4 *output-workspace*) 1)
      (assign-window-workspace t1 1)
      (assign-window-workspace t2 1)
      (assign-window-workspace t3 1)

      ;; Manage: t2/t3 pre-placed in the frame; the frameless t1 is
      ;; adopted (pushed front: it becomes the frame's visible
      ;; window) and gets proposed the frame rectangle
      (let ((root (frame-tree-for out4 1)))
        (frame-push-window t3 root)
        (frame-push-window t2 root))
      (setf calls '())
      (apply-layout-for-output out4)
      (ok "frameless window adopted into a frame"
          (and (window-frame t1) t) t)
      (ok "manage proposes frame dims for the visible window"
          (called? t1 :propose-window-dimensions) t)
      (ok "non-current frame window not proposed yet"
          (called? t3 :propose-window-dimensions) nil)

      ;; Split, then render: current window of each frame shown and
      ;; positioned at its frame rectangle; the other window in the
      ;; first frame stays hidden (monocle-per-frame)
      (let ((root (frame-tree-for out4 1)))
        (split-current-frame out4 1 :horizontal)
        ;; t3 hidden; render must not show it (it is not the frame's
        ;; visible window) and must position t1 inside the left frame
        (setf calls '())
        (render-layout-for-output out4)
        (ok "visible window positioned"
            (called? t1 :set-window-position) t)
        (ok "hidden window not shown"
            (called? t3 :show-window) nil)
        (ok "window x stamped from frame"
            (window-x t1) (frame-x (frame-first-child root)))
        (ok "window y includes the bar offset"
            (window-y t1) (frame-y (frame-first-child root))))

      ;; A dead window disappears from its frame during manage
      (remhash :t2 *windows*)
      (apply-layout-for-output out4)
      (ok "dead windows pruned from frames"
          (sort (mapcar #'window-proxy
                        (frame-windows
                          (frame-first-child (frame-tree-for out4 1))))
                #'string<)
          '(:t1 :t3)))

    ;; --- switch-layout rebuilds from a template ---

    (let* ((out5 (make-instance 'stumpway-output :proxy :out5-proxy
                                :width 1920 :height 1080))
           (m1 (make-instance 'stumpway-window :proxy :m1))
           (m2 (make-instance 'stumpway-window :proxy :m2))
           (m3 (make-instance 'stumpway-window :proxy :m3)))
      (setf (gethash :m1 *windows*) m1
            (gethash :m2 *windows*) m2
            (gethash :m3 *windows*) m3
            (window-output m1) out5 (window-output m2) out5
            (window-output m3) out5
            (gethash out5 *output-workspace*) 4)
      (assign-window-workspace m1 4)
      (assign-window-workspace m2 4)
      (assign-window-workspace m3 4)
      (setf calls '())
      (switch-layout out5 :master-stack)
      (let ((leaves (frame-leaves (frame-tree-for out5 4))))
        (ok "master-stack template gives two frames"
            (length leaves) 2)
        (ok "one window in the master frame"
            (length (frame-windows (first leaves))) 1)
        (ok "extra windows stack in the second frame"
            (length (frame-windows (second leaves))) 2))
      (switch-layout out5 :monocle)
      (ok "monocle template collapses to one frame"
          (length (frame-leaves (frame-tree-for out5 4))) 1))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
