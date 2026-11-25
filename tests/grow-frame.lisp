;;; Tests for grow-frame and grow-frame-to-edge: the current frame
;;; absorbs the frame beside it, or every frame between it and the
;;; screen edge, and refuses (changing nothing) when the edges do not
;;; line up. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Frames and windows link to each other: print failures shallowly
(setf *print-circle* t
      *print-level* 4
      *print-length* 8)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (last-message nil))
  (flet ((ok (name thunk expected)
           (let ((actual (handler-case (funcall thunk)
                           (error (c) (list :error (princ-to-string c))))))
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))))

    ;; Stub every river request; record the last message shown
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  request-manage-dirty))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (setf last-message (apply #'format nil fmt args))
            nil))

    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal))

    (labels ((mk-out (proxy &optional (width 1920) (height 1080))
               ;; An output showing workspace 1
               (let ((out (make-instance 'rhizome-output :proxy proxy
                                         :width width :height height)))
                 (setf (gethash out *output-workspace*) 1)
                 out))
             (mk-win (proxy out)
               ;; A registered window on OUT, workspace 1, with no frame
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win 1)
                 win))
             (fill-frame (frame &rest wins)
               ;; Put WINS in FRAME, the first one shown
               (setf (frame-windows frame) (copy-list wins))
               (dolist (w wins) (setf (window-frame w) frame))
               wins)
             (focus (win)
               ;; Focus WIN alone, as focus-window does
               (dolist (w (all-windows))
                 (setf (window-focused-p w) (eq w win))))
             (root (out)
               (frame-tree-for out 1))
             (set-root (out tree)
               (setf (gethash (frame-key out 1) *frame-trees*) tree))
             (rect (f)
               (list (frame-x f) (frame-y f) (frame-width f) (frame-height f)))
             (snap (f)
               ;; Shape, rects, split marks, parent links and window
               ;; identities of the tree under F
               (if (frame-first-child f)
                   (list :split (frame-split-direction f) (frame-user-split f)
                         (rect f)
                         (eq (frame-parent (frame-first-child f)) f)
                         (eq (frame-parent (frame-second-child f)) f)
                         (snap (frame-first-child f))
                         (snap (frame-second-child f)))
                   (list :leaf (rect f) (copy-list (frame-windows f)))))
             (tree-windows (out)
               (loop for leaf in (frame-leaves (root out))
                     append (frame-windows leaf)))
             (same-windows-p (before after)
               (and (= (length before) (length after))
                    (every (lambda (w) (member w after :test #'eq)) before)))
             (refused-unchanged (out thunk reason)
               ;; Run THUNK; return (result tree-unchanged-p
               ;; message-gives-REASON-p)
               (let ((before (snap (root out)))
                     (current (current-frame-for out 1)))
                 (setf last-message nil)
                 (let ((result (funcall thunk)))
                   (list result
                         (and (equal before (snap (root out)))
                              (eq current (current-frame-for out 1)))
                         (and last-message
                              (search reason last-message)
                              t))))))

      ;; --- Control: the unchanged-tree check can fail ---

      (let* ((out (mk-out :gf-control))
             (r (root out)))
        (split-frame r :horizontal 1/2)
        (let ((before (snap r)))
          (incf (frame-width (frame-first-child r)) 10)
          (ok "control: the tree snapshot sees a moved dividing line"
              (lambda () (equal before (snap r)))
              nil)))

      ;; --- Simple two-way split, growing right ---

      (let* ((out (mk-out :gf-h-right))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-hr-a1 out)) (a2 (mk-win :gf-hr-a2 out))
             (b1 (mk-win :gf-hr-b1 out)) (b2 (mk-win :gf-hr-b2 out)))
        (fill-frame a a1 a2)
        (fill-frame b b1 b2)
        (focus a1)
        (let ((before (tree-windows out)))
          (ok "grow right in a side-by-side split succeeds"
              (lambda () (grow-current-frame out 1 :right))
              t)
          (ok "the grown frame becomes the whole screen"
              (lambda () (list (eq (root out) a) (rect a) (frame-parent a)))
              (list t '(0 0 1920 1080) nil))
          (ok "the absorbed windows go behind the shown window, in order"
              (lambda () (frame-windows a))
              (list a1 a2 b1 b2))
          (ok "no window is lost or duplicated"
              (lambda () (same-windows-p before (tree-windows out)))
              t)
          (ok "every window points at the grown frame"
              (lambda () (every (lambda (w) (eq (window-frame w) a))
                                (list a1 a2 b1 b2)))
              t)
          (ok "the shown window and focus are unchanged"
              (lambda () (list (eq (frame-current-window a) a1)
                               (window-focused-p a1)
                               (eq (current-frame-for out 1) a)))
              '(t t t))))

      ;; --- Simple two-way split, growing left ---

      (let* ((out (mk-out :gf-h-left))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-hl-a1 out))
             (b1 (mk-win :gf-hl-b1 out)) (b2 (mk-win :gf-hl-b2 out)))
        (fill-frame a a1)
        (fill-frame b b1 b2)
        (focus b1)
        (ok "grow left in a side-by-side split succeeds"
            (lambda () (grow-current-frame out 1 :left))
            t)
        (ok "the right frame takes the whole screen, its window still shown"
            (lambda () (list (eq (root out) b) (rect b)
                             (frame-windows b)
                             (eq (frame-current-window b) b1)))
            (list t '(0 0 1920 1080) (list b1 b2 a1) t)))

      ;; --- Stacked split, growing down and up ---

      (let* ((out (mk-out :gf-v-down))
             (r (root out))
             (b (split-frame r :vertical 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-vd-a1 out)) (b1 (mk-win :gf-vd-b1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (focus a1)
        (ok "grow down in a stacked split takes the lower frame"
            (lambda () (list (grow-current-frame out 1 :down)
                             (eq (root out) a) (rect a) (frame-windows a)))
            (list t t '(0 0 1920 1080) (list a1 b1))))

      (let* ((out (mk-out :gf-v-up))
             (r (root out))
             (b (split-frame r :vertical 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-vu-a1 out)) (b1 (mk-win :gf-vu-b1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (focus b1)
        (ok "grow up in a stacked split takes the upper frame"
            (lambda () (list (grow-current-frame out 1 :up)
                             (eq (root out) b) (rect b) (frame-windows b)))
            (list t t '(0 0 1920 1080) (list b1 a1))))

      ;; --- Nested: the neighbour is a sibling deeper in the tree ---

      (let* ((out (mk-out :gf-nested-sib))
             (r (root out))
             (right (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (c (split-frame right :vertical 1/2))
             (b (frame-first-child right))
             (a1 (mk-win :gf-ns-a1 out)) (b1 (mk-win :gf-ns-b1 out))
             (c1 (mk-win :gf-ns-c1 out)))
        (setf (frame-user-split r) t)
        (fill-frame a a1)
        (fill-frame b b1)
        (fill-frame c c1)
        (focus b1)
        (ok "a nested frame grows down into its sibling"
            (lambda () (grow-current-frame out 1 :down))
            t)
        (ok "the outer split and its mark stay; the grown frame fills its half"
            (lambda () (snap (root out)))
            (list :split :horizontal t '(0 0 1920 1080) t t
                  (list :leaf '(0 0 960 1080) (list a1))
                  (list :leaf '(960 0 960 1080) (list b1 c1)))))

      ;; --- Nested: the neighbour shares the whole edge but is a cousin ---

      (let* ((out (mk-out :gf-cousin))
             (r (root out))
             (bottom (split-frame r :vertical 1/2))
             (top (frame-first-child r))
             (b (split-frame top :horizontal 1/2))
             (a (frame-first-child top))
             (d (split-frame bottom :horizontal 1/2))
             (c (frame-first-child bottom))
             (a1 (mk-win :gf-co-a1 out)) (b1 (mk-win :gf-co-b1 out))
             (c1 (mk-win :gf-co-c1 out)) (d1 (mk-win :gf-co-d1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (fill-frame c c1)
        (fill-frame d d1)
        (focus a1)
        (let ((before (tree-windows out)))
          (ok "a frame grows down into the cousin that shares its whole edge"
              (lambda () (grow-current-frame out 1 :down))
              t)
          (ok "the tree becomes the grown column beside the untouched pair"
              (lambda () (snap (root out)))
              (list :split :horizontal nil '(0 0 1920 1080) t t
                    (list :leaf '(0 0 960 1080) (list a1 c1))
                    (list :split :vertical nil '(960 0 960 1080) t t
                          (list :leaf '(960 0 960 540) (list b1))
                          (list :leaf '(960 540 960 540) (list d1)))))
          (ok "the untouched frames are the same frame objects"
              (lambda () (list (eq (first (frame-leaves (root out))) a)
                               (eq (second (frame-leaves (root out))) b)
                               (eq (third (frame-leaves (root out))) d)))
              '(t t t))
          (ok "no window is lost after growing into a cousin"
              (lambda () (same-windows-p before (tree-windows out)))
              t)))

      ;; --- Refusal: the neighbour is a split; each leaf shares part ---

      (let* ((out (mk-out :gf-partial))
             (r (root out))
             (right (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (c (split-frame right :vertical 1/2))
             (b (frame-first-child right)))
        (fill-frame a (mk-win :gf-pa-a1 out))
        (fill-frame b (mk-win :gf-pa-b1 out))
        (fill-frame c (mk-win :gf-pa-c1 out))
        (focus (first (frame-windows a)))
        (ok "two frames sharing parts of the edge: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :right))
                        "2 frames share that edge"))
            '(nil t t)))

      ;; --- Refusal: the single neighbour runs past the edge ---

      (let* ((out (mk-out :gf-taller))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (left (frame-first-child r))
             (c (split-frame left :vertical 1/2))
             (a (frame-first-child left)))
        (fill-frame a (mk-win :gf-ta-a1 out))
        (fill-frame b (mk-win :gf-ta-b1 out))
        (fill-frame c (mk-win :gf-ta-c1 out))
        (focus (first (frame-windows a)))
        (ok "a neighbour taller than the edge: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :right))
                        "taller than this frame"))
            '(nil t t)))

      (let* ((out (mk-out :gf-wider))
             (r (root out))
             (b (split-frame r :vertical 1/2))
             (top (frame-first-child r))
             (c (split-frame top :horizontal 1/2))
             (a (frame-first-child top)))
        (fill-frame a (mk-win :gf-wi-a1 out))
        (fill-frame b (mk-win :gf-wi-b1 out))
        (fill-frame c (mk-win :gf-wi-c1 out))
        (focus (first (frame-windows a)))
        (ok "a neighbour wider than the edge: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :down))
                        "wider than this frame"))
            '(nil t t)))

      ;; --- Refusal: no neighbour at the screen edge ---

      (let* ((out (mk-out :gf-screen-edge))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r)))
        (fill-frame a (mk-win :gf-se-a1 out))
        (fill-frame b (mk-win :gf-se-b1 out))
        (focus (first (frame-windows a)))
        (ok "grow toward the screen edge: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :left))
                        "cannot grow left: no frame there"))
            '(nil t t))
        (ok "grow up with no frame above: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :up))
                        "cannot grow up: no frame there"))
            '(nil t t)))

      (let ((out (mk-out :gf-single)))
        (fill-frame (root out) (mk-win :gf-si-a1 out))
        (focus (first (frame-windows (root out))))
        (ok "a single frame has no neighbour: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (grow-current-frame out 1 :right))
                        "no frame there"))
            '(nil t t)))

      ;; --- Refusal: the merged frames would leave no split tree ---
      ;;
      ;; A 900x900 screen in a 3x3 grid of 300px cells. Growing the
      ;; bottom-middle cell right would leave a pinwheel, which no
      ;; binary split tree can hold.

      (let* ((out (mk-out :gf-pinwheel 900 900))
             (tree (build-tree-from-rects
                    '((0 0 600 300) (600 0 300 600) (300 600 300 300)
                      (600 600 300 300) (0 300 300 600) (300 300 300 300)))))
        (set-root out tree)
        (let ((cell (find '(300 600 300 300) (frame-leaves tree)
                          :key #'rect :test #'equal)))
          (fill-frame cell (mk-win :gf-pw-a1 out))
          (dolist (leaf (frame-leaves tree))
            (unless (eq leaf cell)
              (fill-frame leaf (mk-win (gensym "GF-PW-") out))))
          (focus (first (frame-windows cell)))
          (ok "a grow that would leave a pinwheel: refused, tree unchanged, why"
              (lambda () (refused-unchanged
                          out (lambda () (grow-current-frame out 1 :right))
                          "not a split tree"))
              '(nil t t))))

      ;; --- grow-frame-to-edge over several frames ---

      (let* ((out (mk-out :gf-edge-many))
             (r (root out))
             (rest (split-frame r :horizontal 1/3))
             (a (frame-first-child r))
             (c (split-frame rest :horizontal 1/2))
             (b (frame-first-child rest))
             (a1 (mk-win :gf-em-a1 out)) (b1 (mk-win :gf-em-b1 out))
             (c1 (mk-win :gf-em-c1 out)) (c2 (mk-win :gf-em-c2 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (fill-frame c c1 c2)
        (focus a1)
        (let ((before (tree-windows out)))
          (ok "grow to the right edge across two frames succeeds"
              (lambda () (grow-current-frame-to-edge out 1 :right))
              t)
          (ok "the frame fills the screen; nearer frames' windows come first"
              (lambda () (list (eq (root out) a) (rect a) (frame-windows a)))
              (list t '(0 0 1920 1080) (list a1 b1 c1 c2)))
          (ok "no window is lost growing to the edge"
              (lambda () (same-windows-p before (tree-windows out)))
              t)
          (ok "the shown window and focus are unchanged after growing to the edge"
              (lambda () (list (eq (frame-current-window a) a1)
                               (window-focused-p a1)
                               (eq (current-frame-for out 1) a)))
              '(t t t))))

      ;; A stacked pair in the way fits inside the frame's height: it
      ;; is absorbed whole, though grow-frame alone refuses it

      (let* ((out (mk-out :gf-edge-stack))
             (r (root out))
             (right (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (c (split-frame right :vertical 1/2))
             (b (frame-first-child right))
             (a1 (mk-win :gf-es-a1 out)) (b1 (mk-win :gf-es-b1 out))
             (c1 (mk-win :gf-es-c1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (fill-frame c c1)
        (focus a1)
        (ok "grow to the edge absorbs a stacked pair in the way"
            (lambda () (list (grow-current-frame-to-edge out 1 :right)
                             (snap (root out))))
            (list t (list :leaf '(0 0 1920 1080) (list a1 b1 c1)))))

      ;; Frames outside the frame's height stay where they are

      (let* ((out (mk-out :gf-edge-row))
             (r (root out))
             (bottom (split-frame r :vertical 1/2))
             (top (frame-first-child r))
             (b (split-frame top :horizontal 1/2))
             (a (frame-first-child top))
             (a1 (mk-win :gf-er-a1 out)) (b1 (mk-win :gf-er-b1 out))
             (d1 (mk-win :gf-er-d1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (fill-frame bottom d1)
        (focus a1)
        (ok "grow to the edge leaves frames outside the frame's height alone"
            (lambda () (list (grow-current-frame-to-edge out 1 :right)
                             (snap (root out))))
            (list t (list :split :vertical nil '(0 0 1920 1080) t t
                          (list :leaf '(0 0 1920 540) (list a1 b1))
                          (list :leaf '(0 540 1920 540) (list d1))))))

      ;; --- grow-frame-to-edge refusals ---

      (let* ((out (mk-out :gf-edge-past))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (left (frame-first-child r))
             (c (split-frame left :vertical 1/2))
             (a (frame-first-child left)))
        (fill-frame a (mk-win :gf-ep-a1 out))
        (fill-frame b (mk-win :gf-ep-b1 out))
        (fill-frame c (mk-win :gf-ep-c1 out))
        (focus (first (frame-windows a)))
        (ok "a frame in the way extends past the frame's height: refused, unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda ()
                              (grow-current-frame-to-edge out 1 :right))
                        "overhangs the top or bottom"))
            '(nil t t)))

      (let* ((out (mk-out :gf-edge-at))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r)))
        (fill-frame a (mk-win :gf-ea-a1 out))
        (fill-frame b (mk-win :gf-ea-b1 out))
        (focus (first (frame-windows b)))
        (ok "already at the screen edge: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda ()
                              (grow-current-frame-to-edge out 1 :right))
                        "already at the screen edge"))
            '(nil t t)))

      ;; --- The commands ---

      (let* ((out (mk-out :gf-cmd))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-cmd-a1 out)) (b1 (mk-win :gf-cmd-b1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (focus a1)
        (ok "grow-frame with an unknown direction: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda ()
                              (execute-command out :grow-frame '("sideways")))
                        "direction must be"))
            '(nil t t))
        (ok "grow-frame with no direction: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda () (execute-command out :grow-frame '()))
                        "direction must be"))
            '(nil t t))
        (ok "the grow-frame command takes a direction word"
            (lambda () (list (execute-command out :grow-frame '("right"))
                             (eq (root out) a) (frame-windows a)))
            (list t t (list a1 b1))))

      (let* ((out (mk-out :gf-cmd-edge))
             (r (root out))
             (b (split-frame r :vertical 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-cme-a1 out)) (b1 (mk-win :gf-cme-b1 out)))
        (fill-frame a a1)
        (fill-frame b b1)
        (focus b1)
        (ok "the grow-frame-to-edge command takes a direction word"
            (lambda () (list (execute-command out :grow-frame-to-edge '("UP"))
                             (eq (root out) b) (frame-windows b)))
            (list t t (list b1 a1))))

      ;; --- A NIL output, as a key binding passes it ---

      (let* ((out (mk-out :gf-cmd-nil))
             (r (root out))
             (b (split-frame r :horizontal 1/2))
             (a (frame-first-child r))
             (a1 (mk-win :gf-cmn-a1 out)) (b1 (mk-win :gf-cmn-b1 out))
             (*outputs* (make-hash-table)))
        (setf (gethash :gf-cmd-nil *outputs*) out)
        (fill-frame a a1)
        (fill-frame b b1)
        (focus a1)
        (ok "grow-frame with a NIL output: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda ()
                              (execute-command nil :grow-frame '("left")))
                        "no frame there"))
            '(nil t t))
        (ok "grow-frame-to-edge with a NIL output: refused, tree unchanged, why"
            (lambda () (refused-unchanged
                        out (lambda ()
                              (execute-command nil :grow-frame-to-edge
                                               '("left")))
                        "already at the screen edge"))
            '(nil t t))
        (ok "grow-frame with a NIL output grows the focused output's frame"
            (lambda () (list (execute-command nil :grow-frame '("right"))
                             (eq (root out) a) (frame-windows a)))
            (list t t (list a1 b1))))

      ;; --- Every refusal fits the message box ---
      ;;
      ;; A 1280px output shows about 98 characters. Each refusal, with
      ;; the longest direction word and a two-digit count, stays within
      ;; 75.

      (let ((shown '()))
        (flet ((refusal (thunk)
                 (setf last-message nil)
                 (funcall thunk)
                 (push last-message shown))
               (rects-output (proxy rects focus-rect)
                 ;; An output tiled as RECTS, every leaf holding a
                 ;; window, the leaf at FOCUS-RECT focused
                 (let* ((out (mk-out proxy))
                        (tree (build-tree-from-rects rects)))
                   (set-root out tree)
                   (dolist (leaf (frame-leaves tree))
                     (fill-frame leaf (mk-win (gensym "GF-LEN-") out)))
                   (focus (first (frame-windows
                                  (find focus-rect (frame-leaves tree)
                                        :key #'rect :test #'equal))))
                   out)))
          ;; No frame there
          (let ((out (rects-output :gf-len-none '((0 0 1920 1080))
                                   '(0 0 1920 1080))))
            (refusal (lambda () (grow-current-frame out 1 :right))))
          ;; Ten frames meet the edge
          (let ((out (rects-output
                      :gf-len-ten
                      (cons '(0 0 960 1080)
                            (loop for i below 10
                                  collect (list 960 (* i 108) 960 108)))
                      '(0 0 960 1080))))
            (refusal (lambda () (grow-current-frame out 1 :right))))
          ;; The neighbour is taller; a frame in the way overhangs
          (let ((out (rects-output
                      :gf-len-taller
                      '((0 0 960 540) (0 540 960 540) (960 0 960 1080))
                      '(0 0 960 540))))
            (refusal (lambda () (grow-current-frame out 1 :right)))
            (refusal (lambda ()
                       (grow-current-frame-to-edge out 1 :right))))
          ;; Already at the screen edge
          (let ((out (rects-output :gf-len-edge
                                   '((0 0 960 1080) (960 0 960 1080))
                                   '(960 0 960 1080))))
            (refusal (lambda ()
                       (grow-current-frame-to-edge out 1 :right)))
            ;; The commands without a direction
            (refusal (lambda ()
                       (execute-command out :grow-frame '("sideways"))))
            (refusal (lambda ()
                       (execute-command out :grow-frame-to-edge
                                        '("sideways")))))
          ;; The result is no split tree
          (let ((out (rects-output
                      :gf-len-pinwheel
                      '((0 0 600 300) (600 0 300 600) (300 600 300 300)
                        (600 600 300 300) (0 300 300 600) (300 300 300 300))
                      '(300 600 300 300))))
            (refusal (lambda () (grow-current-frame out 1 :right)))))
        (ok "every refusal message is at most 75 characters"
            (lambda () (list (length shown)
                             (count nil shown)
                             (remove-if (lambda (m) (<= (length m) 75))
                                        (remove nil shown))))
            '(8 0 nil)))

      (ok "both commands are documented for describe-command"
          (lambda () (list (stringp (gethash :grow-frame *command-docs*))
                           (stringp (gethash :grow-frame-to-edge
                                             *command-docs*))))
          '(t t)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
