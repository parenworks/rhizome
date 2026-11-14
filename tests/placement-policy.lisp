;;; Tests for the frame placement policy (placement.lisp): what fills
;;; the new half of a split, which frame a new window enters, policy
;;; selection by name, and the workspace focus history. River requests
;;; are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name thunk expected)
           (let ((actual (handler-case (funcall thunk)
                           (error (c) (list :error (princ-to-string c))))))
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))))

    ;; Stub every river request; show/hide update state
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  message request-manage-dirty))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (when (and args (typep (first args) 'rhizome-window))
                  (when (eq fn-sym 'show-window)
                    (setf (window-hidden-p (first args)) nil))
                  (when (eq fn-sym 'hide-window)
                    (setf (window-hidden-p (first args)) t)))
                nil))))

    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal))

    (labels ((mk-out (proxy)
               ;; A 1920x1080 output showing workspace 1
               (let ((out (make-instance 'rhizome-output :proxy proxy
                                         :width 1920 :height 1080)))
                 (setf (gethash out *output-workspace*) 1)
                 out))
             (mk-win (proxy out)
               ;; A registered window on OUT, workspace 1, with no frame
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win 1)
                 win))
             (root-win (proxy out)
               ;; A window placed in the root frame of OUT's workspace 1
               (let ((win (mk-win proxy out)))
                 (frame-push-window win (frame-tree-for out 1))
                 win))
             (focus (win)
               ;; Focus WIN alone, as focus-window does
               (dolist (w (all-windows))
                 (unless (eq w win)
                   (setf (window-focused-p w) nil)))
               (setf (window-focused-p win) t))
             (unfocus-all ()
               (dolist (w (all-windows))
                 (setf (window-focused-p w) nil)))
             (leaves (out)
               (frame-leaves (frame-tree-for out 1)))
             (arrive (out win)
               (frames-manage-decisions out 1 (list win)))
             (history (out)
               (if (fboundp 'workspace-focus-history)
                   (funcall 'workspace-focus-history out 1)
                   :no-history-accessor))
             (call-with-tiling (tiling thunk)
               ;; Run THUNK with (:tiling TILING) merged into the config
               (let ((*config* (merge-plist *config*
                                            (list :tiling tiling))))
                 (funcall thunk))))

      ;; --- Default: a split fills its new half with the last hidden window ---

      (let* ((out (mk-out :pp-a-out))
             (w1 (root-win :pp-a1 out))
             (w2 (root-win :pp-a2 out)))
        (focus w1)
        (focus w2)
        (split-current-frame out 1 :horizontal)
        (ok "by default a split fills its new half with the last hidden window"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w2) (list w1)))
        (ok "the split leaves focus on the window that had it"
            (lambda () (window-focused-p w2))
            t)
        (unfocus-all))

      ;; --- Default with no hidden window: the new half stays empty ---

      (let* ((out (mk-out :pp-b-out))
             (w1 (root-win :pp-b1 out)))
        (focus w1)
        (split-current-frame out 1 :horizontal)
        (ok "a split with no hidden window leaves its new half empty"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) nil))
        (unfocus-all))

      ;; --- :new-frame :empty leaves the new half empty ---

      (let* ((out (mk-out :pp-c-out))
             (w1 (root-win :pp-c1 out))
             (w2 (root-win :pp-c2 out)))
        (focus w1)
        (focus w2)
        (call-with-tiling '(:placement (:new-frame :empty))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)))
        (ok ":new-frame :empty leaves the new half empty"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w2 w1) nil))
        (unfocus-all))

      ;; --- Default: a new window enters the focused frame ---

      (let* ((out (mk-out :pp-d-out))
             (w1 (root-win :pp-d1 out))
             (w3 (mk-win :pp-d3 out)))
        (focus w1)
        (split-current-frame out 1 :horizontal)
        (call-with-tiling '(:auto-split nil)
                          (lambda () (arrive out w3)))
        (ok "by default a new window enters the focused frame"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w3 w1) nil))
        (unfocus-all))

      ;; --- Default with auto-split: the focused frame splits ---

      (let* ((out (mk-out :pp-e-out))
             (w1 (root-win :pp-e1 out))
             (w3 (mk-win :pp-e3 out)))
        (focus w1)
        (split-current-frame out 1 :horizontal)
        (arrive out w3)
        (ok "auto-split still splits the frame the policy picks"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) (list w3) nil))
        (unfocus-all))

      ;; --- One config line: a new window enters the empty split frame ---

      (let* ((out (mk-out :pp-f-out))
             (w1 (root-win :pp-f1 out))
             (w3 (mk-win :pp-f3 out)))
        (focus w1)
        (call-with-tiling '(:placement (:new-window (:empty :focused)))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)
                            (arrive out w3)))
        (ok ":new-window (:empty :focused) fills the empty split frame"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) (list w3)))
        (unfocus-all))

      ;; --- :empty with no empty frame falls through to :focused ---

      (let* ((out (mk-out :pp-g-out))
             (w1 (root-win :pp-g1 out))
             (w3 (mk-win :pp-g3 out)))
        (focus w1)
        (call-with-tiling '(:auto-split nil
                            :placement (:new-window (:empty :focused)))
                          (lambda () (arrive out w3)))
        (ok ":empty with no empty frame falls through to the focused frame"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w3 w1)))
        (unfocus-all))

      ;; --- :last picks the frame focused before the current one ---

      (let* ((out (mk-out :pp-h-out))
             (w1 (root-win :pp-h1 out))
             (w2 (mk-win :pp-h2 out))
             (w3 (mk-win :pp-h3 out)))
        (focus w1)
        (call-with-tiling '(:auto-split nil
                            :placement (:new-window (:empty)))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)
                            (arrive out w2)))
        (focus w2)
        (focus w1)
        (call-with-tiling '(:auto-split nil
                            :placement (:new-window (:last)))
                          (lambda () (arrive out w3)))
        (ok ":last picks the frame focused before the current one"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) (list w3 w2)))
        (unfocus-all))

      ;; --- :unfocused picks a frame other than the focused one ---

      (let* ((out (mk-out :pp-i-out))
             (w1 (root-win :pp-i1 out))
             (w2 (mk-win :pp-i2 out))
             (w3 (mk-win :pp-i3 out)))
        (focus w1)
        (call-with-tiling '(:auto-split nil
                            :placement (:new-window (:empty)))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)
                            (arrive out w2)))
        (focus w1)
        (call-with-tiling '(:auto-split nil
                            :placement (:new-window (:unfocused)))
                          (lambda () (arrive out w3)))
        (ok ":unfocused picks a frame other than the focused one"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) (list w3 w2)))
        (unfocus-all))

      ;; --- A function entry chooses the frame ---

      (let* ((out (mk-out :pp-j-out))
             (w1 (root-win :pp-j1 out))
             (w3 (mk-win :pp-j3 out))
             (w4 (mk-win :pp-j4 out))
             (seen nil))
        (focus w1)
        (split-current-frame out 1 :horizontal)
        (call-with-tiling
         (list :auto-split nil
               :placement
               (list :new-window
                     (list (lambda (output workspace window history)
                             (setf seen (list output workspace window
                                              (consp history)))
                             (second (frame-leaves
                                      (frame-tree-for output workspace)))))))
         (lambda () (arrive out w3)))
        (ok "a function entry chooses the frame"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w1) (list w3)))
        (ok "a function entry receives output, workspace, window and history"
            (lambda () seen)
            (list out 1 w3 t))
        (call-with-tiling
         (list :auto-split nil
               :placement
               (list :new-window
                     (list (lambda (&rest args)
                             (declare (ignore args))
                             nil)
                           :focused)))
         (lambda () (arrive out w4)))
        (ok "a function entry returning NIL falls through to the next"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w4 w1) (list w3)))
        (unfocus-all))

      ;; --- A custom policy class, selected by name ---

      (if (fboundp 'define-placement-policy)
          (eval
           '(progn
              (define-placement-policy :test-leftmost test-leftmost-placement
                  (placement-policy)
                ())
              (defmethod placement-new-window-frame
                  ((policy test-leftmost-placement) output workspace window
                   history)
                (declare (ignore window history))
                (first (frame-leaves (frame-tree-for output workspace))))
              (defmethod placement-new-frame-window
                  ((policy test-leftmost-placement) output workspace frame
                   history)
                (declare (ignore output workspace frame history))
                nil)))
          (format t "~&NOTE: define-placement-policy is not defined~%"))
      (let* ((out (mk-out :pp-k-out))
             (w1 (root-win :pp-k1 out))
             (w2 (root-win :pp-k2 out))
             (w3 (mk-win :pp-k3 out)))
        (focus w1)
        (focus w2)
        (call-with-tiling '(:auto-split nil
                            :placement (:policy :test-leftmost))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)
                            (move-window-to-frame-direction out 1 :right)
                            (arrive out w3)))
        (ok "a custom policy selected by name places windows"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w3 w1) (list w2)))
        (unfocus-all))

      ;; --- An unknown policy name falls back to the default ---

      (let* ((out (mk-out :pp-l-out))
             (w1 (root-win :pp-l1 out))
             (w2 (root-win :pp-l2 out)))
        (focus w1)
        (focus w2)
        (call-with-tiling '(:placement (:policy :no-such-policy))
                          (lambda ()
                            (split-current-frame out 1 :horizontal)))
        (ok "an unknown policy name falls back to the default"
            (lambda () (mapcar #'frame-windows (leaves out)))
            (list (list w2) (list w1)))
        (unfocus-all))

      ;; --- Focus history ---

      (let* ((out (mk-out :pp-m-out))
             (w1 (root-win :pp-m1 out))
             (w2 (root-win :pp-m2 out))
             (w3 (root-win :pp-m3 out)))
        (focus w1)
        (focus w2)
        (focus w3)
        (focus w1)
        (ok "the focus history lists the workspace's windows, most recent first"
            (lambda () (history out))
            (list w1 w3 w2))
        (remhash (window-proxy w3) *windows*)
        (ok "the focus history drops dead windows"
            (lambda () (history out))
            (list w1 w2))
        (unfocus-all))

      ;; --- Exports ---

      (ok "the placement protocol symbols are exported"
          (lambda ()
            (mapcar (lambda (name)
                      (nth-value 1 (find-symbol name "RHIZOME")))
                    '("PLACEMENT-POLICY" "PLACEMENT-NEW-FRAME-WINDOW"
                      "PLACEMENT-NEW-WINDOW-FRAME" "DEFINE-PLACEMENT-POLICY"
                      "WORKSPACE-FOCUS-HISTORY")))
          '(:external :external :external :external :external)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
