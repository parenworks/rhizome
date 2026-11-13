;;; Tests for the empty-frame pruning policy (frames.lisp) driven through
;;; the split, move, manage and IPC paths. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record calls instead of touching river; show/hide update state
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  message request-manage-dirty))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (if (and args (typep (first args) 'rhizome-window))
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
             (leaves (out)
               (frame-leaves (frame-tree-for out 1)))
             (nleaves (out)
               (length (leaves out)))
             (arrive (out win)
               (frames-manage-decisions out 1 (list win)))
             (close-win (out win)
               (remhash (window-proxy win) *windows*)
               (frames-manage-decisions out 1 '()))
             (call-with-policy (value thunk)
               (let ((*config* (merge-plist *config*
                                            (list :frames
                                                  (list :prune-empty value)))))
                 (funcall thunk))))

      ;; --- A: a user split survives the manage pass ---

      (let* ((out (mk-out :fp-a-out))
             (w (root-win :fp-a1 out)))
        (setf (window-focused-p w) t)
        (split-current-frame out 1 :horizontal)
        (apply-layout-for-output out)
        (ok "a user split keeps both frames through a manage pass"
            (nleaves out) 2)
        (ok "the split window keeps half the width after a manage pass"
            (let ((frame (window-frame w)))
              (and frame (frame-width frame)))
            960)
        (move-window-to-frame-direction out 1 :right)
        (ok "moving the window right finds the split frame"
            (eq (window-frame w) (second (leaves out)))
            t)
        (setf (window-focused-p w) nil))

      ;; --- B: moving the last window out keeps its frame ---

      (let* ((out (mk-out :fp-b-out))
             (w (root-win :fp-b1 out)))
        (setf (window-focused-p w) t)
        (split-current-frame out 1 :horizontal)
        (move-window-to-frame-direction out 1 :right)
        (apply-layout-for-output out)
        (ok "moving the last window out of a user frame keeps the frame"
            (mapcar (lambda (frame) (length (frame-windows frame)))
                    (leaves out))
            '(0 1))
        (setf (window-focused-p w) nil))

      ;; --- C: an IPC split is a user split ---

      (let ((out (mk-out :fp-c-out)))
        (root-win :fp-c1 out)
        (execute-command out :split-vertical nil)
        (apply-layout-for-output out)
        (ok "an IPC split keeps its frame through a manage pass"
            (nleaves out) 2))

      ;; --- D: an auto-split frame closes with its window ---

      (let* ((out (mk-out :fp-d-out))
             (d1 (mk-win :fp-d1 out))
             (d2 (mk-win :fp-d2 out))
             (after-arrivals nil))
        (arrive out d1)
        (arrive out d2)
        (setf after-arrivals (nleaves out))
        (close-win out d2)
        (ok "closing the window in an auto-split frame prunes the frame"
            (list after-arrivals (nleaves out))
            '(2 1)))

      ;; --- E: an auto-split inside a user frame prunes back to it ---

      (let* ((out (mk-out :fp-e-out))
             (e1 (mk-win :fp-e1 out))
             (e2 (mk-win :fp-e2 out))
             (counts '()))
        (root-win :fp-e0 out)
        (split-current-frame out 1 :horizontal)
        (focus-frame-direction out 1 :right)
        (arrive out e1)
        (arrive out e2)
        (close-win out e2)
        (push (nleaves out) counts)
        (close-win out e1)
        (push (nleaves out) counts)
        (ok "an auto-split inside a user frame prunes back to the user frame"
            (reverse counts)
            '(2 2)))

      ;; --- F: :never keeps every empty frame ---

      (let* ((out (mk-out :fp-f-out))
             (f1 (mk-win :fp-f1 out))
             (f2 (mk-win :fp-f2 out)))
        (call-with-policy :never
                          (lambda ()
                            (arrive out f1)
                            (arrive out f2)
                            (close-win out f2)))
        (ok ":never keeps an auto-split frame after its window closes"
            (nleaves out) 2))

      ;; --- G: :always prunes a user split ---

      (let ((out (mk-out :fp-g-out)))
        (root-win :fp-g1 out)
        (split-current-frame out 1 :horizontal)
        (call-with-policy :always
                          (lambda () (apply-layout-for-output out)))
        (ok ":always prunes a user split"
            (nleaves out) 1))

      ;; --- H: remove-current-split leaves nothing marked ---

      (let* ((out (mk-out :fp-h-out))
             (h1 (mk-win :fp-h1 out))
             (counts '()))
        (root-win :fp-h0 out)
        (split-current-frame out 1 :horizontal)
        (remove-current-split out 1)
        (arrive out h1)
        (push (nleaves out) counts)
        (close-win out h1)
        (push (nleaves out) counts)
        (ok "remove-current-split clears the user mark"
            (reverse counts)
            '(2 1)))

      ;; --- I: removing a nested split keeps the split above it ---

      (let ((out (mk-out :fp-i-out)))
        (root-win :fp-i1 out)
        (split-current-frame out 1 :horizontal)
        (split-current-frame out 1 :vertical)
        (remove-current-split out 1)
        (apply-layout-for-output out)
        (ok "removing one split keeps the user split above it"
            (nleaves out) 2))

      ;; --- J: frame-only leaves nothing marked ---

      (let* ((out (mk-out :fp-j-out))
             (j1 (mk-win :fp-j1 out))
             (counts '()))
        (root-win :fp-j0 out)
        (split-current-frame out 1 :horizontal)
        (split-current-frame out 1 :vertical)
        (frame-only out 1)
        (push (nleaves out) counts)
        (arrive out j1)
        (push (nleaves out) counts)
        (close-win out j1)
        (push (nleaves out) counts)
        (ok "frame-only clears the user mark"
            (reverse counts)
            '(1 2 1)))

      ;; --- Configuration default and exports ---

      (ok "the default prune policy is :auto"
          (config-getf '(:frames) :prune-empty :config (default-config))
          :auto)
      (ok "window-frame, frame-windows and window-output are exported"
          (mapcar (lambda (name)
                    (nth-value 1 (find-symbol name "RHIZOME")))
                  '("WINDOW-FRAME" "FRAME-WINDOWS" "WINDOW-OUTPUT"))
          '(:external :external :external)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
