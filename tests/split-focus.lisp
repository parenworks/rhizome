;;; Tests for the current frame after a split: it stays on the original
;;; half (the first child), with or without a focused window, as
;;; StumpWM's hsplit/vsplit keep focus. River requests are stubbed.
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
             (root-win (proxy out)
               ;; A window on OUT, workspace 1, placed in the root frame
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) out)
                 (assign-window-workspace win 1)
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
             (root (out)
               (frame-tree-for out 1))
             (current (out)
               (current-frame-for out 1)))

      ;; --- An empty frame: the current frame stays on the original half ---

      (let ((out (mk-out :sf-empty-out)))
        (unfocus-all)
        (split-current-frame out 1 :horizontal)
        (ok "splitting an empty frame keeps the current frame on the original half"
            (lambda () (eq (current out) (frame-first-child (root out))))
            t))

      (let ((out (mk-out :sf-empty-v-out)))
        (unfocus-all)
        (split-current-frame out 1 :vertical)
        (ok "a vertical split of an empty frame keeps the current frame on the original half"
            (lambda () (eq (current out) (frame-first-child (root out))))
            t))

      ;; --- A second split splits the original half again ---

      (let ((out (mk-out :sf-twice-out)))
        (unfocus-all)
        (split-current-frame out 1 :horizontal)
        (split-current-frame out 1 :vertical)
        (ok "a second split divides the original half, not the new one"
            (lambda ()
              (let ((first (frame-first-child (root out))))
                (list (frame-split-direction first)
                      (eq (current out) (frame-first-child first)))))
            '(:vertical t)))

      ;; --- An unfocused window: the current frame stays on the original half ---

      (let* ((out (mk-out :sf-unfocused-out))
             (w1 (root-win :sf-u1 out)))
        (unfocus-all)
        (split-current-frame out 1 :horizontal)
        (ok "with no focused window the current frame is the original half, holding the window"
            (lambda ()
              (let ((cur (current out)))
                (list (eq cur (frame-first-child (root out)))
                      (frame-windows cur))))
            (list t (list w1))))

      ;; --- A focused window: the current frame stays on the original half ---

      (let* ((out (mk-out :sf-focused-out))
             (w1 (root-win :sf-f1 out)))
        (focus w1)
        (split-current-frame out 1 :horizontal)
        (ok "with a focused window the current frame stays on the original half"
            (lambda () (eq (current out) (frame-first-child (root out))))
            t)
        (unfocus-all)
        (ok "with focus cleared the current frame still stays on the original half"
            (lambda () (eq (current out) (frame-first-child (root out))))
            t))

      ;; --- The new half is still filled by the placement policy ---

      (let* ((out (mk-out :sf-fill-out))
             (w1 (root-win :sf-p1 out))
             (w2 (root-win :sf-p2 out)))
        (focus w1)
        (focus w2)
        (split-current-frame out 1 :horizontal)
        (ok "the new half still shows the last hidden window"
            (lambda () (frame-windows (frame-second-child (root out))))
            (list w1))
        (ok "the focused window and the current frame stay on the original half"
            (lambda ()
              (list (window-focused-p w2)
                    (eq (current out) (frame-first-child (root out)))
                    (frame-windows (current out))))
            (list t t (list w2)))
        (unfocus-all)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
