;;; Tests for the second medium batch: frame dump/restore (fdump),
;;; window marks, and set-prefix-key.
(ql:quickload :stumpway)

(in-package #:stumpway)

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

    ;; Fresh global state
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *window-number-map* (make-hash-table)
          *window-number-overflow* 10
          *marked-windows* (make-hash-table)
          *keybindings* (make-hash-table :test 'equal)
          *prefix-key-binding* (cons 116 '(:ctrl)))

    ;; --- Frame dump/restore ---

    (let* ((out (make-instance 'stumpway-output :proxy :dump-out
                               :width 1920 :height 1080))
           (w1 (make-instance 'stumpway-window :proxy :d1))
           (w2 (make-instance 'stumpway-window :proxy :d2))
           (w3 (make-instance 'stumpway-window :proxy :d3))
           (w4 (make-instance 'stumpway-window :proxy :d4))
           (tmp-file "/tmp/stumpway-frame-dump-test.lisp"))
      (setf (gethash :d1 *windows*) w1
            (gethash :d2 *windows*) w2
            (gethash :d3 *windows*) w3
            (gethash :d4 *windows*) w4)
      (dolist (w (list w1 w2 w3 w4))
        (setf (window-output w) out)
        (assign-window-number w)
        (assign-window-workspace w 3))
      (setf (gethash out *output-workspace*) 3)

      ;; Build: 960|960, right half stacked; w4 in left, w1 visible +
      ;; w2 hidden in right-top, w3 in right-bottom
      (set-workspace-tree out 3
                          '((0 0 960 1080) (960 0 960 540)
                            (960 540 960 540))
                          '())
      (let* ((root (frame-tree-for out 3))
             (leaves (frame-leaves root)))
        (frame-push-window w4 (first leaves))
        (frame-push-window w2 (second leaves))
        (frame-push-window w1 (second leaves))
        (frame-push-window w3 (third leaves)))

      ;; Dump: the sexp carries rects and numbers
      (let ((sexp (frame-tree-to-sexp (frame-tree-for out 3))))
        (ok "dump is a split at the root"
            (first sexp) :split)
        (ok "dump root rect"
            (dump-rect sexp) '(0 0 1920 1080))
        (ok "dump records leaf numbers"
            (dump-leaf-numbers sexp)
            '((4) (1 2) (3))))

      ;; Dump to file and restore into a clean workspace state
      (dump-frame out 3 tmp-file)
      (ok "dump file written"
          (with-open-file (s tmp-file) (eq (first (read s)) :split)) t)
      ;; Wipe the tree and frames, then restore
      (setf *frame-trees* (make-hash-table :test 'equal))
      (dolist (w (list w1 w2 w3 w4))
        (setf (window-frame w) nil))
      (restore-frame out 3 tmp-file)
      (let* ((root (frame-tree-for out 3))
             (leaves (frame-leaves root)))
        (ok "restored tree has the same shape"
            (list (length leaves)
                  (frame-split-direction root)
                  (frame-split-direction (frame-second-child root)))
            '(3 :horizontal :vertical))
        (ok "restored rects are exact"
            (list (frame-width (second leaves))
                  (frame-height (first (last leaves))))
            '(960 540))
        (ok "restored windows in the right frames"
            (list (window-frame w4) (window-frame w2)
                  (window-frame w1) (window-frame w3))
            (list (first leaves) (second leaves)
                  (second leaves) (third leaves)))
        (ok "restored visible order"
            (mapcar #'window-number (frame-windows (second leaves)))
            '(1 2))
        (ok "restore moved current frame to the populated first leaf"
            (current-frame-for out 3) (first leaves)))

      ;; Scaling: restore the 1920x1080 dump into a 960x540 output
      (let ((small (make-instance 'stumpway-output :proxy :small-out
                                 :width 960 :height 540)))
        (setf (gethash small *output-workspace*) 3)
        (setf *frame-trees* (make-hash-table :test 'equal))
        (dolist (w (list w1 w2 w3 w4))
          (setf (window-frame w) nil))
        (restore-frame small 3 tmp-file)
        (let ((leaves (frame-leaves (frame-tree-for small 3))))
          (ok "restored rects scale to the smaller output"
              (list (frame-width (first leaves))
                    (frame-height (first (last leaves))))
              '(480 270))))

      ;; Windows missing from the dump pile onto the last frame:
      ;; restore with w4's number gone
      (let ((extra (make-instance 'stumpway-window :proxy :extra)))
        (setf (gethash :extra *windows*) extra
              (window-output extra) out)
        (assign-window-number extra)
        (assign-window-workspace extra 3)
        (remhash w4 *window-number-map*)   ; simulate w4 gone
        (setf *frame-trees* (make-hash-table :test 'equal))
        (dolist (w (list w1 w2 w3 extra))
          (setf (window-frame w) nil))
        (restore-frame out 3 tmp-file)
        (ok "unmentioned windows pile onto the last frame"
            (window-frame extra)
            (first (last (frame-leaves (frame-tree-for out 3)))))))

    ;; --- Window marks ---

    (let* ((out (make-instance 'stumpway-output :proxy :mark-out
                                :width 1920 :height 1080))
           (m1 (make-instance 'stumpway-window :proxy :m1))
           (m2 (make-instance 'stumpway-window :proxy :m2)))
        (setf (gethash :m1 *windows*) m1
              (gethash :m2 *windows*) m2)
        (dolist (w (list m1 m2))
          (setf (window-output w) out)
          (assign-window-number w)
          (assign-window-workspace w 5))
        (setf (gethash out *output-workspace*) 5)
        (toggle-window-mark m1)
        (ok "mark sets the flag"
            (window-marked-p m1) t)
        (toggle-window-mark m2)
        (toggle-window-mark m2)
        (ok "second toggle unmarks"
            (window-marked-p m2) nil)
        (ok "marked-windows lists the marked"
            (marked-windows) (list m1))
        ;; closing a marked window drops the mark
        (release-window-mark m1)
        (toggle-window-mark m1)
        (remhash :m1 *windows*)
        (release-window-mark m1)
        (ok "mark released with the window"
            (window-marked-p m1) nil)
        ;; move-marked: m1 gone, mark m2 and move it to workspace 6
        (setf *marked-windows* (make-hash-table))
        (toggle-window-mark m2)
        (move-marked-to-workspace 6)
        (ok "move-marked moves the window"
            (window-workspace m2) 6)
        (ok "move-marked clears the marks"
            (marked-windows) '()))

    ;; --- set-prefix-key ---

    ;; parse-key-spec
    (multiple-value-bind (mods keysym) (parse-key-spec "C-t")
      (ok "parse C-t" (list mods keysym) '((:ctrl) 116)))
    (multiple-value-bind (mods keysym) (parse-key-spec "C-M-t")
      (ok "parse C-M-t" (list mods keysym) '((:ctrl :mod1) 116)))
    (multiple-value-bind (mods keysym) (parse-key-spec "s-Space")
      (ok "parse s-Space" (list mods keysym) '((:mod4) 32)))
    (multiple-value-bind (mods keysym) (parse-key-spec "M-Return")
      (ok "parse M-Return" (list mods keysym) '((:mod1) 65293)))
    (multiple-value-bind (mods keysym) (parse-key-spec "Q")
      (ok "parse bare key has no modifiers" (list mods keysym)
          '(nil 113)))
    (multiple-value-bind (mods keysym) (parse-key-spec "C-banana")
      (ok "junk suffix rejected" (list mods keysym) '((:ctrl) nil)))

    ;; set-prefix-key swaps the binding in the table
    (setf *keybindings* (make-hash-table :test 'equal)
          *prefix-key-binding* (cons 116 '(:ctrl)))
    (setf (gethash (cons 116 '(:ctrl)) *keybindings*) :old)
    (set-prefix-key "C-z")
    (ok "old prefix binding removed"
        (gethash (cons 116 '(:ctrl)) *keybindings*) nil)
    (ok "new prefix binding installed"
        (and (gethash (cons 122 '(:ctrl)) *keybindings*) t) t)
    (ok "prefix binding tracked"
        *prefix-key-binding* '(122 . (:ctrl)))
    (set-prefix-key "C-banana")
    (ok "bad spec leaves the binding alone"
        *prefix-key-binding* '(122 . (:ctrl))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
