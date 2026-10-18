;;; Window rules regression tests.
;;; Rules match app-id/title regexes and assign workspaces.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run (configure -> apply-config
;; starts the bar process otherwise!)
(configure '(:bar (:enabled nil)))

;; request-manage-dirty is a no-op without a WM proxy, so no stubs are
;; needed for river requests here.

(configure '(:window-rules ((:match "vivaldi" :workspace 2)
                            (:match "emacs" :key :title :workspace 3))))

(let ((pass 0) (fail 0))
  (flet ((check (name actual expected)
           (if (eql actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~A, wanted ~A)~%"
                              name actual expected)))))

    ;; App-id match: "vivaldi-stable" contains "vivaldi"
    (let ((win (make-instance 'rhizome-window :proxy :w1)))
      (setf (gethash :w1 *windows*) win
            (window-app-id win) "vivaldi-stable")
      (apply-window-rules win)
      (check "vivaldi rule assigns workspace 2"
             (window-workspace win) 2))

    ;; Title match: "my GNU Emacs" matches "emacs" case-insensitively
    (let ((win (make-instance 'rhizome-window :proxy :w2)))
      (setf (gethash :w2 *windows*) win
            (window-title win) "my GNU Emacs")
      (apply-window-rules win)
      (check "title rule assigns workspace 3"
             (window-workspace win) 3))

    ;; No rule matches: window keeps its current (nil) workspace
    (let ((win (make-instance 'rhizome-window :proxy :w3)))
      (setf (gethash :w3 *windows*) win
            (window-app-id win) "foot")
      (apply-window-rules win)
      (check "unmatched window left alone" (window-workspace win) nil))

    ;; Layout normalization: string layout names become keywords
    (check "string layout normalized"
           (find-layout-symbol "master-stack") :master-stack)

    ;; Rules list survives the config merge heuristic
    (check "rules present in config"
           (length (config-get '(:window-rules))) 2))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
