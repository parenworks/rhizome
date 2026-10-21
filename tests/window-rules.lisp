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
           (length (config-get '(:window-rules))) 2)

    ;; --- identifier rules (river v4 stable IDs) ---

    (configure '(:window-rules ((:match "known-app" :key :identifier
                                        :workspace 5))))

    (let ((win (make-instance 'rhizome-window :proxy :w4)))
      (setf (gethash :w4 *windows*) win
            (window-identifier win) "known-app-123"
            (window-app-id win) "foot")
      (apply-window-rules win)
      (check "identifier rule assigns workspace 5"
             (window-workspace win) 5))

    (let ((win (make-instance 'rhizome-window :proxy :w5)))
      (setf (gethash :w5 *windows*) win
            (window-app-id win) "known-app-similar"
            (window-identifier win) nil)
      (apply-window-rules win)
      (check "identifier rule needs the identifier"
             (window-workspace win) nil))

    ;; --- placement persistence across "restarts" ---

    (let ((*window-state-path* #p"/tmp/rhizome-test-window-state.sexp")
          (*window-state* (make-hash-table :test 'equal)))
      ;; Window with an identifier lives on workspace 4, floats
      (let ((win (make-instance 'rhizome-window :proxy :w6)))
        (setf (gethash :w6 *windows*) win
              (window-identifier win) "persist-me"
              (window-output win) (make-instance 'rhizome-output
                                                  :proxy :out6)
              (window-x win) 10 (window-y win) 10
              (window-width win) 100 (window-height win) 100)
        (assign-window-workspace win 4)
        (float-window win)
        (remember-window-state win)
        ;; "Restart": fresh state hash, fresh window, same identifier
        (let ((*window-state* (make-hash-table :test 'equal)))
          (load-window-state)
          (let ((win2 (make-instance 'rhizome-window :proxy :w7)))
            (setf (gethash :w7 *windows*) win2
                  (window-identifier win2) "persist-me"
                  (window-output win2) (make-instance 'rhizome-output
                                                      :proxy :out7)
                  (window-x win2) 10 (window-y win2) 10
                  (window-width win2) 100 (window-height win2) 100)
            (assign-window-workspace win2 1)
            (restore-window-state win2)
            (check "remembered workspace restored"
                   (window-workspace win2) 4)
            (check "remembered float state restored"
                   (window-floating-p win2) t))))
      ;; The store went through the file
      (check "window state persisted to disk"
             (and (probe-file *window-state-path*) t) t)
      (ignore-errors (delete-file *window-state-path*)))

    ;; A window WITHOUT an identifier remembers nothing
    (let ((*window-state-path* #p"/tmp/rhizome-test-window-state2.sexp")
          (*window-state* (make-hash-table :test 'equal)))
      (let ((win (make-instance 'rhizome-window :proxy :w8)))
        (setf (gethash :w8 *windows*) win
              (window-identifier win) nil)
        (remember-window-state win))
      (check "no identifier, nothing remembered"
             (hash-table-count *window-state*) 0))
    (ignore-errors (delete-file #p"/tmp/rhizome-test-window-state2.sexp"))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1))))
