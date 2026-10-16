;;; Tests for the first "bigger" batch: minor modes (prefix-scoped
;;; keymap overlays), WSE window selection expressions, and fuzzy
;;; filtering (prompt + launcher helpers).
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
          *marked-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *window-number-map* (make-hash-table)
          *window-number-overflow* 10
          *prefix-mode-active* nil
          *prefix-current-map* nil
          *prefix-command-mode* nil
          *iresize-active* nil
          *prefix-describe-key-pending* nil
          *minor-mode-keymaps* (make-hash-table)
          *minor-mode-stack* nil
          *prefix-map* (make-keymap)
          *prefix-key-docs* (make-hash-table))

    ;; --- Fuzzy matching ---

    (ok "empty query matches everything"
        (fuzzy-match-p "" "anything") t)
    (ok "fuzzy matches out-of-order chars"
        (fuzzy-match-p "ff" "Firefox") t)
    (ok "fuzzy skips ahead in order"
        (fuzzy-match-p "fx" "Firefox") t)
    (ok "fuzzy rejects reversed chars"
        (fuzzy-match-p "xf" "Firefox") nil)
    (ok "fuzzy is case-insensitive"
        (fuzzy-match-p "FIR" "firefox") t)
    (ok "substring is also a fuzzy match"
        (fuzzy-match-p "refo" "Firefox") t)
    (ok "filter keeps substring matches first"
        (fuzzy-filter "ff" '("diff" "Firefox" "faff" "nope"))
        '("diff" "faff" "Firefox"))
    (ok "empty query returns candidates unchanged"
        (fuzzy-filter "" '("a" "b")) '("a" "b"))

    ;; --- WSE ---

    (let* ((out (make-instance 'stumpway-output :proxy :wse-out
                              :width 1920 :height 1080))
           (ff (make-instance 'stumpway-window :proxy :ff))
           (em (make-instance 'stumpway-window :proxy :em)))
      (setf (gethash :ff *windows*) ff
            (gethash :em *windows*) em
            (window-app-id ff) "org.mozilla.firefox"
            (window-title ff) "GitHub - stumpway"
            (window-app-id em) "emacs"
            (window-title em) "main.lisp"
            (window-output ff) out
            (window-output em) out
            (window-floating-p ff) t
            (window-focused-p em) t)
      (assign-window-workspace ff 2)
      (assign-window-workspace em 1)
      (assign-window-number ff)
      (assign-window-number em)
      (setf (gethash out *output-workspace*) 1)

      (ok "app-id regex matches"
          (and (wse-matches-p ff '(:app-id "mozilla")) t) t)
      (ok "title regex matches"
          (and (wse-matches-p ff '(:title "stumpway")) t) t)
      (ok "unrelated title does not match"
          (wse-matches-p em '(:title "GitHub")) nil)
      (ok "and combines predicates"
          (wse-windows '(:and (:app-id "emacs") (:focused t))) (list em))
      (ok "or matches either"
          (sort (mapcar #'window-app-id
                        (wse-windows '(:or (:app-id "emacs")
                                           (:app-id "mozilla"))))
                #'string<)
          '("emacs" "org.mozilla.firefox"))
      (ok "not negates"
          (wse-windows '(:not (:float t))) (list em))
      (ok "any matches everything"
          (length (wse-windows '(:any))) 2)
      (ok "float selects floats"
          (wse-windows '(:float t)) (list ff))
      (ok "marked selects marks"
          (progn (toggle-window-mark em)
             (prog1 (wse-windows '(:marked t))
               (remhash em *marked-windows*)))
          (list em))
      (ok "number selects one window"
          (wse-windows (list :number (window-number em))) (list em))
      (ok "wse-map visits matches"
          (let ((seen 0))
            (wse-map '(:float t) (lambda (w) (declare (ignore w)) (incf seen)))
            seen)
          1)

      ;; window rules accept :wse instead of :match
      (configure '(:window-rules ((:wse (:and (:app-id "emacs")
                                             (:float nil))
                                       :float t))))
      (setf (window-floating-p em) nil)
      (apply-window-rules em)
      (ok ":wse rule floats the matching window"
          (window-floating-p em) t))

    ;; --- Minor modes ---

    (setup-default-prefix-bindings)
    (define-minor-mode :test-mode "a test mode")
    (let ((ran nil))
      (define-minor-mode-key :test-mode 97        ; 'a' — normally time
                           (lambda () (setf ran :mode)))
      (ok "mode disabled: prefix binding intact"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*)
            (prefix-handle-key 97)
            (list *prefix-mode-active* ran))
          '(nil nil))
      (enable-minor-mode :test-mode)
      (ok "mode enabled: binding takes precedence"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*
                  ran nil)
            (prefix-handle-key 97)
            (list *prefix-mode-active* ran))
          '(nil :mode))
      ;; a mode binding for a key the prefix map does not have
      (define-minor-mode-key :test-mode 122       ; 'z'
                           (lambda () (setf ran :zed)))
      (ok "mode can bind unbound keys"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*
                  ran nil)
            (prefix-handle-key 122)
            (list *prefix-mode-active* ran))
          '(nil :zed))
      ;; most recently enabled mode wins conflicts
      (define-minor-mode :other-mode)
      (define-minor-mode-key :other-mode 122
                           (lambda () (setf ran :other)))
      (enable-minor-mode :other-mode)
      (ok "newest mode wins conflicts"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*
                  ran nil)
            (prefix-handle-key 122)
            ran)
          :other)
      (disable-minor-mode :other-mode)
      (ok "disabling restores the older mode's binding"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*
                  ran nil)
            (prefix-handle-key 122)
            ran)
          :zed)
      (ok "enabled-minor-modes lists the stack"
          (enabled-minor-modes) '(:test-mode))
      (disable-minor-mode :test-mode)
      (ok "mode disabled: prefix binding back"
          (progn
            (setf *prefix-mode-active* t
                  *prefix-current-map* *prefix-map*
                  ran nil)
            (prefix-handle-key 97)
            (list ran))
          '(nil))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
