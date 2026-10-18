;;; Tests for the interactive prompt and message window logic.
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

    ;; Stub only the rendering; prompt-close is safe against a nil
    ;; state and must run for real — stubbing it once hid a bug where
    ;; the completion callback was cleared before it could run.
    (dolist (fn '(prompt-redraw))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'message-hide)
          (lambda ()
            (setf *message-state* nil
                  *message-expiry* 0)))
    (setf (symbol-function 'show-message)
          (lambda (text &optional (seconds 3))
            (declare (ignore seconds))
            (setf *message-expiry* (+ (get-universal-time) seconds))
            text))

    ;; Prompt filtering
    (setf *prompt-candidates* '("Vivaldi - GitHub" "emacs@desktop" "foot")
          *prompt-query* "git")
    (ok "filter matches substring case-insensitively"
       (prompt-filtered) '("Vivaldi - GitHub"))
    (setf *prompt-query* "")
    (ok "empty query shows all candidates"
       (prompt-filtered) '("Vivaldi - GitHub" "emacs@desktop" "foot"))

    ;; Selection cycling via key handling
    (setf *prompt-candidates* '("alpha" "beta" "gamma")
          *prompt-query* "" *prompt-selection* 0
          *prompt-on-complete* nil)
    (prompt-handle-key 65364)                 ; Down
    (ok "down moves selection" *prompt-selection* 1)
    (prompt-handle-key 65364)                 ; Down
    (prompt-handle-key 65364)                 ; Down (clamped at end)
    (ok "down clamps at last" *prompt-selection* 2)
    (prompt-handle-key 65362)                 ; Up
    (ok "up moves selection" *prompt-selection* 1)
    (prompt-handle-key 65289)                 ; Tab: next, wraps
    (prompt-handle-key 65289)                 ; Tab: wraps to 0
    (ok "tab cycles with wrap" *prompt-selection* 0)

    ;; Enter completes with the selection (real prompt-close runs, so
    ;; the state hash must exist like it does when a prompt is open)
    (let ((completed :none))
      (setf *prompt-active* t
            *prompt-state* (make-hash-table)
            *prompt-expiry* (+ (get-universal-time) 30)
            *prompt-on-complete* (lambda (choice) (setf completed choice)))
      (prompt-handle-key 65364)               ; Down -> "beta"
      (prompt-handle-key 65293)               ; Enter
      (ok "enter completes with selection" completed "beta")
      (ok "enter clears the active flag" *prompt-active* nil)
      (ok "enter clears the callback" *prompt-on-complete* nil))

    ;; Typing filters and resets selection
    (setf *prompt-candidates* '("foo" "bar" "baz") *prompt-query* ""
          *prompt-selection* 2 *prompt-active* t
          *prompt-state* (make-hash-table)
          *prompt-on-complete* nil)
    (prompt-handle-key 98)                    ; 'b'
    (ok "typing filters candidates"
       (prompt-filtered) '("bar" "baz"))
    (ok "typing resets selection" *prompt-selection* 0)

    ;; Message expiry drain
    (setf *message-expiry* (- (get-universal-time) 1)
          *message-state* :fake)
    (message-drain)
    (ok "expired message hides" *message-state* nil)

    ;; Fresh message does not hide
    (setf *message-state* :fake
          *message-expiry* (+ (get-universal-time) 100))
    (message-drain)
    (ok "unexpired message stays" *message-state* :fake)

    ;; message formatting helper
    (ok "message formats"
       (message "workspace ~A" 4) "workspace 4"))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
