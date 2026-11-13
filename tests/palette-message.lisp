;;; Tests for the message a command leaves on screen when it runs from
;;; the command palette or the IPC dispatcher. No compositor: messages
;;; are recorded, not drawn.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (shown '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record every message shown, then let the real show-message run
    (let ((real (symbol-function 'show-message)))
      (setf (symbol-function 'show-message)
            (lambda (text &optional (seconds 3))
              (push (list text seconds) shown)
              (funcall real text seconds))))
    ;; The palette's prompt: keep the completion callback
    (setf (symbol-function 'prompt-open)
          (lambda (title &key candidates on-complete initial)
            (declare (ignore title candidates initial))
            (setf (symbol-function 'palette-test-complete) on-complete)
            nil))

    ;; Commands with known replies and messages
    (define-command :palette-test-reply (output args)
      (declare (ignore output args))
      "hello")
    (define-command :palette-test-empty (output args)
      (declare (ignore output args))
      "")
    (define-command :palette-test-nil (output args)
      (declare (ignore output args))
      nil)
    (define-command :palette-test-own (output args)
      (declare (ignore output args))
      (message "own message")
      "reply")

    (labels ((run-palette (choice)
               (setf shown '())
               (command-palette)
               (funcall (symbol-function 'palette-test-complete) choice))
             (texts () (mapcar #'first shown)))

      ;; s-; time: the time stays on screen
      (setf *last-message* "before")
      (run-palette "time")
      (ok "palette time: last message is the time"
          (and (stringp *last-message*)
               (> (length *last-message*) 10)
               (search ":" *last-message*)
               t)
          t)
      (ok "palette time: no empty message is shown"
          (member "" (texts) :test #'equal) nil)

      ;; A reply with no message of its own is shown
      (setf *last-message* "before")
      (run-palette "palette-test-reply")
      (ok "palette reply: the reply is the last message"
          *last-message* "hello")
      (ok "palette reply: shown for the palette's usual time"
          (first shown) '("hello" 3))

      ;; An empty or missing reply never replaces a message
      (setf *last-message* "keep me")
      (run-palette "palette-test-empty")
      (ok "palette empty reply: the earlier message stays"
          *last-message* "keep me")
      (ok "palette empty reply: nothing is shown" (texts) '())
      (setf *last-message* "keep me")
      (run-palette "palette-test-nil")
      (ok "palette nil reply: the earlier message stays"
          *last-message* "keep me")

      ;; A command's own message wins over its reply
      (run-palette "palette-test-own")
      (ok "palette own message: it stays on screen"
          *last-message* "own message")

      ;; An unknown command still says so
      (run-palette "palette-test-no-such-command")
      (ok "palette unknown command: the reply is shown"
          (and (search "unknown command" *last-message*) t) t)

      ;; The IPC dispatcher follows the same rule
      (setf *last-message* "before" shown '())
      (ok "ipc time: the reply is returned"
          (stringp (ipc-dispatch-line "time")) t)
      (ok "ipc time: last message is the time"
          (and (search ":" *last-message*) (> (length *last-message*) 10) t)
          t)
      (setf shown '())
      (ok "ipc reply: returned to the caller"
          (ipc-dispatch-line "palette-test-reply") "hello")
      (ok "ipc reply: echoed briefly" (first shown) '("hello" 1))
      (setf *last-message* "keep me" shown '())
      (ipc-dispatch-line "palette-test-empty")
      (ok "ipc empty reply: the earlier message stays"
          *last-message* "keep me")
      (ok "ipc eval form still shows its result"
          (progn (ipc-dispatch-line "(+ 40 2)") *last-message*) "42"))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
