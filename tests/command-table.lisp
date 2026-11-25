;;; Tests for the exported command table API: a command defined from
;;; another package through rhizome:define-command runs through
;;; execute-command, the IPC dispatcher, the command palette, and
;;; describe-command; a redefinition replaces it. No compositor:
;;; messages are recorded, not drawn.
(ql:quickload :rhizome)

(defpackage #:command-table-test-module
  (:use #:cl))

(in-package #:command-table-test-module)

(rhizome:define-command :command-table-test-greet (output args)
  "Greet each argument."
  (declare (ignore output))
  (format nil "hello~{ ~A~}" args))

(defun redefine-greet ()
  "Redefine the test command with no docstring."
  (rhizome:define-command :command-table-test-greet (output args)
    (declare (ignore output args))
    "bye"))

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (candidates '()) (complete nil))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Prompts: keep the candidates and the completion callback
    (setf (symbol-function 'prompt-open)
          (lambda (title &key ((:candidates offered)) on-complete initial)
            (declare (ignore title initial))
            (setf candidates (copy-list offered)
                  complete on-complete)
            nil))
    (flet ((prompt (fn choice)
             (funcall fn)
             (funcall complete choice)))

      ;; The registration API is external
      (dolist (name '("DEFINE-COMMAND" "EXECUTE-COMMAND"
                      "FIND-COMMAND" "COMMAND-NAMES"))
        (ok (format nil "~A is external" name)
            (nth-value 1 (find-symbol name "RHIZOME"))
            :external))

      ;; Lookup and listing
      (ok "find-command returns the handler"
          (functionp (rhizome:find-command :command-table-test-greet)) t)
      (ok "find-command of an unknown name is NIL"
          (rhizome:find-command :command-table-test-no-such-command) nil)
      (ok "command-names lists the command once"
          (count :command-table-test-greet (rhizome:command-names)) 1)
      (ok "command-names is sorted"
          (let ((names (rhizome:command-names)))
            (equal names (sort (copy-list names) #'string<)))
          t)
      (ok "command-names lists built-in commands"
          (and (member :lastmsg (rhizome:command-names)) t) t)

      ;; Direct call
      (ok "execute-command runs it and returns its reply"
          (rhizome:execute-command nil :command-table-test-greet '("a" "b"))
          "hello a b")

      ;; IPC dispatch
      (ok "ipc dispatch runs it"
          (ipc-dispatch-line "command-table-test-greet x 2") "hello x 2")
      (ok "ipc unknown command lists it"
          (and (search "COMMAND-TABLE-TEST-GREET"
                       (ipc-dispatch-line "command-table-test-nope"))
               t)
          t)

      ;; Command palette
      (setf *last-message* "before")
      (prompt #'command-palette "command-table-test-greet")
      (ok "palette offers it"
          (and (member "command-table-test-greet" candidates
                       :test #'string=)
               t)
          t)
      (ok "palette runs it" *last-message* "hello")

      ;; describe-command shows the docstring
      (prompt #'describe-command-prompt "command-table-test-greet")
      (ok "describe-command offers it"
          (and (member "command-table-test-greet" candidates
                       :test #'string=)
               t)
          t)
      (ok "describe-command shows the docstring"
          *last-message* "command-table-test-greet: Greet each argument.")

      ;; Redefinition replaces handler and documentation
      (command-table-test-module::redefine-greet)
      (ok "redefinition replaces the handler"
          (rhizome:execute-command nil :command-table-test-greet nil) "bye")
      (ok "redefinition keeps one entry"
          (count :command-table-test-greet (rhizome:command-names)) 1)
      (ok "ipc dispatch runs the new definition"
          (ipc-dispatch-line "command-table-test-greet") "bye")
      (prompt #'describe-command-prompt "command-table-test-greet")
      (ok "redefinition without a docstring drops the old one"
          *last-message* "command-table-test-greet: no documentation"))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
