;;; Tests for hooks and the eval prompt evaluator.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((check (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~A, wanted ~A)~%"
                              name actual expected)))))

    ;; Hooks fire with args
    (let ((got '()))
      (add-hook :test-hook (lambda (a b) (push (list a b) got)))
      (run-hook :test-hook 1 2)
      (check "hook fired with args"
             (first got) '(1 2))
      (remove-hook :test-hook (first (gethash :test-hook *hooks*)))
      (run-hook :test-hook 3 4)
      (check "removed hook does not fire"
             (length got) 1)
      (clear-hook :test-hook))

    ;; Hook errors are isolated and do not stop other handlers
    (let ((got '()))
      (add-hook :test-hook-2 (lambda () (error "boom")))
      (add-hook :test-hook-2 (lambda () (push :after got)))
      (run-hook :test-hook-2)
      (check "handler after error still runs"
             (first got) :after)
      (clear-hook :test-hook-2))

    ;; Eval: simple arithmetic in the rhizome package
    (setf *eval-prompt-query* "(+ 1 2)")
    (eval-prompt-eval *eval-prompt-query*)
    (check "eval (+ 1 2) => 3"
           (read-from-string *eval-prompt-result*) 3)

    ;; Eval: WM symbols are readable without package prefixes
    (setf *eval-prompt-query* "*workspace-count*")
    (eval-prompt-eval *eval-prompt-query*)
    (check "eval resolves WM symbols"
           (read-from-string *eval-prompt-result*)
           *workspace-count*)

    ;; Eval: errors are caught and reported, not thrown
    (setf *eval-prompt-query* "(error \"deliberate\")")
    (eval-prompt-eval *eval-prompt-query*)
    (check "eval errors caught"
           (and (search "error" *eval-prompt-result*) t) t))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
