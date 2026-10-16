;;; Tests for the prefix key engine (C-t chains).
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

    ;; Stub surface-touching functions; message records instead of shows
    (dolist (fn '(exit-prefix-mode))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (let ((messages '()))
      (setf (symbol-function 'message)
            (lambda (fmt &rest args)
              (push (apply #'format nil fmt args) messages)))

      ;; A scratch keymap: 'a' runs a flag, 'b' descends to a nested map
      (let* ((ran :none)
             (nested (make-keymap))
             (root (make-keymap))
             (*prefix-map* root)
             (*prefix-mode-active* t)
             (*prefix-current-map* root)
             (*prefix-expiry* 0))
        ;; Stub exit-prefix-mode above only handles surfaces; emulate
        ;; the state clearing the real one does
        (setf (symbol-function 'exit-prefix-mode)
              (lambda ()
                (setf *prefix-mode-active* nil
                      *prefix-current-map* nil
                      *prefix-expiry* 0)))
        (define-keymap-key root 97 (lambda () (setf ran :a)))   ; a
        (define-keymap-key root 98 nested)                      ; b
        (define-keymap-key nested 99 (lambda () (setf ran :c))) ; c

        ;; Action key runs and ends the chain
        (setf ran :none *prefix-mode-active* t *prefix-current-map* root)
        (prefix-handle-key 97)              ; a
        (ok "action key executes" ran :a)
        (ok "chain ends after action" *prefix-mode-active* nil)

        ;; Nested map: first key descends, second executes
        (setf ran :none *prefix-mode-active* t *prefix-current-map* root)
        (prefix-handle-key 98)              ; b -> descend
        (ok "nested map keeps chain open" *prefix-mode-active* t)
        (ok "current map is the nested map" (eq *prefix-current-map* nested) t)
        (prefix-handle-key 99)              ; c -> action
        (ok "nested action executes" ran :c)

        ;; Unmapped key aborts with feedback
        (setf messages '()
              *prefix-mode-active* t *prefix-current-map* root)
        (prefix-handle-key 122)             ; z
        (ok "unmapped key aborts" *prefix-mode-active* nil)
        (ok "abort shows message" (and (search "unbound" (first messages)) t) t)

        ;; Escape aborts
        (setf messages '()
              *prefix-mode-active* t *prefix-current-map* root)
        (prefix-handle-key 65307)           ; Escape
        (ok "escape aborts" *prefix-mode-active* nil)
        (ok "escape shows aborted" (and (search "abort" (first messages)) t) t)

        ;; Expiry drain
        (setf *prefix-mode-active* t
              *prefix-expiry* (- (get-universal-time) 1))
        (prefix-drain)
        (ok "expired prefix mode drains" *prefix-mode-active* nil)

        ;; Fresh prefix mode does not drain
        (setf *prefix-mode-active* t
              *prefix-expiry* (+ (get-universal-time) 100))
        (prefix-drain)
        (ok "unexpired prefix mode stays" *prefix-mode-active* t)

        ;; Default bindings present
        (setf *prefix-map* (make-keymap)
              *prefix-mode-active* nil *prefix-current-map* nil)
        (setup-default-prefix-bindings)
        (ok "default map binds windowlist" (functionp (gethash 119 *prefix-map*)) t)
        (ok "default map binds eval" (functionp (gethash 58 *prefix-map*)) t)
        (ok "default map binds shell" (functionp (gethash 33 *prefix-map*)) t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
