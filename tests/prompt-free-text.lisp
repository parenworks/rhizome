;;; Tests for free-text completion: a prompt opened without
;;; candidates completes with the typed query, and a prompt opened
;;; with candidates still completes only with a selected candidate.
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
                              name actual expected))))
         (type-text (text)
           (loop for c across text
                 do (prompt-handle-key (char-code c)))))

    ;; Stub the Wayland requests so the real prompt-open and
    ;; prompt-close run without a compositor.
    (setf *compositor* :fake-compositor
          *shm* :fake-shm
          *layer-shell* :fake-layer-shell)
    (setf (symbol-function 'wl:wl-compositor.create-surface)
          (lambda (&rest a) (declare (ignore a)) :fake-surface))
    (setf (symbol-function 'rwm:zwlr-layer-shell-v1.get-layer-surface)
          (lambda (&rest a)
            (declare (ignore a))
            (make-instance 'wl:wl-proxy)))
    (dolist (fn '(rwm:zwlr-layer-surface-v1.set-anchor
                  rwm:zwlr-layer-surface-v1.set-size
                  rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                  rwm:zwlr-layer-surface-v1.set-margin
                  rwm:zwlr-layer-surface-v1.destroy
                  wl:wl-surface.commit
                  wl:wl-surface.destroy
                  prompt-redraw))
      (setf (symbol-function fn)
            (lambda (&rest a) (declare (ignore a)) nil)))
    (setf (symbol-function 'show-message)
          (lambda (text &optional seconds)
            (declare (ignore seconds))
            text))

    ;; Free text: no candidates, the typed query is the choice
    (let ((got :none))
      (prompt-open "Free:" :candidates nil
                           :on-complete (lambda (c) (setf got c)))
      (ok "the stubbed prompt opens" *prompt-active* t)
      (type-text "1")
      (prompt-handle-key 65293)
      (ok "a prompt with no candidates completes with the typed text"
          got "1")
      (ok "return closes the free-text prompt" *prompt-active* nil))

    (let ((got :none))
      (prompt-open "Free:" :candidates nil
                           :on-complete (lambda (c) (setf got c)))
      (prompt-handle-key 65293)
      (ok "an empty free-text prompt completes with nothing" got :none)
      (ok "return closes the empty prompt" *prompt-active* nil))

    ;; Candidate prompts keep requiring a candidate
    (let ((got :none))
      (prompt-open "Pick:" :candidates '("alpha" "beta")
                           :on-complete (lambda (c) (setf got c)))
      (type-text "zz")
      (prompt-handle-key 65293)
      (ok "a candidate prompt still needs a candidate" got :none))

    (let ((got :none))
      (prompt-open "Pick:" :candidates '("alpha" "beta")
                           :on-complete (lambda (c) (setf got c)))
      (type-text "al")
      (prompt-handle-key 65293)
      (ok "a candidate prompt completes with the selected candidate"
          got "alpha"))

    ;; The shell prompt launches what was typed
    (let ((launches '())
          (saved (fdefinition 'uiop:launch-program)))
      (unwind-protect
           (progn
             (setf (fdefinition 'uiop:launch-program)
                   (lambda (command &rest keys)
                     (declare (ignore keys))
                     (push command launches)
                     nil))
             (run-shell-command-prompt)
             (type-text "true")
             (prompt-handle-key 65293))
        (setf (fdefinition 'uiop:launch-program) saved))
      (ok "the shell prompt runs the typed command"
          launches '(("bash" "-c" "true")))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
