;;; Tests for keyboard focus re-asserted after keyboard-exclusive layer
;;; surfaces close (prefix capture, prompt, menu, eval prompt, launcher).
;;; River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (manage-requests 0) (river '()) (hook-count 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record river requests instead of sending them
    (dolist (fn '(message show-message message-hide))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))
    (setf (symbol-function 'request-manage-dirty)
          (lambda (&rest args)
            (declare (ignore args))
            (incf manage-requests)
            nil)
          (symbol-function 'rwm:zwlr-layer-surface-v1.destroy)
          (lambda (x)
            (push (list :destroy-layer x) river)
            nil)
          (symbol-function 'wl:wl-surface.destroy)
          (lambda (x)
            (push (list :destroy-surface x) river)
            nil)
          (symbol-function 'rwm:river-seat-v1.focus-window)
          (lambda (seat-proxy window-proxy)
            (push (list :focus seat-proxy window-proxy) river)
            nil)
          (symbol-function 'rwm:river-seat-v1.clear-focus)
          (lambda (seat-proxy)
            (push (list :clear seat-proxy) river)
            nil))

    (labels ((reset ()
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *pending-focus-requests* nil
                     *pending-close-requests* nil
                     *pending-pointer-warps* nil
                     *prefix-state* nil
                     *expose-active* nil
                     *prompt-active* nil
                     *prompt-state* nil
                     *menu-active* nil
                     *menu-layer-surface* nil
                     *menu-surface* nil
                     *menu-pointer* nil
                     *eval-prompt-active* nil
                     *eval-prompt-state* nil
                     *launcher-active* nil
                     *launcher-layer-surface* nil
                     *launcher-surface* nil
                     manage-requests 0
                     river nil))
             (fake-surfaces (layer surface)
               (let ((h (make-hash-table)))
                 (setf (gethash :layer-surface h) layer
                       (gethash :surface h) surface)
                 h))
             (make-win (proxy)
               (setf (gethash proxy *windows*)
                     (make-instance 'rhizome-window :proxy proxy)))
             (make-seat (proxy)
               (setf (gethash proxy *seats*)
                     (make-instance 'rhizome-seat :proxy proxy)))
             (proxy-of (x)
               (and x (window-proxy x)))
             (pending ()
               (mapcar (lambda (r)
                         (list (proxy-of (car r)) (seat-proxy (cdr r))))
                       *pending-focus-requests*))
             (give-focus (win seat)
               (setf (seat-focused-window seat) win
                     (window-focused-p win) t))
             (close-prefix ()
               (setf *prefix-state* (fake-surfaces :prefix-ls :prefix-surface))
               (exit-prefix-mode))
             (close-prompt ()
               (setf *prompt-active* t
                     *prompt-state* (fake-surfaces :prompt-ls :prompt-surface))
               (prompt-close))
             (close-menu ()
               (setf *menu-active* t
                     *menu-layer-surface* :menu-ls
                     *menu-surface* :menu-surface)
               (menu-close))
             (close-eval-prompt ()
               (setf *eval-prompt-active* t
                     *eval-prompt-state* (fake-surfaces :eval-ls :eval-surface))
               (hide-eval-prompt))
             (close-launcher ()
               (setf *launcher-active* t
                     *launcher-layer-surface* :launcher-ls
                     *launcher-surface* :launcher-surface)
               (hide-launcher))
             (compositor-close-prompt ()
               ;; Open the prompt through its real path with stand-in
               ;; globals, then deliver the layer surface's own :closed
               (let ((layer-surface (make-instance 'wl:wl-proxy))
                     (*compositor* :compositor)
                     (*shm* :shm)
                     (*layer-shell* :layer-shell))
                 (let ((saved (mapcar (lambda (fn)
                                        (cons fn (symbol-function fn)))
                                      '(wl:wl-compositor.create-surface
                                        rwm:zwlr-layer-shell-v1.get-layer-surface
                                        rwm:zwlr-layer-surface-v1.set-anchor
                                        rwm:zwlr-layer-surface-v1.set-size
                                        rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                                        rwm:zwlr-layer-surface-v1.set-margin
                                        wl:wl-surface.commit))))
                   (unwind-protect
                        (progn
                          (dolist (entry saved)
                            (setf (symbol-function (car entry))
                                  (lambda (&rest args)
                                    (declare (ignore args))
                                    nil)))
                          (setf (symbol-function 'wl:wl-compositor.create-surface)
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  :prompt-surface)
                                (symbol-function
                                 'rwm:zwlr-layer-shell-v1.get-layer-surface)
                                (lambda (&rest args)
                                  (declare (ignore args))
                                  layer-surface))
                          (prompt-open "test")
                          (dolist (hook (wl:wl-proxy-hooks layer-surface))
                            (funcall hook :closed)))
                     (dolist (entry saved)
                       (setf (symbol-function (car entry)) (cdr entry))))))))

      ;; --- Prefix capture ---

      (reset)
      (let* ((seat (make-seat :cap-seat-1))
             (a (make-win :cap-a))
             (handler (lambda (&rest args)
                        (declare (ignore args))
                        (incf hook-count))))
        (give-focus a seat)
        (add-hook :focus-changed handler)
        (close-prefix)
        (ok "closing the prefix capture destroys its layer surface"
            (and (member '(:destroy-layer :prefix-ls) river :test #'equal) t) t)
        (ok "closing the prefix capture queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1)))
        (ok "closing the prefix capture asks for a manage sequence"
            (plusp manage-requests) t)
        (ok "re-asserting focus leaves the seat focus unchanged"
            (list (proxy-of (seat-focused-window seat))
                  (and (window-focused-p a) t))
            '(:cap-a t))
        (ok "re-asserting focus runs no focus-changed hook"
            hook-count 0)
        (remove-hook :focus-changed handler)
        (setf river nil)
        (flush-focus-requests)
        (ok "the manage pass sends the re-asserted focus to river"
            (remove-if-not (lambda (e) (eq (first e) :focus)) river)
            '((:focus :cap-seat-1 :cap-a))))

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (setf *prefix-state* nil)
        (exit-prefix-mode)
        (ok "ending a prefix chain with no capture surface queues nothing"
            (list (pending) manage-requests) '(nil 0)))

      ;; --- Prompt ---

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (close-prompt)
        (ok "closing the prompt queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1)))
        (ok "closing the prompt asks for a manage sequence"
            (plusp manage-requests) t))

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (compositor-close-prompt)
        (ok "a compositor close of the prompt queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1))))

      ;; --- Menu, eval prompt, launcher ---

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (close-menu)
        (ok "closing a menu queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1))))

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (close-eval-prompt)
        (ok "closing the eval prompt queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1))))

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (close-launcher)
        (ok "hiding the launcher queues focus for the focused window"
            (pending) '((:cap-a :cap-seat-1))))

      ;; --- Nothing to re-assert ---

      (reset)
      (progn
        (make-seat :cap-seat-1)
        (make-win :cap-a)
        (close-prompt)
        (ok "a seat with no focused window gets no focus request"
            (list (pending) manage-requests) '(nil 0)))

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (d (make-instance 'rhizome-window :proxy :cap-d)))
        (give-focus d seat)
        (close-menu)
        (ok "a dead focused window gets no focus request"
            (pending) nil))

      ;; --- Several teardowns and several seats ---

      (reset)
      (let ((seat (make-seat :cap-seat-1))
            (a (make-win :cap-a)))
        (give-focus a seat)
        (close-prefix)
        (close-prompt)
        (ok "two teardowns queue one request per seat"
            (length *pending-focus-requests*) 1))

      (reset)
      (let ((seat-1 (make-seat :cap-seat-1))
            (seat-2 (make-seat :cap-seat-2))
            (a (make-win :cap-a))
            (b (make-win :cap-b)))
        (give-focus a seat-1)
        (give-focus b seat-2)
        (close-menu)
        (ok "every seat gets its own focused window re-queued"
            (sort (pending) #'string< :key #'prin1-to-string)
            '((:cap-a :cap-seat-1) (:cap-b :cap-seat-2)))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
