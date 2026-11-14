;;; Tests for layer-surface shm buffer lifetime: one live shm file per
;;; surface across repeated configures, none after teardown.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defvar *shm-test-pool-destroys* 0)
(defvar *shm-test-buffer-destroys* 0)

(defun shm-test-live-fds ()
  "Count this process's open fds that point at a rhizome-bar shm file."
  (loop for fd from 0 below 4096
        for target = (sb-unix:unix-readlink (format nil "/proc/self/fd/~D" fd))
        count (and target (search "rhizome-bar-" target))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Stub the Wayland requests these tests reach; the shm file, mmap
    ;; and close are real.
    (setf (symbol-function 'wl:wl-shm.create-pool)
          (lambda (shm fd size) (declare (ignore shm)) (list :pool fd size)))
    (setf (symbol-function 'wl:wl-shm-pool.create-buffer)
          (lambda (pool &rest args) (declare (ignore args)) (list :buffer pool)))
    (setf (symbol-function 'wl:wl-shm-pool.destroy)
          (lambda (pool) (declare (ignore pool)) (incf *shm-test-pool-destroys*)))
    (setf (symbol-function 'wl:wl-buffer.destroy)
          (lambda (buffer) (declare (ignore buffer))
            (incf *shm-test-buffer-destroys*)))
    (dolist (fn '(wl:wl-surface.attach wl:wl-surface.damage
                  wl:wl-surface.commit wl:wl-surface.destroy
                  rwm:zwlr-layer-surface-v1.ack-configure
                  rwm:zwlr-layer-surface-v1.destroy
                  rwm:zwlr-layer-surface-v1.set-anchor
                  rwm:zwlr-layer-surface-v1.set-size
                  rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                  rwm:zwlr-layer-surface-v1.set-margin
                  message-render prompt-redraw eval-prompt-redraw
                  expose-restore-if-active))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))

    (let ((base 0))

      ;; Repeated ensure on one state keeps exactly one buffer
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf (gethash :width state) 40 (gethash :height state) 10)
        (setf *shm-test-pool-destroys* 0 *shm-test-buffer-destroys* 0)
        (dotimes (i 5) (bar-ensure-shm state))
        (ok "five ensures hold one shm fd" (- (shm-test-live-fds) base) 1)
        (ok "four previous pools destroyed" *shm-test-pool-destroys* 4)
        (ok "four previous buffers destroyed" *shm-test-buffer-destroys* 4)
        (ok "state holds a live mapping"
            (not (cffi:null-pointer-p (gethash :shm-ptr state))) t)
        (bar-destroy-shm state)
        (ok "destroy leaves no shm fd" (- (shm-test-live-fds) base) 0)
        (ok "destroy frees the last pool" *shm-test-pool-destroys* 5)
        (ok "destroy frees the last buffer" *shm-test-buffer-destroys* 5)
        (ok "destroy twice is harmless"
            (handler-case (progn (bar-destroy-shm state) :ok)
              (error (e) (princ-to-string e)))
            :ok)
        (ok "second destroy frees nothing more"
            (list *shm-test-pool-destroys* *shm-test-buffer-destroys*
                  (- (shm-test-live-fds) base))
            '(5 5 0)))

      ;; A resize between configures re-creates at the new size
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf (gethash :width state) 40 (gethash :height state) 10)
        (bar-ensure-shm state)
        (setf (gethash :height state) 30)
        (bar-ensure-shm state)
        (ok "resize keeps one shm fd" (- (shm-test-live-fds) base) 1)
        (ok "resize records the new size" (gethash :shm-size state)
            (* 40 30 4))
        (bar-destroy-shm state)
        (ok "resize then destroy leaves none" (- (shm-test-live-fds) base) 0))

      ;; Message overlay: three configures, then hide
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf *message-state* state
              (gethash :surface state) :surface
              (gethash :layer-surface state) :layer-surface)
        (dotimes (i 3)
          (message-configure-handler state :layer-surface i 0 0))
        (ok "message: three configures hold one shm fd"
            (- (shm-test-live-fds) base) 1)
        (message-hide)
        (ok "message: hide leaves no shm fd" (- (shm-test-live-fds) base) 0))

      ;; Prefix capture surface: three configures, then exit
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf *prefix-state* state
              *prefix-mode-active* t
              (gethash :surface state) :surface
              (gethash :layer-surface state) :layer-surface)
        (dotimes (i 3)
          (prefix-capture-configure state :layer-surface i 0 0))
        (ok "prefix: three configures hold one shm fd"
            (- (shm-test-live-fds) base) 1)
        (exit-prefix-mode)
        (ok "prefix: exit leaves no shm fd" (- (shm-test-live-fds) base) 0))

      ;; Prompt: three configures, then close
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf *prompt-state* state
              *prompt-active* t
              (gethash :surface state) :surface
              (gethash :layer-surface state) :layer-surface)
        (dotimes (i 3)
          (prompt-configure-handler state :layer-surface i 0 0))
        (ok "prompt: three configures hold one shm fd"
            (- (shm-test-live-fds) base) 1)
        (prompt-close)
        (ok "prompt: close leaves no shm fd" (- (shm-test-live-fds) base) 0))

      ;; Eval prompt: three configures, then hide
      (let ((state (make-hash-table)))
        (setf base (shm-test-live-fds))
        (setf *eval-prompt-state* state
              *eval-prompt-active* t
              (gethash :surface state) :surface
              (gethash :layer-surface state) :layer-surface)
        (dotimes (i 3)
          (eval-prompt-configure-handler state :layer-surface i 0 0))
        (ok "eval prompt: three configures hold one shm fd"
            (- (shm-test-live-fds) base) 1)
        (hide-eval-prompt)
        (ok "eval prompt: hide leaves no shm fd"
            (- (shm-test-live-fds) base) 0))

      ;; The real open functions build the surfaces and their event
      ;; hooks; events go to those hooks. LAYER-SURFACES lists every
      ;; layer surface made, newest first.
      (let ((layer-surface nil) (layer-surfaces '()))
        (setf *compositor* :compositor *shm* :shm *layer-shell* :layer-shell)
        (setf (symbol-function 'wl:wl-compositor.create-surface)
              (lambda (&rest args) (declare (ignore args)) :surface))
        (setf (symbol-function 'rwm:zwlr-layer-shell-v1.get-layer-surface)
              (lambda (&rest args)
                (declare (ignore args))
                (setf layer-surface
                      (make-instance 'rwm:zwlr-layer-surface-v1))
                (push layer-surface layer-surfaces)
                layer-surface))
        (flet ((fire (event &rest args)
                 (dolist (hook (wl:wl-proxy-hooks layer-surface))
                   (apply hook event args)))
               (fire-all (event &rest args)
                 (dolist (ls layer-surfaces)
                   (dolist (hook (wl:wl-proxy-hooks ls))
                     (apply hook event args))))
               (with-buffer (state)
                 (setf (gethash :width state) 40 (gethash :height state) 10)
                 (bar-ensure-shm state)
                 state))

          ;; Message window closed by the compositor
          (setf base (shm-test-live-fds) *message-state* nil)
          (show-message "closing soon" 3)
          (fire :configure 1 0 0)
          (fire :configure 2 0 0)
          (ok "closed message: configures hold one shm fd"
              (- (shm-test-live-fds) base) 1)
          (fire :closed)
          (ok "closed message: no shm fd left" (- (shm-test-live-fds) base) 0)
          (ok "closed message: the state is dropped" *message-state* nil)

          ;; A second message before the first surface is configured
          (setf base (shm-test-live-fds) *message-state* nil
                layer-surfaces '())
          (show-message "first" 3)
          (show-message "second" 3)
          (ok "message before configure: one surface"
              (length layer-surfaces) 1)
          (fire-all :configure 1 0 0)
          (ok "message before configure: one shm fd"
              (- (shm-test-live-fds) base) 1)
          (message-hide)
          (ok "message before configure: hide leaves no shm fd"
              (- (shm-test-live-fds) base) 0)

          ;; A message during a pending resize
          (setf base (shm-test-live-fds) *message-state* nil
                layer-surfaces '())
          (show-message "one line" 3)
          (fire-all :configure 1 0 0)
          (show-message (format nil "two~%lines") 3)
          (show-message "one line again" 3)
          (ok "message during resize: one surface"
              (length layer-surfaces) 1)
          (fire-all :configure 2 0 0)
          (ok "message during resize: one shm fd"
              (- (shm-test-live-fds) base) 1)
          (message-hide)
          (ok "message during resize: hide leaves no shm fd"
              (- (shm-test-live-fds) base) 0)

          ;; The prefix capture surface (no which-key message surface)
          (setf (symbol-function 'show-message)
                (lambda (&rest args) (declare (ignore args)) nil))
          (setf base (shm-test-live-fds)
                *prefix-mode-active* nil *prefix-state* nil)
          (enter-prefix-mode)
          (fire :configure 1 0 0)
          (fire :configure 2 0 0)
          (ok "closed prefix: configures hold one shm fd"
              (- (shm-test-live-fds) base) 1)
          (fire :closed)
          (ok "closed prefix: no shm fd left" (- (shm-test-live-fds) base) 0)
          (ok "closed prefix: the mode ends" *prefix-mode-active* nil)
          (exit-prefix-mode)
          (ok "closed prefix: a later exit frees nothing more"
              (- (shm-test-live-fds) base) 0)

          ;; Prompt
          (setf base (shm-test-live-fds) *prompt-active* nil)
          (prompt-open "closing")
          (fire :configure 1 0 0)
          (fire :configure 2 0 0)
          (ok "closed prompt: configures hold one shm fd"
              (- (shm-test-live-fds) base) 1)
          (fire :closed)
          (ok "closed prompt: no shm fd left" (- (shm-test-live-fds) base) 0)
          (ok "closed prompt: the prompt is inactive" *prompt-active* nil)

          ;; Eval prompt
          (setf base (shm-test-live-fds) *eval-prompt-active* nil)
          (show-eval-prompt)
          (fire :configure 1 0 0)
          (fire :configure 2 0 0)
          (ok "closed eval prompt: configures hold one shm fd"
              (- (shm-test-live-fds) base) 1)
          (fire :closed)
          (ok "closed eval prompt: no shm fd left"
              (- (shm-test-live-fds) base) 0)
          (ok "closed eval prompt: the prompt is inactive"
              *eval-prompt-active* nil)

          ;; Opening again over a state that still holds a buffer frees it
          (setf base (shm-test-live-fds)
                *prompt-active* nil
                *prompt-state* (with-buffer (make-hash-table)))
          (prompt-open "again")
          (ok "replaced prompt state: its buffer is freed"
              (- (shm-test-live-fds) base) 0)
          (prompt-close)
          (setf base (shm-test-live-fds)
                *eval-prompt-active* nil
                *eval-prompt-state* (with-buffer (make-hash-table)))
          (show-eval-prompt)
          (ok "replaced eval prompt state: its buffer is freed"
              (- (shm-test-live-fds) base) 0)
          (hide-eval-prompt)
          (setf base (shm-test-live-fds)
                *prefix-mode-active* nil
                *prefix-state* (with-buffer (make-hash-table)))
          (enter-prefix-mode)
          (ok "replaced prefix state: its buffer is freed"
              (- (shm-test-live-fds) base) 0)
          (exit-prefix-mode)
          (ok "replaced states: nothing left after teardown"
              (- (shm-test-live-fds) base) 0)))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
