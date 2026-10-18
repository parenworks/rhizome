;;; IPC (stumpish) tests: dispatch logic and a real socket round-trip.
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
         (send-and-drain (line)
           "Send LINE from a client thread while this (main) thread
drains the command queue — mirroring the WM's poll loop."
           (let ((reply :pending))
             (bt:make-thread
               (lambda ()
                 (setf reply (ipc-send-command (ipc-socket-path) line)))
               :name "test-ipc-client")
             (loop while (eq reply :pending)
                   do (ipc-drain)
                      (sleep 0.01))
             reply)))

    ;; Dispatch: eval form
    (ok "dispatch eval form" (ipc-dispatch-line "(+ 1 2)") "3")

    ;; Dispatch: unknown command reports helpfully
    (ok "dispatch unknown command"
       (and (search "unknown command"
                    (ipc-dispatch-line "frobnicate")) t)
       t)

    ;; A fake output so workspace commands have real state
    (let ((out (make-instance 'rhizome-output :proxy :test-out)))
      (setf (gethash :test-out *outputs*) out)

      ;; Socket round-trip on a THROWAWAY path: never touch the real
      ;; rhizome.sock, which may belong to a running WM!
      (let ((path "/tmp/rhizome-test.sock"))
        (cffi:foreign-funcall "unlink" :string path :int)
        (stop-ipc-server)
        (setf *ipc-thread* nil *ipc-fd* nil)
        (start-ipc-server path)
        (sleep 0.2)

        (ok "socket round-trip eval"
            (let ((reply :pending))
              (bt:make-thread
                (lambda () (setf reply (ipc-send-command path "(+ 40 2)")))
                :name "test-ipc-client")
              (loop while (eq reply :pending)
                    do (ipc-drain) (sleep 0.01))
              reply)
            "42")
        (ok "socket round-trip command"
            (let ((reply :pending))
              (bt:make-thread
                (lambda ()
                  (setf reply (ipc-send-command path "switch-workspace 4")))
                :name "test-ipc-client")
              (loop while (eq reply :pending)
                    do (ipc-drain) (sleep 0.01))
              reply)
            "workspace 4")
        (ok "socket round-trip state changed"
            (let ((reply :pending))
              (bt:make-thread
                (lambda ()
                  (setf reply (ipc-send-command path "current-workspace")))
                :name "test-ipc-client")
              (loop while (eq reply :pending)
                    do (ipc-drain) (sleep 0.01))
              reply)
            "4")

        (stop-ipc-server)
        (cffi:foreign-funcall "unlink" :string path :int))
      (remhash :test-out *outputs*)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
