;;; IPC (stumpish) tests: dispatch logic and a real socket round-trip.
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
    (let ((out (make-instance 'stumpway-output :proxy :test-out)))
      (setf (gethash :test-out *outputs*) out)

      ;; Socket round-trip
      (cffi:foreign-funcall "unlink" :string (ipc-socket-path) :int)
      (stop-ipc-server)
      (setf *ipc-thread* nil *ipc-fd* nil)
      (start-ipc-server)
      (sleep 0.2)

      (ok "socket round-trip eval"
          (send-and-drain "(+ 40 2)") "42")
      (ok "socket round-trip command"
          (send-and-drain "switch-workspace 4") "workspace 4")
      (ok "socket round-trip state changed"
          (send-and-drain "current-workspace") "4")

      (stop-ipc-server)
      (cffi:foreign-funcall "unlink" :string (ipc-socket-path) :int)
      (remhash :test-out *outputs*)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
