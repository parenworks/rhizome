;;; ipc-socket-path.lisp: the WM and session/rhizome-cmd name the same
;;; IPC socket. $XDG_RUNTIME_DIR/rhizome.sock when XDG_RUNTIME_DIR is
;;; set and non-empty, else /tmp/rhizome-UID.sock. The client's path is
;;; read from its "no socket at PATH" message, so no socket is opened.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun client-socket-path (xdg-runtime-dir)
  "The socket path session/rhizome-cmd uses with XDG_RUNTIME_DIR set to
XDG-RUNTIME-DIR (:unset removes it), read from its error message, or
the client's whole output when the message is missing."
  (let* ((client (namestring
                  (asdf:system-relative-pathname :rhizome
                                                 "session/rhizome-cmd")))
         (env (if (eq xdg-runtime-dir :unset)
                  (list "env" "-u" "XDG_RUNTIME_DIR")
                  (list "env" (format nil "XDG_RUNTIME_DIR=~A"
                                      xdg-runtime-dir))))
         (output (uiop:run-program (append env (list "bash" client
                                                     "current-workspace"))
                                   :output :string
                                   :error-output :output
                                   :ignore-error-status t))
         (start (search "no socket at " output)))
    (if start
        (let ((path-start (+ start (length "no socket at "))))
          (subseq output path-start (position #\Space output
                                              :start path-start)))
        output)))

(defun server-socket-path (xdg-runtime-dir)
  "ipc-socket-path with XDG_RUNTIME_DIR set to XDG-RUNTIME-DIR (:unset
removes it); the variable is restored afterwards."
  (let ((saved (sb-posix:getenv "XDG_RUNTIME_DIR")))
    (unwind-protect
         (progn
           (if (eq xdg-runtime-dir :unset)
               (sb-posix:unsetenv "XDG_RUNTIME_DIR")
               (sb-posix:setenv "XDG_RUNTIME_DIR" xdg-runtime-dir 1))
           (ipc-socket-path))
      (if saved
          (sb-posix:setenv "XDG_RUNTIME_DIR" saved 1)
          (sb-posix:unsetenv "XDG_RUNTIME_DIR")))))

(let* ((pass 0) (fail 0)
       (fallback (format nil "/tmp/rhizome-~A.sock"
                         (cffi:foreign-funcall "getuid" :int)))
       (runtime (format nil "/tmp/rhizome-test-runtime-~A"
                        (get-universal-time))))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (ensure-directories-exist (format nil "~A/" runtime))
    (unwind-protect
         (progn
           ;; --- the WM's path ---
           (ok "server: XDG_RUNTIME_DIR set"
               (server-socket-path runtime)
               (format nil "~A/rhizome.sock" runtime))
           (ok "server: XDG_RUNTIME_DIR unset"
               (server-socket-path :unset) fallback)
           (ok "server: XDG_RUNTIME_DIR empty"
               (server-socket-path "") fallback)

           ;; --- the client agrees ---
           (if (probe-file fallback)
               (format t "~&NOTE: ~A exists; fallback client checks skipped~%"
                       fallback)
               (progn
                 (ok "client: XDG_RUNTIME_DIR unset"
                     (client-socket-path :unset) fallback)
                 (ok "client: XDG_RUNTIME_DIR empty"
                     (client-socket-path "") fallback)))
           (ok "client: XDG_RUNTIME_DIR set"
               (client-socket-path runtime)
               (format nil "~A/rhizome.sock" runtime)))
      (uiop:delete-empty-directory (format nil "~A/" runtime))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
