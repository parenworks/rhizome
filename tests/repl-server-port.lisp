;;; Tests for REPL-SERVER-PORT and START-SLYNK idempotence. Server
;;; creation goes through REPL-SERVER-CREATE, which every case stubs:
;;; no slynk server is started and slynk is never loaded.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defparameter *hook-present*
  (and (fboundp 'repl-server-create) (fboundp 'repl-server-port))
  "True when the server-creation hook and the port reader exist. Without
them START-SLYNK would start a real server, so the cases that call it
are recorded as failures instead.")

(defvar *create-calls* 0
  "Number of times the stubbed REPL-SERVER-CREATE ran.")

(defvar *release* t
  "The blocking stub waits until this is true.")

(defun settle ()
  "Join every rhizome-slynk thread, so a start has finished."
  (dolist (thread (bt:all-threads))
    (when (and (equal (bt:thread-name thread) "rhizome-slynk")
               (not (eq thread (bt:current-thread))))
      (bt:join-thread thread))))

(defun reset-server-state ()
  "Forget any server recorded by an earlier case."
  (settle)
  (setf (symbol-value '*repl-server-port*) nil
        (symbol-value '*repl-server-starting*) nil
        *create-calls* 0
        *release* t))

(defun call-with-stub (stub thunk)
  "Run THUNK with REPL-SERVER-CREATE replaced by STUB. Return THUNK's
value, or :NO-HOOK when the hook is missing."
  (if (not *hook-present*)
      :no-hook
      (let ((original (fdefinition 'repl-server-create)))
        (unwind-protect
             (progn (setf (fdefinition 'repl-server-create) stub)
                    (funcall thunk))
          (settle)
          (setf (fdefinition 'repl-server-create) original)))))

(defun port-now ()
  "REPL-SERVER-PORT, or :UNDEFINED when it does not exist."
  (if (fboundp 'repl-server-port)
      (funcall 'repl-server-port)
      :undefined))

(defun with-slynk-config (user-plist thunk)
  "Run THUNK with *CONFIG* set from USER-PLIST merged over the defaults."
  (let ((*config* (merge-plist (default-config) user-plist)))
    (funcall thunk)))

(defun listening-stub (&optional actual)
  "A REPL-SERVER-CREATE stub that counts calls and returns ACTUAL, or
the requested port when ACTUAL is NIL."
  (lambda (port)
    (incf *create-calls*)
    (or actual port)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    (ok "repl-server-port is exported from RHIZOME"
        (nth-value 1 (find-symbol "REPL-SERVER-PORT" :rhizome))
        :external)

    (when *hook-present* (reset-server-state))
    (ok "no server started: port is NIL"
        (port-now) nil)

    ;; --- disabled: nothing starts ---
    (when *hook-present* (reset-server-state))
    (ok "disabled: start-slynk creates no server and the port stays NIL"
        (call-with-stub
         (listening-stub)
         (lambda ()
           (with-slynk-config '(:slynk (:enabled nil))
             (lambda ()
               (let ((value (start-slynk)))
                 (settle)
                 (list value *create-calls* (port-now)))))))
        '(nil 0 nil))

    ;; --- enabled: port recorded once the server listens ---
    (when *hook-present* (reset-server-state))
    (ok "enabled: the configured port is reported once the server listens"
        (call-with-stub
         (listening-stub)
         (lambda ()
           (with-slynk-config '(:slynk (:port 4010))
             (lambda ()
               (start-slynk)
               (settle)
               (list *create-calls* (port-now))))))
        '(1 4010))

    ;; --- second call: no second server, existing port returned ---
    (setf *create-calls* 0)
    (ok "second start-slynk creates no second server and returns the port"
        (call-with-stub
         (listening-stub)
         (lambda ()
           (with-slynk-config '(:slynk (:port 4010))
             (lambda ()
               (let ((value (start-slynk)))
                 (settle)
                 (list value *create-calls* (port-now)))))))
        '(4010 0 4010))

    ;; --- the port the server reports wins over the requested one ---
    (when *hook-present* (reset-server-state))
    (ok "the port returned by server creation is the one reported"
        (call-with-stub
         (listening-stub 49153)
         (lambda ()
           (with-slynk-config '(:slynk (:port 0))
             (lambda ()
               (start-slynk)
               (settle)
               (port-now)))))
        49153)

    ;; --- a call while the first is still starting ---
    (when *hook-present* (reset-server-state))
    (ok "start-slynk during a start in progress creates no second server"
        (call-with-stub
         (lambda (port)
           (incf *create-calls*)
           (loop until *release* do (sleep 0.01))
           port)
         (lambda ()
           (with-slynk-config '(:slynk (:port 4012))
             (lambda ()
               (setf *release* nil)
               (start-slynk)
               (let ((during (list (start-slynk) (port-now))))
                 (setf *release* t)
                 (settle)
                 (list during *create-calls* (port-now)))))))
        '((nil nil) 1 4012))

    ;; --- failure: logged, not signalled, port stays NIL, retry allowed ---
    (when *hook-present* (reset-server-state))
    (ok "a failed start signals nothing and leaves the port NIL"
        (call-with-stub
         (lambda (port)
           (declare (ignore port))
           (incf *create-calls*)
           (error "address in use"))
         (lambda ()
           (with-slynk-config '(:slynk (:port 4013))
             (lambda ()
               (handler-case
                   (progn (start-slynk)
                          (settle)
                          (list *create-calls* (port-now)))
                 (error (e) (list :signalled (princ-to-string e))))))))
        '(1 nil))
    (setf *create-calls* 0)
    (ok "after a failed start, start-slynk tries again"
        (call-with-stub
         (listening-stub)
         (lambda ()
           (with-slynk-config '(:slynk (:port 4013))
             (lambda ()
               (start-slynk)
               (settle)
               (list *create-calls* (port-now))))))
        '(1 4013)))

  (when *hook-present* (reset-server-state))
  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (unless (zerop fail)
    (sb-ext:exit :code 1)))
