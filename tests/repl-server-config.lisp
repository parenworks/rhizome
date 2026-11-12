;;; Tests for REPL server config resolution (REPL-SERVER-SETTINGS):
;;; the :slynk group, the :swank fallback for older configs, and the
;;; defaults. Pure config lookups; no server is started.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defun settings-for (user-plist)
  "REPL-SERVER-SETTINGS for USER-PLIST merged over the defaults, as
(enabled-p port)."
  (multiple-value-list
   (repl-server-settings :config (merge-plist (default-config) user-plist))))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- defaults ---
    (ok "defaults: enabled on 4005"
        (settings-for '()) '(t 4005))
    (ok "bare config with no groups at all: enabled on 4005"
        (multiple-value-list (repl-server-settings :config '()))
        '(t 4005))

    ;; --- :slynk is read ---
    (ok ":slynk port is honoured"
        (settings-for '(:slynk (:port 4010))) '(t 4010))
    (ok ":slynk can disable the server"
        (settings-for '(:slynk (:enabled nil))) '(nil 4005))

    ;; --- :swank fallback ---
    (ok ":swank port is honoured when :slynk is untouched"
        (settings-for '(:swank (:port 4006))) '(t 4006))
    (ok ":swank can still disable the server"
        (settings-for '(:swank (:enabled nil))) '(nil 4005))
    (ok ":swank keys it omits take the defaults"
        (settings-for '(:swank (:enabled t))) '(t 4005))

    ;; --- :slynk wins over :swank ---
    (ok ":slynk wins when both are set"
        (settings-for '(:swank (:port 4006) :slynk (:port 4010)))
        '(t 4010))
    (ok ":slynk enabled wins over :swank disabled"
        (settings-for '(:swank (:enabled nil) :slynk (:port 4011)))
        '(t 4011))

    ;; --- the default config carries no :swank group ---
    (ok "default config holds :slynk, not :swank"
        (list (nth-value 1 (config-get '(:slynk) :config (default-config)))
              (nth-value 1 (config-get '(:swank) :config (default-config))))
        '(t nil))

    ;; --- start-swank stays callable for older configs ---
    (let ((calls 0)
          (original (fdefinition 'start-slynk)))
      (ok "start-swank is still exported and calls start-slynk"
          (list (nth-value 1 (find-symbol "START-SWANK" :rhizome))
                (unwind-protect
                     (progn
                       (setf (fdefinition 'start-slynk)
                             (lambda () (incf calls) nil))
                       (handler-case
                           (progn (funcall (find-symbol "START-SWANK" :rhizome))
                                  calls)
                         (error () calls)))
                  (setf (fdefinition 'start-slynk) original)))
          '(:external 1))))

  (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
  (unless (zerop fail)
    (sb-ext:exit :code 1)))
