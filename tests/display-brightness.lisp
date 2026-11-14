;;; Tests for the display menu's brightness argv: a signed step must
;;; use brightnessctl's relative syntax (N%+ / N%-), an absolute level
;;; plain N%. Runs without brightnessctl installed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off: no call made while this suite runs may spawn a bar process.
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- relative steps ---

    (ok "step +10 raises by 10%"
        (brightness-step-argument 10) "10%+")
    (ok "step -10 lowers by 10%"
        (brightness-step-argument -10) "10%-")
    (ok "step argument never starts with a dash"
        (char= (char (brightness-step-argument -25) 0) #\-) nil)

    ;; --- absolute levels ---

    (ok "absolute 50 sets 50%"
        (brightness-absolute-argument 50) "50%")
    (ok "absolute 100 sets 100%"
        (brightness-absolute-argument 100) "100%")

    ;; --- full argv ---

    (ok "step argv"
        (brightness-step-command -10)
        '("brightnessctl" "set" "10%-"))
    (ok "absolute argv"
        (brightness-absolute-command 50)
        '("brightnessctl" "set" "50%"))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (when (plusp fail)
      (uiop:quit 1))))
