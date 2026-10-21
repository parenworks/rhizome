;;; Regression test for the bar fork-bomb: the bar process loads the
;;; user config at startup; any (rhizome:configure ...) in it must
;;; never reach bar process management. Before the *bar-process-mode*
;;; guard, a bar applying config spawned another bar, which loaded the
;;; config and spawned another — exponential bars on screen, forcing
;;; the user to power off the machine.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (spawned 0) (killed 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Stub the EFFECT layer (the real functions keep their guards)
    (setf (symbol-function 'uiop:launch-program)
          (lambda (&rest args)
            (declare (ignore args))
            (incf spawned)
            nil))
    (setf (symbol-function 'uiop:run-program)
          (lambda (&rest args)
            (declare (ignore args))
            (incf killed)))
    (setf (symbol-function 'uiop:terminate-process)
          (lambda (&rest args)
            (declare (ignore args))
            nil))
    (setf (symbol-function 'uiop:wait-process)
          (lambda (&rest args)
            (declare (ignore args))
            nil))
    (setf *bar-process* nil
          *bar-process-mode* nil
          *wm-process-mode* t
          *config* (list :bar (list :enabled t)))

    ;; WM mode: applying config starts the bar (one spawn, one reap)
    (bar-apply-config)
    (ok "WM mode: bar config applies (one spawn)"
        spawned 1)
    (ok "WM mode flag is off by default"
        *bar-process-mode* nil)

    ;; Test process: NOT the WM. Nothing may ever be spawned or
    ;; reaped from here — the regression behind the bars that kept
    ;; dying on every test run.
    (setf spawned 0 killed 0
          *wm-process-mode* nil
          ;; Arm the font as resolved so font-apply-config stays
          ;; quiet (it would fc-match through the stub below)
          *ft-face* :fake
          *ft-font-family* (config-getf '(:theme) :font-family
                                        :default "monospace")
          *ft-font-size* (config-getf '(:theme) :font-size :default 12))
    (bar-apply-config)
    (ok "test process: bar-apply-config never touches bars"
        (list spawned killed) '(0 0))
    (setf *config* (list :bar (list :enabled nil)))
    (configure '(:bar (:height 24)))
    (ok "test process: configure never reaps the user's bar"
        killed 0)
    (setf *config* (list :bar (list :enabled t))
          *ft-face* nil
          *wm-process-mode* t)

    ;; Bar-process mode: the fork-bomb scenario. None of these may
    ;; spawn or kill anything, no matter what config they see.
    (setf spawned 0 killed 0
          *bar-process-mode* t)
    (bar-apply-config)
    (ok "bar process: bar-apply-config never spawns"
        spawned 0)
    (toggle-bar)
    (ok "bar process: toggle-bar never spawns"
        spawned 0)
    (start-bar)
    (ok "bar process: start-bar never spawns"
        spawned 0)
    (stop-bar)
    (ok "bar process: stop-bar never reaps or spawns"
        (list spawned killed)
        '(0 0))

    ;; The exact chain from the incident: user config -> configure ->
    ;; apply-config, inside the bar process
    (setf spawned 0)
    (let ((*config* '(:bar (:enabled t :modules (:left (:clock))))))
      (configure '(:bar (:height 24)))
      (ok "bar process: user config loading never restarts bars"
          (list spawned
                ;; but the values WERE merged, as the bar needs them
                (config-getf '(:bar) :height :default 20))
          '(0 24)))

    ;; Back in WM mode, config applies work again
    (setf spawned 0
          *bar-process* nil
          *bar-process-mode* nil)
    (bar-apply-config)
    (ok "WM mode: applying works again"
        spawned 1))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
