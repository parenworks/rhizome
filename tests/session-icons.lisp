;;; Tests for session management (lock/logout/idle) and the bar
;;; icons module (discovery, scaling, hitbox action wiring).
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (spawned nil))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Stub the process spawner and messages
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'uiop:launch-program)
          (lambda (cmd &rest rest)
            (declare (ignore rest))
            (push cmd spawned)
            :spawned))

    ;; Fresh state
    (setf *idle-locker-process* nil
          *logout-armed-at* 0
          *bar-icon-cache* (make-hash-table :test 'equal)
          spawned nil)

    ;; --- Lock ---

    (let ((*config* '(:lock (:command "swaylock"
                            :args ("--daemonize" "--screensaver")))))
      (lock-screen)
      (ok "lock spawns the configured locker"
          (first spawned)
          '("bash" "-c" "swaylock --daemonize --screensaver")))
    (setf spawned nil)
    (let ((*config* '(:lock (:command ("my-locker" "--foo")))))
      (lock-screen)
      (ok "lock accepts a full command list"
          (first spawned)
          '("bash" "-c" "my-locker --foo")))
    (setf spawned nil)
    (let ((*config* nil))
      (lock-screen)
      (ok "lock defaults to swaylock"
          (and (search "swaylock" (third (first spawned))) t) t))

    ;; Idle locker: only spawns with :idle-seconds, and only once
    (setf spawned nil)
    (let ((*config* '(:lock (:idle-seconds 300))))
      (start-idle-locker)
      (ok "idle locker armed with swayidle"
          (first spawned)
          '("swayidle" "-w" "timeout" "300" "swaylock"))
      (setf spawned nil)
      (start-idle-locker)
      (ok "idle locker spawns only once"
          spawned nil))
    (setf *idle-locker-process* nil)
    (setf spawned nil)
    (let ((*config* '(:lock (:idle-seconds 0))))
      (start-idle-locker)
      (ok "no idle locker without idle-seconds"
          spawned nil))

    ;; --- Logout ---

    ;; exit_session needs a live compositor; test the dispatch only.
    (let ((*wm-proxy* nil) (*wm-proxy-version* 0))
      (ok "logout method falls back to SIGTERM without a v4+ compositor"
          (logout-method) :sigterm))
    (let ((*wm-proxy* t) (*wm-proxy-version* 4))
      (ok "logout method uses exit_session on a v4 compositor"
          (logout-method) :exit-session))
    (let ((*wm-proxy* t) (*wm-proxy-version* 5))
      (ok "logout method uses exit_session on a v5 compositor"
          (logout-method) :exit-session))
    (let ((*wm-proxy* nil) (*wm-proxy-version* 0))
      (setf spawned nil)
      (logout-session)
      (ok "fallback logout SIGTERMs river (no river-classic riverctl)"
          (and (search "pkill" (first (first spawned)))
               (not (search "riverctl" (first (first spawned))))
               t)
          t))
    (setf *logout-armed-at* 0)
    (setf spawned nil)
    (logout-confirm)
    (ok "first logout press only arms"
        spawned nil)
    (logout-confirm)
    (ok "second quick press logs out"
        (and spawned t) t)
    (setf spawned nil)
    (setf *logout-armed-at* (- (get-universal-time) 10))
    (logout-confirm)
    (ok "stale arm re-arms instead of logging out"
        spawned nil)

    ;; --- Detach (WM hot-swap) ---

    ;; Without a WM proxy: detaching still remembers placements and
    ;; disables the bar, but never sends the stop request.
    (setf *detach-pending* nil
          spawned nil)
    (let ((*window-state-path* #p"/tmp/rhizome-test-detach-state.sexp")
          (*window-state* (make-hash-table :test 'equal))
          (stopped 0))
      (setf (symbol-function 'rwm:river-window-manager-v1.stop)
            (lambda (&rest args) (declare (ignore args)) (incf stopped)))
      (let ((win (make-instance 'rhizome-window :proxy :detach-1)))
        (setf (gethash :detach-1 *windows*) win
              (window-identifier win) "detach-id"
              (window-output win) (make-instance 'rhizome-output
                                                  :proxy :detach-out)
              (window-x win) 5 (window-y win) 5
              (window-width win) 50 (window-height win) 50)
        (assign-window-workspace win 7)
        (detach-wm)
        (ok "detach remembers placements before leaving"
            (getf (gethash "detach-id" *window-state*) :workspace) 7)
        (ok "detach disables the bar (supervisor stands down)"
            (config-getf '(:bar) :enabled) nil)
        (ok "detach without a compositor sends no stop"
            stopped 0)
        (ok "detach leaves no pending flag without a proxy"
            *detach-pending* nil))
      ;; With a proxy: the stop request goes out and the flag arms
      (let* ((real-dirty (symbol-function 'request-manage-dirty))
             (*wm-proxy* :fake-wm))
        ;; request-manage-dirty would reach wayflan's type assertion
        (setf (symbol-function 'request-manage-dirty)
              (lambda (&rest args) (declare (ignore args)) nil))
        (detach-wm)
        (ok "detach sends river_window_manager_v1.stop"
            stopped 1)
        (ok "detach arms the finished handshake"
            *detach-pending* t)
        (setf *detach-pending* nil
              (symbol-function 'request-manage-dirty) real-dirty)))
    (setf *config* (default-config))
    (ignore-errors (delete-file #p"/tmp/rhizome-test-detach-state.sexp"))

    ;; --- Bar icons ---

    (ok "icons module recognized"
        (icons-module-p '(:icons ((:icon "x")))) t)
    (ok "plain modules are not icons modules"
        (icons-module-p :clock) nil)
    (ok "icons width counts entries"
        (bar-icons-width '(:icons ((:icon "a") (:icon "b"))))
        40)                          ; 2 x (16 + 4)
    ;; icon discovery: name → hicolor PNG (flameshot ships one)
    (ok "icon name resolved to a real PNG"
        (and (find-icon-file "flameshot")
             (and (search ".png" (find-icon-file "flameshot")) t))
        t)
    (ok "missing icon resolves to nothing"
        (find-icon-file "definitely-not-an-icon-xyz") nil)
    ;; absolute path passthrough
    (ok "absolute icon path passes through"
        (find-icon-file
          "/usr/share/icons/hicolor/48x48/apps/flameshot.png")
        "/usr/share/icons/hicolor/48x48/apps/flameshot.png")
    ;; scaling: 48x48 source → 16x16 output
    (let* ((png (pngload:load-file
                  "/usr/share/icons/hicolor/48x48/apps/flameshot.png"
                  :flatten t))
           (scaled (scale-icon (pngload:width png)
                              (pngload:height png)
                              (pngload:data png)
                              16)))
      (ok "icon scaled to 16x16"
          (list (first scaled) (second scaled)
                (length (third scaled)))
          '(16 16 1024)))
    ;; load-icon caches and returns scaled data
    (let ((icon (load-icon "flameshot")))
      (ok "load-icon decodes to bar size"
          (and icon (= (first icon) 16) (= (length (third icon)) 1024))
          t)
      (ok "load-icon caches"
          (eq (load-icon "flameshot") icon) t))
    ;; icon entries carry their own actions
    (ok "icon entry :on-click found by the action chain"
        (bar-module-action '(:icon "flameshot" :on-click "flameshot gui")
                           :on-click)
        "flameshot gui")
    (ok "icon entry without action yields nil"
        (bar-module-action '(:icon "x") :on-scroll-up) nil)
    (ok "atom modules still use the config path"
        (bar-module-action :clock :on-click) nil) ; unconfigured
    (ok "segment width counts an icons module"
        (bar-segment-width (list '(:icons ((:icon "a") (:icon "b")))))
        (+ 40 *bar-module-gap*))

    ;; --- Launcher: user-local desktop files + rescan ---

    (ok "home applications dir resolves with the username intact"
        (and (member #P"/home/glenn/.local/share/applications/"
                     (launcher-desktop-paths)
                     :test #'equal)
             t)
        t)
    (ok "launcher rescan repopulates the app list"
        (let ((*launcher-app-list* nil))
          (launcher-refresh-app-list)
          (and *launcher-app-list*
               (plusp (length *launcher-app-list*))))
        t))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
