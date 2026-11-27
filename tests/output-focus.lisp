;;; Tests for focusing an output from an empty desktop: MOD+Left on
;;; empty desktop focuses the output under the pointer in both modes,
;;; MOD+Left on a window still starts a move, and under :niri pointer
;;; motion onto another output's empty desktop makes that output
;;; current. Two fake outputs side by side; river requests are stubbed
;;; and pointer events arrive through handle-seat-event.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; keep geometry exact
(configure '(:bar (:enabled nil)
             :animations (:enabled nil)
             :pointer (:warp-on-switch nil)))

(let ((pass 0) (fail 0))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (try (name thunk expected)
             ;; An error (e.g. an undefined function) is a failure,
             ;; not a crash: every case still reports
             (ok name
                 (handler-case (funcall thunk)
                   (error (e) (list :error (princ-to-string e))))
                 expected)))

    ;; Record nothing; keep river out of it
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders show-window
                  hide-window message show-message request-manage-dirty
                  recommend-dimension-bounds set-window-clip-box
                  set-window-position animation-start))
      (setf (symbol-function fn) (lambda (&rest args)
                                   (declare (ignore args))
                                   nil)))

    ;; Fresh global state
    (setf *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *seats* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *pending-focus-requests* nil
          *pending-autofocus* nil
          *selected-output* nil
          *last-registered-window* nil)

    ;; HDMI-A-1 at x=0 (1920x1080) shows empty workspace 1; DVI-D-1 at
    ;; x=1920 (1920x1200) shows workspace 3, whose terminal covers the
    ;; left half of the output.
    (let* ((hdmi (make-instance 'rhizome-output :proxy :hdmi-proxy
                                :x 0 :y 0 :width 1920 :height 1080))
           (dvi (make-instance 'rhizome-output :proxy :dvi-proxy
                               :x 1920 :y 0 :width 1920 :height 1200))
           (seat (make-instance 'rhizome-seat :proxy :seat-proxy))
           (term (make-instance 'rhizome-window :proxy :term-proxy
                                :app-id "foot"))
           (stale (make-instance 'rhizome-window :proxy :stale-proxy
                                 :app-id "foot"))
           (focus-calls 0))
      (setf (gethash :hdmi-proxy *outputs*) hdmi
            (gethash :dvi-proxy *outputs*) dvi
            (gethash :seat-proxy *seats*) seat
            (gethash hdmi *output-workspace*) 1
            (gethash dvi *output-workspace*) 3
            (gethash :term-proxy *windows*) term
            (gethash :stale-proxy *windows*) stale)
      (setf (window-output term) dvi
            (window-x term) 1920 (window-y term) 0
            (window-width term) 960 (window-height term) 1200)
      (assign-window-workspace term 3)
      ;; A window on HDMI's hidden workspace 2 keeps its last geometry
      (setf (window-output stale) hdmi
            (window-x stale) 0 (window-y stale) 0
            (window-width stale) 1920 (window-height stale) 1080
            (window-hidden-p stale) t)
      (assign-window-workspace stale 2)

      ;; Count focus-output calls without changing what they do
      (let ((original (symbol-function 'focus-output)))
        (setf (symbol-function 'focus-output)
              (lambda (&rest args)
                (incf focus-calls)
                (apply original args))))

      (labels ((move-pointer (x y)
                 (handle-seat-event seat :pointer-position x y))
               (mod-click ()
                 (pointer-drag-start seat :move))
               (mode (global &optional overrides)
                 (configure (list :pointer (list :output-focus global)
                                  :workspaces
                                  (list :output-focus overrides))))
               (start-on-term ()
                 ;; Pointer over the focused terminal on DVI
                 (setf (seat-op-pending seat) nil
                       (window-floating-p term) nil
                       *selected-output* nil)
                 (move-pointer 2000 100)
                 (focus-window term seat)
                 (setf focus-calls 0)))

        ;; --- MOD+Left on empty desktop, both modes ---

        (dolist (m '(:stumpwm :niri))
          (mode m)
          (start-on-term)
          (try (format nil "MOD+Left on empty desktop focuses the output under the pointer (~(~A~))" m)
               (lambda ()
                 (setf (seat-pointer-x seat) 500 (seat-pointer-y seat) 500)
                 (mod-click)
                 (list (eq (current-output) hdmi)
                       (focused-window)
                       (seat-op-pending seat)))
               '(t nil nil)))

        (mode :stumpwm)
        (start-on-term)
        (try "MOD+Left on the current output's empty desktop starts no move"
             (lambda ()
               (setf (seat-pointer-x seat) 3500 (seat-pointer-y seat) 500)
               (mod-click)
               (list (eq (current-output) dvi)
                     (seat-op-pending seat)))
             '(t nil))

        ;; --- MOD+Left on a window still starts a move ---

        (dolist (m '(:stumpwm :niri))
          (mode m)
          (start-on-term)
          (setf *selected-output* hdmi)
          (try (format nil "MOD+Left on a window starts a move (~(~A~))" m)
               (lambda ()
                 (setf (seat-pointer-x seat) 2000 (seat-pointer-y seat) 100)
                 (mod-click)
                 (list (eq (getf (seat-op-pending seat) :win) term)
                       (getf (seat-op-pending seat) :type)
                       (eq (focused-window) term)
                       focus-calls))
               '(t :move t 0)))

        ;; --- Motion onto another output's empty desktop ---

        (mode :niri)
        (start-on-term)
        (try "under :niri, motion onto another output's empty desktop makes it current"
             (lambda ()
               (move-pointer 500 500)
               (list (eq (current-output) hdmi) (focused-window) focus-calls))
             '(t nil 1))
        (try "repeated motion within that output does not focus it again"
             (lambda ()
               (move-pointer 600 500)
               (move-pointer 700 520)
               (move-pointer 800 540)
               focus-calls)
             1)
        (try "keyboard focus elsewhere is not taken back by motion within the output"
             (lambda ()
               (focus-output dvi)
               (setf focus-calls 0)
               (move-pointer 900 560)
               (move-pointer 1000 580)
               (list (eq (current-output) dvi) (eq (focused-window) term)
                     focus-calls))
             '(t t 0))

        (mode :stumpwm)
        (start-on-term)
        (try "under :stumpwm, motion onto another output's empty desktop changes nothing"
             (lambda ()
               (move-pointer 500 500)
               (move-pointer 600 500)
               (list (eq (current-output) dvi) (eq (focused-window) term)
                     focus-calls))
             '(t t 0))

        ;; --- Motion over a window changes nothing ---

        ;; HDMI current with focus cleared; the pointer then crosses
        ;; onto DVI over its terminal
        (dolist (m '(:stumpwm :niri))
          (mode m)
          (start-on-term)
          (move-pointer 500 500)
          (focus-output hdmi)
          (setf focus-calls 0)
          (try (format nil "motion over a window on another output changes nothing (~(~A~))" m)
               (lambda ()
                 (move-pointer 2100 300)
                 (move-pointer 2200 400)
                 (list (eq (current-output) hdmi) (focused-window)
                       focus-calls))
               '(t nil 0)))

        ;; --- A pending crossing yields to a later keyboard choice ---

        ;; HDMI current with focus cleared; the pointer crosses onto
        ;; DVI over its terminal, so that crossing is still pending
        ;; when the keyboard picks an output and the pointer reaches
        ;; DVI's empty right half.
        (mode :niri)
        (start-on-term)
        (move-pointer 500 500)
        (try "a pending crossing does not act after focus-output picks a third output"
             (lambda ()
               (let ((third (make-instance 'rhizome-output
                                           :proxy :third-proxy
                                           :x 3840 :y 0
                                           :width 1920 :height 1080)))
                 (setf (gethash :third-proxy *outputs*) third
                       (gethash third *output-workspace*) 5)
                 (unwind-protect
                      (progn
                        (move-pointer 2000 100)
                        (focus-output third)
                        (setf focus-calls 0)
                        (move-pointer 3500 500)
                        (move-pointer 3600 520)
                        (list (eq (current-output) third) (focused-window)
                              focus-calls))
                   (remhash :third-proxy *outputs*)
                   (remhash third *output-workspace*))))
             '(t nil 0))

        (mode :niri)
        (start-on-term)
        (move-pointer 500 500)
        (try "a pending crossing does not act after focus-output picks its output and then the first"
             (lambda ()
               (move-pointer 2000 100)
               (focus-output dvi)
               (focus-output hdmi)
               (setf focus-calls 0)
               (move-pointer 3500 500)
               (move-pointer 3600 520)
               (list (eq (current-output) hdmi) (focused-window)
                     focus-calls))
             '(t nil 0))

        ;; --- Per-workspace override beats the global value ---

        (mode :stumpwm '(1 :niri))
        (start-on-term)
        (try "a :niri override on the target workspace beats global :stumpwm"
             (lambda ()
               (move-pointer 500 500)
               (list (eq (current-output) hdmi) focus-calls))
             '(t 1))

        (mode :niri '(1 :stumpwm))
        (start-on-term)
        (try "a :stumpwm override on the target workspace beats global :niri"
             (lambda ()
               (move-pointer 500 500)
               (list (eq (current-output) dvi) focus-calls))
             '(t 0))

        (mode :niri '(3 :stumpwm))
        (start-on-term)
        (try "an override on another workspace leaves the target on the global value"
             (lambda ()
               (move-pointer 500 500)
               (list (eq (current-output) hdmi) focus-calls))
             '(t 1))

        (mode :stumpwm)
        (try "an unset global value means :stumpwm"
             (lambda ()
               (configure '(:pointer (:output-focus nil)))
               (output-focus-mode hdmi))
             :stumpwm)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
