;;; Tests for multi-head behaviour: the current output, where new
;;; windows open, focusing another output, moving a window to another
;;; output, and IPC commands acting on the current output. Two fake
;;; outputs side by side; river requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run; keep geometry exact
(configure '(:bar (:enabled nil)
             :animations (:enabled nil)
             :pointer (:warp-on-switch nil)))

(let ((pass 0) (fail 0) (calls '()))
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
                 expected))
           (positions-of (win)
             (loop for c in calls
                   when (and (eq (first c) :set-window-position)
                             (eq (second c) win))
                     collect (third c))))

    ;; Record calls instead of touching river
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-borders show-window
                  hide-window message show-message request-manage-dirty
                  recommend-dimension-bounds set-window-clip-box))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (if (and args (typep (first args) 'rhizome-window))
                    (progn
                      (push (list (intern (string fn-sym) :keyword)
                                  (first args))
                            calls)
                      (when (eq fn-sym 'show-window)
                        (setf (window-hidden-p (first args)) nil))
                      (when (eq fn-sym 'hide-window)
                        (setf (window-hidden-p (first args)) t)))
                    (push (list (intern (string fn-sym) :keyword) args)
                          calls))
                nil))))
    (setf (symbol-function 'set-window-position)
          (lambda (win x y)
            (push (list :set-window-position win (list x y)) calls)
            nil))

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
          *last-registered-window* nil)

    ;; HDMI-A-1 at x=0 (1920x1080), DVI-D-1 at x=1920 (1920x1200).
    ;; HDMI registers first, so it is (first (all-outputs)).
    (let* ((hdmi (make-instance 'rhizome-output :proxy :hdmi-proxy
                                :x 0 :y 0 :width 1920 :height 1080))
           (dvi (make-instance 'rhizome-output :proxy :dvi-proxy
                               :x 1920 :y 0 :width 1920 :height 1200))
           (seat (make-instance 'rhizome-seat :proxy :seat-proxy)))
      (setf (gethash :hdmi-proxy *outputs*) hdmi
            (gethash :dvi-proxy *outputs*) dvi
            (gethash :seat-proxy *seats*) seat
            (gethash hdmi *output-workspace*) 1
            (gethash dvi *output-workspace*) 3)
      (flet ((new-window (proxy &optional app-id)
               (let ((win (make-instance 'rhizome-window :proxy proxy
                                         :app-id app-id)))
                 (setf (gethash proxy *windows*) win)
                 win))
             (manage-pass ()
               ;; Same order as on-manage-sequence: every output in
               ;; registration order, HDMI first
               (maphash (lambda (proxy out)
                          (declare (ignore proxy))
                          (apply-layout-for-output out))
                        *outputs*))
             (render-pass ()
               (maphash (lambda (proxy out)
                          (declare (ignore proxy))
                          (render-layout-for-output out))
                        *outputs*)))

        (ok "first output is HDMI (registration order)"
            (first (all-outputs)) hdmi)

        ;; --- current-output ---

        (try "with nothing focused the pointer's output is current"
             (lambda ()
               (setf (seat-pointer-x seat) 2500 (seat-pointer-y seat) 300)
               (eq (current-output) dvi))
             t)
        (try "with the pointer on HDMI, HDMI is current"
             (lambda ()
               (setf (seat-pointer-x seat) 100 (seat-pointer-y seat) 100)
               (eq (current-output) hdmi))
             t)

        ;; A terminal already open on DVI, focused
        (let ((kitty (new-window :kitty-proxy "kitty")))
          (setf (window-output kitty) dvi)
          (assign-window-workspace kitty 3)
          (manage-pass)
          (focus-window kitty seat)

          (try "the focused window's output is current"
               (lambda () (eq (current-output) dvi))
               t)

          ;; --- New windows open on the current output ---

          (let ((firefox (new-window :firefox-proxy "firefox")))
            (manage-pass)
            (ok "a new window opens on the current output, not the first"
                (eq (window-output firefox) dvi) t)
            (ok "a new window takes the current output's workspace"
                (window-workspace firefox) 3)
            (ok "the new window is adopted into a frame there"
                (and (window-frame firefox)
                     (member (window-frame firefox)
                             (frame-leaves (frame-tree-for dvi 3)))
                     t)
                t)
            ;; Clean up: firefox closes
            (frame-remove-window firefox)
            (remhash :firefox-proxy *windows*)
            (manage-pass))

          ;; A new float opens on the current output too
          (let ((dialog (new-window :dialog-proxy "dialog")))
            (setf (window-floating-p dialog) t)
            (manage-pass)
            (ok "a new float opens on the current output"
                (eq (window-output dialog) dvi) t)
            (remhash :dialog-proxy *windows*))

          ;; --- Focusing another output ---

          (let ((term (new-window :term-proxy "foot")))
            (setf (window-output term) hdmi)
            (assign-window-workspace term 1)
            (manage-pass)

            (try "focus-next-output focuses the next output's frame window"
                 (lambda ()
                   (focus-next-output)
                   (list (eq (current-output) hdmi)
                         (eq (focused-window) term)))
                 '(t t))
            (try "focus-next-output cycles back to the first output"
                 (lambda ()
                   (focus-next-output)
                   (list (eq (current-output) dvi)
                         (eq (focused-window) kitty)))
                 '(t t))
            (try "focus-previous-output cycles the other way"
                 (lambda ()
                   (focus-previous-output)
                   (eq (current-output) hdmi))
                 t)
            (try "focus-output-right moves to the output on the right"
                 (lambda ()
                   (focus-output-right)
                   (eq (current-output) dvi))
                 t)
            (try "focus-output-right at the right edge stays put"
                 (lambda ()
                   (focus-output-right)
                   (eq (current-output) dvi))
                 t)
            (try "focus-output-left moves to the output on the left"
                 (lambda ()
                   (focus-output-left)
                   (eq (current-output) hdmi))
                 t)

            ;; --- Moving a window to another output ---

            (try "move-window-next-output moves the focused window"
                 (lambda ()
                   (focus-window term seat)
                   (move-window-next-output)
                   (list (eq (window-output term) dvi)
                         (window-workspace term)
                         (window-frame term)
                         (eq (focused-window) term)
                         (eq (current-output) dvi)))
                 '(t 3 nil t t))
            (ok "the moved window left its frame on the old output"
                (some (lambda (leaf) (member term (frame-windows leaf)))
                      (frame-leaves (frame-tree-for hdmi 1)))
                nil)
            (setf calls '())
            (manage-pass)
            (ok "the next manage pass adopts it into the target's frame"
                (and (window-frame term)
                     (member (window-frame term)
                             (frame-leaves (frame-tree-for dvi 3)))
                     t)
                t)
            (ok "the old output's tree is pruned back to an empty root"
                (let ((root (frame-tree-for hdmi 1)))
                  (list (length (frame-leaves root))
                        (frame-windows root)))
                '(1 nil))
            (render-pass)
            (ok "the moved window renders on the target output"
                (let ((pos (first (positions-of term))))
                  (and pos (>= (first pos) 1920)))
                t)
            (ok "the moved window is not hidden"
                (window-hidden-p term) nil)

            ;; --- Focusing an empty output ---

            (try "focusing an empty output clears focus and marks it current"
                 (lambda ()
                   (focus-next-output)
                   (list (eq (current-output) hdmi)
                         (focused-window)
                         (and (assoc nil *pending-focus-requests*) t)))
                 '(t nil t))
            (let ((editor (new-window :editor-proxy "emacs")))
              (manage-pass)
              (ok "the next new window opens on the empty output"
                  (eq (window-output editor) hdmi) t)
              (try "move-window-previous-output works from the empty output's arrival"
                   (lambda ()
                     (focus-window editor seat)
                     (move-window-previous-output)
                     (eq (window-output editor) dvi))
                   t))

            ;; --- Moving a float translates its position ---

            (let ((float (new-window :float-proxy "pavucontrol")))
              (setf (window-output float) dvi
                    (window-floating-p float) t
                    (window-float-x float) 2100
                    (window-float-y float) 200
                    (window-float-width float) 400
                    (window-float-height float) 300)
              (assign-window-workspace float 3)
              (focus-window float seat)
              (try "a moved float keeps its offset on the target output"
                   (lambda ()
                     (move-window-output-left)
                     (list (eq (window-output float) hdmi)
                           (window-float-x float)
                           (window-float-y float)
                           (window-workspace float)))
                   '(t 180 200 1)))

            ;; --- Commands and IPC ---

            (ok "output commands are registered"
                (every (lambda (k) (and (gethash k *command-table*) t))
                       '(:focus-next-output :focus-previous-output
                         :focus-output-left :focus-output-right
                         :move-window-next-output :move-window-previous-output
                         :move-window-output-left :move-window-output-right))
                t)
            (focus-window kitty seat)
            (try "IPC commands receive the current output"
                 (lambda () (ipc-dispatch-line "current-workspace"))
                 "3")
            (try "the focus-next-output command runs over IPC"
                 (lambda ()
                   (ipc-dispatch-line "focus-next-output")
                   (eq (current-output) hdmi))
                 t)
            (try "switch-workspace defaults to the current output"
                 (lambda ()
                   (focus-window kitty seat)
                   (switch-workspace 5)
                   (list (current-workspace dvi) (current-workspace hdmi)))
                 '(5 1))))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
