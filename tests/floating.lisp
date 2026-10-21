;;; Tests for the floating window engine (floating.lisp).
;;; River-touching functions are stubbed so the manage/render
;;; integration can be exercised without a live compositor.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (called? (win tag)
           (and (member (list tag win) calls :test #'equal) t))
         (tag-called? (tag)
           (and (find tag calls :key #'car) t)))

    ;; Record calls instead of touching river. show/hide also update
    ;; the hidden flag like the real functions do, so render-pass
    ;; transition logic works.
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  message request-manage-dirty))
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

    ;; Fresh global state for these tests
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table))

    (let ((out (make-instance 'rhizome-output :proxy :out-proxy
                              :width 1920 :height 1080))
          (term (make-instance 'rhizome-window :proxy :term-proxy))
          (gimp (make-instance 'rhizome-window :proxy :gimp-proxy)))

      (setf (gethash :term-proxy *windows*) term
            (gethash :gimp-proxy *windows*) gimp
            (window-output term) out
            (window-x term) 100 (window-y term) 50
            (window-width term) 800 (window-height term) 600)
      (assign-window-workspace term 1)

      ;; --- float/unfloat state ---

      (setf (window-x gimp) 100 (window-y gimp) 50
            (window-width gimp) 400 (window-height gimp) 300)

      (float-window gimp)
      (ok "float-window marks floating" (window-floating-p gimp) t)
      (ok "float defaults position from window"
          (list (window-float-x gimp) (window-float-y gimp)) '(100 50))
      (ok "float defaults size from window"
          (list (window-float-width gimp) (window-float-height gimp))
          '(400 300))
      ;; request-manage-dirty takes no window argument
      (ok "float requests relayout"
          (tag-called? :request-manage-dirty) t)

      (setf calls '())
      (float-window gimp)                  ; already floating: no-op
      (ok "float-window is idempotent"
          (tag-called? :request-manage-dirty) nil)

      (unfloat-window gimp)
      (ok "unfloat clears floating" (window-floating-p gimp) nil)
      (ok "unfloat requests relayout"
          (tag-called? :request-manage-dirty) t)

      (setf calls '())
      (toggle-float gimp)
      (ok "toggle-float floats" (window-floating-p gimp) t)
      (toggle-float gimp)
      (ok "toggle-float tiles again" (window-floating-p gimp) nil)

      ;; --- maximize (client maximize_requested / IPC toggle) ---

      ;; Bar disabled in tests, so the usable area is the full output.
      (setf calls '())
      (maximize-window term)               ; tiled 800x600 at (100,50)
      (ok "maximize floats the window" (window-floating-p term) t)
      (ok "maximize covers the usable area"
          (list (window-float-x term) (window-float-y term)
                (window-float-width term) (window-float-height term))
          '(0 0 1920 1080))
      (ok "maximize remembers it was tiled"
          (window-maximize-prev term) :tiled)
      (ok "maximize flags the window" (window-maximized-p term) t)
      (ok "maximize requests relayout"
          (tag-called? :request-manage-dirty) t)

      (setf calls '())
      (maximize-window term)
      (ok "maximize is idempotent"
          (tag-called? :request-manage-dirty) nil)

      (unmaximize-window term)
      (ok "unmaximize restores the tiling"
          (window-floating-p term) nil)
      (ok "unmaximize clears the flag" (window-maximized-p term) nil)
      (ok "unmaximize clears the saved state"
          (window-maximize-prev term) nil)

      ;; A float maximizing must restore its own geometry, not tile.
      (setf (window-output gimp) out)
      (setf (window-float-x gimp) 100 (window-float-y gimp) 50
            (window-float-width gimp) 400 (window-float-height gimp) 300)
      (toggle-float gimp)                  ; float with own geometry
      (maximize-window gimp)
      (ok "float maximize covers the usable area"
          (list (window-float-x gimp) (window-float-y gimp)
                (window-float-width gimp) (window-float-height gimp))
          '(0 0 1920 1080))
      (ok "float maximize saves its geometry"
          (window-maximize-prev gimp) '(:floating 100 50 400 300))
      (unmaximize-window gimp)
      (ok "float unmaximize restores geometry, stays floating"
          (list (window-floating-p gimp)
                (window-float-x gimp) (window-float-y gimp)
                (window-float-width gimp) (window-float-height gimp))
          '(t 100 50 400 300))

      ;; Size hints clamp a maximize: a fixed-size window must not stretch.
      (setf (window-hint-max-width term) 500 (window-hint-max-height term) 400)
      (maximize-window term)
      (ok "maximize respects max size hints"
          (list (window-float-width term) (window-float-height term))
          '(500 400))
      (setf (window-hint-max-width term) nil (window-hint-max-height term) nil)
      (unmaximize-window term)

      ;; IPC toggle flips both ways
      (setf calls '())
      (toggle-maximize term)
      (ok "toggle-maximize maximizes" (window-maximized-p term) t)
      (toggle-maximize term)
      (ok "toggle-maximize restores" (window-maximized-p term) nil)

      ;; --- geometry helpers ---

      (setf (window-output gimp) out)
      (setf (window-float-x gimp) 5000 (window-float-y gimp) -500
            (window-float-width gimp) 300 (window-float-height gimp) 200)
      (float-clamp-position gimp)
      (ok "clamp pulls back onto output" (window-float-x gimp) 1890)
      (ok "clamp pulls up onto output" (window-float-y gimp) -170)

      (setf (window-hint-min-width gimp) 120
            (window-hint-max-width gimp) 600)
      (ok "clamp respects min hint"
          (float-clamp-dim gimp 90 :width) 120)
      (ok "clamp respects max hint"
          (float-clamp-dim gimp 999 :width) 600)
      (ok "clamp keeps in-range sizes"
          (float-clamp-dim gimp 300 :width) 300)
      (setf (window-hint-min-width gimp) nil
            (window-hint-max-width gimp) nil)
      (ok "clamp floors at default without hints"
          (float-clamp-dim gimp 10 :height) 50)

      ;; --- interactive resize math ---

      (setf (window-float-x gimp) 200 (window-float-y gimp) 100
            (window-float-width gimp) 300 (window-float-height gimp) 200
            (window-hint-min-width gimp) nil (window-hint-min-height gimp) nil
            (window-hint-max-width gimp) nil (window-hint-max-height gimp) nil)
      (float-apply-resize
        gimp '(:dx 40 :dy 20 :edges (:bottom :right)
                :origin-x 200 :origin-y 100
                :origin-w 300 :origin-h 200))
      (ok "resize from bottom-right grows"
          (list (window-float-x gimp) (window-float-y gimp)
                (window-float-width gimp) (window-float-height gimp))
          '(200 100 340 220))

      (setf (window-float-x gimp) 200 (window-float-y gimp) 100
            (window-float-width gimp) 300 (window-float-height gimp) 200)
      (float-apply-resize
        gimp '(:dx -60 :dy -30 :edges (:top :left)
                :origin-x 200 :origin-y 100
                :origin-w 300 :origin-h 200))
      (ok "resize from top-left moves the corner"
          (list (window-float-x gimp) (window-float-y gimp)
                (window-float-width gimp) (window-float-height gimp))
          '(140 70 360 230))

      ;; --- manage pass: floats excluded from tiling, proposed separately ---

      (setf calls '()
            (window-floating-p gimp) t
            (window-float-x gimp) 400 (window-float-y gimp) 300
            (window-float-width gimp) 300 (window-float-height gimp) 200
            (window-output gimp) out)
      (assign-window-workspace gimp 1)
      (setf (gethash out *output-workspace*) 1)
      (apply-layout-for-output out)
      ;; The tiled window gets its layout proposal...
      (ok "tiled window proposed" (funcall #'called? term :propose-window-dimensions) t)
      ;; ...but the float is proposed with its own float size, not a
      ;; layout cell, and is told it is not tiled.
      (ok "float gets set-tiled none"
          (funcall #'called? gimp :set-window-tiled-state) t)
      (ok "float keeps its own size"
          (list (window-float-width gimp) (window-float-height gimp))
          '(300 200))

      ;; A fresh float (no output yet) is adopted onto the workspace
      (let ((fresh (make-instance 'rhizome-window :proxy :fresh-proxy)))
        (setf (gethash :fresh-proxy *windows*) fresh
              (window-floating-p fresh) t
              (window-float-x fresh) 10 (window-float-y fresh) 20)
        (setf calls '())
        (apply-layout-for-output out)
        (ok "fresh float adopted onto output" (eq (window-output fresh) out) t)
        (ok "fresh float stamped with workspace" (window-workspace fresh) 1)
        (ok "fresh float proposed 0x0 (client picks its size)"
            (called? fresh :propose-window-dimensions) t)
        (remhash :fresh-proxy *windows*))

      ;; --- render pass: floats positioned, shown, hidden with workspaces ---

      (setf calls '()
            (window-hidden-p gimp) t)
      (render-layout-for-output out)
      (ok "float shown when hidden" (funcall #'called? gimp :show-window) t)
      (ok "float positioned at float coords"
          (funcall #'called? gimp :set-window-position) t)
      (ok "float not hidden after render" (window-hidden-p gimp) nil)

      ;; A float on another workspace is hidden, not rendered
      (assign-window-workspace gimp 2)
      (setf calls '())
      (render-layout-for-output out)
      (ok "other-workspace float hidden"
          (funcall #'called? gimp :hide-window) t)
      (ok "other-workspace float not positioned"
          (funcall #'called? gimp :set-window-position) nil)
      (assign-window-workspace gimp 1)

      ;; --- interactive op requests ---

      (let ((seat (make-instance 'rhizome-seat :proxy :seat-proxy)))
        (setf (window-floating-p gimp) nil)
        (float-op-request gimp seat :move)
        (ok "non-floating drag is ignored" (seat-op-pending seat) nil)
        (setf (window-floating-p gimp) t)
        (float-op-request gimp seat :move)
        (ok "floating drag is queued"
            (getf (seat-op-pending seat) :type) :move)
        (ok "drag queues the window"
            (getf (seat-op-pending seat) :win) gimp))

      ;; --- window rules :float ---

      (setf calls '())
      (configure '(:window-rules ((:match "gimp" :float t))))
      (setf (window-floating-p gimp) nil
            (window-app-id gimp) "org.gimp.GIMP")
      (apply-window-rules gimp)
      (ok "rule :match gimp floats the window" (window-floating-p gimp) t)
      (setf (window-app-id gimp) nil
            (window-floating-p gimp) nil)
      (setf (window-app-id term) "foot")
      (apply-window-rules term)
      (ok "non-matching windows stay tiled" (window-floating-p term) nil))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
