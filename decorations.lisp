;;; Decoration surfaces — pseudo-shadows and focus glow for floats
;;;
;;; River gives the WM a surface per window via
;;; river_window_v1.get_decoration_below/above: the WM sizes it,
;;; draws it, and positions it with river_decoration_v1.set_offset
;;; (relative to the window's top-left — negative offsets reach
;;; outside the window rect, which is how a shadow band works).
;;;
;;; Rhizome draws a soft alpha-gradient pseudo-shadow around every
;;; floating window (decoration_below), tinted toward the accent
;;; color when the window is focused — so focused floats glow.
;;; Tiled windows stay with river's crisp set_borders: the shadow
;;; belongs to floats.
;;;
;;; Protocol discipline (the whole point of today's lessons):
;;;   get_decoration_below   — anytime, but the surface must be
;;;                            role-less and buffer-less (fresh)
;;;   set_offset             — RENDER sequence only
;;;   sync_next_commit       — RENDER sequence only, and the
;;;                            surface must commit before the
;;;                            render_finish that ends the pass
;;;   destroy                — anytime
;;; Everything decoration-related therefore happens inside
;;; decorations-render-pass, called from on-render-sequence.

(in-package #:rhizome)

;;; --- State ---

(defvar *decorations* (make-hash-table :test #'eq)
  "WINDOW → decoration state list:
(:proxy ... :surface ... :ptr ... :fd ... :size ... :pool ...
 :buffer ... :w ... :h ... :focused ... :offset-sent ...)")

(defun decoration-config (&key (enabled t) (radius 12) (alpha 0.4)
                          (focused-alpha 0.6))
  "(:decorations ...) settings with defaults."
  (list :enabled (config-getf '(:decorations) :enabled :default enabled)
        :radius (config-getf '(:decorations) :radius :default radius)
        :alpha (config-getf '(:decorations) :alpha :default alpha)
        :focused-alpha (config-getf '(:decorations) :focused-alpha
                                    :default focused-alpha)))

(defun decoration-radius ()
  "The shadow band width in px (0 disables)."
  (let* ((cfg (decoration-config))
         (enabled (getf cfg :enabled))
         (radius (getf cfg :radius)))
    (if (and enabled (plusp radius) (realp radius))
        (round radius)
        0)))

(defun decoration-window-size (win)
  "The window's rendered size as (VALUES W H)."
  (values (or (window-float-width win) (window-width win) 100)
          (or (window-float-height win) (window-height win) 100)))

;;; --- Lifecycle ---

(defun cleanup-window-decoration (win)
  "Destroy WIN's decoration (proxy, surface, shm). Safe when
absent. Called from the :closed hook and when a window un-floats."
  (let ((state (gethash win *decorations*)))
    (when state
      (remhash win *decorations*)
      (handler-case
          (progn
            (when (getf state :proxy)
              (rwm:river-decoration-v1.destroy (getf state :proxy)))
            (when (getf state :surface)
              (wl:wl-surface.destroy (getf state :surface)))
            (when (getf state :buffer)
              (wl:wl-buffer.destroy (getf state :buffer)))
            (when (getf state :pool)
              (wl:wl-shm-pool.destroy (getf state :pool)))
            (when (getf state :ptr)
              (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                                    :unsigned-long (getf state :size) :int))
            (when (getf state :fd)
              (cffi:foreign-funcall "close" :int (getf state :fd) :int)))
        (error (e)
          (format t "~&; Rhizome: decoration cleanup error: ~A~%" e))))))

(defun ensure-window-decoration (win)
  "Create the decoration (surface + river_decoration_below proxy)
for WIN if it lacks one. The surface must be fresh: no role, no
buffer. get_decoration_below is anytime-legal."
  (let ((state (gethash win *decorations*)))
    (when (and (null state)
               *compositor* (window-proxy win)
               (plusp (decoration-radius)))
      (let ((surface (wl:wl-compositor.create-surface *compositor*)))
        (handler-case
            (let ((proxy (rwm:river-window-v1.get-decoration-below
                           (window-proxy win) surface)))
              (setf (gethash win *decorations*)
                    (list :proxy proxy
                          :surface surface
                          :w 0 :h 0
                          :focused (window-focused-p win)
                          :offset-sent nil)))
          (error (e)
            (wl:wl-surface.destroy surface)
            (format t "~&; Rhizome: decoration creation failed: ~A~%" e)))))))

;;; --- Drawing ---

(defun decoration-shm (state width height)
  "The decoration's ARGB shm buffer, resized when needed."
  (let ((needed (* width height 4)))
    (if (and (getf state :ptr)
             (= (getf state :size) needed))
        state
        (progn
          ;; free the old buffer if any
          (when (getf state :buffer)
            (wl:wl-buffer.destroy (getf state :buffer)))
          (when (getf state :pool)
            (wl:wl-shm-pool.destroy (getf state :pool)))
          (when (getf state :ptr)
            (cffi:foreign-funcall "munmap" :pointer (getf state :ptr)
                                  :unsigned-long (getf state :size) :int))
          (when (getf state :fd)
            (cffi:foreign-funcall "close" :int (getf state :fd) :int))
          (let* ((fd (cffi:foreign-funcall "memfd_create" :string "deco" :int 0 :int))
                 (ptr (if (minusp fd)
                          nil
                          (progn
                            (cffi:foreign-funcall "ftruncate" :int fd
                                                  :long needed :int)
                            (cffi:foreign-funcall "mmap"
                                                  :pointer (cffi:null-pointer)
                                                  :unsigned-long needed
                                                  :int 3 :int 1
                                                  :int fd :long 0 :pointer)))))
            (if (or (minusp fd) (cffi:null-pointer-p ptr))
                nil
                (let* ((pool (wl:wl-shm.create-pool *shm* fd needed))
                       (buffer (wl:wl-shm-pool.create-buffer
                                 pool 0 width height (* width 4) :argb8888)))
                  (setf (getf state :ptr) ptr
                        (getf state :fd) fd
                        (getf state :size) needed
                        (getf state :pool) pool
                        (getf state :buffer) buffer)
                  state)))))))

(defun decoration-draw (state width height radius base-alpha
                                 focus-alpha r g b)
  "The pseudo-shadow: a soft alpha-gradient band of width RADIUS
around the window rect, darkest at the window edge, fully
transparent at the band's outer rim (rounded corners). Focused
windows tint toward (r g b). Premultiplied ARGB: channels are
scaled by the alpha, so c* ≤ a always holds."
  (let ((ptr (getf state :ptr)))
    (unless (cffi:null-pointer-p ptr)
      (let* ((win-w (- width (* 2 radius)))
             (win-h (- height (* 2 radius)))
             (left radius) (right (+ radius win-w))
             (top radius) (bottom (+ radius win-h))
             (alpha-0 (* 255 (if (getf state :focused)
                                 focus-alpha base-alpha)))
             ;; Straight shadow color: dark grey, tinted halfway
             ;; toward the accent
             (cr (+ 30 (* (- r 30) 0.5)))
             (cg (+ 30 (* (- g 30) 0.5)))
             (cb (+ 36 (* (- b 36) 0.5))))
        (loop for py below height
              do (let* ((dy (max 0
                                 (cond ((< py top) (- top py))
                                       ((> py bottom) (- py bottom))
                                       (t 0)))))
                   (loop for px below width
                         do (let* ((dx (max 0
                                             (cond ((< px left) (- left px))
                                                   ((> px right) (- px right))
                                                   (t 0))))
                                    (dist (sqrt (+ (* dx dx) (* dy dy)))))
                              (if (>= dist radius)
                                  ;; Outside the band: transparent
                                  (setf (cffi:mem-aref ptr :unsigned-int
                                                       (+ (* width py) px))
                                        0)
                                  ;; Inside: falloff over the band,
                                  ;; eased quadratic
                                  (let* ((fall (if (zerop radius)
                                                   0.0
                                                   (- 1.0 (/ dist radius))))
                                         (k (* fall fall))
                                         (a (round (* alpha-0 k)))
                                         ;; premultiplied channels
                                         (sr (min 255 (round (* cr a) 255)))
                                         (sg (min 255 (round (* cg a) 255)))
                                         (sb (min 255 (round (* cb a) 255))))
                                    (setf (cffi:mem-aref ptr :unsigned-int
                                                         (+ (* width py) px))
                                          (logior (ash a 24)
                                                  (ash sr 16)
                                                  (ash sg 8)
                                                  sb))))))))))))

;;; --- Render pass (render-sequence only) ---

(defun decoration-redraw (state dw dh radius cfg ar ag ab)
  "Redraw WIN's shadow buffer and commit it. Called only inside a
render sequence: sync_next_commit is render-only and the surface
commit must land before the render_finish that ends this pass."
  (let ((buffered (decoration-shm state dw dh)))
    (when buffered
      (setf state buffered)
      (decoration-draw state dw dh radius
                       (getf cfg :alpha) (getf cfg :focused-alpha)
                       ar ag ab)
      (rwm:river-decoration-v1.sync-next-commit (getf state :proxy))
      (wl:wl-surface.attach (getf state :surface) (getf state :buffer) 0 0)
      (wl:wl-surface.damage (getf state :surface) 0 0 dw dh)
      (wl:wl-surface.commit (getf state :surface))))
    ;; the updated plist — threaded back to the caller, which
    ;; writes it into *decorations* (setf/getf rebinds, not mutates)
    state)

(defun decoration-update-for-window (win radius cfg ar ag ab)
  "Keep WIN's decoration current (render pass only). The state is
a plist: setf/getf REBINDS rather than mutates, so the updated
list is threaded and written back to the hash."
  (ensure-window-decoration win)
  (let ((state (gethash win *decorations*)))
    (when state
      (multiple-value-bind (w h) (decoration-window-size win)
        (let ((dw (+ w (* 2 radius)))
              (dh (+ h (* 2 radius))))
          (when (or (/= (getf state :w) dw)
                    (/= (getf state :h) dh)
                    (not (eql (getf state :focused)
                              (window-focused-p win))))
            (setf (getf state :w) dw
                  (getf state :h) dh
                  (getf state :focused) (window-focused-p win))
            (setf state (decoration-redraw state dw dh radius cfg ar ag ab)))
          (unless (getf state :offset-sent)
            (rwm:river-decoration-v1.set-offset
              (getf state :proxy) (- radius) (- radius))
            (setf (getf state :offset-sent) t))
          ;; write the (possibly rethreaded) plist back
          (setf (gethash win *decorations*) state)))))
  nil)

(declaim (ftype (function () null) decorations-render-pass))
(defun decorations-render-pass ()
  "Called from on-render-sequence: keep every floating window's
pseudo-shadow current. All river_decoration_v1 requests here are
render-sequence legal: set_offset and sync_next_commit are
render-only, and the commit lands before this render sequence's
render_finish. Returns NIL."
  (let ((radius (decoration-radius)))
    (when (plusp radius)
      (let* ((cfg (decoration-config))
             (ar 122) (ag 162) (ab 247))
        (multiple-value-bind (r g b)
            (handler-case (theme-border-rgb t)
              (error () (values ar ag ab)))
          (loop for win being the hash-values of *windows*
                if (and (window-floating-p win)
                        (not (window-minimized-p win))
                        (gethash (window-proxy win) *windows*))
                  do (decoration-update-for-window win radius cfg r g b)
                else
                  do (cleanup-window-decoration win)))))
    nil))

;;; Cleanup on window close
(add-hook :window-closed #'cleanup-window-decoration)
