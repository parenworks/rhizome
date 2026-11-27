;;; The animation engine
;;;
;;; River renders; the WM only proposes geometry. So rhizome
;;; animates by keeping a decaying per-window OFFSET: the render
;;; passes compute final positions exactly as before and then ask
;;; ANIMATION-POSITION to interpolate toward them from a recorded
;;; start. A pump in the WM's main loop (animation-drain) requests
;;; fresh manage/render sequences at ~60fps while anything moves.
;;;
;;; Two start modes:
;;;   :absolute — :from-x/:from-y (defaults: the window's current
;;;     rendered position). Interpolate from there to whatever the
;;;     render pass computes. Expose glides, float/maximize slides.
;;;   :relative — :dx/:dy offset at t=0 decaying to 0. The final
;;;     position is unknown at trigger time. Workspace slide-ins.
;;;
;;; Config: (:animations (:enabled t :duration-ms 200
;;;                       :easing :ease-out-cubic))
;;; Easing: :linear, :ease-out-cubic (default), :ease-in-out.

(in-package #:rhizome)

(defparameter *animations* (make-hash-table)
  "Window → plist (:mode :from-x :from-y :dx :dy :start :duration
:ease). DEFVAR: reloading must not freeze mid-flight windows.")

(defparameter *animation-started-count* 0
  "Animations started (test introspection).")

(defun animations-enabled-p ()
  (config-getf '(:animations) :enabled :default t))

(defun animation-duration ()
  "Duration in universal-time subseconds — internal-time units per
millisecond, so animations work off INTERNAL-REAL-TIME."
  (max 0 (config-getf '(:animations) :duration-ms :default 200)))

(defun animation-easing ()
  (config-getf '(:animations) :easing :default :ease-out-cubic))

(defun ease (name t*)
  "Easing functions over normalized T* in [0,1] → [0,1]. T* is
coerced to a float first: SBCL's internal-real-time arithmetic is
EXACT RATIONAL, and rational easings leak fractional pixel
positions into river's int32 request args — a mid-request type
error that corrupts the wire and gets the client terminated."
  (let ((t* (coerce (min 1.0 (max 0.0 t*)) 'single-float)))
    (ecase name
      (:linear t*)
      ((:ease-out-cubic)
       ;; fast out, gentle landing
       (- 1 (expt (- 1 t*) 3)))
      (:ease-in-out
       (if (< t* 0.5)
           (* 4 t* t* t*)
           (- 1 (/ (expt (- 2 (* 2 t*)) 3) 2)))))))

(defun animation-active-p (win)
  "Whether WIN has an animation in flight."
  (and (gethash win *animations*) t))

(defun animations-running-p ()
  (plusp (hash-table-count *animations*)))

(defun animation-start (win &key (from-x nil) (from-y nil)
                                 (dx 0) (dy 0)
                                 (duration (animation-duration))
                                 (easing (animation-easing)))
  "Start animating WIN. Absolute mode (from-x/from-y given or
defaulted to the current rendered position) glides from there to
whatever the next render pass computes; relative mode (dx/dy)
decays an offset to zero. No-op when animations are disabled or
the duration is zero — instant placement."
  (when (and win (animations-enabled-p) (plusp duration))
    (incf *animation-started-count*)
    (setf (gethash win *animations*)
          (list :mode (cond ((or from-x from-y) :absolute)
                            ;; NB: 0 offsets are truthy in CL —
                            ;; a zero-dx start is absolute-from-now
                            ((or (plusp (abs dx)) (plusp (abs dy)))
                             :relative)
                            (t :absolute))
                :from-x (or from-x (window-x win))
                :from-y (or from-y (window-y win))
                :dx dx
                :dy dy
                :start (get-internal-real-time)
                :duration (* duration
                             (/ internal-time-units-per-second 1000))
                :ease easing))))

(defun animation-position (win x y)
  "The position WIN should render at this frame, given the layout's
final (X Y). Interpolates while an animation is in flight and
RETURNS the target and clears the animation once finished, so a
missed frame never freezes a window mid-air."
  (let ((anim (gethash win *animations*)))
    (if (null anim)
        (values x y)
        (let* ((elapsed (- (get-internal-real-time)
                            (getf anim :start)))
               (raw (/ elapsed (getf anim :duration))))
          (if (>= raw 1)
              (progn
                (remhash win *animations*)
                (values x y))
              (let* ((k (ease (getf anim :ease) raw))
                     (fx (getf anim :from-x))
                     (fy (getf anim :from-y)))
                (ecase (getf anim :mode)
                  (:absolute
                   ;; Whole pixels: river's set_position takes int32
                   (values (round (+ fx (* (- x fx) k)))
                           (round (+ fy (* (- y fy) k)))))
                  (:relative
                   (values (round (+ x (* (getf anim :dx) (- 1 k))))
                           (round (+ y (* (getf anim :dy) (- 1 k)))))))))))))

(defun animation-drain ()
  "Main-loop pump: while anything animates, request fresh
manage/render sequences at frame cadence. Returns T when
animating (the main loop skips its idle sleep and frame-caps
instead). Coalesced: while a manage sequence is already pending
(*pending-manage*), no duplicate manage_dirty goes on the wire —
one request per cycle is the contract."
  (when (and *wm-proxy* (animations-running-p))
    (unless *pending-manage*
      (request-manage-dirty))
    t))

;;; --- Clip boxes: glides never overdraw the bar ---

(defvar *clipped-windows* (make-hash-table :test #'eq)
  "Windows with a clip box currently set on river — so the clip
can be cleared (0 0 0 0) once the animation lands. Cleared on
window close via the :window-closed hook.")

(defun window-clip-box-for (win ax ay rx ry rw rh)
  "The clip box, as a list (x y width height) in WIN-local
coordinates, covering the part of WIN, drawn at (AX AY), that lies
inside the screen rectangle RX RY RW RH. A window size not yet known
(NIL or 0) clips at the rectangle's edge. Width and height are at
least 1: a 0 dimension disables clipping on river."
  (flet ((extent (size far-edge)
           (if (and size (plusp size)) (min size far-edge) far-edge)))
    (let* ((cx (max 0 (- rx ax)))
           (cy (max 0 (- ry ay)))
           (cw (max 1 (- (extent (window-width win) (- (+ rx rw) ax)) cx)))
           (ch (max 1 (- (extent (window-height win) (- (+ ry rh) ay)) cy))))
      (list cx cy cw ch))))

(defun animation-apply (win x y &optional clip-rect)
  "Render-pass entry: the position WIN should draw at this frame
for layout target (X Y), using animation-position's math, plus clip
box management (set_clip_box is render-sequence-legal; this runs
only from render paths). While ANY animation glides the window,
it is clipped to its output's usable area (below the bar), so
intermediate positions draw over neither the bar nor a neighbouring
output. CLIP-RECT, a list (x y width height) in screen coordinates,
clips the window to that rectangle whether or not it animates, and
takes precedence over the glide clip when both apply. With neither,
any clip set earlier clears. A clip box goes on the wire only when
it changes."
  (multiple-value-bind (ax ay) (animation-position win x y)
    (let* ((anim (gethash win *animations*))
           (out (window-output win))
           (rect (cond (clip-rect)
                       ((and anim out)
                        (let ((uy (bar-reserved-height out)))
                          (list (or (output-x out) 0)
                                (+ (or (output-y out) 0) uy)
                                (or (output-width out) 1920)
                                (max 1 (- (or (output-height out) 1080)
                                          uy))))))))
      (cond
        (rect
         (let ((box (apply #'window-clip-box-for win ax ay rect)))
           (unless (equal box (gethash win *clipped-windows*))
             (apply #'set-window-clip-box win box)
             (setf (gethash win *clipped-windows*) box))))
        ((gethash win *clipped-windows*)
         (remhash win *clipped-windows*)
         (set-window-clip-box win 0 0 0 0))))
    (values ax ay)))

(defun clear-window-clips-on-close (win)
  "Forget WIN's clip-tracking entry when the window goes away."
  (remhash win *clipped-windows*))

(add-hook :window-closed #'clear-window-clips-on-close)
