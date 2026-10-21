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
instead)."
  (when (and *wm-proxy* (animations-running-p))
    (request-manage-dirty)
    t))
