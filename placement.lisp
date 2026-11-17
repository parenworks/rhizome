;;; Window placement policy for frame tiling
;;;
;;; Two decisions in the frame tree are left to a placement policy:
;;; what fills the new half of a split frame, and which frame a new
;;; window enters. A policy is an instance of a PLACEMENT-POLICY
;;; subclass registered by name with DEFINE-PLACEMENT-POLICY; the
;;; config selects one and passes it options:
;;;
;;;   (:tiling (:placement (:policy :stumpwm
;;;                         :new-frame :last-window
;;;                         :new-window (:focused))))
;;;
;;; The default policy, :stumpwm, follows StumpWM's *new-frame-action*
;;; and *new-window-preferred-frame*. Auto-split ((:tiling :auto-split))
;;; still applies to the frame a policy picks for a new window.

(in-package #:rhizome)

;;; --- Focus history ---

(defvar *focus-history* nil
  "Windows in the order they last took focus, most recent first.")

(defun window-alive-p (win)
  "T while WIN is still registered with the window table."
  (and (window-proxy win)
       (eq (gethash (window-proxy win) *windows*) win)))

(defun note-window-focus (win)
  "Move WIN to the front of the focus history and drop dead windows."
  (setf *focus-history*
        (cons win (remove-if (lambda (w)
                               (or (eq w win) (not (window-alive-p w))))
                             *focus-history*))))

(defmethod (setf window-focused-p) :after (value (win rhizome-window))
  "Record WIN in the focus history whenever it takes focus."
  (when value
    (note-window-focus win)))

(defun workspace-focus-history (output workspace)
  "The live windows of WORKSPACE on OUTPUT that have held focus, most
recently focused first."
  (remove-if-not (lambda (win)
                   (and (window-alive-p win)
                        (eq (window-output win) output)
                        (eql (window-workspace win) workspace)))
                 *focus-history*))

;;; --- Protocol ---

(defclass placement-policy ()
  ((options :initarg :options :initform nil :accessor placement-options
            :documentation "The (:tiling (:placement ...)) plist."))
  (:documentation "Base class of frame placement policies."))

(defgeneric placement-new-frame-window (policy output workspace frame history)
  (:documentation "Return the window to show in FRAME, the empty new
half of a split on WORKSPACE of OUTPUT, or NIL to leave it empty.
HISTORY is the workspace's focus history, most recent first."))

(defgeneric placement-new-window-frame (policy output workspace window history)
  (:documentation "Return the leaf frame of WORKSPACE on OUTPUT that the
new WINDOW enters, or NIL for the current frame. HISTORY is the
workspace's focus history, most recent first."))

(defun placement-option (policy key default)
  "The value of KEY in POLICY's options, or DEFAULT when absent."
  (getf (placement-options policy) key default))

;;; --- Registry ---

(defvar *placement-policies* (make-hash-table)
  "Maps placement policy names (keywords) to class names.")

(defvar *placement-default-policy* :stumpwm
  "The policy used when the config names none or an unknown one.")

(defmacro define-placement-policy (name class (&rest superclasses)
                                   slots &rest class-options)
  "Define CLASS, a placement policy, and register it under NAME (a
keyword) for (:tiling (:placement (:policy NAME))). SUPERCLASSES
default to (placement-policy); SLOTS and CLASS-OPTIONS are as for
DEFCLASS. Specialize placement-new-frame-window and
placement-new-window-frame on CLASS."
  `(progn
     (defclass ,class ,(or superclasses '(placement-policy))
       ,slots
       ,@class-options)
     (setf (gethash ,name *placement-policies*) ',class)
     ',class))

(defvar *placement-unknown-names* nil
  "Unknown policy names already reported, so each is logged once.")

(defun current-placement-policy ()
  "An instance of the policy (:tiling (:placement (:policy ...))) names,
carrying the placement options. An unknown name is logged and the
default policy is used."
  (let* ((options (config-getf '(:tiling) :placement))
         (options (if (listp options) options nil))
         (name (getf options :policy *placement-default-policy*))
         (class (gethash name *placement-policies*)))
    (unless class
      (unless (member name *placement-unknown-names*)
        (push name *placement-unknown-names*)
        (format t "~&; Rhizome: unknown placement policy ~S, using ~S~%"
                name *placement-default-policy*))
      (setf class (gethash *placement-default-policy* *placement-policies*)))
    (make-instance class :options options)))

;;; --- The StumpWM policy ---

(define-placement-policy :stumpwm stumpwm-placement (placement-policy)
  ()
  (:documentation "StumpWM's placement. Options:

:new-frame   :last-window (default) shows the most recently focused
             tiled window not visible in any frame; :empty leaves the
             new half empty.
:new-window  a preference list; the first entry that yields a frame
             wins, else the focused frame. Entries:
             :focused    the current frame (default);
             :last       the frame focused before the current one;
             :empty      the first empty frame;
             :unfocused  the first frame other than the current one;
             a function, or a symbol naming one, of (output workspace
             window history) returning a frame or NIL."))

(defun hidden-tiled-windows (output workspace)
  "Tiled windows of WORKSPACE on OUTPUT that sit in a frame without
being its visible window."
  (let ((visible (visible-frame-windows output workspace)))
    (remove-if (lambda (win)
                 (or (window-floating-p win)
                     (window-minimized-p win)
                     (null (window-frame win))
                     (member win visible :test #'eq)))
               (windows-on-workspace output workspace))))

(defmethod placement-new-frame-window ((policy stumpwm-placement)
                                       output workspace frame history)
  (declare (ignore frame))
  (when (eq (placement-option policy :new-frame :last-window) :last-window)
    (let ((hidden (hidden-tiled-windows output workspace)))
      (or (find-if (lambda (win) (member win hidden :test #'eq)) history)
          (first hidden)))))

(defun stumpwm-preferred-frame (entry output workspace window history)
  "The frame one :new-window preference ENTRY yields, or NIL."
  (let* ((leaves (frame-leaves (frame-tree-for output workspace)))
         (current (current-frame-for output workspace)))
    (cond ((or (functionp entry)
               (and entry (symbolp entry) (not (keywordp entry))
                    (fboundp entry)))
           (handler-case (funcall entry output workspace window history)
             (error (c)
               (format t "~&; Rhizome: placement entry ~S failed: ~A~%"
                       entry c)
               nil)))
          (t
           (case entry
             (:focused current)
             (:last (loop for win in history
                          for frame = (window-frame win)
                          when (and frame
                                    (not (eq frame current))
                                    (member frame leaves :test #'eq))
                            return frame))
             (:empty (find-if (lambda (leaf) (null (frame-windows leaf)))
                              leaves))
             (:unfocused (find-if (lambda (leaf) (not (eq leaf current)))
                                  leaves)))))))

(defmethod placement-new-window-frame ((policy stumpwm-placement)
                                       output workspace window history)
  (let ((entries (placement-option policy :new-window '(:focused)))
        (leaves (frame-leaves (frame-tree-for output workspace))))
    (loop for entry in (if (listp entries) entries (list entries))
          for frame = (stumpwm-preferred-frame entry output workspace
                                               window history)
          when (member frame leaves :test #'eq)
            return frame)))

;;; --- Call sites ---

(defun placement-frame-for-new-window (win output workspace)
  "The leaf frame WIN enters on WORKSPACE of OUTPUT, as the configured
policy chooses; the current frame when the policy yields no leaf of
this workspace or signals an error."
  (let ((leaves (frame-leaves (frame-tree-for output workspace)))
        (frame (handler-case
                   (placement-new-window-frame
                    (current-placement-policy) output workspace win
                    (workspace-focus-history output workspace))
                 (error (c)
                   (format t "~&; Rhizome: placement policy error: ~A~%" c)
                   nil))))
    (if (and (frame-p frame) (member frame leaves :test #'eq))
        frame
        (current-frame-for output workspace))))

(defun placement-fill-new-frame (frame output workspace)
  "Move the window the configured policy picks into FRAME, the empty
new half of a split. A pick that is not a tiled window of this
workspace, or an error, leaves FRAME empty."
  (let ((win (handler-case
                 (placement-new-frame-window
                  (current-placement-policy) output workspace frame
                  (workspace-focus-history output workspace))
               (error (c)
                 (format t "~&; Rhizome: placement policy error: ~A~%" c)
                 nil))))
    (when (and (typep win 'rhizome-window)
               (window-alive-p win)
               (not (window-floating-p win))
               (eq (window-output win) output)
               (eql (window-workspace win) workspace))
      (frame-remove-window win)
      (frame-push-window win frame))))

;;; --- A float returning to the tiling ---
;;;
;;; A re-tiled float is not a new window: it sits where it was put.
;;; It enters an empty frame near that spot; a workspace with no
;;; empty frame splits its largest frame instead of the focused one.

(defun frame-point-distance (frame px py)
  "Squared distance from the output-relative point PX PY to FRAME's
rectangle; 0 when the point lies inside it."
  (flet ((gap (p lo size)
           (cond ((< p lo) (- lo p))
                 ((>= p (+ lo size)) (- p (+ lo size -1)))
                 (t 0))))
    (let ((dx (gap px (frame-x frame) (frame-width frame)))
          (dy (gap py (frame-y frame) (frame-height frame))))
      (+ (* dx dx) (* dy dy)))))

(defun retile-centre (win)
  "The output-relative centre of floating WIN as two values, or NIL
when neither its committed size nor its float size is known."
  (let ((output (window-output win))
        (w (if (plusp (window-width win))
               (window-width win)
               (window-float-width win)))
        (h (if (plusp (window-height win))
               (window-height win)
               (window-float-height win))))
    (when (and w h (plusp w) (plusp h))
      (values (- (+ (or (window-float-x win) (window-x win) 0)
                    (floor w 2))
                 (output-x output))
              (- (+ (or (window-float-y win) (window-y win) 0)
                    (floor h 2))
                 (output-y output)
                 (bar-reserved-height))))))

(defun nearest-frame (frames cx cy)
  "The frame of FRAMES nearest the point CX CY, the earliest of equally
near frames; the first of FRAMES when CX is NIL."
  (if (null cx)
      (first frames)
      (let ((best nil)
            (best-distance nil))
        (dolist (frame frames best)
          (let ((d (frame-point-distance frame cx cy)))
            (when (or (null best-distance) (< d best-distance))
              (setf best frame
                    best-distance d)))))))

(defun retile-target-frame (win)
  "The leaf frame floating WIN enters on return to the tiling, as two
values: the frame and T when it must be split first, or NIL.
Candidates are the leaves of WIN's workspace on WIN's output, in this
order:
1. The empty leaf under the float's centre.
2. The empty leaf nearest the centre.
3. With no empty leaf, the largest leaf by area, to split when
   (:tiling :auto-split) is on and the leaf is large enough.
Among equal candidates the one nearest the centre wins, then the
first in tree order. Without a known centre the first in tree order
wins. NIL when WIN has no output, its workspace scrolls or floats
every window, or no leaf qualifies."
  (let ((output (window-output win))
        (workspace (window-workspace win)))
    (when (and output workspace
               (not (scroll-workspace-p output workspace))
               (not (floating-workspace-p output workspace)))
      (multiple-value-bind (cx cy) (retile-centre win)
        (let* ((leaves (frame-leaves (frame-tree-for output workspace)))
               (empty (remove-if #'frame-windows leaves)))
          (if empty
              (nearest-frame empty cx cy)
              (let* ((area (lambda (f) (* (frame-width f) (frame-height f))))
                     (largest (reduce #'max leaves :key area))
                     (frame (nearest-frame
                             (remove largest leaves :key area :test-not #'=)
                             cx cy)))
                (when (and frame
                           (config-getf '(:tiling) :auto-split :default t)
                           (frame-splittable-p frame))
                  (values frame t)))))))))

(defun place-retiled-window (win)
  "Put WIN, a float returning to the tiling, in the frame
retile-target-frame picks, splitting it first when that frame is
occupied, and make WIN's frame current. Returns WIN's frame, or NIL
to leave WIN frameless for the manage pass, which places it as a new
window."
  (multiple-value-bind (frame split) (retile-target-frame win)
    (when frame
      (let ((output (window-output win))
            (workspace (window-workspace win)))
        (frame-remove-window win)
        (if split
            (split-frame-for-window win frame output workspace)
            (progn
              (frame-push-window win frame)
              (setf (gethash (frame-key output workspace) *current-frames*)
                    frame)))
        (window-frame win)))))
