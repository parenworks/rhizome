;;; Window management for Stumpway
;;;
;;; Each river_window_v1 is wrapped in a stumpway-window CLOS instance
;;; that tracks the window's state (dimensions, title, app_id, focus,
;;; tiling frame assignment, etc.)

(in-package #:stumpway)

(defclass stumpway-window ()
  ((proxy       :initarg :proxy       :accessor window-proxy)
   (node        :initarg :node        :accessor window-node :initform nil)
   (title       :initarg :title       :accessor window-title :initform nil)
   (app-id      :initarg :app-id      :accessor window-app-id :initform nil)
   (width       :initarg :width       :accessor window-width :initform 0)
   (height      :initarg :height      :accessor window-height :initform 0)
   (x           :initarg :x           :accessor window-x :initform 0)
   (y           :initarg :y           :accessor window-y :initform 0)
   (output      :initarg :output      :accessor window-output :initform nil)
   (frame       :initarg :frame       :accessor window-frame :initform nil)
   (fullscreen  :initarg :fullscreen  :accessor window-fullscreen-p :initform nil)
   (hidden      :initarg :hidden      :accessor window-hidden-p :initform nil)
   (focused     :initarg :focused     :accessor window-focused-p :initform nil))
  (:documentation "Represents a managed window in the river compositor."))

(defvar *last-registered-window* nil
  "The most recently registered window, for auto-focus.")

(defun register-window (proxy)
  "Register a new river_window_v1 proxy and set up event handlers."
  (let ((win (make-instance 'stumpway-window :proxy proxy)))
    (setf (gethash proxy *windows*) win
          *last-registered-window* win)
    (push (lambda (event-name &rest args)
            (apply #'handle-window-event win event-name args))
          (wl:wl-proxy-hooks proxy))
    (format t "~&; Stumpway: new window registered~%")))

(defun handle-window-event (win event-name &rest args)
  "Dispatch river_window_v1 events for WIN."
  (handler-case
      (case event-name
        (:dimensions
         (destructuring-bind (width height) args
           (setf (window-width win) width
                 (window-height win) height)))

        (:dimensions-hint
         (destructuring-bind (min-w min-h max-w max-h) args
           (declare (ignore min-w min-h max-w max-h))
           ;; Could store these for layout constraints
           nil))

        (:title
         (destructuring-bind (title) args
           (setf (window-title win) title)
           (format t "~&; Stumpway: window title: ~A~%" title)))

        (:app-id
         (destructuring-bind (app-id) args
           (setf (window-app-id win) app-id)))

        (:closed
         (format t "~&; Stumpway: window closed~%")
         (remhash (window-proxy win) *windows*)
         (remhash win *window-workspace*)
         (remhash win *minimized-windows*))

        (:fullscreen-requested
         (destructuring-bind (output) args
           (declare (ignore output))
           (setf (window-fullscreen-p win) t)
           (request-manage-dirty)))

        (:exit-fullscreen-requested
         (setf (window-fullscreen-p win) nil)
           (request-manage-dirty))

        (:maximize-requested
         (request-manage-dirty))

        (:unmaximize-requested
         (request-manage-dirty))

        (:minimize-requested
         (setf (window-hidden-p win) t)
         (setf (window-minimized-p win) t)
         (request-manage-dirty))

        (:pointer-move-requested
         ;; Could start interactive move via seat op
         nil)

        (:pointer-resize-requested
         ;; Could start interactive resize via seat op
         nil)

        (t
         (format t "~&; Stumpway: unhandled window event ~A~%" event-name)))
    (error (e)
      (format t "~&; Stumpway: error handling window event ~A: ~A~%" event-name e))))

(defun all-windows ()
  "Return a list of all registered windows."
  (loop for win being the hash-values of *windows* collect win))

(defun windows-on-output (output)
  "Return windows assigned to OUTPUT, including hidden ones."
  (remove-if (lambda (win)
               (not (eq (window-output win) output)))
             (all-windows)))

(defun focus-window (win seat)
  "Give keyboard focus to WIN on SEAT."
  (when (and (window-proxy win) (seat-proxy seat))
    ;; Clear focus on all other windows
    (dolist (w (all-windows))
      (unless (eq w win)
        (setf (window-focused-p w) nil)))
    (rwm:river-seat-v1.focus-window (seat-proxy seat) (window-proxy win))
    (setf (window-focused-p win) t
          (seat-focused-window seat) win)))

(defun close-window (win)
  "Request that WIN be closed."
  (rwm:river-window-v1.close (window-proxy win)))

(defun propose-window-dimensions (win width height)
  "Propose dimensions for WIN during a manage sequence."
  (rwm:river-window-v1.propose-dimensions (window-proxy win) width height))

(defun set-window-position (win x y)
  "Set the render position of WIN during a render sequence.
Requires that the window's node has been created."
  (when (window-node win)
    (rwm:river-node-v1.set-position (window-node win) x y)))

(defun set-window-borders (win edges width r g b a)
  "Set borders on WIN during a render sequence."
  (rwm:river-window-v1.set-borders (window-proxy win) edges width r g b a))

(defun show-window (win)
  "Show WIN (un-hide it) during a render sequence."
  (rwm:river-window-v1.show (window-proxy win))
  (setf (window-hidden-p win) nil))

(defun hide-window (win)
  "Hide WIN during a render sequence."
  (rwm:river-window-v1.hide (window-proxy win))
  (setf (window-hidden-p win) t))

(defun make-window-node (win)
  "Create the render list node for WIN. Must be called once per window."
  (unless (window-node win)
    (let ((node (rwm:river-window-v1.get-node (window-proxy win))))
      (setf (window-node win) node))))
