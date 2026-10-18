;;; Output (monitor) management for Rhizome
;;;
;;; Each river_output_v1 is wrapped in a rhizome-output CLOS instance
;;; that tracks the output's position, dimensions, and assigned windows.

(in-package #:rhizome)

(defclass rhizome-output ()
  ((proxy    :initarg :proxy    :accessor output-proxy)
   (x        :initarg :x        :accessor output-x :initform 0)
   (y        :initarg :y        :accessor output-y :initform 0)
   (width    :initarg :width    :accessor output-width :initform 0)
   (height   :initarg :height   :accessor output-height :initform 0)
   (wl-output-name :initarg :wl-output-name :accessor output-wl-output-name :initform nil)
   (current-layout :initarg :current-layout :accessor output-current-layout :initform :master-stack)
   (windows  :initarg :windows  :accessor output-windows :initform nil))
  (:documentation "Represents a logical output (monitor) managed by rhizome."))

(defun register-output (proxy)
  "Register a new river_output_v1 proxy and set up event handlers."
  (let ((output (make-instance 'rhizome-output :proxy proxy)))
    (setf (gethash proxy *outputs*) output)
    (push (lambda (event-name &rest args)
            (apply #'handle-output-event output event-name args))
          (wl:wl-proxy-hooks proxy))
    (run-hook :output-created output)
    (format t "~&; Rhizome: new output registered~%")
    ;; Register with river_layer_shell_v1 if available
    (when *river-layer-shell*
      (handler-case
          (let ((ls-output (rwm:river-layer-shell-v1.get-output
                             *river-layer-shell* proxy)))
            (push (lambda (event-name &rest args)
                    (case event-name
                      (:non_exclusive_area
                       (destructuring-bind (x y w h) args
                         (declare (ignore x y w h)))
                       nil)))
                  (wl:wl-proxy-hooks ls-output))
            (format t "~&; Rhizome: registered output with river_layer_shell_v1~%"))
          (error (e)
            (format t "~&; Rhizome: failed to register output with layer shell: ~A~%" e))))))

(defun handle-output-event (output event-name &rest args)
  "Dispatch river_output_v1 events for OUTPUT."
  (case event-name
    (:position
     (destructuring-bind (x y) args
       (setf (output-x output) x
             (output-y output) y)))

    (:dimensions
     (destructuring-bind (width height) args
       (setf (output-width output) width
             (output-height output) height)
       ;; Frame trees cache their geometry: rescale them to the new
       ;; size and re-tile (defined in frames.lisp, which loads later)
       (handler-case
           (progn
             (refit-output-frame-trees output)
             (when *wm-proxy*
               (request-manage-dirty)))
         (error (e)
           (format t "~&; Rhizome: error refitting frame trees: ~A~%" e)))))

    (:wl-output
     (destructuring-bind (name) args
       (setf (output-wl-output-name output) name)))

    (:removed
     (format t "~&; Rhizome: output removed~%")
     (remhash (output-proxy output) *outputs*))

    (t
     (format t "~&; Rhizome: unhandled output event ~A~%" event-name))))

(defun all-outputs ()
  "Return a list of all registered outputs."
  (loop for output being the hash-values of *outputs* collect output))

(defun find-output-for-window (win)
  "Find which output a window should be managed on.
For now, uses the first output or the output the pointer is on."
  (let ((outputs (all-outputs)))
    (when outputs
      (first outputs))))
