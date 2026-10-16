;;; Transient on-screen messages for Stumpway
;;;
;;; The StumpWM message-window equivalent: show text briefly on
;;; screen, auto-hide after a timeout. Expiry is drained by the WM's
;;; main poll loop — no threads, no timers.

(in-package #:stumpway)

(defparameter *message-state* nil
  "Surface state hash for the visible message.")
(defparameter *message-expiry* 0
  "Universal time when the current message hides.")
(defparameter *message-width* 700)
(defparameter *message-height* 22)

(defun show-message (text &optional (seconds 3))
  "Show TEXT on screen for SECONDS (default 3). If a message is
already showing, replace it."
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Stumpway: ~A~%" text)
    (return-from show-message))
  (setf *message-expiry* (+ (get-universal-time) seconds))
  ;; Reuse the visible surface when possible
  (if (and *message-state* (gethash :shm-ptr *message-state*))
      (message-render text)
      (let ((state (make-hash-table)))
        (setf *message-state* state)
        (let ((surface (wl:wl-compositor.create-surface *compositor*)))
          (setf (gethash :surface state) surface)
          (let ((layer-surface
                  (rwm:zwlr-layer-shell-v1.get-layer-surface
                    *layer-shell* surface nil :top "stumpway-message")))
            (setf (gethash :layer-surface state) layer-surface)
            (push (lambda (event-name &rest args)
                    (declare (ignore args))
                    (when (eq event-name :closed)
                      (setf *message-state* nil)))
                  (wl:wl-proxy-hooks layer-surface))
            (rwm:zwlr-layer-surface-v1.set-anchor layer-surface nil)
            (rwm:zwlr-layer-surface-v1.set-size layer-surface
              *message-width* *message-height*)
            (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
              layer-surface :none)
            (rwm:zwlr-layer-surface-v1.set-margin layer-surface 60 0 0 0)
            ;; Render immediately if the buffer exists (configure will
            ;; create it); store the text for the first render
            (setf (gethash :text state) text)
            (wl:wl-surface.commit surface))))))

(defun message-render (text)
  "Render TEXT into the message surface."
  (let ((state *message-state*))
    (when state
      (let ((ptr (gethash :shm-ptr state)))
        (when (and ptr (not (cffi:null-pointer-p ptr)))
          (let ((width (gethash :width state))
                (height (gethash :height state)))
            (multiple-value-bind (br bg bb)
                (parse-hex-color (config-getf '(:bar) :background
                                             :default "#1a1b26"))
              (bar-clear ptr width height br bg bb))
            (multiple-value-bind (ar ag ab)
                (parse-hex-color (config-getf '(:bar) :accent
                                             :default "#7aa2f7"))
              (bar-draw-text ptr width height text
                             8 (floor (- height 16) 2) ar ag ab))
            (wl:wl-surface.attach (gethash :surface state)
                                  (gethash :wl-buffer state) 0 0)
            (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
            (wl:wl-surface.commit (gethash :surface state))))))))

(defun message-configure-handler (state layer-surface serial width height)
  "Acknowledge the layer surface configure and render the pending text."
  (declare (ignore width height))
  (setf (gethash :width state) *message-width*
        (gethash :height state) *message-height*)
  (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
  (bar-ensure-shm state)
  (message-render (gethash :text state "")))

(defun message-hide ()
  "Hide the on-screen message."
  (let ((state *message-state*))
    (setf *message-state* nil
          *message-expiry* 0)
    (when state
      (handler-case
          (progn
            (when (gethash :layer-surface state)
              (rwm:zwlr-layer-surface-v1.destroy
                (gethash :layer-surface state)))
            (when (gethash :surface state)
              (wl:wl-surface.destroy (gethash :surface state)))
            (bar-destroy-shm state))
        (error (e)
          (format t "~&; Stumpway: error hiding message: ~A~%" e))))))

(defun message-drain ()
  "Hide the message when its expiry passes. Called from the main loop."
  (when (and *message-state*
             (>= (get-universal-time) *message-expiry*))
    (message-hide)))

(defun message (format-string &rest args)
  "Show a formatted message on screen (like StumpWM's message)."
  (show-message (apply #'format nil format-string args)))
