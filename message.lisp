;;; Transient on-screen messages for Rhizome
;;;
;;; The StumpWM message-window equivalent: show text briefly on
;;; screen, auto-hide after a timeout. Expiry is drained by the WM's
;;; main poll loop — no threads, no timers.

(in-package #:rhizome)

(defparameter *message-state* nil
  "Surface state hash for the visible message.")
(defparameter *message-expiry* 0
  "Universal time when the current message hides.")
(defparameter *message-width* 700)
(defparameter *message-height* 22)
(defparameter *message-line-height* 20
  "Pixels per message line (16px glyphs + 4px gap).")
(defparameter *last-message* nil
  "The most recently shown message text (lastmsg, copy-last-message).")

(defun message-lines (text)
  "TEXT split into its display lines."
  (cl-ppcre:split "\\n" text))

(defun message-height-for (text)
  "Surface height needed to display TEXT (all of its lines)."
  (max 22 (+ 4 (* (length (message-lines text))
                  *message-line-height*))))

(defun show-message (text &optional (seconds 3))
  "Show TEXT on screen for SECONDS (default 3). If a message is
already showing, replace it. Newlines start new lines."
  (setf *last-message* text)
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Rhizome: ~A~%" text)
    (return-from show-message))
  (setf *message-expiry* (+ (get-universal-time) seconds))
  (let ((height (message-height-for text)))
    (if (and *message-state* (gethash :shm-ptr *message-state*))
        (if (= height *message-height*)
            (message-render text)
            ;; Different height (more/fewer lines): drop the buffer and
            ;; resize; the configure that follows re-creates the buffer
            ;; at the new size and renders :text
            (progn
              (setf *message-height* height
                    (gethash :text *message-state*) text)
              (bar-destroy-shm *message-state*)
              (rwm:zwlr-layer-surface-v1.set-size
                (gethash :layer-surface *message-state*)
                *message-width* height)
              (wl:wl-surface.commit (gethash :surface *message-state*))))
        (let ((state (make-hash-table)))
          (setf *message-state* state
                *message-height* height)
          ;; The render helpers expect the global wl_shm in the state hash
          (setf (gethash :shm state) *shm*)
          (let ((surface (wl:wl-compositor.create-surface *compositor*)))
            (setf (gethash :surface state) surface)
            (let ((layer-surface
                    (rwm:zwlr-layer-shell-v1.get-layer-surface
                      *layer-shell* surface nil :top "rhizome-message")))
              (setf (gethash :layer-surface state) layer-surface)
              (push (lambda (event-name &rest args)
                      (case event-name
                        (:configure
                         (destructuring-bind (serial width height) args
                           (message-configure-handler
                             state layer-surface serial width height)))
                        (:closed
                         (setf *message-state* nil))))
                    (wl:wl-proxy-hooks layer-surface))
              (rwm:zwlr-layer-surface-v1.set-anchor layer-surface nil)
              (rwm:zwlr-layer-surface-v1.set-size layer-surface
                *message-width* height)
              (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                layer-surface :none)
              (rwm:zwlr-layer-surface-v1.set-margin layer-surface 60 0 0 0)
              ;; Store the text for the first render (configure will
              ;; create the buffer)
              (setf (gethash :text state) text)
              (wl:wl-surface.commit surface)))))))

(defun message-render (text)
  "Render TEXT into the message surface, one line per newline."
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
              (loop for line in (message-lines text)
                    for i from 0
                    do (bar-draw-text ptr width height line
                                      8 (+ 2 (* i *message-line-height*))
                                      ar ag ab)))
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
          (format t "~&; Rhizome: error hiding message: ~A~%" e))))))

(defun message-drain ()
  "Hide the message when its expiry passes. Called from the main loop."
  (when (and *message-state*
             (>= (get-universal-time) *message-expiry*))
    (message-hide)))

(defun message (format-string &rest args)
  "Show a formatted message on screen (like StumpWM's message)."
  (show-message (apply #'format nil format-string args)))

(defun lastmsg ()
  "Redisplay the most recent message (StumpWM's C-t m)."
  (if *last-message*
      (show-message *last-message*)
      (message "no previous message")))

(defun time-message ()
  "Show the current date and time (StumpWM's C-t a)."
  (multiple-value-bind (sec min hour day month year dow)
      (decode-universal-time (get-universal-time))
    (declare (ignore sec year))
    (let ((days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (months #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                    "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
      (message "~A ~A ~2,'0D  ~2,'0D:~2,'0D"
               (aref days (mod dow 7))
               (aref months (1- month))
               day hour min))))
