;;; Live eval prompt for Stumpway
;;;
;;; Alt+; opens a small centered layer-shell prompt. Type a Lisp
;;; expression (full ASCII keyboard support via the shift-aware
;;; evdev-to-xkb), press Enter to evaluate it in the live WM image,
;;; Escape to cancel. This is the StumpWM-style eval: redefine layouts,
;;; change state, inspect anything — without restarting the compositor.

(in-package #:stumpway)

(defparameter *eval-prompt-active* nil
  "Whether the eval prompt is open.")
(defparameter *eval-prompt-query* ""
  "Text typed into the eval prompt.")
(defparameter *eval-prompt-state* nil
  "Surface state hash for the open prompt: surface, layer-surface,
shm-fd, shm-ptr, shm-size, wl-pool, wl-buffer, width, height.")
(defparameter *eval-prompt-result* nil
  "Most recent evaluation result, displayed in the prompt.")
(defparameter *eval-prompt-result-visible* nil
  "Whether the result line is currently shown.")

(defparameter *eval-prompt-width* 700
  "Width of the eval prompt in pixels.")
(defparameter *eval-prompt-height* 22
  "Height of the eval prompt in pixels.")

;;; --- Key handling ---

(defun eval-prompt-handle-key (keysym)
  "Handle a key press while the eval prompt is open. Returns T."
  (cond
    ;; Escape — cancel
    ((= keysym 65307)
     (hide-eval-prompt))
    ;; Enter — evaluate
    ((= keysym 65293)
     (eval-prompt-eval *eval-prompt-query*))
    ;; Backspace
    ((= keysym 65288)
     (when (plusp (length *eval-prompt-query*))
       (setf *eval-prompt-query*
             (subseq *eval-prompt-query* 0 (1- (length *eval-prompt-query*))))
       (setf *eval-prompt-result-visible* nil))
     (eval-prompt-redraw))
    ;; Printable ASCII — append to query
    ((and (>= keysym 32) (<= keysym 126))
     (setf *eval-prompt-query*
           (concatenate 'string *eval-prompt-query*
                        (string (code-char keysym)))
           *eval-prompt-result-visible* nil)
     (eval-prompt-redraw)))
  t)

;;; --- Evaluation ---

(defun eval-prompt-eval (text)
  "Evaluate TEXT in the live WM image and display the result.
Errors are caught and shown instead of killing the WM."
  (let ((expr (handler-case
                  (let ((*package* (find-package "STUMPWAY")))
                    (read-from-string text))
                (end-of-file ()
                  :incomplete)
                (error (e)
                  e))))
    (cond
      ((eql expr :incomplete)
       (setf *eval-prompt-result* "incomplete expression"
             *eval-prompt-result-visible* t))
      ((typep expr 'error)
       (setf *eval-prompt-result* (format nil "read error: ~A" expr)
             *eval-prompt-result-visible* t))
      (t
       (let ((results
               (handler-case
                   (multiple-value-list
                     (let ((*package* (find-package "STUMPWAY")))
                       (eval expr)))
                 (error (e)
                   (list (format nil "error: ~A" e))))))
         (setf *eval-prompt-result*
               (format nil "~{~A~^ ~}" (mapcar (lambda (r) (format nil "~A" r))
                                               results))
               *eval-prompt-result-visible* t)
         (format t "~&; Stumpway: eval ~S => ~A~%"
                 text *eval-prompt-result*))))
    ;; Keep the query so successive evals can be edited; redraw shows result
    (eval-prompt-redraw)))

;;; --- Rendering ---

(defun eval-prompt-redraw ()
  "Render the prompt into its shm buffer and commit."
  (let ((state *eval-prompt-state*))
    (when state
      (let ((ptr (gethash :shm-ptr state)))
        (when (and ptr (not (cffi:null-pointer-p ptr)))
          (let ((width (gethash :width state))
                (height (gethash :height state)))
            (multiple-value-bind (br bg bb)
                (parse-hex-color (config-getf '(:bar) :background
                                             :default "#1a1b26"))
              (bar-clear ptr width height br bg bb))
            (let ((y (floor (- height 16) 2)))
              (multiple-value-bind (ar ag ab)
                  (parse-hex-color (config-getf '(:bar) :accent
                                               :default "#7aa2f7"))
                (bar-draw-text ptr width height "Eval: " 8 y ar ag ab))
              (multiple-value-bind (fr fg fb)
                  (parse-hex-color (config-getf '(:bar) :foreground
                                               :default "#c0caf5"))
                (bar-draw-text ptr width height *eval-prompt-query*
                               (+ 8 40) y fr fg fb))
              (when *eval-prompt-result-visible*
                (multiple-value-bind (ar ag ab)
                    (parse-hex-color (config-getf '(:bar) :accent
                                                 :default "#7aa2f7"))
                  (bar-draw-text ptr width height *eval-prompt-result*
                                 (+ 8 48 (bar-text-width *eval-prompt-query*))
                                 y ar ag ab))))
            ;; Attach, damage, commit
            (wl:wl-surface.attach (gethash :surface state)
                                  (gethash :wl-buffer state) 0 0)
            (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
            (wl:wl-surface.commit (gethash :surface state))))))))

(defun eval-prompt-configure-handler (state layer-surface serial width height)
  "Acknowledge the layer surface configure."
  (setf (gethash :width state) *eval-prompt-width*
        (gethash :height state) *eval-prompt-height*)
  (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
  (bar-ensure-shm state)
  (eval-prompt-redraw))

;;; --- Show / hide ---

(defun show-eval-prompt ()
  "Open the live eval prompt (no-op if already open)."
  (when *eval-prompt-active*
    (return-from show-eval-prompt))
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Stumpway: cannot show eval prompt (missing globals)~%")
    (return-from show-eval-prompt))
  (setf *eval-prompt-query* ""
        *eval-prompt-result* nil
        *eval-prompt-result-visible* nil
        *eval-prompt-state* (make-hash-table))
  (let ((state *eval-prompt-state*))
    (let ((surface (wl:wl-compositor.create-surface *compositor*)))
      (setf (gethash :surface state) surface)
      (let ((layer-surface
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface nil :top "stumpway-eval")))
        (setf (gethash :layer-surface state) layer-surface)
        (push (lambda (event-name &rest args)
                (case event-name
                  (:configure
                   (destructuring-bind (serial width height) args
                     (eval-prompt-configure-handler
                       state layer-surface serial width height)))
                  (:closed
                   (setf *eval-prompt-active* nil))))
              (wl:wl-proxy-hooks layer-surface))
        (rwm:zwlr-layer-surface-v1.set-anchor layer-surface nil)
        (rwm:zwlr-layer-surface-v1.set-size layer-surface
          *eval-prompt-width* *eval-prompt-height*)
        (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
          layer-surface :exclusive)
        (rwm:zwlr-layer-surface-v1.set-margin layer-surface 60 0 0 0)
        (setf *eval-prompt-active* t)
        (wl:wl-surface.commit surface)))))

(defun hide-eval-prompt ()
  "Close the live eval prompt."
  (unless *eval-prompt-active*
    (return-from hide-eval-prompt))
  (setf *eval-prompt-active* nil)
  (let ((state *eval-prompt-state*))
    (handler-case
        (progn
          (when (gethash :layer-surface state)
            (rwm:zwlr-layer-surface-v1.destroy (gethash :layer-surface state)))
          (when (gethash :surface state)
            (wl:wl-surface.destroy (gethash :surface state))))
      (error (e)
        (format t "~&; Stumpway: error closing eval prompt: ~A~%" e)))
    (bar-destroy-shm state)
    (setf *eval-prompt-state* nil))
  (format t "~&; Stumpway: eval prompt closed~%"))
