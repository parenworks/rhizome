;;; Generalized input prompt for Stumpway
;;;
;;; An interactive layer-shell input bar with filtering and
;;; selection — the machinery behind windowlist, workspace selection
;;; and the command palette. Type to filter, Up/Down (or Tab) to
;;; select, Enter to complete, Escape to cancel.
;;;
;;;   (prompt-open "Window:" :candidates '("a" "b")
;;;                :on-complete (lambda (choice) ...))

(in-package #:stumpway)

(defparameter *prompt-active* nil)
(defparameter *prompt-title* "")
(defparameter *prompt-query* "")
(defparameter *prompt-candidates* nil
  "Full candidate list (strings).")
(defparameter *prompt-selection* 0
  "Index into the filtered candidates.")
(defparameter *prompt-on-complete* nil
  "Function called with the selected string on Enter.")
(defparameter *prompt-state* nil
  "Surface state hash: surface, layer-surface, shm-*, wl-buffer,
width, height.")

(defparameter *prompt-width* 700)
(defparameter *prompt-height* 200
  "Input line plus visible candidate rows; sized on open.")

;;; --- Filtering ---

(defparameter *prompt-expiry* 0
  "Universal time after which an unattended prompt auto-closes.")

(defun prompt-drain ()
  "Auto-close an unattended prompt after 30 seconds, so a keyboard-
exclusive surface can never trap input. Called from the main loop."
  (when (and *prompt-active*
             (plusp *prompt-expiry*)
             (>= (get-universal-time) *prompt-expiry*))
    (format t "~&; Stumpway: prompt timed out~%")
    (prompt-close)))

(defun prompt-filtered ()
  "Candidates matching the current query (fuzzy: characters in
order, case-insensitive; substring matches rank first)."
  (fuzzy-filter *prompt-query* *prompt-candidates*))

(defun prompt-selected ()
  "The currently selected candidate string, or NIL."
  (nth *prompt-selection* (prompt-filtered)))

;;; --- Key handling ---

(defun prompt-handle-key (keysym)
  "Handle a key press while the prompt is open. Returns T."
  (let ((filtered-count (length (prompt-filtered))))
    (cond
      ;; Escape — cancel
      ((= keysym 65307)
       (prompt-close))
      ;; Enter — complete with the selection
      ((= keysym 65293)
       ;; Capture the callback before closing: prompt-close clears it
       (let* ((choice (prompt-selected))
              (on-complete *prompt-on-complete*))
         (prompt-close)
         (when (and choice on-complete)
           (handler-case
               (funcall on-complete choice)
             (error (e)
               (format t "~&; Stumpway: prompt complete error: ~A~%" e))))))
      ;; Up / Down / Tab — cycle selection
      ((= keysym 65362)
       (when (plusp *prompt-selection*)
         (decf *prompt-selection*))
       (prompt-redraw))
      ((= keysym 65364)
       (when (< *prompt-selection* (1- filtered-count))
         (incf *prompt-selection*))
       (prompt-redraw))
      ((= keysym 65289)                    ; Tab: next, wrap around
       (setf *prompt-selection*
             (mod (1+ *prompt-selection*) (max 1 filtered-count)))
       (prompt-redraw))
      ;; Backspace
      ((= keysym 65288)
       (when (plusp (length *prompt-query*))
         (setf *prompt-query*
               (subseq *prompt-query* 0 (1- (length *prompt-query*)))))
       (setf *prompt-selection* 0)
       (prompt-redraw))
      ;; Printable ASCII — extend the query
      ((and (>= keysym 32) (<= keysym 126))
       (setf *prompt-query*
             (concatenate 'string *prompt-query*
                          (string (code-char keysym))))
       (setf *prompt-selection* 0)
       (prompt-redraw))))
  t)

;;; --- Rendering ---

(defun prompt-redraw ()
  "Render the prompt into its shm buffer and commit."
  (let ((state *prompt-state*))
    (when state
      (let ((ptr (gethash :shm-ptr state)))
        (when (and ptr (not (cffi:null-pointer-p ptr)))
          (let ((width (gethash :width state))
                (height (gethash :height state)))
            (multiple-value-bind (br bg bb)
                (parse-hex-color (config-getf '(:bar) :background
                                             :default "#1a1b26"))
              (bar-clear ptr width height br bg bb))
            (let ((y (floor (- height 16) 2))
                  (row-height 16)
                  (row-count 0))
              ;; Title and query on the input line
              (multiple-value-bind (ar ag ab)
                  (parse-hex-color (config-getf '(:bar) :accent
                                               :default "#7aa2f7"))
                (bar-draw-text ptr width height *prompt-title*
                               8 2 ar ag ab))
              (multiple-value-bind (fr fg fb)
                  (parse-hex-color (config-getf '(:bar) :foreground
                                               :default "#c0caf5"))
                (bar-draw-text ptr width height *prompt-query*
                               (+ 8 (bar-text-width *prompt-title*)) 2
                               fr fg fb)
                ;; Candidate rows below the input line
                (dolist (candidate (prompt-filtered))
                  (when (>= (+ row-height 8) (- height (* row-height
                                                          row-count)))
                    (return))
                  (let ((row-y (* row-height row-count)))
                    (when (= row-count *prompt-selection*)
                      ;; Highlight the selected row
                      (multiple-value-bind (ar ag ab)
                          (parse-hex-color (config-getf '(:bar) :accent
                                                       :default "#7aa2f7"))
                        (loop for dx below (- width 16)
                              do (bar-set-pixel ptr width (+ 8 dx)
                                                (+ row-height row-y)
                                                ar ag ab))))
                    (bar-draw-text ptr width height candidate
                                   8 (+ row-height row-y)
                                   fr fg fb)
                    (incf row-count)))))
            ;; Attach, damage, commit
            (wl:wl-surface.attach (gethash :surface state)
                                  (gethash :wl-buffer state) 0 0)
            (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
            (wl:wl-surface.commit (gethash :surface state))))))))

(defun prompt-configure-handler (state layer-surface serial width height)
  "Acknowledge the layer surface configure."
  (declare (ignore width height))
  (setf (gethash :width state) *prompt-width*
        (gethash :height state) *prompt-height*)
  (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
  (bar-ensure-shm state)
  (prompt-redraw))

;;; --- Open / close ---

(defun prompt-open (title &key candidates on-complete (initial ""))
  "Open an interactive prompt. CANDIDATES is a list of strings,
ON-COMPLETE is called with the selected string on Enter."
  (when *prompt-active*
    (prompt-close))
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Stumpway: cannot show prompt (missing globals)~%")
    (return-from prompt-open))
  (setf *prompt-title* title
        *prompt-query* initial
        *prompt-candidates* candidates
        *prompt-selection* 0
        *prompt-on-complete* on-complete
        *prompt-state* (make-hash-table)
        *prompt-expiry* (+ (get-universal-time) 30))
  (let ((state *prompt-state*))
    ;; The render helpers expect the global wl_shm in the state hash
    (setf (gethash :shm state) *shm*)
    (let ((surface (wl:wl-compositor.create-surface *compositor*)))
      (setf (gethash :surface state) surface)
      (let ((layer-surface
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface nil :top "stumpway-prompt")))
        (setf (gethash :layer-surface state) layer-surface)
        (push (lambda (event-name &rest args)
                (case event-name
                  (:configure
                   (destructuring-bind (serial width height) args
                     (prompt-configure-handler
                       state layer-surface serial width height)))
                  (:closed
                   (setf *prompt-active* nil))))
              (wl:wl-proxy-hooks layer-surface))
        (rwm:zwlr-layer-surface-v1.set-anchor layer-surface nil)
        (rwm:zwlr-layer-surface-v1.set-size layer-surface
          *prompt-width* *prompt-height*)
        (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
          layer-surface :exclusive)
        (rwm:zwlr-layer-surface-v1.set-margin layer-surface 60 0 0 0)
        (setf *prompt-active* t)
        (wl:wl-surface.commit surface)))))

(defun prompt-close ()
  "Close the interactive prompt."
  (unless *prompt-active*
    (return-from prompt-close))
  (setf *prompt-active* nil
        *prompt-expiry* 0)
  (let ((state *prompt-state*))
    (handler-case
        (progn
          (when (gethash :layer-surface state)
            (rwm:zwlr-layer-surface-v1.destroy (gethash :layer-surface state)))
          (when (gethash :surface state)
            (wl:wl-surface.destroy (gethash :surface state))))
      (error (e)
        (format t "~&; Stumpway: error closing prompt: ~A~%" e)))
    (bar-destroy-shm state)
    (setf *prompt-state* nil
          *prompt-on-complete* nil)))
