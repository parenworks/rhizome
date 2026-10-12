;;; Stumpway bar - a configurable status bar / mode-line
;;;
;;; A mode-line in the StumpWM tradition, rendered directly from the
;;; window manager's own state. The bar runs on its own Wayland
;;; connection in its own thread so its tick loop never blocks the WM
;;; event loop; it is a zwlr layer-shell surface with an exclusive zone
;;; so river reserves space for it and tiled windows avoid it.
;;;
;;; Configuration lives in *config* (see config.lisp). Modules are
;;; functions returning strings; the :left/:center/:right module lists
;;; can be changed live from the REPL.

(in-package #:stumpway)

;;; --- State ---

(defparameter *bar-thread* nil
  "The bar's tick thread.")
(defparameter *bar-running* nil
  "Whether the bar thread should keep running.")
(defparameter *bar-state* nil
  "Connection/surface state plist for the running bar:
:display :compositor :shm :layer-shell :output :surface
:layer-surface :shm-fd :shm-ptr :shm-size :wl-pool :wl-buffer
:width :height :mapped")

(defvar *bar-cache* (make-hash-table)
  "Maps module names to (cons timestamp string) for rate-limited modules.")

;;; --- WM snapshot ---
;;;
;;; The bar runs as its own process, so it reads the WM's state
;;; snapshot from the file written by update-wm-snapshot in
;;; wm-protocol.lisp.

(defvar *wm-snapshot* '(:layout nil :windows 0 :workspace 1
                        :workspace-count 9 :focused nil)
  "Fallback snapshot, used until the WM writes a real one.")

(defun bar-snapshot ()
  "The WM's state snapshot (workspace, windows, layout, focus),
read from the snapshot file. Refreshed at most once per tick."
  (bar-cached :snapshot 0
              (lambda ()
                (or (handler-case
                      (with-open-file (s (snapshot-pathname))
                        (read s))
                      (error () nil))
                    *wm-snapshot*))))

;;; --- Pointer state (module click/scroll support) ---

(defparameter *bar-pointer-x* nil
  "Pointer x position on the bar surface, NIL when outside.")
(defparameter *bar-axis-accumulator* 0
  "Accumulated vertical scroll axis values since the last axis-stop.")
(defparameter *bar-hitboxes* nil
  "Module click regions: list of (module-name start-x end-x),
recorded during rendering.")

;;; --- Module helpers ---

(defun bar-cached (module interval thunk)
  "Return the cached value for MODULE, refreshing via THUNK at most
once per INTERVAL seconds."
  (let* ((entry (gethash module *bar-cache*))
         (now (get-universal-time)))
    (when (or (null entry) (>= (- now (car entry)) interval))
      (setf entry (cons now
                        (handler-case
                            (or (funcall thunk) "")
                          (error () ""))))
      (setf (gethash module *bar-cache*) entry))
    (cdr entry)))

(defun bar-read-file (path)
  "Return the first line of a file, trimmed, or NIL."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (let ((line (read-line s nil nil)))
        (when line (string-trim " " line))))))

(defun bar-join (strings)
  "Join non-empty strings with a double space separator."
  (let ((parts (remove "" strings :test #'string=)))
    (with-output-to-string (out)
      (loop for part in parts
            for first = t then nil
            unless first do (princ "  " out)
            do (princ part out)))))

;;; --- Modules ---
;;;
;;; Each module is a function of no arguments returning either a
;;; string, or a list of (text . color) parts where color is a hex
;;; string ("#7aa2f7") or NIL for the segment's base color.

(defun bar-module-layout ()
  (let ((layout (getf (bar-snapshot) :layout)))
    (when layout (string-downcase (symbol-name layout)))))

(defun bar-module-windows ()
  (let ((count (getf (bar-snapshot) :windows)))
    (when (plusp count)
      (format nil "~D win~:P" count))))

(defun bar-module-focused ()
  (getf (bar-snapshot) :focused))

(defun bar-module-workspaces ()
  "Workspace numbers, with the current one in the accent color."
  (let* ((snapshot (bar-snapshot))
         (current (getf snapshot :workspace 1))
         (count (getf snapshot :workspace-count 9))
         (accent (config-getf '(:bar) :accent :default "#7aa2f7")))
    (loop for i from 1 to count
          collect (cons (format nil "~A" i)
                        (if (= i current) accent nil)))))

(defun bar-module-battery ()
  "Battery percentage from /sys/class/power_supply, e.g. \"75%\"."
  (bar-cached :battery
              (config-getf '(:modules :battery) :interval :default 5)
              (lambda ()
                (let ((cap (bar-read-file "/sys/class/power_supply/BAT1/capacity"))
                      (status (bar-read-file "/sys/class/power_supply/BAT1/status")))
                  (when cap
                    (cond
                      ((string-equal status "Charging")
                       (concatenate 'string cap "%+"))
                      ((string-equal status "Discharging")
                       (concatenate 'string cap "%"))
                      (t (concatenate 'string cap "%"))))))))

(defun bar-module-volume ()
  "Volume percentage via wpctl, e.g. \"VOL 45%\" or \"MUTED\"."
  (bar-cached :volume
              (config-getf '(:modules :volume) :interval :default 5)
              (lambda ()
                (let* ((output (uiop:run-program
                                 '("wpctl" "get-volume" "@DEFAULT_AUDIO_SINK@")
                                 :output :string :ignore-error-status t))
                       (line (with-input-from-string (s output)
                               (read-line s nil nil))))
                  (cond
                    ((and line (search "MUTED" line)) "MUTED")
                    ((and line (search "Volume:" line))
                     (let* ((num (string-trim " " (subseq line (1+ (search ":" line)))))
                            (vol (read-from-string num)))
                       (format nil "VOL ~A%" (round (* 100 vol)))))
                    (t ""))))))

(defun bar-module-network ()
  "Active network connection name via nmcli, e.g. \"WIFI MyWifi\"."
  (bar-cached :network
              (config-getf '(:modules :network) :interval :default 10)
              (lambda ()
                (let ((output (uiop:run-program
                                '("nmcli" "-t" "-f" "NAME,TYPE"
                                  "connection" "show" "--active")
                                :output :string :ignore-error-status t)))
                  (with-input-from-string (s output)
                    (let ((line (read-line s nil nil)))
                      (when (and line (plusp (length line)))
                        (let* ((colon (position #\: line))
                               (name (if colon (subseq line 0 colon) line))
                               (type (if colon (subseq line (1+ colon)) "")))
                          (cond
                            ((search "wireless" type)
                             (concatenate 'string "WIFI " name))
                            ((search "ethernet" type)
                             "ETH")
                            (t name))))))))))

(defun bar-module-clock ()
  "Clock formatted per the config's :format (a small strftime subset:
%H %M %S %d %m %y %Y %a %A %b %B)."
  (bar-format-clock
    (config-getf '(:modules :clock) :format :default "%a %d %b %H:%M")))

(defun bar-format-clock (format)
  "Format the current time according to FORMAT, supporting a small
strftime subset: %H %M %S %d %m %y %Y %a %A %b %B."
  (multiple-value-bind (second minute hour date month year day-of-week)
      (get-decoded-time)
    (let ((short-days #("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
          (long-days #("Sunday" "Monday" "Tuesday" "Wednesday"
                       "Thursday" "Friday" "Saturday"))
          (short-months #("Jan" "Feb" "Mar" "Apr" "May" "Jun"
                           "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))
          (long-months #("January" "February" "March" "April" "May" "June"
                         "July" "August" "September" "October" "November" "December")))
      (with-output-to-string (out)
        (loop with i = 0
              while (< i (length format))
              for char = (char format i)
              do (cond
                   ((and (char= char #\%) (< (1+ i) (length format)))
                    (let ((code (char format (1+ i))))
                      (princ (case code
                               (#\H (format nil "~2,'0D" hour))
                               (#\M (format nil "~2,'0D" minute))
                               (#\S (format nil "~2,'0D" second))
                               (#\d (format nil "~2,'0D" date))
                               (#\m (format nil "~2,'0D" month))
                               (#\y (format nil "~2,'0D" (mod year 100)))
                               (#\Y (format nil "~D" year))
                               (#\a (aref short-days day-of-week))
                               (#\A (aref long-days day-of-week))
                               (#\b (aref short-months (1- month)))
                               (#\B (aref long-months (1- month)))
                               (t (string code)))
                             out))
                    (incf i 2))
                   (t (princ char out) (incf i 1))))))))

(defparameter *bar-modules*
  '((:workspaces . bar-module-workspaces)
    (:layout . bar-module-layout)
    (:windows . bar-module-windows)
    (:focused . bar-module-focused)
    (:clock . bar-module-clock)
    (:battery . bar-module-battery)
    (:volume . bar-module-volume)
    (:network . bar-module-network))
  "Maps configured module names to their rendering functions.")

(defparameter *bar-module-gap* 16
  "Pixels of empty space between modules.")

(defun bar-module-parts (module)
  "Render one module to a list of (text . color) parts (color NIL =
the segment's base color). Unknown or failing modules render empty."
  (let ((fn (cdr (assoc module *bar-modules*))))
    (when fn
      (handler-case
          (let ((out (funcall fn)))
            (typecase out
              (string (list (cons out nil)))
              (list out)
              (t nil)))
        (error () nil)))))

(defun bar-segment-modules (position)
  "The configured module list for :left, :center or :right."
  (config-getf '(:bar :modules) position :default nil))

(defun bar-segment-width (modules)
  "Total pixel width of a module list, including inter-module gaps."
  (+ (reduce #'+ (mapcar (lambda (module)
                          (* 8 (reduce #'+ (mapcar (lambda (part)
                                                     (length (first part)))
                                                   (bar-module-parts module)))))
                        modules)
             :initial-value 0)
     (* *bar-module-gap* (length modules))))

;;; --- Rendering ---

(defun bar-set-pixel (ptr width px py r g b)
  "Set pixel at (PX, PY) to RGB color (BGRA layout in the buffer)."
  (let ((offset (* (+ (* py width) px) 4)))
    (setf (cffi:mem-aref ptr :unsigned-char offset) b
          (cffi:mem-aref ptr :unsigned-char (1+ offset)) g
          (cffi:mem-aref ptr :unsigned-char (+ offset 2)) r
          (cffi:mem-aref ptr :unsigned-char (+ offset 3)) 255)))

(defun bar-clear (ptr width height r g b)
  "Fill the bar buffer with a solid color."
  (let ((pixel (logior (ash 255 24) (ash r 16) (ash g 8) b)))
    (loop for i below (* width height) do
      (setf (cffi:mem-aref ptr :unsigned-int i) pixel))))

(defun bar-draw-text (ptr width height text x y r g b)
  "Draw TEXT at (X, Y) using the 8x16 bitmap font, in RGB color."
  (unless (gethash 32 *font-8x16*)
    (init-font))
  (loop for char across text
        for i from 0
        for gx = (+ x (* i 8))
        while (< (+ gx 8) width) do
    (let* ((code (char-code char))
           (glyph (or (gethash code *font-8x16*)
                      (gethash 63 *font-8x16*))))
      (loop for row from 0 below 16
            for py = (+ y row)
            while (< py height) do
        (let ((bits (aref glyph row)))
          (loop for col from 0 below 8
                for px = (+ gx col)
                while (< px width) do
            (when (logbitp (- 7 col) bits)
              (bar-set-pixel ptr width px py r g b))))))))

(defun bar-text-width (text)
  (* 8 (length text)))

(defun bar-draw-module (ptr width height x y parts base-r base-g base-b module)
  "Draw one module's PARTS starting at X, using the base color for
parts without an explicit color. Records a click hitbox for MODULE.
Returns the x after the module plus gap."
  (let ((start x))
    (dolist (part parts)
      (let* ((text (first part))
             (color (rest part))
             (r base-r) (g base-g) (b base-b))
        (when color
          (setf (values r g b) (parse-hex-color color)))
        (bar-draw-text ptr width height text x y r g b)
        (incf x (bar-text-width text))))
    (let ((end x))
      (when (and module (> end start))
        (push (list module start end) *bar-hitboxes*)))
    (+ x *bar-module-gap*)))

(defun bar-render (state)
  "Render the bar content into the shm buffer and commit the surface."
  (let* ((ptr (gethash :shm-ptr state))
         (width (gethash :width state))
         (height (gethash :height state))
         (padding (config-getf '(:bar) :padding :default 6)))
    (when (or (null ptr) (cffi:null-pointer-p ptr))
      (return-from bar-render))
    (multiple-value-bind (br bg bb) (parse-hex-color
                                       (config-getf '(:bar) :background
                                                    :default "#1a1b26"))
      (multiple-value-bind (fr fg fb) (parse-hex-color
                                        (config-getf '(:bar) :foreground
                                                     :default "#c0caf5"))
        (bar-clear ptr width height br bg bb)
        (let ((y (floor (- height 16) 2)))
          (setf *bar-hitboxes* nil)
          ;; Left segment
          (let ((x padding))
            (dolist (module (bar-segment-modules :left))
              (setf x (bar-draw-module ptr width height x y
                                        (bar-module-parts module)
                                        fr fg fb module))))
          ;; Center segment
          (let* ((modules (bar-segment-modules :center))
                 (total (bar-segment-width modules))
                 (x (floor (- width total) 2)))
            (dolist (module modules)
              (setf x (bar-draw-module ptr width height x y
                                       (bar-module-parts module)
                                       fr fg fb module))))
          ;; Right segment
          (let* ((modules (bar-segment-modules :right))
                 (total (bar-segment-width modules))
                 (x (- width padding total)))
            (dolist (module modules)
              (setf x (bar-draw-module ptr width height x y
                                       (bar-module-parts module)
                                       fr fg fb module)))))))
    ;; Attach, damage, commit
    (wl:wl-surface.attach (gethash :surface state) (gethash :wl-buffer state) 0 0)
    (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
    (wl:wl-surface.commit (gethash :surface state))))

;;; --- Shm buffer management ---

(defun bar-destroy-shm (state)
  "Release the bar's shared memory resources."
  (when (gethash :wl-buffer state)
    (wl:wl-buffer.destroy (gethash :wl-buffer state)))
  (when (gethash :wl-pool state)
    (wl:wl-shm-pool.destroy (gethash :wl-pool state)))
  (when (and (gethash :shm-ptr state)
             (not (cffi:null-pointer-p (gethash :shm-ptr state))))
    (cffi:foreign-funcall "munmap"
      :pointer (gethash :shm-ptr state)
      :unsigned-long (gethash :shm-size state)
      :int)
    (cffi:foreign-funcall "close" :int (gethash :shm-fd state) :int))
  (setf (gethash :wl-buffer state) nil
        (gethash :wl-pool state) nil
        (gethash :shm-ptr state) nil
        (gethash :shm-fd state) nil
        (gethash :shm-size state) 0))

(defun bar-ensure-shm (state)
  "Create the shm buffer for the bar's current dimensions."
  (let* ((width (gethash :width state))
         (height (gethash :height state))
         (size (* width height 4))
         (filename (format nil "stumpway-bar-~A-~A"
                           (get-universal-time) (random 100000)))
         (path (concatenate 'string "/dev/shm/" filename))
         (fd (cffi:foreign-funcall "open"
               :string path
               :int (logior 2 64)             ; O_RDWR | O_CREAT
               :int 384                        ; 0600
               :int)))
    (when (< fd 0)
      (format t "~&; Stumpway: bar: failed to create shm file~%")
      (return-from bar-ensure-shm nil))
    (cffi:foreign-funcall "unlink" :string path :int)
    (cffi:foreign-funcall "ftruncate" :int fd :long size :int)
    (let ((ptr (cffi:foreign-funcall "mmap"
                  :pointer (cffi:null-pointer)
                  :unsigned-long size
                  :int 3                       ; PROT_READ | PROT_WRITE
                  :int 1                       ; MAP_SHARED
                  :int fd
                  :long 0
                  :pointer)))
      (when (cffi:null-pointer-p ptr)
        (cffi:foreign-funcall "close" :int fd :int)
        (return-from bar-ensure-shm nil))
      (setf (gethash :shm-fd state) fd
            (gethash :shm-ptr state) ptr
            (gethash :shm-size state) size)
      (setf (gethash :wl-pool state)
            (wl:wl-shm.create-pool (gethash :shm state) fd size))
      (setf (gethash :wl-buffer state)
            (wl:wl-shm-pool.create-buffer
              (gethash :wl-pool state) 0 width height (* width 4) :xrgb8888))
      t)))

;;; --- Connection lifecycle ---

(defun bar-bind-global (state registry name interface version)
  "Bind a registry global for the bar."
  (let ((iface-class (wl:find-interface-named interface)))
    (when iface-class
      (cond
        ((string= interface "wl_compositor")
         (setf (gethash :compositor state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((string= interface "wl_shm")
         (setf (gethash :shm state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((string= interface "zwlr_layer_shell_v1")
         (setf (gethash :layer-shell state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((and (string= interface "wl_output")
              (null (gethash :output state)))
         (setf (gethash :output state)
               (wl:wl-registry.bind registry name iface-class version)))
        ((and (string= interface "wl_seat")
              (null (gethash :seat state)))
         (setf (gethash :seat state)
               (wl:wl-registry.bind registry name iface-class
                                    (min version 5))))))))

(defun bar-layer-surface-handler (state layer-surface event-name args)
  "Handle events on the bar's layer surface."
  (case event-name
    (:configure
     (destructuring-bind (serial width height) args
       (let ((w (if (plusp width) width 0))
             (h (config-getf '(:bar) :height :default 20)))
         (setf (gethash :width state) w
               (gethash :height state) h)
         (rwm:zwlr-layer-surface-v1.ack-configure layer-surface serial)
         (when (plusp w)
           (bar-ensure-shm state)
           (bar-render state)
           (setf (gethash :mapped state) t)))))
    (:closed
     (setf *bar-running* nil))))

;;; --- Change detection ---
;;;
;;; The bar polls for state changes (WM snapshot file, clock, cached
;;; module values) at the configured :interval and only re-renders the
;;; buffer when something actually changed, so a fast poll is cheap.

(defparameter *bar-last-content* nil
  "Printed representation of the last rendered content.")

(defun bar-content-changed-p ()
  "Whether any configured module's rendered parts changed since the
last commit. Re-renders are skipped when nothing changed."
  (let* ((modules (append (bar-segment-modules :left)
                           (bar-segment-modules :center)
                           (bar-segment-modules :right)))
         (sig (with-output-to-string (s)
                (dolist (module modules)
                  (princ module s)
                  (princ " = " s)
                  (prin1 (bar-module-parts module) s)
                  (terpri s)))))
    (unless (equal sig *bar-last-content*)
      (setf *bar-last-content* sig)
      t)))

;;; --- Pointer handling (clickable modules) ---

(defun bar-module-at (x)
  "The module whose rendered area contains X, or NIL."
  (let ((hit (find-if (lambda (hb)
                        (and (<= (second hb) x)
                             (< x (third hb))))
                      *bar-hitboxes*)))
    (when hit (first hit))))

(defun bar-run-command (command)
  "Run a shell COMMAND asynchronously."
  (handler-case
      (uiop:launch-program (list "bash" "-c" command))
    (error (e)
      (format t "~&; Stumpway: bar: command failed: ~A~%" e))))

(defun bar-module-action (module key)
  "The configured shell command for MODULE's action KEY
(:on-click, :on-scroll-up, :on-scroll-down), if any."
  (config-getf (list :modules module) key :default nil))

(defun bar-handle-click (x)
  "Handle a left click at X: run the module's :on-click command."
  (let ((module (bar-module-at x)))
    (when module
      (format t "~&; Stumpway: bar: click on ~A~%" module)
      (let ((command (bar-module-action module :on-click)))
        (when command
          (bar-run-command command))))))

(defun bar-handle-scroll (amount)
  "Handle a vertical scroll gesture over a module."
  (let* ((module (and *bar-pointer-x* (bar-module-at *bar-pointer-x*)))
         (key (if (plusp amount) :on-scroll-up :on-scroll-down))
         (command (and module (bar-module-action module key))))
    (when command
      (bar-run-command command))))

(defun bar-pointer-event (event-name args)
  "Handle wl_pointer events on the bar surface."
  (handler-case
      (case event-name
        (:enter
         (destructuring-bind (surface x y) args
           (declare (ignore surface y))
           (setf *bar-pointer-x* x)))
        (:leave
         (destructuring-bind (surface) args
           (declare (ignore surface))
           (setf *bar-pointer-x* nil)))
        (:motion
         (destructuring-bind (time x y) args
           (declare (ignore time y))
           (setf *bar-pointer-x* x)))
        (:button
         (destructuring-bind (serial time button state) args
           (declare (ignore serial time))
           ;; 272 = BTN_LEFT
           (when (and (= button 272) (eq state :pressed))
             (let ((x *bar-pointer-x*))
               (when x (bar-handle-click x))))))
        (:axis
         (destructuring-bind (time axis value) args
           (declare (ignore time))
           (when (eq axis :vertical-scroll)
             (incf *bar-axis-accumulator* value))))
        (:axis-stop
         ;; Gesture ended: act on the accumulated scroll if it was
         ;; significant, then reset.
         (let ((amount *bar-axis-accumulator*))
           (setf *bar-axis-accumulator* 0)
           (when (>= (abs amount) 10)
             (bar-handle-scroll amount))))
        (t nil))
    (error (e)
      (declare (ignore e))
      nil)))

(defun bar-setup-pointer (state)
  "Set up a wl_pointer on the bar's seat for clickable modules."
  (let ((seat (gethash :seat state)))
    (when seat
      (push (lambda (event-name &rest args)
              (when (eq event-name :capabilities)
                (destructuring-bind (caps) args
                  (when (and (member :pointer caps)
                             (null (gethash :pointer state)))
                    (let ((pointer (wl:wl-seat.get-pointer seat)))
                      (setf (gethash :pointer state) pointer)
                      (push (lambda (event-name &rest args)
                              (bar-pointer-event event-name args))
                            (wl:wl-proxy-hooks pointer)))))))
            (wl:wl-proxy-hooks seat)))))

(defun bar-run-loop (state)
  "Open the bar's Wayland connection and drive its tick loop."
  (flet ((safe-roundtrip (display)
           ;; Wayflan raises on events it cannot decode (e.g. wl_shm
           ;; advertises non-standard fourcc formats); skip and carry on.
           (handler-case
               (wl:wl-display-roundtrip display)
             (error (e)
               (format t "~&; Stumpway: bar: ignored event error: ~A~%" e))))
         (safe-dispatch (display)
           (handler-case
               (wl:wl-display-dispatch-event display)
             (error (e)
               (format t "~&; Stumpway: bar: ignored event error: ~A~%" e)))))
    (handler-case
        (wl:with-open-display (display)
          (setf (gethash :display state) display)
          (setf (gethash :height state) (config-getf '(:bar) :height :default 20))
          (let ((registry (wl:wl-display.get-registry display)))
            (push (lambda (event-name &rest args)
                    (when (eq event-name :global)
                      (destructuring-bind (name interface version) args
                        (bar-bind-global state registry name interface version))))
                  (wl:wl-proxy-hooks registry))
            (safe-roundtrip display)
            (unless (and (gethash :compositor state)
                         (gethash :shm state)
                         (gethash :layer-shell state))
              (format t "~&; Stumpway: bar: missing compositor/shm/layer-shell globals~%")
              (return-from bar-run-loop))
            (let ((surface (wl:wl-compositor.create-surface
                            (gethash :compositor state))))
              (setf (gethash :surface state) surface)
              (let ((layer-surface
                      (rwm:zwlr-layer-shell-v1.get-layer-surface
                        (gethash :layer-shell state) surface
                        (gethash :output state) :top "stumpway-bar")))
                (setf (gethash :layer-surface state) layer-surface)
                (push (lambda (event-name &rest args)
                        (bar-layer-surface-handler state layer-surface
                                                   event-name args))
                      (wl:wl-proxy-hooks layer-surface))
                (rwm:zwlr-layer-surface-v1.set-anchor
                  layer-surface '(:top :left :right))
                (rwm:zwlr-layer-surface-v1.set-exclusive-zone
                  layer-surface (config-getf '(:bar) :height :default 20))
                (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
                  layer-surface :none)
                (rwm:zwlr-layer-surface-v1.set-size layer-surface
                  0 (config-getf '(:bar) :height :default 20))
                (wl:wl-surface.commit surface)
                ;; Pointer input for clickable modules
                (bar-setup-pointer state)
                (loop for i below 100
                      until (or (gethash :mapped state) (not *bar-running*))
                      do (safe-roundtrip display)
                         (sleep 0.05))
                (loop while *bar-running*
                      do (when (and (gethash :mapped state)
                                    (bar-content-changed-p))
                           (bar-render state))
                         (loop while (and *bar-running*
                                          (wl:wl-display-listen display))
                               do (safe-dispatch display))
                         (sleep (config-getf '(:bar) :interval :default 0.15)))
                (bar-destroy-shm state)))))
      (error (e)
        (format t "~&; Stumpway: bar thread error: ~A~%" e)))))


(defun run-bar ()
  "Entry point for the standalone bar process. Loads the user config,
then runs the bar's connection loop on this process's main thread."
  (setf *bar-running* t)
  (load-user-config)
  (let ((state (make-hash-table)))
    (setf *bar-state* state)
    (bar-run-loop state)
    (setf *bar-state* nil)))

;;; --- Process lifecycle (the bar runs as its own process) ---

(defparameter *bar-process* nil
  "The running bar process, a uiop:process object.")

(defun bar-alive-p ()
  "Whether the bar process is running."
  (and *bar-process*
       (uiop:process-alive-p *bar-process*)))

(defun bar-log-path ()
  "Pathname of the bar process's log file."
  (merge-pathnames #P".cache/stumpway/bar.log"
                   (user-homedir-pathname)))

(defun start-bar ()
  "Start the bar as its own process (no-op if already running)."
  (when (bar-alive-p)
    (return-from start-bar))
  (ensure-directories-exist (bar-log-path))
  (with-open-file (s (bar-log-path) :direction :output
                     :if-exists :supersede))
  (setf *bar-process*
        (uiop:launch-program
          (list "sbcl"
                "--eval" "(ql:quickload :stumpway)"
                "--eval" "(stumpway:run-bar)")
          :output (bar-log-path)
          :error-output :output))
  (format t "~&; Stumpway: bar started~%"))

(defun stop-bar ()
  "Stop the bar process."
  (when (and *bar-process* (uiop:process-alive-p *bar-process*))
    (handler-case
        (uiop:terminate-process *bar-process*)
      (error (e)
        (format t "~&; Stumpway: error stopping bar: ~A~%" e))))
  (when *bar-process*
    (ignore-errors (uiop:wait-process *bar-process*)))
  (setf *bar-process* nil)
  (format t "~&; Stumpway: bar stopped~%"))

(defun bar-reserved-height ()
  "Vertical pixels reserved for the bar (0 when disabled/not running).
River reports full output dimensions to the WM even when the bar's
layer surface has an exclusive zone, so the WM reserves the space
itself in its layout math."
  (if (and (config-getf '(:bar) :enabled :default t)
           (bar-alive-p))
      (config-getf '(:bar) :height :default 20)
      0))

(defun toggle-bar ()
  "Toggle the status bar on/off."
  (if (bar-alive-p)
      (stop-bar)
      (start-bar))
  ;; Re-tile windows to use (or free) the bar's space
  (when *wm-proxy*
    (request-manage-dirty)))

(defun bar-apply-config ()
  "Apply the current config to the bar: start, stop or restart as needed."
  (cond
    ((not (config-getf '(:bar) :enabled :default t))
     (when (bar-alive-p) (stop-bar)))
    ((bar-alive-p)
     ;; Restart to pick up structural changes (height, modules, colors
     ;; are picked up by the new process from the config file).
     (stop-bar)
     (start-bar))
    (t
     (start-bar))))
