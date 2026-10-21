;;; Rhizome bar - a configurable status bar / mode-line
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

(in-package #:rhizome)

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
                        :workspace-count 9 :occupied nil :focused nil)
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
(defparameter *tray-render-stamp* 0
  "The *tray-changed* value at the last render; a mismatch forces a
bar re-render even when text modules are unchanged.")
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

(defun bar-module-tray ()
  "The tray module's text part: none (icons only)."
  nil)

(defun tray-item-icon (item)
  "An item's drawable icon: (w h data) from IconPixmap (ARGB32
words), or the theme icon resolved through load-icon from
IconName. NIL when the item has nothing drawable."
  (or (getf item :icon)
      (and (getf item :icon-name)
           (load-icon (getf item :icon-name)))
      (and (getf item :icon-name)
           (load-icon (pathname-name (getf item :icon-name))))))

(defun bar-draw-tray (ptr width height x y base-r base-g base-b)
  "Draw every tray item's icon at (X, Y); one hitbox per item so
clicks/scrolls route to tray-click / tray-scroll. Returns the x
after the module plus gap. Structure mirrors bar-draw-icons."
  (dolist (item *tray-items*)
    (let* ((icon (tray-item-icon item))
           (start x))
      (when icon
        (destructuring-bind (iw ih idata) icon
          (loop for iy below ih
                for py = (+ y iy)
                while (< py height)
                do (loop for ix below *bar-icon-size*
                         for px = (+ x ix)
                         while (< px width)
                         do (let* ((idx (* (+ (* iy *bar-icon-size*) ix) 4))
                                   (a (aref idata (+ idx 3)))
                                   (r (aref idata idx))
                                   (g (aref idata (+ idx 1)))
                                   (b (aref idata (+ idx 2))))
                              (when (plusp a)
                                (bar-set-pixel
                                  ptr width px py
                                  (floor (/ (+ (* a r) (* (- 255 a) base-r)) 255))
                                  (floor (/ (+ (* a g) (* (- 255 a) base-g)) 255))
                                  (floor (/ (+ (* a b) (* (- 255 a) base-b))
                                            255)))))))
        (incf x *bar-icon-size*)
        (push (list item start x) *bar-hitboxes*)))
    (incf x 4))
  (+ x *bar-module-gap*)))

(defun bar-module-layout ()
  (let ((layout (getf (bar-snapshot) :layout)))
    (when layout (string-downcase (symbol-name layout)))))

(defun bar-module-windows ()
  (let ((count (getf (bar-snapshot) :windows)))
    (when (plusp count)
      (format nil "~D win~:P" count))))

(defun bar-module-focused ()
  (getf (bar-snapshot) :focused))

(defun bar-module-recording ()
  "A REC marker while any screen capture session is active
(screencasts, portals; v5 capture_sessions events via the
snapshot's :recording flag). Add :recording to a bar segment to
use it."
  (if (getf (bar-snapshot) :recording)
      (let ((color (config-getf '(:bar) :recording-color
                                :default "#f7768e")))
        (list (list* "REC " color)))
      ""))

(defun bar-module-workspaces ()
  "Workspace numbers with breathing room: the active workspace in
the accent color, underlined; occupied workspaces in the
foreground color; empty ones muted. Occupancy comes from the WM
snapshot's :occupied list (workspace numbers with visible
windows)."
  (let* ((snapshot (bar-snapshot))
         (current (getf snapshot :workspace 1))
         (count (getf snapshot :workspace-count 9))
         (occupied (getf snapshot :occupied))
         (accent (config-getf '(:bar) :accent :default "#7aa2f7"))
         (fg (config-getf '(:bar) :foreground :default "#c0caf5"))
         (muted (config-getf '(:bar) :muted :default "#565f89"))
         (gap (config-getf '(:bar :workspaces) :gap :default 8)))
    (loop for i from 1 to count
          collect (list* (format nil "~A" i)
                        (cond ((= i current) accent)
                              ((member i occupied) fg)
                              (t muted))
                        (append (when (= i current) '(:underline t))
                                (list :gap gap))))))

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
    ;; Common Lisp counts day-of-week from 0 = Monday
    (let ((short-days #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))
          (long-days #("Monday" "Tuesday" "Wednesday" "Thursday"
                       "Friday" "Saturday" "Sunday"))
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
    (:recording . bar-module-recording)
    (:clock . bar-module-clock)
    (:battery . bar-module-battery)
    (:volume . bar-module-volume)
    (:network . bar-module-network))
  "Maps configured module names to their rendering functions.")

(defparameter *bar-module-gap* 16
  "Pixels of empty space between modules.")

(defun bar-module-parts (module)
  "Render one module to a list of parts. A part is
(text color . plist): color NIL means the segment's base color,
and the plist tail carries render flags — :gap N adds N pixels of
breathing room after the part, :underline T draws the part's color
as a 2px line under the text (the active-workspace marker).
Unknown or failing modules render empty."
  (let ((fn (cdr (assoc module *bar-modules*))))
    (when fn
      (handler-case
          (let ((out (funcall fn)))
            (typecase out
              (string (list (cons out nil)))
              (list out)
              (t nil)))
        (error () nil)))))

(defun bar-part-flags (part)
  "The plist tail of a part. NIL for the classic dotted shape
((text . color)) — (cdr part) being an atom, not a list, is the
telltale."
  (let ((rest (cdr part)))
    (when (listp rest) (cddr part))))

(defun bar-part-width (part)
  "Pixel width of one part: the text's rendered advance plus its gap."
  (+ (bar-text-width (first part))
     (or (getf (bar-part-flags part) :gap 0) 0)))

(defun bar-segment-modules (position)
  "The configured module list for :left, :center or :right."
  (config-getf '(:bar :modules) position :default nil))

(defun bar-segment-width (modules)
  "Total pixel width of a module list, including inter-module gaps
and per-part :gap flags, so centered/right segments line up with
what is actually drawn."
  (let ((content
          (reduce #'+
                  (mapcar (lambda (module)
                            (cond
                              ((icons-module-p module)
                               (bar-icons-width module))
                              ((tray-module-p module)
                               (bar-tray-width))
                              (t
                               (reduce #'+
                                       (mapcar #'bar-part-width
                                               (bar-module-parts module))
                                       :initial-value 0))))
                          modules)
                  :initial-value 0)))
    (+ content (* *bar-module-gap* (length modules)))))

;;; --- Icons module (application icons with click actions) ---
;;;
;;; A module spec of the form (:icons (entry ...)) renders each
;;; entry's icon as a 16x16 image; entries carry their own click
;;; actions inline:
;;;
;;;   (:icons ((:icon "flameshot" :on-click "flameshot gui")
;;;            (:icon "variety" :on-click "variety")
;;;            (:icon "/path/to/custom.png" :on-click "rhizome-cmd lock")))

(defparameter *bar-icon-size* 16
  "Rendered icon size (fits the 20px default bar).")
(defparameter *bar-icon-cache* (make-hash-table :test 'equal)
  "Maps icon specs to scaled RGBA data (list width height vector).")
(defparameter *bar-icon-dirs*
  '("/usr/share/icons/hicolor/128x128/apps/"
    "/usr/share/icons/hicolor/64x64/apps/"
    "/usr/share/icons/hicolor/48x48/apps/"
    "/usr/share/icons/hicolor/256x256/apps/"
    "/usr/share/icons/hicolor/scalable/apps/"
    "/usr/share/pixmaps/")
  "Icon search order for name specs (PNG only; SVG skipped).")

(defun icons-module-p (module)
  "Whether MODULE is an icons module spec: (:icons (entry ...))."
  (and (listp module) (eq (first module) :icons)))

(defun tray-module-p (module)
  "Whether MODULE is the tray module (:tray)."
  (eq module :tray))

(defun bar-tray-width ()
  "Pixel width of the tray module: every item's icon plus spacing."
  (+ (* (length *tray-items*) (+ *bar-icon-size* 4))
     (if *tray-items* *bar-module-gap* 0)))

(defun bar-icons-entries (module)
  "The icon entries of an icons module."
  (second module))

(defun bar-icons-width (module)
  "Pixel width of an icons module (icons + inter-icon gaps)."
  (* (+ *bar-icon-size* 4) (length (bar-icons-entries module))))

(defun find-icon-file (spec)
  "Resolve an :icon spec — an absolute path, or an icon name searched
in the standard theme directories (PNG only)."
  (cond ((and (find #\/ spec) (probe-file spec)) spec)
        (t (loop for dir in *bar-icon-dirs*
                 for file = (concatenate 'string dir spec ".png")
                 when (probe-file file)
                   return file))))

(defun scale-icon (source-w source-h source-data target-size)
  "Scale decoded RGBA data to TARGET-SIZE x TARGET-SIZE,
nearest-neighbor. Returns (width height data)."
  (let ((out (make-array (* target-size target-size 4)
                         :element-type '(unsigned-byte 8)
                         :initial-element 0)))
    (loop for ty below target-size
          for sy = (floor (* ty source-h) target-size)
          do (loop for tx below target-size
                   for sx = (floor (* tx source-w) target-size)
                   for sidx = (* (+ (* sy source-w) sx) 4)
                   for tidx = (* (+ (* ty target-size) tx) 4)
                   do (loop for c below 4
                            do (setf (aref out (+ tidx c))
                                     (row-major-aref source-data (+ sidx c))))))
    (list target-size target-size out)))

(defun load-icon (spec &key (size *bar-icon-size*))
  "SPEC's icon, decoded and scaled to SIZE px; cached. NIL if the
icon cannot be found or decoded."
  (or (gethash (cons spec size) *bar-icon-cache*)
      (let ((file (find-icon-file spec)))
        (when file
          (setf (gethash (cons spec size) *bar-icon-cache*)
                (handler-case
                    (let ((png (pngload:load-file file :flatten t)))
                      (scale-icon (pngload:width png)
                                  (pngload:height png)
                                  (pngload:data png)
                                  size))
                  (error ()
                    (setf (gethash (cons spec size) *bar-icon-cache*)
                          nil))))))))

(defun bar-draw-icons (ptr width height x y module base-r base-g base-b)
  "Draw an icons module's entries at (X, Y); one hitbox per entry so
the existing click/scroll chain runs each entry's :on-click /
:on-scroll-* commands. Returns the x after the module plus gap."
  (dolist (entry (bar-icons-entries module))
    (let* ((icon (load-icon (getf entry :icon)))
           (start x))
      (when icon
        (destructuring-bind (iw ih idata) icon
          (declare (ignore iw))
          (loop for iy below ih
                for py = (+ y iy)
                while (< py height)
                do (loop for ix below *bar-icon-size*
                         for px = (+ x ix)
                         while (< px width)
                         do (let* ((idx (* (+ (* iy *bar-icon-size*) ix) 4))
                                   (a (aref idata (+ idx 3)))
                                   (r (aref idata idx))
                                   (g (aref idata (+ idx 1)))
                                   (b (aref idata (+ idx 2))))
                              (when (plusp a)
                                (bar-set-pixel
                                  ptr width px py
                                  (floor (/ (+ (* a r) (* (- 255 a) base-r)) 255))
                                  (floor (/ (+ (* a g) (* (- 255 a) base-g)) 255))
                                  (floor (/ (+ (* a b) (* (- 255 a) base-b))
                                            255))))))))
        (incf x *bar-icon-size*)
        (push (list entry start x) *bar-hitboxes*)))
    (incf x 4))
  (+ x *bar-module-gap*))

;;; --- Rendering ---

(defun bar-set-pixel (ptr width px py r g b)
  "Set pixel at (PX, PY) to RGB color (BGRA layout in the buffer),
premultiplied by the surface alpha (ARGB buffers)."
  (let* ((a (bar-surface-alpha))
         (offset (* (+ (* py width) px) 4)))
    (setf (cffi:mem-aref ptr :unsigned-char offset)
            (round (* b a) 255)
          (cffi:mem-aref ptr :unsigned-char (1+ offset))
            (round (* g a) 255)
          (cffi:mem-aref ptr :unsigned-char (+ offset 2))
            (round (* r a) 255)
          (cffi:mem-aref ptr :unsigned-char (+ offset 3)) a)))

(defun bar-surface-alpha ()
  "The bar/message/prompt surfaces' alpha byte. (:bar :opacity
1.0) = opaque; the buffers are ARGB8888, which Wayland requires
PREMULTIPLIED — every writer premultiplies by this."
  (round (* 255 (min 1.0 (max 0.0
            (config-getf '(:bar) :opacity :default 0.9))))))

(defun bar-clear (ptr width height r g b)
  "Fill the bar buffer with a solid, translucent-capable color
(premultiplied ARGB)."
  (let* ((a (bar-surface-alpha))
         (pixel (logior (ash a 24)
                        (ash (round (* r a) 255) 16)
                        (ash (round (* g a) 255) 8)
                        (round (* b a) 255))))
    (loop for i below (* width height) do
      (setf (cffi:mem-aref ptr :unsigned-int i) pixel))))

(defun bar-draw-text (ptr width height text x y r g b)
  "Draw TEXT at (X, Y) — the top of the text box — in RGB color.
Delegates to the font module: TrueType via freetype when available,
the 8x16 bitmap font otherwise (identical legacy metrics). The
buffer is premultiplied ARGB, so the color is scaled by the
surface alpha down in the font module."
  (font-ensure-initialized)
  (font-draw-text ptr width height text x y r g b
                  :alpha (bar-surface-alpha)))

(defun bar-text-width (text)
  (font-text-width text))

(defun bar-draw-module (ptr width height x y parts base-r base-g base-b module)
  "Draw one module's PARTS starting at X, using the base color for
parts without an explicit color. Parts may carry :gap and
:underline flags (see bar-module-parts). Records a click hitbox
for MODULE. Returns the x after the module plus gap."
  (let ((start x))
    (dolist (part parts)
      (let* ((text (first part))
             (color (second part))
             (flags (bar-part-flags part))
             (r base-r) (g base-g) (b base-b))
        (when color
          (setf (values r g b) (parse-hex-color color)))
        (bar-draw-text ptr width height text x y r g b)
        (let ((tw (bar-text-width text)))
          ;; The active-workspace marker: a 2px line in the part's
          ;; color, flush under the text box.
          (when (getf flags :underline)
            (loop for uy below 2
                  for py = (+ y (font-line-height) uy)
                  while (< py height) do
              (loop for uxx below tw
                    for px = (+ x uxx)
                    while (< px width) do
                (bar-set-pixel ptr width px py r g b))))
          (incf x (+ tw (or (getf flags :gap 0) 0))))))
    (let ((end x))
      (when (and module (> end start))
        (push (list module start end) *bar-hitboxes*)))
    (+ x *bar-module-gap*)))

(defun bar-draw-segment-item (ptr width height x y module fr fg fb br bg bb)
  "Draw one module of a bar segment (text or icons); returns the x
after it."
  (cond
    ((tray-module-p module)
     (bar-draw-tray ptr width height x y br bg bb))
    ((icons-module-p module)
     (bar-draw-icons ptr width height x y module br bg bb))
    (t (bar-draw-module ptr width height x y
                         (bar-module-parts module)
                         fr fg fb module))))

(defun %bar-render (state)
  "Render the bar content into the shm buffer and commit the surface."
  (let* ((ptr (gethash :shm-ptr state))
         (width (gethash :width state))
         (height (gethash :height state))
         (padding (config-getf '(:bar) :padding :default 6)))
    (when (or (null ptr) (cffi:null-pointer-p ptr))
      (return-from %bar-render))
    (multiple-value-bind (br bg bb) (parse-hex-color
                                       (config-getf '(:bar) :background
                                                    :default "#1a1b26"))
      (multiple-value-bind (fr fg fb) (parse-hex-color
                                        (config-getf '(:bar) :foreground
                                                     :default "#c0caf5"))
        (bar-clear ptr width height br bg bb)
        (let ((y (floor (- height (font-line-height)) 2)))
          (setf *bar-hitboxes* nil)
          ;; Left segment
          (let ((x padding))
            (dolist (module (bar-segment-modules :left))
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb))))
          ;; Center segment
          (let* ((modules (bar-segment-modules :center))
                 (total (bar-segment-width modules))
                 (x (floor (- width total) 2)))
            (dolist (module modules)
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb))))
          ;; Right segment
          (let* ((modules (bar-segment-modules :right))
                 (total (bar-segment-width modules))
                 (x (- width padding total)))
            (dolist (module modules)
              (setf x (bar-draw-segment-item ptr width height x y module
                                            fr fg fb br bg bb)))))))
    (wl:wl-surface.attach (gethash :surface state) (gethash :wl-buffer state) 0 0)
    (wl:wl-surface.damage (gethash :surface state) 0 0 width height)
    (wl:wl-surface.commit (gethash :surface state))))

(defvar *bar-render-lock* (bt:make-lock "bar-render")
  "Serializes bar rendering. Two threads call bar-render: the main
tick loop and the dbus-thread redraws (tray registrations).
Concurrent renders on one shm buffer were always a latent race —
visual glitches and double commits — that the font engine turned
fatal (a freetype face corrupts under concurrent FT_Load_Char, see
font.lisp's *font-lock*).")

(defun bar-render (state)
  (bt:with-lock-held (*bar-render-lock*)
    (%bar-render state)))

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
         (filename (format nil "rhizome-bar-~A-~A"
                           (get-universal-time) (random 100000)))
         (path (concatenate 'string "/dev/shm/" filename))
         (fd (cffi:foreign-funcall "open"
               :string path
               :int (logior 2 64)             ; O_RDWR | O_CREAT
               :int 384                        ; 0600
               :int)))
    (when (< fd 0)
      (format t "~&; Rhizome: bar: failed to create shm file~%")
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
              (gethash :wl-pool state) 0 width height (* width 4)
              :argb8888))                    ; premultiplied alpha
      t)))

;;; --- Connection lifecycle ---

(defun bar-bind-global (state registry name interface version)
  "Bind a registry global for the bar."
  (let ((iface-class (wl:find-interface-named interface)))
    (when iface-class
      (cond
        ((string= interface "wl_compositor")
         ;; Cap at version 5, mirroring the WM's bind: wl_surface v6
         ;; added preferred_buffer_scale/transform events our wayflan
         ;; bindings cannot demarshal — one such event desyncs the
         ;; bar's whole event stream ("Malformed Wayland message",
         ;; destroyed proxies, a dead bar). This was the WM-side fix
         ;; (wm-protocol.lisp) that the bar's own connection never
         ;; got.
         (let ((v (min version 5)))
           (setf (gethash :compositor state)
                 (wl:wl-registry.bind registry name iface-class v))))
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
     ;; River closed our layer surface. This is how the bar dies
     ;; silently — say so before *bar-running* stops the loop (the
     ;; WM's supervisor will restart us within 15s).
     (format t "~&; Rhizome: bar: layer surface closed by compositor, exiting~%")
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
      (format t "~&; Rhizome: bar: command failed: ~A~%" e))))

(defun bar-module-action (module key)
  "The configured shell command for MODULE's action KEY
(:on-click, :on-scroll-up, :on-scroll-down), if any. Icons-module
entries carry their actions inline."
  (cond (;; an icons entry: (:icon "name" :on-click "command" ...)
         (and (listp module) (getf module :icon))
         (getf module key))
        (t (config-getf (list :modules module) key :default nil))))

(defun bar-handle-click (x)
  "Handle a left click at X: run the module's :on-click command."
  (let* ((hit (find-if (lambda (hb)
                         (and (<= (second hb) x)
                              (< x (third hb))))
                       *bar-hitboxes*))
         (module (first hit)))
    (when module
      (cond
        ;; Tray item: Activate / SecondaryActivate on the item
        ((and (listp module) (getf module :name))
         (format t "~&; Rhizome: bar: tray click ~A~%" (getf module :name))
         (tray-click module))
        ;; Workspace numbers: switch to the clicked workspace via IPC
        ((eq module :workspaces)
         (let ((workspace (1+ (floor (- x (second hit)) 8))))
           (when (<= 1 workspace (getf (bar-snapshot) :workspace-count 9))
             (format t "~&; Rhizome: bar: click workspace ~D~%" workspace)
             (bar-run-command
               (format nil "rhizome-cmd switch-workspace ~D" workspace)))))
        (t
         (format t "~&; Rhizome: bar: click on ~A~%" module)
         (let ((command (bar-module-action module :on-click)))
           (when command
             (bar-run-command command))))))))

(defun bar-handle-right-click (x)
  "Handle a right click at X: a tray item opens its dbusmenu via
the WM's prompt machinery (IPC round-trip: the menu data lives
here, the prompt surfaces live there)."
  (let* ((hit (find-if (lambda (hb)
                         (and (<= (second hb) x)
                              (< x (third hb))))
                       *bar-hitboxes*))
         (module (first hit)))
    (when (and (listp module) (getf module :name) (getf module :menu))
      (format t "~&; Rhizome: bar: tray menu ~A~%" (getf module :name))
      ;; the Menu property is (o . "/path") — an object-path variant
      (let ((menu-path (if (consp (getf module :menu))
                           (cdr (getf module :menu))
                           (getf module :menu))))
        (bar-run-command
          (format nil "rhizome-cmd \"(rhizome:tray-show-menu \\\"~A\\\" \\\"~A\\\")\""
                  (getf module :name) menu-path))))))

(defun bar-handle-scroll (amount)
  "Handle a vertical scroll gesture over a module."
  (let* ((module (and *bar-pointer-x* (bar-module-at *bar-pointer-x*)))
         (key (if (plusp amount) :on-scroll-up :on-scroll-down))
         (command (and module (bar-module-action module key))))
    (cond
      ;; Tray item: Scroll(delta, vertical) on the item
      ((and (listp module) (getf module :name))
       (tray-scroll module (if (plusp amount) 1 -1)))
      (command
       (bar-run-command command)))))

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
           ;; 272 = BTN_LEFT, 273 = BTN_RIGHT
           (when (eq state :pressed)
             (let ((x *bar-pointer-x*))
               (when x
                 (cond
                   ((= button 272) (bar-handle-click x))
                   ((= button 273) (bar-handle-right-click x))))))))
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
               (format t "~&; Rhizome: bar: ignored event error: ~A~%" e))))
         (safe-dispatch (display)
           (handler-case
               (wl:wl-display-dispatch-event display)
             (error (e)
               (format t "~&; Rhizome: bar: ignored event error: ~A~%" e)))))
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
              (format t "~&; Rhizome: bar: missing compositor/shm/layer-shell globals~%")
              (return-from bar-run-loop))
            (let ((surface (wl:wl-compositor.create-surface
                            (gethash :compositor state))))
              (setf (gethash :surface state) surface)
              (let ((layer-surface
                      (rwm:zwlr-layer-shell-v1.get-layer-surface
                        (gethash :layer-shell state) surface
                        (gethash :output state) :top "rhizome-bar")))
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
                      do (let ((tray-was *tray-changed*))
                           (tray-drain)
                           (when (/= tray-was *tray-changed*)
                             ;; new/removed items: invalidate content
                             (setf *bar-last-content* nil)))
                         (when (and (gethash :mapped state)
                                    (or (bar-content-changed-p)
                                        (/= *tray-render-stamp*
                                            *tray-changed*)))
                           (setf *tray-render-stamp* *tray-changed*)
                           (bar-render state))
                         (loop while (and *bar-running*
                                          (wl:wl-display-listen display))
                               do (safe-dispatch display))
                         (sleep (config-getf '(:bar) :interval :default 0.15)))
                (bar-destroy-shm state)))))
      (error (e)
        (format t "~&; Rhizome: bar thread error: ~A~%" e)))))


(defparameter *bar-process-mode* nil
  "T in the bar process itself (set by run-bar BEFORE anything else).
Guards all bar process spawning: the bar loads the user config at
startup, and any (rhizome:configure ...) in it must never reach
bar-apply-config — the bar applying config used to spawn another bar,
which loaded the config and spawned another: exponential bars on
screen. Only the WM process ever manages bar processes.")

(defun run-bar ()
  "Entry point for the standalone bar process. Loads the user config,
then runs the bar's connection loop on this process's main thread.
Runs in *bar-process-mode*: this process never spawns or stops bars,
no matter what the user config asks for."
  (setf *bar-process-mode* t)
  (setf *bar-running* t)
  (load-user-config)
  (font-ensure-initialized)
  ;; The tray lives here: this process owns the StatusNotifierWatcher
  ;; bus name, serves registrations, and renders the items.
  (handler-case (tray-start)
    (error (e)
      (format t "~&; Rhizome: tray: startup failed: ~A~%" e)))
  (let ((state (make-hash-table)))
    (setf *bar-state* state)
    (bar-run-loop state)
    (setf *bar-state* nil)
    (format t "~&; Rhizome: bar loop ended~%")))

;;; --- Process lifecycle (the bar runs as its own process) ---

(defvar *bar-process* nil
  "The running bar process, a uiop:process object. DEFVAR: reloading
  bar.lisp must not orphan a running bar (bar-reserved-height no
  longer depends on this, but toggle-bar still uses it).")

(defun bar-alive-p ()
  "Whether the bar process is running."
  (and *bar-process*
       (uiop:process-alive-p *bar-process*)))

(defun bar-log-path ()
  "Pathname of the bar process's log file."
  (merge-pathnames #P".cache/rhizome/bar.log"
                   (user-homedir-pathname)))

(defun start-bar ()
  "Start the bar as its own process (no-op if already running). The
WM process only — and it first reaps any orphan bars left behind by
past incidents (they would stack on screen)."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from start-bar))
  (when *bar-process-mode*
    (return-from start-bar))
  (when (bar-alive-p)
    (return-from start-bar))
  ;; Reap orphan bar processes: exact-match the run-bar eval, never
  ;; the WM (its arg is (rhizome:run))
  (ignore-errors
    (uiop:run-program '("pkill" "-f" "rhizome:run-bar")
                      :ignore-error-status t))
  (ensure-directories-exist (bar-log-path))
  (with-open-file (s (bar-log-path) :direction :output
                     :if-exists :supersede))
  (setf *bar-process*
        (uiop:launch-program
          (list "sbcl"
                "--non-interactive"
                "--eval" "(ql:quickload :rhizome)"
                "--eval" "(rhizome:run-bar)")
          :output (bar-log-path)
          :error-output :output))
  (format t "~&; Rhizome: bar started~%"))

(defun stop-bar ()
  "Stop the bar process. Kills tracked AND orphan bars (the latter
left by past incidents); WM process only."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from stop-bar))
  (when *bar-process-mode*
    (return-from stop-bar))
  (when (and *bar-process* (uiop:process-alive-p *bar-process*))
    (handler-case
        (uiop:terminate-process *bar-process*)
      (error (e)
        (format t "~&; Rhizome: error stopping bar: ~A~%" e))))
  (when *bar-process*
    (ignore-errors (uiop:wait-process *bar-process*)))
  ;; Reap orphans too (exact-match, never the WM)
  (ignore-errors
    (uiop:run-program '("pkill" "-f" "rhizome:run-bar")
                      :ignore-error-status t))
  (setf *bar-process* nil)
  (format t "~&; Rhizome: bar stopped~%"))

(defun bar-reserved-height ()
  "Vertical pixels reserved for the bar (0 when disabled).
River reports full output dimensions to the WM even when the bar's
layer surface has an exclusive zone, so the WM reserves the space
itself in its layout math. Config-driven, NOT process-driven: the
bar draws in this space whether the WM spawned it or not, and
gating on in-image process tracking breaks whenever bar.lisp is
reloaded or the bar is started independently (windows then tiled
under the bar, their menus hidden behind it)."
  (if (config-getf '(:bar) :enabled :default t)
      (config-getf '(:bar) :height :default 20)
      0))

(defun toggle-bar ()
  "Toggle the status bar on/off. Persists the intent into
(:bar :enabled) so the supervisor honors it — a toggle-off must not
be undone fifteen seconds later. bar-apply-config handles the
start/stop and the reserved-space re-tiling."
  (when *bar-process-mode*
    (return-from toggle-bar))
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from toggle-bar))
  (configure (list :bar (list :enabled (not (bar-alive-p))))))

(defvar *bar-supervisor* nil
  "The WM's bar supervisor thread. The bar's deaths were silent and
manual for far too long.")

(defun start-bar-supervisor ()
  "Keep the bar alive (WM process only, idempotent): every 15
seconds, when the config wants a bar and none is alive, start one
and say why. Honors a deliberate toggle-off because toggle-bar
persists the intent in (:bar :enabled)."
  (when (or *bar-process-mode* *bar-supervisor*)
    (return-from start-bar-supervisor))
  (setf *bar-supervisor*
        (bt:make-thread
          (lambda ()
            (loop
              (handler-case
                  (progn
                    (sleep 15)
                    (when (and (config-getf '(:bar) :enabled :default t)
                               (not (bar-alive-p)))
                      (format t "~&; Rhizome: bar supervisor: bar is down, restarting~%")
                      (start-bar)))
                (error () nil))))
          :name "rhizome-bar-supervisor")))

(defun bar-apply-config ()
  "Apply the current config to the bar: start, stop or restart as
needed. WM-process only — the bar process itself never manages bar
processes (see *bar-process-mode*), and neither does anything else:
a TEST process whose (configure '(:bar (:enabled nil))) used to
flow into stop-bar's pkill and reap the user's live bar."
  (unless (and (boundp '*wm-process-mode*) *wm-process-mode*)
    (return-from bar-apply-config))
  (when *bar-process-mode*
    (return-from bar-apply-config))
  (let ((before (bar-reserved-height)))
    (cond
      ((not (config-getf '(:bar) :enabled :default t))
       (when (bar-alive-p) (stop-bar)))
      ((bar-alive-p)
       ;; Restart to pick up structural changes (height, modules, colors
       ;; are picked up by the new process from the config file).
       (stop-bar)
       (start-bar))
      (t
       (start-bar)))
    ;; The reserved space may have changed (height, enabled): rescale
    ;; existing frame trees and re-tile into it
    (unless (eql before (bar-reserved-height))
      (dolist (out (all-outputs))
        (refit-output-frame-trees out))
      (when *wm-proxy*
        (request-manage-dirty)))))
