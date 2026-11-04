;;; Native Wayland application launcher for Rhizome
;;;
;;; Uses wlr-layer-shell to create an overlay popup, renders text
;;; to a shared memory buffer, and handles keyboard input directly.
;;;
;;; Inspired by fuzzel and the Lisp Machines' prompt interface.

(in-package #:rhizome)

(defvar *launcher-active* nil
  "Whether the launcher popup is currently shown.")

(defvar *launcher-surface* nil
  "The wl_surface for the launcher popup.")

(defvar *launcher-backdrop-surface* nil
  "The fullscreen dim layer under the launcher (the modal feel).")
(defvar *launcher-backdrop-layer-surface* nil)
(defvar *launcher-backdrop-ptr* nil)
(defvar *launcher-backdrop-fd* nil)
(defvar *launcher-backdrop-size* 0)
(defvar *launcher-backdrop-pool* nil)
(defvar *launcher-backdrop-buffer* nil)
(defvar *launcher-layer-surface* nil
  "The zwlr_layer_surface_v1 for the launcher popup.")

(defvar *launcher-query* ""
  "Current search query string.")

(defvar *launcher-selection* 0
  "Index of the currently selected entry.")

(defvar *launcher-scroll-offset* 0
  "Scroll offset for the entry list (index of first visible entry).")

(defvar *launcher-entries* nil
  "Filtered list of (display-name . command) pairs currently shown.")

(defvar *launcher-width* 600)
(defvar *launcher-height* 400)
(defvar *launcher-line-height* 36)
(defvar *launcher-padding* 12)

;;; --- Launcher entry list ---

(defvar *launcher-app-list* nil
  "List of (display-name . command) pairs loaded from .desktop files.")

(defun launcher-desktop-paths ()
  "Return list of directories to search for .desktop files.
Follows the XDG Desktop Entry specification."
  (labels ((as-directory (path)
             "PATH as a directory pathname (trailing slash matters:
merge-pathnames against a slashless path treats its last component
as a filename — \"/home/glenn\" + \"applications/\" merged to
\"/home/applications/glenn\" in the wild, hiding user-local desktop
entries like a freshly installed Variety)."
             (let ((p (if (pathnamep path) path (pathname path))))
               (if (uiop:directory-pathname-p p)
                   p
                   (uiop:ensure-directory-pathname p)))))
    (let ((xdg-dirs '()))
      ;; XDG_DATA_HOME/applications
      (let ((xdg-data-home (sb-ext:posix-getenv "XDG_DATA_HOME")))
        (when (and xdg-data-home (string/= xdg-data-home ""))
          (push (merge-pathnames "applications/"
                                 (as-directory xdg-data-home))
                xdg-dirs)))
      ;; ~/.local/share/applications
      (let ((home (sb-ext:posix-getenv "HOME")))
        (when home
          (push (merge-pathnames ".local/share/applications/"
                                 (as-directory home))
                xdg-dirs)))
      ;; XDG_DATA_DIRS/applications (system-wide)
      (let ((xdg-data-dirs (sb-ext:posix-getenv "XDG_DATA_DIRS")))
        (when (and xdg-data-dirs (string/= xdg-data-dirs ""))
          (dolist (dir (ppcre:split ":" xdg-data-dirs))
            (when (string/= dir "")
              (push (merge-pathnames "applications/"
                                     (as-directory dir))
                    xdg-dirs)))))
      ;; Fallback system dirs
      (push #P"/usr/local/share/applications/" xdg-dirs)
      (push #P"/usr/share/applications/" xdg-dirs)
      (remove-duplicates xdg-dirs :test #'equal))))

(defun launcher-parse-desktop-file (path)
  "Parse a .desktop file, return (name command icon) if it should
be shown — icon is the Icon= spec (may be NIL). Returns NIL for
NoDisplay=true, Hidden=true, or Type != Application."
  (handler-case
      (with-open-file (f path :direction :input :external-format :utf-8)
        (let ((in-desktop-entry nil)
              (name nil)
              (exec nil)
              (icon nil)
              (no-display nil)
              (hidden nil)
              (entry-type nil)
              (terminal nil))
          (loop for line = (read-line f nil nil) while line do
            (cond
              ((ppcre:scan "^\\s*$" line) nil)  ; blank line
              ((ppcre:scan "^\\s*#" line) nil)   ; comment
              ((ppcre:scan "^\\[.*\\]" line)
               (setf in-desktop-entry (string= (string-trim "[] " line) "Desktop Entry")))
              (in-desktop-entry
               (ppcre:register-groups-bind (key value)
                   ("^\\s*(\\w+)\\s*=\\s*(.*)$" line)
                 (cond
                   ((string-equal key "Name") (setf name value))
                   ((string-equal key "Exec") (setf exec value))
                   ((string-equal key "Icon") (setf icon value))
                   ((string-equal key "NoDisplay") (setf no-display (string-equal value "true")))
                   ((string-equal key "Hidden") (setf hidden (string-equal value "true")))
                   ((string-equal key "Type") (setf entry-type value))
                   ((string-equal key "Terminal") (setf terminal (string-equal value "true"))))))))
          (when (and name exec
                     (not no-display)
                     (not hidden)
                     (string-equal entry-type "Application"))
            ;; Clean up Exec: remove field codes like %f, %u, %F, %U, etc.
            (let ((clean-exec (ppcre:regex-replace-all
                                " %([fFuUickdDvVmM])" exec "")))
              (list name (if terminal
                             (format nil "alacritty --command ~A" clean-exec)
                             clean-exec)
                    icon)))))
    (error (e)
      (declare (ignore e))
      nil)))

(defun launcher-scan-desktop-files ()
  "Scan XDG directories for .desktop files and build the app list.
De-duplicates by display name (first occurrence wins)."
  (let ((seen (make-hash-table :test 'equal))
        (entries '()))
    (dolist (dir (launcher-desktop-paths))
      (when (probe-file dir)
        (dolist (file (directory (merge-pathnames "*.desktop" dir)
                        :resolve-symlinks nil))
          (let ((entry (launcher-parse-desktop-file file)))
            (when (and entry (not (gethash (car entry) seen)))
              (setf (gethash (car entry) seen) t)
              (push entry entries))))))
    (sort entries #'string-lessp :key #'car)))

(defun launcher-refresh-app-list ()
  "Refresh the app list from .desktop files."
  (setf *launcher-app-list* (launcher-scan-desktop-files))
  (format t "~&; Rhizome: loaded ~A desktop entries~%" (length *launcher-app-list*)))

;;; --- Frecency: most-used apps first (the fuzzel killer feature) ---

(defparameter *launcher-history-path*
  (merge-pathnames #P".local/share/rhizome/launcher-history.sexp"
                   (user-homedir-pathname))
  "Launch counts by display name. LET-bindable for tests.")

(defparameter *launcher-history* (make-hash-table :test #'equal)
  "Display name → plist (:count N :last universal-time).")

(defun launcher-history-load ()
  "Read launch history. Missing file = nothing launched yet."
  (handler-case
      (when (probe-file *launcher-history-path*)
        (clrhash *launcher-history*)
        (with-open-file (s *launcher-history-path*)
          (dolist (entry (read s))
            (setf (gethash (car entry) *launcher-history*)
                  (cdr entry)))))
    (error ()
      (clrhash *launcher-history*))))

(defun launcher-history-save ()
  "Persist launch history (atomic: write beside, rename over)."
  (handler-case
      (let ((entries
              (loop for name being the hash-keys of *launcher-history*
                    collect (cons name (gethash name *launcher-history*)))))
        (ensure-directories-exist *launcher-history-path*)
        (let ((tmp (merge-pathnames "launcher-history.sexp.tmp"
                                    *launcher-history-path*)))
          (with-open-file (s tmp :direction :output :if-exists :supersede)
            (write entries :stream s :readably t))
          (when (probe-file *launcher-history-path*)
            (delete-file *launcher-history-path*))
          (rename-file tmp *launcher-history-path*)))
    (error (e)
      (format t "~&; Rhizome: launcher history save failed: ~A~%" e))))

(defun launcher-record-launch (name)
  "Record a launch: count +1, timestamp now, persist."
  (let ((h (gethash name *launcher-history*)))
    (setf (gethash name *launcher-history*)
          (list :count (1+ (or (getf h :count) 0))
                :last (get-universal-time))))
  (launcher-history-save))

(defun launcher-frecency-score (name)
  "Count with a recency boost: +3 within the last day, +1 within
the last week. Unlaunched apps score 0."
  (let ((h (gethash name *launcher-history*)))
    (if (null h)
        0
        (let* ((count (getf h :count 0))
               (age (- (get-universal-time) (getf h :last 0)))
               (boost (cond ((< age 86400) 3)
                            ((< age 604800) 1)
                            (t 0))))
          (+ count boost)))))

(defun launcher-sort-by-frecency (entries)
  "Most-used first; ties alphabetical. Only meaningful with an
empty query — filtered results keep their relevance order."
  (sort (copy-list entries)
        (lambda (a b)
          (let ((sa (launcher-frecency-score (car a)))
                (sb (launcher-frecency-score (car b))))
            (if (= sa sb)
                (string-lessp (car a) (car b))
                (> sa sb))))))

(defun launcher-filter-entries ()
  "Filter *launcher-app-list* by the current query string (fuzzy:
characters in order, case-insensitive; substring matches rank
first)."
  (let ((q (string-downcase *launcher-query*)))
    (if (string= q "")
        (launcher-sort-by-frecency *launcher-app-list*)
        (let ((direct '()) (fuzzy '()))
          (dolist (entry *launcher-app-list*)
            (let ((name (and entry (string-downcase (car entry)))))
              (cond ((null name))
                    ((search q name) (push entry direct))
                    ((fuzzy-match-p q name) (push entry fuzzy)))))
          (nconc (nreverse direct) (nreverse fuzzy))))))

(defun launcher-visible-count (height)
  "How many entries fit in the visible area."
  (max 0 (floor (- height *launcher-padding* *launcher-line-height* *launcher-padding*)
                *launcher-line-height*)))

(defun launcher-update-scroll (height)
  "Ensure *launcher-selection* is visible by adjusting *launcher-scroll-offset*."
  (let ((visible (launcher-visible-count height)))
    (cond
      ((< *launcher-selection* *launcher-scroll-offset*)
       (setf *launcher-scroll-offset* *launcher-selection*))
      ((>= *launcher-selection* (+ *launcher-scroll-offset* visible))
       (setf *launcher-scroll-offset* (- *launcher-selection* visible -1))))
    (let ((max-offset (max 0 (- (length *launcher-entries*) visible))))
      (when (> *launcher-scroll-offset* max-offset)
        (setf *launcher-scroll-offset* max-offset)))))

;;; --- Pixel rendering ---

(defun launcher-render (buffer width height)
  "Render the launcher UI into BUFFER (a vector of unsigned bytes, BGRA format).
Simple text rendering using a built-in 8x16 bitmap font."
  ;; Clear to dark background (RGBA: 30, 30, 40, 255)
  (fill buffer 0)
  (loop for i below (* width height) do
    (let ((offset (* i 4)))
      (setf (aref buffer offset) 28       ; B
            (aref buffer (1+ offset)) 28         ; G
            (aref buffer (+ offset 2)) 38        ; R
            (aref buffer (+ offset 3)) 255)))    ; A

  ;; Draw query prompt line
  (launcher-draw-text buffer width "Run: "
                      *launcher-padding* *launcher-padding*
                      100 100 120)
  (launcher-draw-text buffer width *launcher-query*
                      (+ *launcher-padding* 48) *launcher-padding*
                      230 230 230)

  ;; Draw cursor (block) after query
  (let ((cursor-x (+ *launcher-padding* 48
                     (* 8 (length *launcher-query*))))
        (cursor-y *launcher-padding*))
    (loop for dy below 16
          for py = (+ cursor-y dy)
          while (< py height) do
      (loop for dx below 8
            for px = (+ cursor-x dx)
            while (< px width) do
        (let ((offset (* (+ (* py width) px) 4)))
          (setf (aref buffer offset) 200
                (aref buffer (1+ offset)) 200
                (aref buffer (+ offset 2)) 200
                (aref buffer (+ offset 3)) 255)))))

  ;; Draw filtered entries (scrolled)
  (let ((entries *launcher-entries*)
        (y-start (+ *launcher-padding* *launcher-line-height*)))
    (loop for entry in (nthcdr *launcher-scroll-offset* entries)
          for i from *launcher-scroll-offset*
          for y = (+ y-start (* (- i *launcher-scroll-offset*) *launcher-line-height*))
          while (< (+ y *launcher-line-height*) height) do
      ;; Highlight selected entry
      (when (= i *launcher-selection*)
        (loop for dy below *launcher-line-height*
              for py = (+ y dy)
              while (< py height) do
          (loop for dx below (- width (* 2 *launcher-padding*))
                for px = (+ *launcher-padding* dx)
                while (< px width) do
            (let ((offset (* (+ (* py width) px) 4)))
              (setf (aref buffer offset) 60
                    (aref buffer (1+ offset)) 90
                    (aref buffer (+ offset 2)) 140
                    (aref buffer (+ offset 3)) 255)))))
      ;; Draw entry text
      (multiple-value-bind (r g b)
          (if (= i *launcher-selection*)
              (values 255 255 255)
              (values 180 180 190))
        (launcher-draw-text buffer width (car entry)
                            *launcher-padding* (+ y 4)
                            r g b)))))

;;; --- Minimal 8x16 bitmap font ---
;;; We use a compact hex-encoded font for basic ASCII.
;;; Each character is 8 pixels wide, 16 pixels tall, encoded as 16 bytes.

(defparameter *font-8x16*
  (make-hash-table)
  "Maps character codes to 16-byte bitmaps for 8x16 font.")

(defun init-font ()
  "Initialize the minimal bitmap font (ASCII printable chars)."
  ;; Space (empty)
  (setf (gethash 32 *font-8x16*)
        (make-array 16 :initial-element 0))
  ;; Load font data for printable ASCII (33-126)
  ;; We use a simple encoding: each byte is a row, MSB = leftmost pixel
  (loop for code from 33 to 126 do
    (setf (gethash code *font-8x16*)
          (launcher-font-glyph code))))

(defun launcher-font-glyph (code)
  "Return a 16-byte bitmap for character CODE.
Uses a compact 8x16 bitmap font for ASCII printable characters."
  (let ((glyph (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0)))
    ;; Each entry is 16 bytes, one per row, MSB=leftmost pixel
    (case code
      ;; Space (32) - all zeros (already initialized)
      (33 ; ! 
       (setf (aref glyph 0) #x18 (aref glyph 1) #x18 (aref glyph 2) #x18
             (aref glyph 3) #x18 (aref glyph 4) #x18 (aref glyph 5) #x08
             (aref glyph 6) #x00 (aref glyph 7) #x00 (aref glyph 8) #x00
             (aref glyph 9) #x18 (aref glyph 10) #x18))
      (34 ; "
       (setf (aref glyph 0) #x66 (aref glyph 1) #x66 (aref glyph 2) #x66
             (aref glyph 3) #x24 (aref glyph 4) #x00 (aref glyph 5) #x00
             (aref glyph 6) #x00 (aref glyph 7) #x00 (aref glyph 8) #x00
             (aref glyph 9) #x00 (aref glyph 10) #x00 (aref glyph 11) #x00))
      (35 ; #
       (setf (aref glyph 2) #x28 (aref glyph 3) #x28 (aref glyph 4) #x7C
             (aref glyph 5) #x28 (aref glyph 6) #x7C (aref glyph 7) #x28
             (aref glyph 8) #x28 (aref glyph 9) #x28))
      (36 ; $
       (setf (aref glyph 0) #x10 (aref glyph 1) #x38 (aref glyph 2) #x54
             (aref glyph 3) #x50 (aref glyph 4) #x38 (aref glyph 5) #x14
             (aref glyph 6) #x54 (aref glyph 7) #x38 (aref glyph 8) #x10))
      (37 ; %
       (setf (aref glyph 0) #x62 (aref glyph 1) #x64 (aref glyph 2) #x08
             (aref glyph 3) #x10 (aref glyph 4) #x20 (aref glyph 5) #x4C
             (aref glyph 6) #x46))
      (38 ; &
       (setf (aref glyph 2) #x30 (aref glyph 3) #x48 (aref glyph 4) #x48
             (aref glyph 5) #x30 (aref glyph 6) #x4A (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x3A (aref glyph 10) #x00))
      (39 ; '
       (setf (aref glyph 0) #x18 (aref glyph 1) #x18 (aref glyph 2) #x08
             (aref glyph 3) #x10 (aref glyph 4) #x00 (aref glyph 5) #x00
             (aref glyph 6) #x00 (aref glyph 7) #x00 (aref glyph 8) #x00))
      (40 ; (
       (setf (aref glyph 2) #x08 (aref glyph 3) #x10 (aref glyph 4) #x20
             (aref glyph 5) #x20 (aref glyph 6) #x20 (aref glyph 7) #x20
             (aref glyph 8) #x20 (aref glyph 9) #x20 (aref glyph 10) #x10
             (aref glyph 11) #x08))
      (41 ; )
       (setf (aref glyph 2) #x20 (aref glyph 3) #x10 (aref glyph 4) #x08
             (aref glyph 5) #x08 (aref glyph 6) #x08 (aref glyph 7) #x08
             (aref glyph 8) #x08 (aref glyph 9) #x08 (aref glyph 10) #x10
             (aref glyph 11) #x20))
      (42 ; *
       (setf (aref glyph 3) #x00 (aref glyph 4) #x10 (aref glyph 5) #x54
             (aref glyph 6) #x38 (aref glyph 7) #x54 (aref glyph 8) #x10))
      (43 ; +
       (setf (aref glyph 4) #x00 (aref glyph 5) #x10 (aref glyph 6) #x10
             (aref glyph 7) #x7C (aref glyph 8) #x10 (aref glyph 9) #x10))
      (44 ; ,
       (setf (aref glyph 10) #x18 (aref glyph 11) #x18 (aref glyph 12) #x08
             (aref glyph 13) #x10))
      (45 ; -
       (setf (aref glyph 6) #x00 (aref glyph 7) #x7C (aref glyph 8) #x00))
      (46 ; .
       (setf (aref glyph 10) #x18 (aref glyph 11) #x18))
      (47 ; /
       (setf (aref glyph 0) #x04 (aref glyph 1) #x08 (aref glyph 2) #x10
             (aref glyph 3) #x20 (aref glyph 4) #x40 (aref glyph 5) #x80))
      ;; Digits 0-9
      (48 ; 0
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x4C
             (aref glyph 5) #x54 (aref glyph 6) #x64 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (49 ; 1
       (setf (aref glyph 2) #x10 (aref glyph 3) #x30 (aref glyph 4) #x10
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x38))
      (50 ; 2
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x04
             (aref glyph 5) #x08 (aref glyph 6) #x10 (aref glyph 7) #x20
             (aref glyph 8) #x40 (aref glyph 9) #x7C))
      (51 ; 3
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x04
             (aref glyph 5) #x18 (aref glyph 6) #x04 (aref glyph 7) #x04
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (52 ; 4
       (setf (aref glyph 2) #x08 (aref glyph 3) #x18 (aref glyph 4) #x28
             (aref glyph 5) #x48 (aref glyph 6) #x7C (aref glyph 7) #x08
             (aref glyph 8) #x08 (aref glyph 9) #x08))
      (53 ; 5
       (setf (aref glyph 2) #x7C (aref glyph 3) #x40 (aref glyph 4) #x78
             (aref glyph 5) #x04 (aref glyph 6) #x04 (aref glyph 7) #x04
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (54 ; 6
       (setf (aref glyph 2) #x18 (aref glyph 3) #x20 (aref glyph 4) #x40
             (aref glyph 5) #x78 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (55 ; 7
       (setf (aref glyph 2) #x7C (aref glyph 3) #x04 (aref glyph 4) #x08
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x10))
      (56 ; 8
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x38 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (57 ; 9
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x3C (aref glyph 7) #x04
             (aref glyph 8) #x08 (aref glyph 9) #x30))
      (58 ; :
       (setf (aref glyph 4) #x18 (aref glyph 5) #x18 (aref glyph 6) #x00
             (aref glyph 7) #x00 (aref glyph 8) #x00 (aref glyph 9) #x00
             (aref glyph 10) #x18 (aref glyph 11) #x18))
      (59 ; ;
       (setf (aref glyph 4) #x18 (aref glyph 5) #x18 (aref glyph 6) #x00
             (aref glyph 7) #x00 (aref glyph 8) #x00 (aref glyph 9) #x00
             (aref glyph 10) #x18 (aref glyph 11) #x08 (aref glyph 12) #x10))
      (60 ; <
       (setf (aref glyph 3) #x08 (aref glyph 4) #x10 (aref glyph 5) #x20
             (aref glyph 6) #x40 (aref glyph 7) #x20 (aref glyph 8) #x10
             (aref glyph 9) #x08))
      (61 ; =
       (setf (aref glyph 5) #x00 (aref glyph 6) #x7C (aref glyph 7) #x00
             (aref glyph 8) #x7C (aref glyph 9) #x00))
      (62 ; >
       (setf (aref glyph 3) #x40 (aref glyph 4) #x20 (aref glyph 5) #x10
             (aref glyph 6) #x08 (aref glyph 7) #x10 (aref glyph 8) #x20
             (aref glyph 9) #x40))
      (63 ; ?
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x04
             (aref glyph 5) #x08 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x00 (aref glyph 9) #x00 (aref glyph 10) #x10
             (aref glyph 11) #x10))
      (64 ; @
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x4C
             (aref glyph 5) #x54 (aref glyph 6) #x54 (aref glyph 7) #x54
             (aref glyph 8) #x40 (aref glyph 9) #x38))
      ;; Uppercase A-Z
      (65 ; A
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x7C (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (66 ; B
       (setf (aref glyph 2) #x78 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x78 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x78))
      (67 ; C
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x40
             (aref glyph 5) #x40 (aref glyph 6) #x40 (aref glyph 7) #x40
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (68 ; D
       (setf (aref glyph 2) #x78 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x78))
      (69 ; E
       (setf (aref glyph 2) #x7C (aref glyph 3) #x40 (aref glyph 4) #x40
             (aref glyph 5) #x78 (aref glyph 6) #x40 (aref glyph 7) #x40
             (aref glyph 8) #x40 (aref glyph 9) #x7C))
      (70 ; F
       (setf (aref glyph 2) #x7C (aref glyph 3) #x40 (aref glyph 4) #x40
             (aref glyph 5) #x78 (aref glyph 6) #x40 (aref glyph 7) #x40
             (aref glyph 8) #x40 (aref glyph 9) #x40))
      (71 ; G
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x40
             (aref glyph 5) #x5C (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (72 ; H
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x7C (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (73 ; I
       (setf (aref glyph 2) #x38 (aref glyph 3) #x10 (aref glyph 4) #x10
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x38))
      (74 ; J
       (setf (aref glyph 2) #x1C (aref glyph 3) #x08 (aref glyph 4) #x08
             (aref glyph 5) #x08 (aref glyph 6) #x08 (aref glyph 7) #x48
             (aref glyph 8) #x48 (aref glyph 9) #x30))
      (75 ; K
       (setf (aref glyph 2) #x44 (aref glyph 3) #x48 (aref glyph 4) #x50
             (aref glyph 5) #x60 (aref glyph 6) #x50 (aref glyph 7) #x48
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (76 ; L
       (setf (aref glyph 2) #x40 (aref glyph 3) #x40 (aref glyph 4) #x40
             (aref glyph 5) #x40 (aref glyph 6) #x40 (aref glyph 7) #x40
             (aref glyph 8) #x40 (aref glyph 9) #x7C))
      (77 ; M
       (setf (aref glyph 2) #x44 (aref glyph 3) #x6C (aref glyph 4) #x54
             (aref glyph 5) #x54 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (78 ; N
       (setf (aref glyph 2) #x44 (aref glyph 3) #x64 (aref glyph 4) #x54
             (aref glyph 5) #x4C (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (79 ; O
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (80 ; P
       (setf (aref glyph 2) #x78 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x78 (aref glyph 6) #x40 (aref glyph 7) #x40
             (aref glyph 8) #x40 (aref glyph 9) #x40))
      (81 ; Q
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x54
             (aref glyph 8) #x48 (aref glyph 9) #x34))
      (82 ; R
       (setf (aref glyph 2) #x78 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x78 (aref glyph 6) #x50 (aref glyph 7) #x48
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (83 ; S
       (setf (aref glyph 2) #x38 (aref glyph 3) #x44 (aref glyph 4) #x40
             (aref glyph 5) #x38 (aref glyph 6) #x04 (aref glyph 7) #x04
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (84 ; T
       (setf (aref glyph 2) #x7C (aref glyph 3) #x10 (aref glyph 4) #x10
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x10))
      (85 ; U
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x38))
      (86 ; V
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x44 (aref glyph 6) #x28 (aref glyph 7) #x28
             (aref glyph 8) #x10 (aref glyph 9) #x10))
      (87 ; W
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x44
             (aref glyph 5) #x54 (aref glyph 6) #x54 (aref glyph 7) #x54
             (aref glyph 8) #x54 (aref glyph 9) #x28))
      (88 ; X
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x28
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x28
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (89 ; Y
       (setf (aref glyph 2) #x44 (aref glyph 3) #x44 (aref glyph 4) #x28
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x10))
      (90 ; Z
       (setf (aref glyph 2) #x7C (aref glyph 3) #x04 (aref glyph 4) #x08
             (aref glyph 5) #x10 (aref glyph 6) #x20 (aref glyph 7) #x40
             (aref glyph 8) #x40 (aref glyph 9) #x7C))
      ;; Lowercase a-z (same as uppercase but shifted down 2 rows and smaller)
      (97 ; a
       (setf (aref glyph 4) #x38 (aref glyph 5) #x44 (aref glyph 6) #x04
             (aref glyph 7) #x3C (aref glyph 8) #x44 (aref glyph 9) #x44
             (aref glyph 10) #x44 (aref glyph 11) #x3C))
      (98 ; b
       (setf (aref glyph 2) #x40 (aref glyph 3) #x40 (aref glyph 4) #x78
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x78))
      (99 ; c
       (setf (aref glyph 4) #x38 (aref glyph 5) #x44 (aref glyph 6) #x40
             (aref glyph 7) #x40 (aref glyph 8) #x40 (aref glyph 9) #x44
             (aref glyph 10) #x38))
      (100 ; d
       (setf (aref glyph 2) #x04 (aref glyph 3) #x04 (aref glyph 4) #x3C
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x3C))
      (101 ; e
       (setf (aref glyph 4) #x38 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x7C (aref glyph 8) #x40 (aref glyph 9) #x44
             (aref glyph 10) #x38))
      (102 ; f
       (setf (aref glyph 2) #x18 (aref glyph 3) #x20 (aref glyph 4) #x7C
             (aref glyph 5) #x20 (aref glyph 6) #x20 (aref glyph 7) #x20
             (aref glyph 8) #x20 (aref glyph 9) #x20))
      (103 ; g
       (setf (aref glyph 4) #x3C (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x3C (aref glyph 9) #x04
             (aref glyph 10) #x38 (aref glyph 11) #x44 (aref glyph 12) #x38))
      (104 ; h
       (setf (aref glyph 2) #x40 (aref glyph 3) #x40 (aref glyph 4) #x78
             (aref glyph 5) #x44 (aref glyph 6) #x44 (aref glyph 7) #x44
             (aref glyph 8) #x44 (aref glyph 9) #x44))
      (105 ; i
       (setf (aref glyph 2) #x10 (aref glyph 3) #x00 (aref glyph 4) #x30
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x38))
      (106 ; j
       (setf (aref glyph 2) #x08 (aref glyph 3) #x00 (aref glyph 4) #x18
             (aref glyph 5) #x08 (aref glyph 6) #x08 (aref glyph 7) #x08
             (aref glyph 8) #x48 (aref glyph 9) #x48 (aref glyph 10) #x30))
      (107 ; k
       (setf (aref glyph 2) #x40 (aref glyph 3) #x40 (aref glyph 4) #x48
             (aref glyph 5) #x50 (aref glyph 6) #x60 (aref glyph 7) #x50
             (aref glyph 8) #x48 (aref glyph 9) #x44))
      (108 ; l
       (setf (aref glyph 2) #x30 (aref glyph 3) #x10 (aref glyph 4) #x10
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x38))
      (109 ; m
       (setf (aref glyph 4) #x6C (aref glyph 5) #x54 (aref glyph 6) #x54
             (aref glyph 7) #x54 (aref glyph 8) #x54 (aref glyph 9) #x44))
      (110 ; n
       (setf (aref glyph 4) #x78 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x44 (aref glyph 9) #x44))
      (111 ; o
       (setf (aref glyph 4) #x38 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x44 (aref glyph 9) #x38))
      (112 ; p
       (setf (aref glyph 4) #x78 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x78 (aref glyph 9) #x40
             (aref glyph 10) #x40 (aref glyph 11) #x40))
      (113 ; q
       (setf (aref glyph 4) #x3C (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x3C (aref glyph 9) #x04
             (aref glyph 10) #x04 (aref glyph 11) #x04))
      (114 ; r
       (setf (aref glyph 4) #x78 (aref glyph 5) #x44 (aref glyph 6) #x40
             (aref glyph 7) #x40 (aref glyph 8) #x40 (aref glyph 9) #x40))
      (115 ; s
       (setf (aref glyph 4) #x38 (aref glyph 5) #x44 (aref glyph 6) #x40
             (aref glyph 7) #x38 (aref glyph 8) #x04 (aref glyph 9) #x44
             (aref glyph 10) #x38))
      (116 ; t
       (setf (aref glyph 2) #x10 (aref glyph 3) #x10 (aref glyph 4) #x7C
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x18))
      (117 ; u
       (setf (aref glyph 4) #x44 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x44 (aref glyph 9) #x3C))
      (118 ; v
       (setf (aref glyph 4) #x44 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x28 (aref glyph 8) #x28 (aref glyph 9) #x10))
      (119 ; w
       (setf (aref glyph 4) #x44 (aref glyph 5) #x44 (aref glyph 6) #x54
             (aref glyph 7) #x54 (aref glyph 8) #x54 (aref glyph 9) #x28))
      (120 ; x
       (setf (aref glyph 4) #x44 (aref glyph 5) #x44 (aref glyph 6) #x28
             (aref glyph 7) #x10 (aref glyph 8) #x28 (aref glyph 9) #x44
             (aref glyph 10) #x44))
      (121 ; y
       (setf (aref glyph 4) #x44 (aref glyph 5) #x44 (aref glyph 6) #x44
             (aref glyph 7) #x44 (aref glyph 8) #x3C (aref glyph 9) #x04
             (aref glyph 10) #x38 (aref glyph 11) #x44 (aref glyph 12) #x38))
      (122 ; z
       (setf (aref glyph 4) #x7C (aref glyph 5) #x04 (aref glyph 6) #x08
             (aref glyph 7) #x10 (aref glyph 8) #x20 (aref glyph 9) #x40
             (aref glyph 10) #x7C))
      ;; Punctuation
      (91 ; [
       (setf (aref glyph 2) #x38 (aref glyph 3) #x20 (aref glyph 4) #x20
             (aref glyph 5) #x20 (aref glyph 6) #x20 (aref glyph 7) #x20
             (aref glyph 8) #x20 (aref glyph 9) #x38))
      (92 ; backslash
       (setf (aref glyph 0) #x80 (aref glyph 1) #x40 (aref glyph 2) #x20
             (aref glyph 3) #x10 (aref glyph 4) #x08 (aref glyph 5) #x04))
      (93 ; ]
       (setf (aref glyph 2) #x38 (aref glyph 3) #x08 (aref glyph 4) #x08
             (aref glyph 5) #x08 (aref glyph 6) #x08 (aref glyph 7) #x08
             (aref glyph 8) #x08 (aref glyph 9) #x38))
      (94 ; ^
       (setf (aref glyph 1) #x10 (aref glyph 2) #x28 (aref glyph 3) #x44))
      (95 ; _
       (setf (aref glyph 13) #x7C))
      (96 ; `
       (setf (aref glyph 1) #x20 (aref glyph 2) #x10))
      (123 ; {
       (setf (aref glyph 2) #x0C (aref glyph 3) #x10 (aref glyph 4) #x10
             (aref glyph 5) #x20 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x0C))
      (124 ; |
       (setf (aref glyph 2) #x10 (aref glyph 3) #x10 (aref glyph 4) #x10
             (aref glyph 5) #x10 (aref glyph 6) #x10 (aref glyph 7) #x10
             (aref glyph 8) #x10 (aref glyph 9) #x10))
      (125 ; }
       (setf (aref glyph 2) #x30 (aref glyph 3) #x08 (aref glyph 4) #x08
             (aref glyph 5) #x04 (aref glyph 6) #x08 (aref glyph 7) #x08
             (aref glyph 8) #x08 (aref glyph 9) #x30))
      (126 ; ~
       (setf (aref glyph 5) #x28 (aref glyph 6) #x54))
      (t
       ;; Unknown: draw a filled block
       (setf (aref glyph 3) #x7C (aref glyph 4) #x44 (aref glyph 5) #x44
             (aref glyph 6) #x44 (aref glyph 7) #x44 (aref glyph 8) #x44
             (aref glyph 9) #x7C)))
    glyph))

(defun launcher-draw-text (buffer width text x y r g b)
  "Draw TEXT at (X, Y) in BUFFER with color (R, G, B).
Uses the 8x16 bitmap font. Each char is 8px wide, 16px tall."
  (unless (gethash 32 *font-8x16*)
    (init-font))
  (loop for char across text
        for i from 0
        for gx = (+ x (* i 8))
        while (< (+ gx 8) width) do
    (let* ((code (char-code char))
           (glyph (or (gethash code *font-8x16*)
                      (gethash 63 *font-8x16*))))  ; '?' fallback
      (loop for row from 0 below 16
            for py = (+ y row)
            while (< py (array-dimension buffer 0)) do  ; bounds check via buffer
            (let ((bits (aref glyph row)))
            (loop for col from 0 below 8
                  for px = (+ gx col)
                  while (< px width) do
              (when (logbitp (- 7 col) bits)
                (let ((offset (* (+ (* py width) px) 4)))
                  (when (< offset (- (length buffer) 4))
                    (setf (aref buffer offset) b
                          (aref buffer (1+ offset)) g
                          (aref buffer (+ offset 2)) r
                          (aref buffer (+ offset 3)) 255))))))))))

;;; --- Shared memory buffer ---

(defvar *launcher-shm-fd* nil)
(defvar *launcher-shm-ptr* nil)
(defvar *launcher-shm-size* 0)
(defvar *launcher-wl-pool* nil)
(defvar *launcher-wl-buffer* nil)

(defun launcher-create-shm (width height)
  "Create a shared memory buffer for the launcher surface.
Sets up *launcher-shm-fd*, *launcher-shm-ptr*, *launcher-wl-pool*,
*launcher-wl-buffer*. Returns the Lisp buffer vector."
  (let* ((size (* width height 4))
         (filename (format nil "rhizome-launcher-~A-~A"
                           (get-universal-time) (random 100000)))
         (path (concatenate 'string "/dev/shm/" filename))
         (fd (cffi:foreign-funcall "open"
               :string path
               :int (logior 2 64)  ; O_RDWR | O_CREAT
               :int 384              ; 0600
               :int)))
    (when (< fd 0)
      (let ((errno (cffi:foreign-funcall "__errno_location" :pointer)))
        (format t "~&; Rhizome: failed to create shm file, errno=~A path=~A~%"
                (cffi:mem-aref errno :int) path))
      (return-from launcher-create-shm nil))
    ;; Unlink the file (it persists as shared memory until munmap)
    (cffi:foreign-funcall "unlink" :string path :int)
    ;; Truncate to desired size
    (cffi:foreign-funcall "ftruncate" :int fd :long size :int)
    ;; mmap
    (let ((ptr (cffi:foreign-funcall "mmap"
                  :pointer (cffi:null-pointer)
                  :unsigned-long size
                  :int 3              ; PROT_READ | PROT_WRITE
                  :int 1              ; MAP_SHARED
                  :int fd
                  :long 0
                  :pointer)))
      (when (cffi:null-pointer-p ptr)
        (format t "~&; Rhizome: mmap failed~%")
        (cffi:foreign-funcall "close" :int fd :int)
        (return-from launcher-create-shm nil))
      (setf *launcher-shm-fd* fd
            *launcher-shm-ptr* ptr
            *launcher-shm-size* size)
      ;; Create wl_shm_pool and wl_buffer
      (setf *launcher-wl-pool* (wl:wl-shm.create-pool *shm* fd size))
      (setf *launcher-wl-buffer* (wl:wl-shm-pool.create-buffer
                                   *launcher-wl-pool* 0 width height
                                   (* width 4)  ; stride
                                   :argb8888))  ; premultiplied alpha
      t)))

(defun launcher-copy-to-shm (buffer)
  "Copy Lisp BUFFER contents to the shared memory mmap'd region."
  (when (and *launcher-shm-ptr* (not (cffi:null-pointer-p *launcher-shm-ptr*)))
    (loop for i below (length buffer) do
      (setf (cffi:mem-aref *launcher-shm-ptr* :unsigned-char i) (aref buffer i)))))

(defun launcher-surface-alpha ()
  "The launcher surface's alpha byte ((:launcher :opacity 1.0 =
opaque). The buffer is premultiplied ARGB — callers scale colors
by this."
  (round (* 255 (min 1.0 (max 0.0
            (config-getf '(:launcher) :opacity :default 0.9))))))

(defun launcher-set-pixel (ptr width height px py b g r a)
  "Set pixel at (PX, PY) to BGRA, premultiplying RGB by A (the
buffer is premultiplied ARGB per the Wayland requirement). The
offset comes from surface-index — the ONE row-major formula in
the codebase, bounds-checked; out-of-range writes are no-ops."
  (let ((idx (surface-index width height px py)))
    (unless (null idx)
      (let ((offset (* 4 idx)))
        (setf (cffi:mem-aref ptr :unsigned-char offset)
                (round (* b a) 255)
              (cffi:mem-aref ptr :unsigned-char (1+ offset))
                (round (* g a) 255)
              (cffi:mem-aref ptr :unsigned-char (+ offset 2))
                (round (* r a) 255)
              (cffi:mem-aref ptr :unsigned-char (+ offset 3)) a)))))

(defun launcher-clear-shm (ptr width height b g r a)
  "Fill the entire shm buffer with a solid, translucent-capable
BGRA color (premultiplied: RGB scaled by A)."
  (let ((pixel (logior (ash a 24)
                       (ash (round (* r a) 255) 16)
                       (ash (round (* g a) 255) 8)
                       (round (* b a) 255))))
    (loop for i below (* width height) do
      (setf (cffi:mem-aref ptr :unsigned-int i) pixel))))

(defun launcher-draw-text-shm (ptr width text x y r g b)
  "Draw TEXT at (X, Y) directly to shm with color (R, G, B).
Delegates to the font module (TrueType or the 8x16 bitmap
fallback); the buffer is premultiplied ARGB, so the text color is
scaled by the launcher's surface alpha down in the font module."
  (font-ensure-initialized)
  (font-draw-text ptr width *launcher-height* text x y r g b
                  :alpha (launcher-surface-alpha)))

(defun launcher-icon-size ()
  "The launcher's rendered icon size ((:launcher :icon-size))."
  (config-getf '(:launcher) :icon-size :default 20))

(defun launcher-icon-spacing ()
  "Pixels between an entry's icon and its text."
  8)

(defun launcher-draw-icon (ptr width height spec x y)
  "Draw SPEC's icon (name or path) at (X, Y), alpha-blended into
the premultiplied launcher surface, vertically centered on the
row. Reuses the bar's icon discovery/scaling (bar.lisp)."
  (let* ((size (launcher-icon-size))
         (icon (load-icon spec :size size)))
    (when icon
      (destructuring-bind (iw ih idata) icon
        (declare (ignore iw))
        (let ((sa (launcher-surface-alpha)))
          (loop for iy below ih
                for py = (+ y iy)
                while (< py height) do
            (loop for ix below size
                  for px = (+ x ix)
                  while (< px width) do
              (let* ((sidx (* 4 (+ (* iy size) ix)))
                     (a (row-major-aref idata (+ sidx 3))))
                (when (plusp a)
                  (let ((offset (* 4 (+ (* py width) px))))
                    (flet ((over (idx c)
                             ;; straight RGBA icon over the premultiplied
                             ;; surface: the fg scales by BOTH alphas
                             ;; (icon edge + surface) — divide by 255
                             ;; twice; a single division let values past
                             ;; 255 and type-errored the byte store
                             (let ((bg (cffi:mem-aref
                                         ptr :unsigned-char
                                         (+ offset idx)))
                                   (fgp (round (* c a sa) 65025)))
                               (setf (cffi:mem-aref
                                       ptr :unsigned-char (+ offset idx))
                                     (min 255
                                          (max 0
                                               (round (+ (* bg (- 255 a))
                                                         (* fgp 255))
                                                      255)))))))
                      ;; straight RGBA over premultiplied bg
                      (over 0 (row-major-aref idata (+ sidx 2)))
                      (over 1 (row-major-aref idata (+ sidx 1)))
                      (over 2 (row-major-aref idata sidx)))))))))))))

(defun launcher-color (key)
  "A launcher surface color as (values r g b), from (:launcher KEY)."
  (parse-hex-color
    (config-getf '(:launcher) key
                 :default (case key
                            (:background "#23142d")
                            (:accent "#7aa2f7")
                            (:prompt-color "#8c64aa")
                            (:query-color "#dcc8f0")
                            (:cursor-color "#b4a0d2")
                            (:selection-background "#503278")
                            (:selected-color "#f0e6ff")
                            (:entry-color "#aa96c8")
                            (:border-color "#4a3a66")
                            (:placeholder-color "#6b5a8a")
                            (otherwise "#ffffff")))))

(defun launcher-corner-radius ()
  "The launcher's corner radius (px) — the fuzzel look."
  (config-getf '(:launcher) :corner-radius :default 12))

(defun launcher-round-test (x y width height radius)
  "For a pixel (X Y) in a WIDTHxHEIGHT surface with corner RADIUS:
(values inside-p border-p). Corners are quarter circles; the
border is the 1px ring at the rounded rect's edge."
  (let ((cx (cond ((< x radius) radius)
                  ((> x (- width radius 1)) (- width radius 1))
                  (t x)))
        (cy (cond ((< y radius) radius)
                  ((> y (- height radius 1)) (- height radius 1))
                  (t y)))
        (edge (or (= x 0) (= x (1- width))
                  (= y 0) (= y (1- height)))))
    ;; when the pixel is inside the corner squares, the clamp above
    ;; moved (cx,cy) to the arc center: outside the arc = outside
    (let* ((in-corner (or (and (< x radius) (< y radius))
                          (and (< x radius) (> y (- height radius 1)))
                          (and (> x (- width radius 1)) (< y radius))
                          (and (> x (- width radius 1))
                               (> y (- height radius 1)))))
           (d (sqrt (+ (expt (- cx x) 2) (expt (- cy y) 2)))))
      (cond
        ((and in-corner (> d radius)) (values nil nil))
        ((or edge (and in-corner (>= d (- radius 1.5)))) (values t t))
        (t (values t nil))))))

(defun launcher-paint-background (ptr width height r g b)
  "The fuzzel look: a rounded-corner fill with a 1px hairline
border, alpha-cut at the corners. Premultiplied ARGB."
  (multiple-value-bind (brd-r brd-g brd-b) (launcher-color :border-color)
    (let ((a (launcher-surface-alpha))
          (radius (launcher-corner-radius)))
      (loop for y below height
            do (loop for x below width
                     do (multiple-value-bind (inside border-p)
                            (launcher-round-test x y width height radius)
                          (when inside
                            (if border-p
                                (launcher-set-pixel ptr width height x y
                                                     brd-b brd-g brd-r a)
                                (launcher-set-pixel ptr width height x y
                                                     b g r a)))))))))

(defun launcher-render-to-shm (width height)
  "Render the launcher UI directly to the shared memory buffer.
All colors come from the (:launcher ...) config group; text goes
through the font module (see font.lisp)."
  (let ((ptr *launcher-shm-ptr*))
    (when (or (not ptr) (cffi:null-pointer-p ptr))
      (return-from launcher-render-to-shm))
    (font-ensure-initialized)
    (multiple-value-bind (br bg bb) (launcher-color :background)
      (launcher-paint-background ptr width height br bg bb))
      (multiple-value-bind (pr pg pb) (launcher-color :prompt-color)
        (multiple-value-bind (qr qg qb) (launcher-color :query-color)
          (multiple-value-bind (cr cg cb) (launcher-color :cursor-color)
            (let ((prompt-text "Run: ")
                  (query-x (+ *launcher-padding*
                              (font-text-width "Run: "))))
              ;; Draw query prompt line
              (launcher-draw-text-shm ptr width prompt-text
                                      *launcher-padding* *launcher-padding*
                                      pr pg pb)
              (launcher-draw-text-shm ptr width *launcher-query*
                                      query-x *launcher-padding*
                                      qr qg qb)
              ;; Draw cursor (block) after query
              (let ((cursor-x (+ query-x (font-text-width *launcher-query*)))
                    (cursor-y *launcher-padding*))
                (loop for dy below (font-line-height)
                      for py = (+ cursor-y dy)
                      while (< py height) do
                  (loop for dx below 8
                        for px = (+ cursor-x dx)
                        while (< px width) do
                    (launcher-set-pixel ptr width height px py
                                         cb cg cr (launcher-surface-alpha)))))
              ;; Placeholder: the dim hint when no query yet
              (when (string= *launcher-query* "")
                (multiple-value-bind (phr phg phb)
                    (launcher-color :placeholder-color)
                  (launcher-draw-text-shm
                    ptr width "type to search..."
                    (+ query-x 12) *launcher-padding* phr phg phb)))
              ;; Match count, top-right
              (multiple-value-bind (phr phg phb)
                  (launcher-color :placeholder-color)
                (let ((count-text (format nil "~D of ~D"
                                          (length *launcher-entries*)
                                          (length *launcher-app-list*))))
                  (launcher-draw-text-shm
                    ptr width count-text
                    (- width *launcher-padding*
                       (font-text-width count-text))
                    *launcher-padding* phr phg phb)))
              ;; Draw filtered entries (scrolled)
              (multiple-value-bind (selr selg selb)
                  (launcher-color :selection-background)
                (multiple-value-bind (str stg stb) (launcher-color :selected-color)
                  (multiple-value-bind (etr etg etb) (launcher-color :entry-color)
                    (let ((y-start (+ *launcher-padding* *launcher-line-height*)))
                      (loop for entry in (nthcdr *launcher-scroll-offset* *launcher-entries*)
                            for i from *launcher-scroll-offset*
                            for y = (+ y-start (* (- i *launcher-scroll-offset*) *launcher-line-height*))
                            while (< (+ y *launcher-line-height*) height) do
                        ;; Highlight selected entry: a rounded PILL
                        ;; (the fuzzel selection), inset from the
                        ;; padding, 6px radius
                        (when (= i *launcher-selection*)
                          (let* ((pill-x (- *launcher-padding* 6))
                                 (pill-w (- width (* 2 pill-x)))
                                 (pill-r 6))
                            (loop for dy below *launcher-line-height*
                                  for py = (+ y dy)
                                  while (< py height) do
                              (loop for dx below pill-w
                                    for px = (+ pill-x dx)
                                    while (< px width) do
                                (multiple-value-bind (inside border-p)
                                    (launcher-round-test dx dy pill-w
                                                         *launcher-line-height*
                                                         pill-r)
                                  (declare (ignore border-p))
                                  (when (and inside
                                             (< dy (- *launcher-line-height* 2))
                                             (> dy 1))
                                    (launcher-set-pixel
                                      ptr width height px py
                                      selb selg selr
                                      (launcher-surface-alpha))))))))
                        ;; Draw entry text, centered in the row, past
                        ;; the entry's icon (aligned even when an
                        ;; entry has no icon). The typed query is
                        ;; highlighted inside the label (accent).
                        (multiple-value-bind (tr tg tb)
                            (if (= i *launcher-selection*)
                                (values str stg stb)
                                (values etr etg etb))
                          (launcher-draw-icon
                            ptr width height (third entry)
                            *launcher-padding*
                            (+ y (floor (- *launcher-line-height*
                                           (launcher-icon-size))
                                        2)))
                          (let* ((text-x (+ *launcher-padding*
                                            (launcher-icon-size)
                                            (launcher-icon-spacing)))
                                 (text-y (+ y (floor (- *launcher-line-height*
                                                        (font-line-height))
                                                     2)))
                                 (label (car entry))
                                 (match-pos (and (plusp (length *launcher-query*))
                                                 (search *launcher-query* label
                                                         :test #'char-equal))))
                            (if match-pos
                              (let ((mlen (length *launcher-query*)))
                                (multiple-value-bind (acr acg acb)
                                    (launcher-color :accent)
                                  ;; before / match / after segments
                                  (launcher-draw-text-shm ptr width
                                    (subseq label 0 match-pos)
                                    text-x text-y tr tg tb)
                                  (let ((mx (+ text-x
                                               (font-text-width
                                                 (subseq label 0 match-pos)))))
                                    (launcher-draw-text-shm ptr width
                                      (subseq label match-pos
                                              (+ match-pos mlen))
                                      mx text-y acr acg acb)
                                    (launcher-draw-text-shm ptr width
                                      (subseq label (+ match-pos mlen))
                                      (+ mx (font-text-width
                                              (subseq label match-pos
                                                      (+ match-pos mlen))))
                                      text-y tr tg tb))))
                              ;; no match to highlight (fuzzy hits)
                              (launcher-draw-text-shm ptr width label
                                                      text-x text-y
                                                      tr tg tb)))))))))))))))

(defun launcher-destroy-shm ()
  "Destroy the shared memory resources."
  (when *launcher-wl-buffer*
    (wl:wl-buffer.destroy *launcher-wl-buffer*)
    (setf *launcher-wl-buffer* nil))
  (when *launcher-wl-pool*
    (wl:wl-shm-pool.destroy *launcher-wl-pool*)
    (setf *launcher-wl-pool* nil))
  (when (and *launcher-shm-ptr* (not (cffi:null-pointer-p *launcher-shm-ptr*)))
    (cffi:foreign-funcall "munmap" :pointer *launcher-shm-ptr* :unsigned-long *launcher-shm-size* :int)
    (setf *launcher-shm-ptr* nil))
  (when *launcher-shm-fd*
    (cffi:foreign-funcall "close" :int *launcher-shm-fd* :int)
    (setf *launcher-shm-fd* nil))
  (setf *launcher-shm-size* 0))

;;; --- Launcher show/hide ---

(defun launcher-configure-handler (serial width height)
  "Handle layer surface configure event."
  (format t "~&; Rhizome [debug]: launcher configure ~Ax~A serial ~A~%" width height serial)
  ;; Use compositor-suggested size or our requested size
  (let ((w (if (> width 0) width *launcher-width*))
        (h (if (> height 0) height *launcher-height*)))
    (setf *launcher-width* w *launcher-height* h))
  ;; Ack the configure
  (rwm:zwlr-layer-surface-v1.ack-configure *launcher-layer-surface* serial)
  ;; Now we can render and attach buffer
  (launcher-redraw))

(defun launcher-ensure-shm (width height)
  "Ensure shm buffer exists for the given dimensions. Recreates if size changed."
  (let ((needed-size (* width height 4)))
    (cond
      ;; No shm yet, or size changed — create new
      ((or (not *launcher-shm-ptr*)
           (cffi:null-pointer-p *launcher-shm-ptr*)
           (/= *launcher-shm-size* needed-size))
       (launcher-destroy-shm)
       (launcher-create-shm width height))
      ;; Shm exists and is the right size — reuse
      (t t))))

(defun launcher-redraw ()
  "Render the launcher and commit the surface."
  (unless *launcher-active*
    (return-from launcher-redraw))
  (launcher-update-scroll *launcher-height*)
  ;; Reuse shm buffer if size hasn't changed
  (unless (launcher-ensure-shm *launcher-width* *launcher-height*)
    (format t "~&; Rhizome: failed to create shm buffer~%")
    (return-from launcher-redraw))
  ;; Render directly to shared memory
  (launcher-render-to-shm *launcher-width* *launcher-height*)
  ;; Attach buffer to surface, mark damage, commit
  (wl:wl-surface.attach *launcher-surface* *launcher-wl-buffer* 0 0)
  (wl:wl-surface.damage *launcher-surface* 0 0 *launcher-width* *launcher-height*)
  (wl:wl-surface.commit *launcher-surface*))

(defun launcher-destroy-backdrop ()
  "Tear down the launcher's backdrop dim."
  (handler-case
      (progn
        (when *launcher-backdrop-buffer*
          (wl:wl-buffer.destroy *launcher-backdrop-buffer*))
        (when *launcher-backdrop-pool*
          (wl:wl-shm-pool.destroy *launcher-backdrop-pool*))
        (when (and *launcher-backdrop-ptr*
                   (not (cffi:null-pointer-p *launcher-backdrop-ptr*)))
          (cffi:foreign-funcall "munmap" :pointer *launcher-backdrop-ptr*
                                :unsigned-long *launcher-backdrop-size* :int))
        (when *launcher-backdrop-fd*
          (cffi:foreign-funcall "close" :int *launcher-backdrop-fd* :int)))
    (error () nil))
  (handler-case
      (when *launcher-backdrop-layer-surface*
        (rwm:zwlr-layer-surface-v1.destroy *launcher-backdrop-layer-surface*)))
  (handler-case
      (when *launcher-backdrop-surface*
        (wl:wl-surface.destroy *launcher-backdrop-surface*)))
  (setf *launcher-backdrop-surface* nil
        *launcher-backdrop-layer-surface* nil
        *launcher-backdrop-ptr* nil
        *launcher-backdrop-fd* nil
        *launcher-backdrop-size* 0
        *launcher-backdrop-pool* nil
        *launcher-backdrop-buffer* nil))

(defun launcher-show-backdrop ()
  "The fullscreen dim under the launcher: a ~20%-black layer
surface, created BEFORE the launcher (stacks below it in the same
:top layer). Input-transparent: clicks pass through to windows."
  (let ((dim-alpha (config-getf '(:launcher) :dim-alpha :default 60)))
    (when (and *compositor* *shm* *layer-shell* (plusp dim-alpha))
      (let ((surface (wl:wl-compositor.create-surface *compositor*)))
        (setf *launcher-backdrop-surface* surface
              *launcher-backdrop-layer-surface*
              (rwm:zwlr-layer-shell-v1.get-layer-surface
                *layer-shell* surface nil :top "rhizome-launcher-backdrop"))
        (push (lambda (event-name &rest args)
                (when (eq event-name :configure)
                  (destructuring-bind (serial width height) args
                    (rwm:zwlr-layer-surface-v1.ack-configure
                      *launcher-backdrop-layer-surface* serial)
                    (when (and (> width 0) (> height 0))
                      ;; one-time buffer: fill with dim black
                      (let ((needed (* width height 4)))
                        (setf *launcher-backdrop-fd*
                              (cffi:foreign-funcall "memfd_create"
                                                    :string "dim" :int 0 :int))
                        (cffi:foreign-funcall "ftruncate" :int *launcher-backdrop-fd*
                                              :long needed :int)
                        (setf *launcher-backdrop-ptr*
                              (cffi:foreign-funcall "mmap"
                                                    :pointer (cffi:null-pointer)
                                                    :unsigned-long needed
                                                    :int 3 :int 1
                                                    :int *launcher-backdrop-fd*
                                                    :long 0 :pointer))
                        (setf *launcher-backdrop-size* needed
                              *launcher-backdrop-pool*
                              (wl:wl-shm.create-pool *shm*
                                                      *launcher-backdrop-fd*
                                                      needed)
                              *launcher-backdrop-buffer*
                              (wl:wl-shm-pool.create-buffer
                                *launcher-backdrop-pool* 0 width height
                                (* width 4) :argb8888))
                      (let* ((a dim-alpha)
                             (pixel (ash a 24)))
                        (loop for i below (* width height) do
                          (setf (cffi:mem-aref *launcher-backdrop-ptr*
                                               :unsigned-int i) pixel)))
                      ;; transparent to input: clicks pass through
                      (let ((region (wl:wl-compositor.create-region
                                      *compositor*)))
                        (wl:wl-surface.set-input-region surface region)
                        (wl:wl-region.destroy region))
                      (wl:wl-surface.attach surface *launcher-backdrop-buffer*
                                            0 0)
                      (wl:wl-surface.damage surface 0 0 width height)
                      (wl:wl-surface.commit surface))))))
              (wl:wl-proxy-hooks *launcher-backdrop-layer-surface*))
        ;; no anchor + size 0 0: the compositor gives the full
        ;; output on configure
        (rwm:zwlr-layer-surface-v1.set-size *launcher-backdrop-layer-surface* 0 0)
        (wl:wl-surface.commit surface)))))

(defun show-launcher ()
  "Show the native launcher popup."
  (format t "~&; Rhizome [debug]: show-launcher called, active=~A~%" *launcher-active*)
  (force-output)
  (when *launcher-active*
    (return-from show-launcher))
  (format t "~&; Rhizome [debug]: compositor=~A shm=~A layer-shell=~A~%"
          *compositor* *shm* *layer-shell*)
  (force-output)
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Rhizome: missing compositor/shm/layer-shell, cannot show launcher~%")
    (return-from show-launcher))
  ;; Rescan .desktop files on every open: fresh installs (pacman -S
  ;; variety ...) appear on the next launcher open, no restart
  (launcher-refresh-app-list)
  (launcher-history-load)
  (setf *launcher-query* ""
        *launcher-selection* 0
        *launcher-scroll-offset* 0
        *launcher-entries* (launcher-filter-entries))
  (format t "~&; Rhizome: showing native launcher~%")
  (force-output)
  (unwind-protect
       (progn
         ;; Backdrop first: created earlier = stacks BELOW the
         ;; launcher within the :top layer
         (launcher-show-backdrop)
         ;; Create wl_surface
         (setf *launcher-surface* (wl:wl-compositor.create-surface *compositor*))
         ;; Create layer surface (output=nil = let compositor choose, layer=2 = top)
         (setf *launcher-layer-surface*
               (rwm:zwlr-layer-shell-v1.get-layer-surface
                 *layer-shell* *launcher-surface* nil :top "rhizome-launcher"))
         ;; Set up event handler for configure
         (push (lambda (event-name &rest args)
                 (case event-name
                   (:configure
                    (destructuring-bind (serial width height) args
                      (launcher-configure-handler serial width height)))
                   (:closed
                    (format t "~&; Rhizome: launcher surface closed~%")
                    (setf *launcher-active* nil))))
               (wl:wl-proxy-hooks *launcher-layer-surface*))
         ;; Set size, anchor (center = no anchor edges), keyboard interactivity
         (rwm:zwlr-layer-surface-v1.set-size *launcher-layer-surface*
           *launcher-width* *launcher-height*)
         (rwm:zwlr-layer-surface-v1.set-anchor *launcher-layer-surface* nil)  ; no anchor = centered
         (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity *launcher-layer-surface* :exclusive)
         ;; Mark active only after successful setup
         (setf *launcher-active* t)
         ;; Initial commit (no buffer) to trigger configure event
         (wl:wl-surface.commit *launcher-surface*))
    (unless *launcher-active*
      ;; Error occurred before we marked active - clean up
      (when *launcher-layer-surface*
        (rwm:zwlr-layer-surface-v1.destroy *launcher-layer-surface*)
        (setf *launcher-layer-surface* nil))
      (when *launcher-surface*
        (wl:wl-surface.destroy *launcher-surface*)
        (setf *launcher-surface* nil)))))

(defun hide-launcher ()
  "Hide the native launcher popup."
  (unless *launcher-active*
    (return-from hide-launcher))
  (setf *launcher-active* nil)
  (format t "~&; Rhizome: hiding launcher~%")
  ;; Destroy layer surface and surface
  (handler-case
      (progn
        (when *launcher-layer-surface*
          (rwm:zwlr-layer-surface-v1.destroy *launcher-layer-surface*))
        (when *launcher-surface*
          (wl:wl-surface.destroy *launcher-surface*)))
    (error (e)
      (format t "~&; Rhizome: error destroying launcher surface: ~A~%" e)))
  (setf *launcher-layer-surface* nil
        *launcher-surface* nil)
  (launcher-destroy-backdrop)
  (launcher-destroy-shm))

(defun launcher-handle-key (keysym modifiers)
  "Handle a key press while the launcher is active.
Returns T if the key was consumed, NIL if not."
  (declare (ignore modifiers))
  (cond
    ;; Escape - close launcher
    ((= keysym 65307)
     (hide-launcher)
     t)
    ;; Return - launch selected entry
    ((= keysym 65293)
     (let ((entry (nth *launcher-selection* *launcher-entries*)))
       (when entry
         (format t "~&; Rhizome: launching ~A~%" (second entry))
         ;; Frecency: this app goes to the top next time
         (handler-case (launcher-record-launch (car entry))
           (error () nil))
         (uiop:launch-program (list "bash" "-c" (second entry))))
       (hide-launcher))
     (launcher-redraw)
     t)
    ;; Backspace - remove last char from query
    ((= keysym 65288)
     (when (> (length *launcher-query*) 0)
       (setf *launcher-query* (subseq *launcher-query* 0 (1- (length *launcher-query*)))
             *launcher-selection* 0
             *launcher-entries* (launcher-filter-entries)))
     (launcher-redraw)
     t)
    ;; Up arrow - previous selection
    ((= keysym 65362)
     (when (> *launcher-selection* 0)
       (decf *launcher-selection*))
     (launcher-redraw)
     t)
    ;; Down arrow - next selection
    ((= keysym 65364)
     (when (< *launcher-selection* (1- (length *launcher-entries*)))
       (incf *launcher-selection*))
     (launcher-redraw)
     t)
    ;; Printable ASCII - add to query
    ((and (>= keysym 32) (<= keysym 126))
     (setf *launcher-query*
           (concatenate 'string *launcher-query* (string (code-char keysym)))
           *launcher-selection* 0
           *launcher-entries* (launcher-filter-entries))
     (launcher-redraw)
     t)
    (t nil)))
