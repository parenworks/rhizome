;;; Native Wayland application launcher for Stumpway
;;;
;;; Uses wlr-layer-shell to create an overlay popup, renders text
;;; to a shared memory buffer, and handles keyboard input directly.
;;;
;;; Inspired by fuzzel and the Lisp Machines' prompt interface.

(in-package #:stumpway)

(defvar *launcher-active* nil
  "Whether the launcher popup is currently shown.")

(defvar *launcher-surface* nil
  "The wl_surface for the launcher popup.")

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
(defvar *launcher-line-height* 28)
(defvar *launcher-padding* 12)

;;; --- Launcher entry list ---

(defvar *launcher-app-list* nil
  "List of (display-name . command) pairs loaded from .desktop files.")

(defun launcher-desktop-paths ()
  "Return list of directories to search for .desktop files.
Follows the XDG Desktop Entry specification."
  (let ((xdg-dirs '())
        (home (sb-ext:posix-getenv "HOME")))
    ;; XDG_DATA_HOME/applications
    (let ((xdg-data-home (sb-ext:posix-getenv "XDG_DATA_HOME")))
      (when (and xdg-data-home (string/= xdg-data-home ""))
        (push (merge-pathnames "applications/" xdg-data-home) xdg-dirs)))
    ;; ~/.local/share/applications
    (when home
      (push (merge-pathnames ".local/share/applications/" home) xdg-dirs))
    ;; XDG_DATA_DIRS/applications (system-wide)
    (let ((xdg-data-dirs (sb-ext:posix-getenv "XDG_DATA_DIRS")))
      (when (and xdg-data-dirs (string/= xdg-data-dirs ""))
        (dolist (dir (ppcre:split ":" xdg-data-dirs))
          (when (string/= dir "")
            (push (merge-pathnames "applications/" dir) xdg-dirs)))))
    ;; Fallback system dirs
    (push #P"/usr/local/share/applications/" xdg-dirs)
    (push #P"/usr/share/applications/" xdg-dirs)
    (remove-duplicates xdg-dirs :test #'equal)))

(defun launcher-parse-desktop-file (path)
  "Parse a .desktop file, return (display-name . command) if it should be shown.
Returns NIL for NoDisplay=true, Hidden=true, or Type != Application."
  (handler-case
      (with-open-file (f path :direction :input :external-format :utf-8)
        (let ((in-desktop-entry nil)
              (name nil)
              (exec nil)
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
              (cons name (if terminal
                             (format nil "alacritty --command ~A" clean-exec)
                             clean-exec))))))
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
  (format t "~&; Stumpway: loaded ~A desktop entries~%" (length *launcher-app-list*)))

(defun launcher-filter-entries ()
  "Filter *launcher-app-list* by the current query string (fuzzy:
characters in order, case-insensitive; substring matches rank
first)."
  (let ((q (string-downcase *launcher-query*)))
    (if (string= q "")
        (copy-list *launcher-app-list*)
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
         (filename (format nil "stumpway-launcher-~A-~A"
                           (get-universal-time) (random 100000)))
         (path (concatenate 'string "/dev/shm/" filename))
         (fd (cffi:foreign-funcall "open"
               :string path
               :int (logior 2 64)  ; O_RDWR | O_CREAT
               :int 384              ; 0600
               :int)))
    (when (< fd 0)
      (let ((errno (cffi:foreign-funcall "__errno_location" :pointer)))
        (format t "~&; Stumpway: failed to create shm file, errno=~A path=~A~%"
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
        (format t "~&; Stumpway: mmap failed~%")
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
                                   :xrgb8888))  ; WL_SHM_FORMAT_XRGB8888
      t)))

(defun launcher-copy-to-shm (buffer)
  "Copy Lisp BUFFER contents to the shared memory mmap'd region."
  (when (and *launcher-shm-ptr* (not (cffi:null-pointer-p *launcher-shm-ptr*)))
    (loop for i below (length buffer) do
      (setf (cffi:mem-aref *launcher-shm-ptr* :unsigned-char i) (aref buffer i)))))

(defun launcher-set-pixel (ptr width px py b g r a)
  "Set pixel at (px, py) in the shm buffer to BGRA."
  (let ((offset (* (+ (* py width) px) 4)))
    (setf (cffi:mem-aref ptr :unsigned-char offset) b
          (cffi:mem-aref ptr :unsigned-char (1+ offset)) g
          (cffi:mem-aref ptr :unsigned-char (+ offset 2)) r
          (cffi:mem-aref ptr :unsigned-char (+ offset 3)) a)))

(defun launcher-clear-shm (ptr width height b g r a)
  "Fill the entire shm buffer with a solid BGRA color."
  (let ((pixel (logior (ash a 24) (ash r 16) (ash g 8) b)))
    (loop for i below (* width height) do
      (setf (cffi:mem-aref ptr :unsigned-int i) pixel))))

(defun launcher-draw-text-shm (ptr width text x y r g b)
  "Draw TEXT at (X, Y) directly to shm with color (R, G, B)."
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
            while (< py *launcher-height*) do
            (let ((bits (aref glyph row)))
              (loop for col from 0 below 8
                    for px = (+ gx col)
                    while (< px width) do
                (when (logbitp (- 7 col) bits)
                  (launcher-set-pixel ptr width px py b g r 255))))))))

(defun launcher-render-to-shm (width height)
  "Render the launcher UI directly to the shared memory buffer."
  (let ((ptr *launcher-shm-ptr*))
    (when (or (not ptr) (cffi:null-pointer-p ptr))
      (return-from launcher-render-to-shm))
    ;; Clear to dark purple background
    (launcher-clear-shm ptr width height 35 20 45 255)
    ;; Draw query prompt line (muted purple)
    (launcher-draw-text-shm ptr width "Run: "
                             *launcher-padding* *launcher-padding*
                             140 100 170)
    ;; Draw query text (light lavender)
    (launcher-draw-text-shm ptr width *launcher-query*
                             (+ *launcher-padding* 48) *launcher-padding*
                             220 200 240)
    ;; Draw cursor (block) after query (light purple)
    (let ((cursor-x (+ *launcher-padding* 48
                       (* 8 (length *launcher-query*))))
          (cursor-y *launcher-padding*))
      (loop for dy below 16
            for py = (+ cursor-y dy)
            while (< py height) do
        (loop for dx below 8
              for px = (+ cursor-x dx)
              while (< px width) do
          (launcher-set-pixel ptr width px py 180 160 210 255))))
    ;; Draw filtered entries (scrolled)
    (let ((y-start (+ *launcher-padding* *launcher-line-height*)))
      (loop for entry in (nthcdr *launcher-scroll-offset* *launcher-entries*)
            for i from *launcher-scroll-offset*
            for y = (+ y-start (* (- i *launcher-scroll-offset*) *launcher-line-height*))
            while (< (+ y *launcher-line-height*) height) do
        ;; Highlight selected entry (purple)
        (when (= i *launcher-selection*)
          (loop for dy below *launcher-line-height*
                for py = (+ y dy)
                while (< py height) do
            (loop for dx below (- width (* 2 *launcher-padding*))
                  for px = (+ *launcher-padding* dx)
                  while (< px width) do
              (launcher-set-pixel ptr width px py 80 50 120 255))))
        ;; Draw entry text
        (multiple-value-bind (tr tg tb)
            (if (= i *launcher-selection*)
                (values 240 230 255)   ; selected: white-lavender
                (values 170 150 200))  ; unselected: muted purple
          (launcher-draw-text-shm ptr width (car entry)
                                   *launcher-padding* (+ y 4)
                                   tr tg tb))))))

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
  (format t "~&; Stumpway [debug]: launcher configure ~Ax~A serial ~A~%" width height serial)
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
    (format t "~&; Stumpway: failed to create shm buffer~%")
    (return-from launcher-redraw))
  ;; Render directly to shared memory
  (launcher-render-to-shm *launcher-width* *launcher-height*)
  ;; Attach buffer to surface, mark damage, commit
  (wl:wl-surface.attach *launcher-surface* *launcher-wl-buffer* 0 0)
  (wl:wl-surface.damage *launcher-surface* 0 0 *launcher-width* *launcher-height*)
  (wl:wl-surface.commit *launcher-surface*))

(defun show-launcher ()
  "Show the native launcher popup."
  (format t "~&; Stumpway [debug]: show-launcher called, active=~A~%" *launcher-active*)
  (force-output)
  (when *launcher-active*
    (return-from show-launcher))
  (format t "~&; Stumpway [debug]: compositor=~A shm=~A layer-shell=~A~%"
          *compositor* *shm* *layer-shell*)
  (force-output)
  (unless (and *compositor* *shm* *layer-shell*)
    (format t "~&; Stumpway: missing compositor/shm/layer-shell, cannot show launcher~%")
    (return-from show-launcher))
  ;; Scan .desktop files if not yet loaded
  (unless *launcher-app-list*
    (launcher-refresh-app-list))
  (setf *launcher-query* ""
        *launcher-selection* 0
        *launcher-scroll-offset* 0
        *launcher-entries* (launcher-filter-entries))
  (format t "~&; Stumpway: showing native launcher~%")
  (force-output)
  (unwind-protect
       (progn
         ;; Create wl_surface
         (setf *launcher-surface* (wl:wl-compositor.create-surface *compositor*))
         ;; Create layer surface (output=nil = let compositor choose, layer=2 = top)
         (setf *launcher-layer-surface*
               (rwm:zwlr-layer-shell-v1.get-layer-surface
                 *layer-shell* *launcher-surface* nil :top "stumpway-launcher"))
         ;; Set up event handler for configure
         (push (lambda (event-name &rest args)
                 (case event-name
                   (:configure
                    (destructuring-bind (serial width height) args
                      (launcher-configure-handler serial width height)))
                   (:closed
                    (format t "~&; Stumpway: launcher surface closed~%")
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
  (format t "~&; Stumpway: hiding launcher~%")
  ;; Destroy layer surface and surface
  (handler-case
      (progn
        (when *launcher-layer-surface*
          (rwm:zwlr-layer-surface-v1.destroy *launcher-layer-surface*))
        (when *launcher-surface*
          (wl:wl-surface.destroy *launcher-surface*)))
    (error (e)
      (format t "~&; Stumpway: error destroying launcher surface: ~A~%" e)))
  (setf *launcher-layer-surface* nil
        *launcher-surface* nil)
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
         (format t "~&; Stumpway: launching ~A~%" (cdr entry))
         (uiop:launch-program (list "bash" "-c" (cdr entry))))
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
