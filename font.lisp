;;; Anti-aliased font rendering for rhizome's drawn surfaces
;;;
;;; The bar, message window, prompts and launcher all blit text into
;;; wl_shm BGRA buffers. Historically that meant a built-in 8x16
;;; bitmap font. This module adds real TrueType rendering through
;;; libfreetype (already a transitive cffi user), resolved via
;;; fontconfig, with per-character glyph caching:
;;;
;;;   (rhizome:configure
;;;     '(:theme (:font-family "JetBrains Mono" :font-size 12)))
;;;
;;; The configured family goes through `fc-match`, so generic names
;;; work ("monospace", "sans-serif") and a missing family falls back
;;; to the system's best match rather than failing. If freetype,
;;; fontconfig, or the font file is unavailable for any reason, the
;;; bitmap font keeps working with its original metrics — the
;;; fallback is pixel-identical to the old renderer.
;;;
;;; Surfaces treat Y as the TOP of the text box (the legacy bitmap
;;; semantics); the baseline conversion happens inside font-draw-text.
;;; All width/height queries (font-text-width, font-line-height,
;;; font-ascent) answer in pixels for the currently configured size.

(in-package #:rhizome)

;;; --- freetype bindings ---

(cffi:define-foreign-library libfreetype
  (:unix (:or "libfreetype.so.6" "libfreetype.so")))

(cffi:defcstruct ft-vector
  (x :long) (y :long))

(cffi:defcstruct ft-bitmap
  (rows :unsigned-int)
  (width :unsigned-int)
  (pitch :int)
  (buffer :pointer)
  (num-grays :short)
  (pixel-mode :unsigned-char)
  (palette-mode :unsigned-char)
  (palette :pointer))

(cffi:defcstruct ft-glyph-metrics
  (width :long) (height :long)
  (hori-bearing-x :long) (hori-bearing-y :long) (hori-advance :long)
  (vert-bearing-x :long) (vert-bearing-y :long) (vert-advance :long))

;; Prefix of FT_GlyphSlotRec_ up to the fields we read. Natural
;; alignment matches the C layout; freetype has been ABI-stable here
;; for two decades.
(cffi:defcstruct ft-glyph-slot
  (library :pointer)
  (face :pointer)
  (next :pointer)
  (reserved :pointer)
  (generic-data :pointer)
  (generic-finalizer :pointer)
  (metrics (:struct ft-glyph-metrics))
  (linear-hori-advance :long)
  (linear-vert-advance :long)
  (advance (:struct ft-vector))
  (format :int)
  (bitmap (:struct ft-bitmap))
  (bitmap-left :int)
  (bitmap-top :int))

(cffi:defcstruct ft-bbox
  (x0 :long) (y0 :long) (x1 :long) (y1 :long))

;; Prefix of FT_FaceRec_ up to size/glyph; everything after is
;; private/driver fields we never touch.
(cffi:defcstruct ft-face-rec
  (num-faces :long)
  (face-index :long)
  (face-flags :long)
  (style-flags :long)
  (num-glyphs :long)
  (family-name :pointer)
  (style-name :pointer)
  (num-fixed-sizes :int)
  (available-sizes :pointer)
  (num-charmaps :int)
  (charmaps :pointer)
  (generic-data :pointer)
  (generic-finalizer :pointer)
  (bbox (:struct ft-bbox))
  (units-per-em :unsigned-short)
  (ascender :short)
  (descender :short)
  (height :short)
  (max-advance-width :short)
  (max-advance-height :short)
  (underline-position :short)
  (underline-thickness :short)
  (glyph :pointer)
  (size :pointer))

(cffi:defcstruct ft-size-metrics
  (x-ppem :unsigned-short)
  (y-ppem :unsigned-short)
  (x-scale :long)
  (y-scale :long)
  (ascender :long)
  (descender :long)
  (height :long)
  (max-advance :long))

(cffi:defcstruct ft-size-rec
  (face :pointer)
  (generic-data :pointer)
  (generic-finalizer :pointer)
  (metrics (:struct ft-size-metrics))
  (internal :pointer))

(cffi:defcfun ("FT_Init_FreeType" ft-init-freetype) :int
  (library :pointer))                        ; FT_Library* out

(cffi:defcfun ("FT_New_Face" ft-new-face) :int
  (library :pointer) (path :string) (index :long) (face :pointer))

(cffi:defcfun ("FT_Set_Pixel_Sizes" ft-set-pixel-sizes) :int
  (face :pointer) (width :unsigned-int) (height :unsigned-int))

(cffi:defcfun ("FT_Load_Char" ft-load-char) :int
  (face :pointer) (char-code :unsigned-long) (flags :int))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +ft-load-render+ #x4)         ; render glyph to bitmap
  (defconstant +ft-pixel-mode-gray+ 2))       ; 8-bit anti-aliased

;;; --- state ---

(defvar *ft-library* nil
  "The freetype library handle, or NIL before a successful init.")

(defvar *ft-face* nil
  "The loaded font face for the configured family/size, or NIL when
the bitmap fallback is active.")

(defvar *ft-font-size* 0
  "Pixel size the face was loaded at (26.6 fixed-point for freetype
metrics; plain pixels here).")

(defvar *ft-font-family* nil
  "The family the face was resolved from (for change detection).")

(defparameter *font-glyph-cache* (make-hash-table)
  "Char code → glyph plist (:width :height :left :top :advance
:data) of 8-bit alpha values.")

;;; --- init / resolution ---

(defun fontconfig-resolve (family size)
  "Ask fontconfig for the best font file for FAMILY at SIZE px.
Returns a path string or NIL (no fc-match on PATH, no match, or
fc-match failed). One-shot per face init, so the subprocess cost
is irrelevant."
  (ignore-errors
    (let ((out (uiop:run-program
                 (list "fc-match" "-f" "%{file}"
                       (format nil "~A:size=~A" family size))
                 :output :string :ignore-error-status t)))
      (let ((path (string-right-trim '(#\newline #\space #\tab) out)))
        (if (and (plusp (length path)) (probe-file path))
            path
            nil)))))

(defun font-shutdown ()
  "Drop the freetype face and glyph cache (bitmap fallback active)."
  (setf *ft-face* nil
        *ft-font-size* 0
        *ft-font-family* nil)
  (clrhash *font-glyph-cache*))

(defun font-init (&key (force nil))
  "Open the configured font (:theme :font-family/:font-size) in
freetype. Returns T on success; on any failure the bitmap font
stays active and NIL is returned. Idempotent unless FORCE."
  (when (and *ft-face* (not force))
    (return-from font-init t))
  (font-shutdown)
  (block init
    (handler-case
        (progn
          (unless *ft-library*
            (cffi:use-foreign-library libfreetype)
            (cffi:with-foreign-object (lib :pointer)
              (unless (zerop (ft-init-freetype lib))
                (return-from init nil))
              (setf *ft-library* (cffi:mem-ref lib :pointer))))
          (let* ((family (config-getf '(:theme) :font-family
                                      :default "monospace"))
                 (size (config-getf '(:theme) :font-size :default 12))
                 (path (fontconfig-resolve family size)))
            (unless path
              (return-from init nil))
            (cffi:with-foreign-object (face :pointer)
              (unless (zerop (ft-new-face *ft-library* path 0 face))
                (return-from init nil))
              (setf *ft-face* (cffi:mem-ref face :pointer)))
            (unless (zerop (ft-set-pixel-sizes *ft-face* 0 size))
              (return-from init nil))
            (setf *ft-font-size* size
                  *ft-font-family* family)
            (clrhash *font-glyph-cache*)
            (format t "~&; Rhizome: font: ~A at ~Apx (freetype)~%"
                    family size)
            t))
      (error (e)
        (declare (ignore e))
        (font-shutdown)
        nil))))

(defun font-active-p ()
  "Whether TrueType rendering is live (vs. the bitmap fallback)."
  (and *ft-face* t))

(defvar *font-init-tried* nil
  "Whether a font resolution was attempted in this process. Without
this, a machine with no freetype/fontconfig would re-run fc-match
on every frame.")

(defun font-ensure-initialized ()
  "Resolve the font once per process (lazily, from whichever surface
draws first). Cheap after the first call."
  (unless *font-init-tried*
    (setf *font-init-tried* t)
    (unless (font-init)
      (format t
              "~&; Rhizome: font: 8x16 bitmap fallback (no TrueType font resolved)~%")))
  (font-active-p))

(defun font-apply-config ()
  "Re-resolve the font after a live :theme font change (called from
apply-config; the bar restarts separately and re-resolves on its
own startup). Skips the resolution when nothing changed."
  (let ((family (config-getf '(:theme) :font-family :default "monospace"))
        (size (config-getf '(:theme) :font-size :default 12)))
    (unless (and (font-active-p)
                 (string= family (or *ft-font-family* ""))
                 (= size *ft-font-size*))
      (when (font-init :force t)
        (format t "~&; Rhizome: font: re-resolved after :theme change~%")))))

;;; --- glyph rendering ---

(defun font-glyph (code)
  "Render and cache glyph CODE from the active face. A plist with
:width :height :left :top :advance :data (a (unsigned-byte 8)
vector of alpha values), or NIL without a face / for an
unmappable character."
  (or (gethash code *font-glyph-cache*)
      (when *ft-face*
        (when (zerop (ft-load-char *ft-face* code +ft-load-render+))
          ;; glyph is a POINTER field: read its VALUE (the slot), not
          ;; the field's address — foreign-slot-value, not -pointer.
          (let* ((slot (cffi:foreign-slot-value
                         *ft-face* '(:struct ft-face-rec) 'glyph))
                 (bmp (cffi:foreign-slot-pointer
                        slot '(:struct ft-glyph-slot) 'bitmap))
                 (rows (cffi:foreign-slot-value
                         bmp '(:struct ft-bitmap) 'rows))
                 (cols (cffi:foreign-slot-value
                         bmp '(:struct ft-bitmap) 'width))
                 (mode (cffi:foreign-slot-value
                         bmp '(:struct ft-bitmap) 'pixel-mode))
                 (left (cffi:foreign-slot-value
                         slot '(:struct ft-glyph-slot) 'bitmap-left))
                 (top (cffi:foreign-slot-value
                        slot '(:struct ft-glyph-slot) 'bitmap-top))
                 (advance (ash (cffi:foreign-slot-value
                                 (cffi:foreign-slot-pointer
                                   slot '(:struct ft-glyph-slot) 'advance)
                                 '(:struct ft-vector) 'x)
                               -6))
                 (buffer (cffi:foreign-slot-value
                           bmp '(:struct ft-bitmap) 'buffer)))
            (when (and (plusp rows) (plusp cols)
                       (= mode +ft-pixel-mode-gray+))
              (let ((data (make-array (* rows cols)
                                      :element-type '(unsigned-byte 8))))
                (dotimes (i (* rows cols))
                  (setf (aref data i)
                        (cffi:mem-aref buffer :unsigned-char i)))
                (setf (gethash code *font-glyph-cache*)
                      (list :width cols :height rows
                            :left left :top top
                            :advance advance :data data)))))))))

;;; --- metrics ---

(defun font-ascent ()
  "Pixels from the text baseline to the top of the line box.
Bitmap fallback: the legacy 16px cell, text flush at the top."
  (if *ft-face*
      (let* ((size (cffi:foreign-slot-value
                     *ft-face* '(:struct ft-face-rec) 'size))
             (metrics (cffi:foreign-slot-pointer
                        size '(:struct ft-size-rec) 'metrics)))
        (ash (cffi:foreign-slot-value
               metrics '(:struct ft-size-metrics) 'ascender)
             -6))
      13))

(defun font-descent ()
  "Pixels from the baseline to the bottom of the line box."
  (if *ft-face*
      (let* ((size (cffi:foreign-slot-value
                     *ft-face* '(:struct ft-face-rec) 'size))
             (metrics (cffi:foreign-slot-pointer
                        size '(:struct ft-size-rec) 'metrics)))
        (- (ash (cffi:foreign-slot-value
                  metrics '(:struct ft-size-metrics) 'descender)
                -6)))
      3))

(defun font-line-height ()
  "Row height for one line of text at the configured size."
  (if *ft-face*
      (max 16 (+ (font-ascent) (font-descent)))
      16))

(defun font-text-width (text)
  "Pixel width of TEXT at the configured size. Unmappable
characters count a generic advance."
  (if *ft-face*
      (loop for ch across text
            sum (let ((glyph (font-glyph (char-code ch))))
                  (if glyph
                      (getf glyph :advance)
                      (max 4 (floor (* *ft-font-size* 2) 3)))))
      (* 8 (length text))))

;;; --- blitting ---

(declaim (inline font-blend-pixel))
(defun font-blend-pixel (ptr offset r g b a)
  "Alpha-blend the BGRA pixel at byte OFFSET in PTR:
out = bg + (fg - bg) * a / 255. The alpha byte is left alone (the
buffers are always opaque)."
  (flet ((blend (idx fg)
           (let ((bg (cffi:mem-aref ptr :unsigned-char (+ offset idx))))
             (setf (cffi:mem-aref ptr :unsigned-char (+ offset idx))
                   (round (+ (* bg 255) (* (- fg bg) a)) 255)))))
    (blend 0 b)                          ; B
    (blend 1 g)                          ; G
    (blend 2 r)))                        ; R

(defun font-draw-text (ptr width height text x y r g b)
  "Draw TEXT into the BGRA buffer PTR at (X, Y) in color (R G B).

Y is the TOP of the text box in both modes (the legacy bitmap
semantics every caller already uses); the TrueType path converts
to a baseline internally. Text is clipped at the buffer edges."
  (if *ft-face*
      (let ((baseline (+ y (font-ascent))))
        (loop with cx = x
              for ch across text
              for glyph = (font-glyph (char-code ch))
              while (< cx width)
              do (when glyph
                   (let* ((gw (getf glyph :width))
                          (gh (getf glyph :height))
                          (data (getf glyph :data))
                          (gx (+ cx (getf glyph :left)))
                          (gy (+ baseline (- (getf glyph :top)))))
                     (dotimes (row gh)
                       (let ((py (+ gy row)))
                         (when (and (>= py 0) (< py height))
                           (dotimes (col gw)
                             (let ((px (+ gx col)))
                               (when (and (>= px 0) (< px width))
                                 (let ((a (aref data (+ row (* gh col)))))
                                   (declare (type (unsigned-byte 8) a))
                                   (when (plusp a)
                                     (font-blend-pixel
                                       ptr (* 4 (+ py (* width px)))
                                       r g b a)))))))))
                   (incf cx (getf glyph :advance))))))
      ;; Bitmap fallback: byte-for-byte the legacy renderer
      (progn
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
                    (let ((offset (* 4 (+ py (* width px)))))
                      (setf (cffi:mem-aref ptr :unsigned-char offset) b
                            (cffi:mem-aref ptr :unsigned-char (+ offset 1)) g
                            (cffi:mem-aref ptr :unsigned-char (+ offset 2)) r
                            (cffi:mem-aref ptr :unsigned-char (+ offset 3)) 255)))))))))))
