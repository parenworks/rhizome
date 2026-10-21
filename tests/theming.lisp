;;; Tests for the theming/font layer: color parsing, :theme and
;;; :launcher config plumbing, border colors, and the font module —
;;; TrueType via freetype when available, the 8x16 bitmap fallback
;;; otherwise (both paths are exercised regardless).
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- color parsing ---

    (multiple-value-bind (r g b) (parse-hex-color "#7aa2f7")
      (ok "hex color parses with #"
          (list r g b) '(122 162 247)))
    (multiple-value-bind (r g b) (parse-hex-color "1a1b26")
      (ok "hex color parses without #"
          (list r g b) '(26 27 38)))

    ;; --- :theme / :launcher config plumbing ---

    (ok "default font family"
        (config-getf '(:theme) :font-family) "monospace")
    (ok "default font size"
        (config-getf '(:theme) :font-size) 12)
    (ok "default launcher background"
        (config-getf '(:launcher) :background) "#23142d")
    (multiple-value-bind (r g b) (launcher-color :background)
      (ok "launcher color parsed"
          (list r g b) '(35 20 45)))

    ;; --- border colors ---

    (multiple-value-bind (r g b) (theme-border-rgb t)
      (ok "focused border default"
          (list r g b) '(122 162 247)))
    (multiple-value-bind (r g b) (theme-border-rgb nil)
      (ok "unfocused border default"
          (list r g b) '(64 64 64)))
    (configure '(:theme (:focused-border "#ff0000")))
    (multiple-value-bind (r g b) (theme-border-rgb t)
      (ok "focused border override"
          (list r g b) '(255 0 0)))

    ;; --- message metrics follow the font ---

    (ok "message line height positive"
        (> (message-line-height) 0) t)

    ;; --- TrueType path (machine-dependent; skipped assertions if
    ;; freetype/fontconfig is unavailable) ---

    (if (font-init)
        (progn
          (ok "font active after init" (font-active-p) t)
          (ok "ascent positive" (> (font-ascent) 0) t)
          (ok "descent sane" (>= (font-descent) 0) t)
          (ok "line height sane"
              (> (font-line-height) 4) t)
          (let ((glyph (font-glyph (char-code #\A))))
            (ok "glyph A rendered"
                (and glyph
                     (getf glyph :data)
                     (plusp (getf glyph :width))
                     (plusp (getf glyph :advance)))
                t))
          (ok "glyph cache returns the same object"
              (eq (font-glyph (char-code #\A))
                  (font-glyph (char-code #\A)))
              t)
          (ok "text width positive"
              (> (font-text-width "Hello") 0) t)
          (ok "wider text is wider"
              (> (font-text-width "Hello world")
                 (font-text-width "Hi"))
              t)
          ;; Draw into a scratch BGRA buffer: pixels must change.
          (let* ((w 128) (h 32)
                 (buf (cffi:foreign-alloc
                        :unsigned-char :count (* w h 4))))
            (dotimes (i (* w h 4))
              (setf (cffi:mem-aref buf :unsigned-char i) 0))
            (font-draw-text buf w h "Ag" 2 2 255 255 255)
            (ok "TrueType draw lights pixels"
                (> (loop for i below (* w h)
                         count (plusp
                                 (cffi:mem-aref
                                   buf :unsigned-char (* 4 i))))
                   0)
                t)
            (cffi:foreign-free buf)))
        (format t "~&SKIP: freetype/fontconfig unavailable; bitmap fallback assertions only~%"))

    ;; --- bitmap fallback path (always exercised) ---

    (let ((*ft-face* nil))
      (ok "fallback width is 8*chars"
          (font-text-width "Hi") 16)
      (ok "fallback line height is 16"
          (font-line-height) 16)
      (let* ((w 32) (h 16)
             (buf (cffi:foreign-alloc
                    :unsigned-char :count (* w h 4))))
        (dotimes (i (* w h 4))
          (setf (cffi:mem-aref buf :unsigned-char i) 0))
        (font-draw-text buf w h "!" 0 0 255 255 255)
        (ok "bitmap draw lights pixels"
            (> (loop for i below (* w h)
                     count (plusp
                             (cffi:mem-aref
                               buf :unsigned-char (* 4 i))))
               0)
            t)
        (cffi:foreign-free buf)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (when (plusp fail)
      (uiop:quit 1))))
