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

    ;; --- module part protocol (gap / underline flags) ---

    (ok "dotted part has no flags"
        (bar-part-flags '("1" . "#7aa2f7")) nil)
    (ok "bare part has no flags"
        (bar-part-flags '("2")) nil)
    (ok "flagged part reads its tail"
        (bar-part-flags '("1" "#7aa2f7" :underline t :gap 8))
        '(:underline t :gap 8))
    (ok "part width includes its gap"
        (- (bar-part-width '("ab" nil :gap 7))
           (bar-text-width "ab"))
        7)
    (ok "part width without gap is the text width"
        (bar-part-width '("ab")) (bar-text-width "ab"))

    ;; --- workspaces module: occupancy, underline, gaps ---

    (let ((real-snapshot (symbol-function 'bar-snapshot)))
      (setf (symbol-function 'bar-snapshot)
            (lambda () '(:workspace 3 :workspace-count 9
                         :occupied (2 3 5))))
      (let ((parts (bar-module-parts :workspaces)))
        (ok "workspaces count"
            (length parts) 9)
        (ok "active workspace underlined with accent"
            (third parts) '("3" "#7aa2f7" :underline t :gap 8))
        (ok "occupied workspace in foreground, no underline"
            (second parts) '("2" "#c0caf5" :gap 8))
        (ok "empty workspace muted"
            (first parts) '("1" "#565f89" :gap 8))
        (ok "every workspace part carries the gap"
            (every (lambda (p)
                     (= (getf (bar-part-flags p) :gap 0) 8))
                   parts)
            t))
      (setf (symbol-function 'bar-snapshot) real-snapshot))

    ;; --- snapshot occupancy ---

    (let ((real-write (symbol-function 'bar-write-snapshot)))
      (setf (symbol-function 'bar-write-snapshot)
            (lambda (snapshot) (declare (ignore snapshot)) nil)
            *windows* (make-hash-table)
            *window-workspace* (make-hash-table)
            *minimized-windows* (make-hash-table))
      (let ((w1 (make-instance 'rhizome-window :proxy :occ-1))
            (w2 (make-instance 'rhizome-window :proxy :occ-2))
            (w3 (make-instance 'rhizome-window :proxy :occ-3)))
        (setf (gethash :occ-1 *windows*) w1
              (gethash :occ-2 *windows*) w2
              (gethash :occ-3 *windows*) w3
              (gethash w1 *window-workspace*) 2
              (gethash w2 *window-workspace*) 5
              (gethash w3 *window-workspace*) 5
              (gethash w3 *minimized-windows*) t)
        (update-wm-snapshot)
        (ok "snapshot occupancy lists visible windows sorted"
            (getf *wm-snapshot* :occupied) '(2 5))
        (ok "snapshot occupancy skips minimized windows"
            (and (member 5 (getf *wm-snapshot* :occupied)) t)
            t))
      (setf (symbol-function 'bar-write-snapshot) real-write))

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
          (let ((space (font-glyph 32)))
            (ok "space glyph cached (empty bitmap, real advance)"
                (and space
                     (plusp (getf space :advance))
                     (zerop (getf space :width)))
                t))
          (ok "space advances text width"
              (> (font-text-width "a b") (font-text-width "ab"))
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
