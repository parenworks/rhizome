;;; Tests for border colour encoding: river_window_v1.set_borders
;;; takes four 32-bit pre-multiplied RGBA values, so every render
;;; path must hand set-window-borders the theme colour scaled from
;;; 8-bit channels (#xff -> #xffffffff), never the raw bytes.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (borders '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (colour-for (win)
           ;; (r g b a) of the last set-window-borders call for WIN
           (let ((call (find win borders :key #'first)))
             (and call (last call 4)))))

    ;; River-touching stubs; set-window-borders records its arguments
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position show-window
                  hide-window message request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'set-window-borders)
          (lambda (&rest args) (push args borders) nil))

    ;; --- conversion: 8-bit channels to 32-bit protocol values ---

    (ok "opaque white is all ones"
        (multiple-value-list (border-rgba32 #xff #xff #xff))
        '(#xffffffff #xffffffff #xffffffff #xffffffff))
    (ok "opaque black keeps full alpha"
        (multiple-value-list (border-rgba32 0 0 0))
        '(0 0 0 #xffffffff))
    (ok "each byte scales by #x01010101"
        (multiple-value-list (border-rgba32 #x7a #xa2 #xf7))
        (list (* #x7a #x01010101) (* #xa2 #x01010101)
              (* #xf7 #x01010101) #xffffffff))
    (ok "colour channels are pre-multiplied by alpha"
        (multiple-value-list (border-rgba32 #xff #x80 0 #x80))
        (list (* #x80 #x01010101) (floor (* #x80 #x80 #x01010101) #xff)
              0 (* #x80 #x01010101)))
    (ok "zero alpha clears every channel"
        (multiple-value-list (border-rgba32 #xff #xff #xff 0))
        '(0 0 0 0))

    (ok "theme focused default in protocol units"
        (multiple-value-list (theme-border-rgba32 t))
        (list (* #x7a #x01010101) (* #xa2 #x01010101)
              (* #xf7 #x01010101) #xffffffff))
    (ok "theme unfocused default in protocol units"
        (multiple-value-list (theme-border-rgba32 nil))
        (list (* #x40 #x01010101) (* #x40 #x01010101)
              (* #x40 #x01010101) #xffffffff))
    (ok "theme-border-rgb still returns bytes"
        (multiple-value-list (theme-border-rgb t))
        '(#x7a #xa2 #xf7))

    ;; --- render paths ---

    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *scroll-states* (make-hash-table :test 'equal))

    (let* ((focused (list (* #x7a #x01010101) (* #xa2 #x01010101)
                          (* #xf7 #x01010101) #xffffffff))
           (unfocused (list (* #x40 #x01010101) (* #x40 #x01010101)
                            (* #x40 #x01010101) #xffffffff))
           (out (make-instance 'rhizome-output :proxy :bc-out
                                               :width 1920 :height 1080))
           (t1 (make-instance 'rhizome-window :proxy :bc-t1))
           (t2 (make-instance 'rhizome-window :proxy :bc-t2))
           (fl (make-instance 'rhizome-window :proxy :bc-fl)))
      (setf (gethash :bc-t1 *windows*) t1
            (gethash :bc-t2 *windows*) t2
            (gethash :bc-fl *windows*) fl
            (gethash out *output-workspace*) 3)
      (dolist (win (list t1 t2 fl))
        (setf (window-output win) out
              (window-width win) 800
              (window-height win) 600)
        (assign-window-workspace win 3))
      (set-workspace-tree out 3 '((0 0 960 1080) (960 0 960 1080))
                          (list t1 t2))
      (setf (window-focused-p t1) t)

      ;; Frame tree
      (setf borders '())
      (frames-render-decisions out 3 (list t1 t2))
      (ok "frames: focused window border in protocol units"
          (colour-for t1) focused)
      (ok "frames: unfocused window border in protocol units"
          (colour-for t2) unfocused)

      ;; Floating
      (setf (window-x fl) 100 (window-y fl) 100)
      (float-window fl)
      (setf (window-focused-p t1) nil
            (window-focused-p fl) t)
      (setf borders '())
      (float-render-decisions out (list t1 t2))
      (ok "floating: focused float border in protocol units"
          (colour-for fl) focused)
      (setf (window-focused-p fl) nil)
      (setf borders '())
      (float-render-decisions out (list t1 t2))
      (ok "floating: unfocused float border in protocol units"
          (colour-for fl) unfocused)
      (unfloat-window fl)
      (setf (gethash :bc-fl *windows*) nil)
      (remhash :bc-fl *windows*)

      ;; Scroll ribbon
      (set-workspace-tree out 3 '((0 0 960 1080) (960 0 960 1080))
                          (list t1 t2))
      (setf (window-focused-p t2) t)
      (scroll-enter out 3)
      (setf borders '())
      (scroll-render-decisions out 3 (list t1 t2))
      (ok "scroll: focused column border in protocol units"
          (colour-for t2) focused)
      (ok "scroll: unfocused column border in protocol units"
          (colour-for t1) unfocused))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (unless (zerop fail)
      (uiop:quit 1))))
