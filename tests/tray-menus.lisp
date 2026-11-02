;;; Tests for the tray dbusmenu → pointer menu path (tray.lisp,
;;; menus.lisp): the layout flattening (separators, disabled
;;; entries, submenu indentation, accelerator stripping) and the
;;; explicit-position menu clamping.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- dbusmenu flattening ---
    ;; A typical GetLayout reply: (id props children); props are
    ;; (NAME (SIG . VALUE)) pairs.
    (let* ((prop (lambda (name value)
                   ;; the decoded a{sv} shape: (NAME (SIG . VALUE))
                   (list name (cons "s" value))))
           (root (list 0
                      (list (funcall prop "children-display" "submenu"))
                      (list
                        ;; a plain entry with an accelerator mark
                        (list 1 (list (funcall prop "label" "Open"))
                              nil)
                        ;; a separator
                        (list 2 (list (funcall prop "type" "separator"))
                              nil)
                        ;; a disabled entry
                        (list 3 (list (funcall prop "label" "Hidden")
                                      (funcall prop "enabled" nil))
                              nil)
                        ;; a submenu with one child
                        (list 4 (list (funcall prop "label" "Options")
                                      (funcall prop "children-display" "submenu"))
                              (list
                                (list 5 (list (funcall prop "label" "Pref_s"))
                                      nil)))))))
      ;; submenu headers are not entries (a flat menu can't nest);
      ;; their children indent one level
      (ok "flatten: entries with labels, in order"
          (mapcar #'car (menu-entries root))
          '("Open" "  Prefs"))
      (ok "flatten: entries carry their dbusmenu ids"
          (mapcar #'cdr (menu-entries root))
          '(1 5))
      ;; variant-wrapped children (signature "ia{sv}av")
      (let ((wrapped (list "ia{sv}av" 7
                           (list (funcall prop "label" "Wrapped"))
                           (list (list "v" 8
                                       (list (funcall prop "label" "Kid"))
                                       nil)))))
        (ok "unwrap: variant nodes strip the signature"
            (menu-unwrap wrapped)
            (list 7
                  (list (funcall prop "label" "Wrapped"))
                  (list (list "v" 8
                              (list (funcall prop "label" "Kid"))
                              nil)))))
      (ok "accelerator marks are stripped"
          (menu-strip-accel "Pref_s") "Prefs"))

    ;; --- explicit-position clamping (the tray menu path) ---
    (let* ((out (make-instance 'rhizome-output :proxy :mm-out
                               :width 1920 :height 1080))
           (*menu-window* nil)
           (*outputs* (make-hash-table)))
      (setf (gethash :mm-out *outputs*) out)
      ;; a menu at the far bottom-right corner clamps on screen
      (multiple-value-bind (x y)
          (let ((*menu-width* 240) (*menu-height* 200))
            (menu-clamp-position-at 1918 1078))
        (ok "bottom-right click clamps on screen"
            (list (< x 1920) (>= x 0) (< y 1080) (>= y 0))
            '(t t t t)))
      ;; a click at the bar (x, 20) keeps the menu near the icon
      (multiple-value-bind (x y)
          (let ((*menu-width* 240) (*menu-height* 200))
            (menu-clamp-position-at 1500 20))
        (ok "bar-level click keeps the menu below the bar"
            (list x y) '(1492 12))))

    (format t "~&=== ~D passed, ~D failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
