;;; Tests for the dbusmenu right-click path: parsing the pinned
;;; GetLayout wire shape (Variety's actual menu), flattening to
;;; selectable entries, accelerator stripping, the Event call, and
;;; the tray-show-menu prompt flow.
(ql:quickload :stumpway :silent t)

(in-package #:stumpway)

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass) (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (setf (symbol-function 'message)
          (lambda (&rest args) (declare (ignore args)) nil))
    (setf (symbol-function 'select-from-menu)
          (lambda (title entries &key on-select)
            (declare (ignore on-select))
            (list :shown title (length entries))))

    ;; --- The pinned Variety menu shape (from the live probe) ---
    ;; Children arrive as variants: (sig id props children)

    (let* ((menu
            '(0 (("children-display" ("s" . "submenu")))
              (("ia{sv}av" 2 (("label" ("s" . "_Next"))) nil)
               ("ia{sv}av" 4 (("enabled" ("b" . t))
                              ("type" ("s" . "separator"))) nil)
               ("ia{sv}av" 5 (("label" ("s" . "wallhaven-vg8w5m.png"))) nil)
               ("ia{sv}av" 7 (("enabled" ("b")) ("visible" ("b"))
                              ("label" ("s" . "Show author"))) nil)
               ("ia{sv}av" 8 (("label" ("s" . "Copy to _Favorites"))) nil)
               ("ia{sv}av" 12 (("children-display" ("s" . "submenu"))
                               ("label" ("s" . "_Image")))
                (("ia{sv}av" 13 (("label" ("s" . "_Next"))) nil)
                 ("ia{sv}av" 19 (("label"
                                   ("s" . "Pause on current"))) nil))))))
          (entries (menu-entries menu)))
      (ok "entries flatten: label + stripped accelerators"
          (mapcar #'first entries)
          '("Next" "wallhaven-vg8w5m.png" "Copy to Favorites"
            "  Next" "  Pause on current"))
      (ok "entries carry the dbusmenu ids"
          (mapcar #'rest entries)
          '(2 5 8 13 19))
      (ok "separator skipped"
          (length entries) 5)
      (ok "disabled+invisible entry skipped"
          (find "Show author" entries :key #'first :test #'string=)
          nil)
      (ok "submenu label itself not selectable"
          (find "Image" entries :key #'first :test #'string=)
          nil)
      (ok "submenu children indented"
          (find "  Next" entries :key #'first :test #'string=)
          '("  Next" . 13)))

    ;; --- accelerator stripping ---

    (ok "accelerators stripped"
        (menu-strip-accel "_Open") "Open")
    (ok "mid-word accelerators stripped"
        (menu-strip-accel "Copy to _Favorites")
        "Copy to Favorites")

    ;; --- the Event payload ---

    (let ((clicked nil))
      (setf (symbol-function 'dbus-call)
            (lambda (conn dest path iface member &key signature values
                      timeout)
              (declare (ignore conn dest path timeout))
              (setf clicked (list iface member signature values))
              (values nil nil)))
      (tray-menu-activate :conn ":1.13" "/Menu" 42)
      (ok "Event carries (id, clicked, variant, timestamp)"
          clicked
          '("com.canonical.dbusmenu" "Event" "isvu"
            (42 "clicked" ("i" . 0) 0))))

    ;; --- tray-show-menu: the prompt path ---

    (setf (symbol-function 'dbus-call)
          (lambda (conn dest path iface member &key signature values
                    timeout)
            (declare (ignore conn dest path signature values timeout))
            (if (and (string= member "GetLayout")
                     (string= iface "com.canonical.dbusmenu"))
                (values (make-dbus-message :type +dbus-msg-return+
                                            :fields '((5 . 1)))
                        (list 99
                              '(0 (("children-display" ("s" . "submenu")))
                                (("ia{sv}av" 2 (("label" ("s" . "_Next")))
                                  nil)
                                 ("ia{sv}av" 3
                                  (("label" ("s" . "_Pause"))) nil)))))
                (values nil nil))))
    (ok "tray-show-menu prompts with the flattened entries"
        (tray-show-menu ":1.13" "/Menu")
        '(:shown "tray menu" 2))

    ;; --- empty menu: message, not a prompt ---

    (setf (symbol-function 'dbus-call)
          (lambda (conn dest path iface member &key signature values
                    timeout)
            (declare (ignore conn dest path iface member signature
                             values timeout))
            (values (make-dbus-message :type +dbus-msg-return+
                                        :fields '((5 . 1)))
                    (list 0
                          '(0 (("children-display" ("s" . "submenu")))
                            nil)))))
    (ok "empty menu shows the message, not a prompt"
        (tray-show-menu ":1.13" "/Menu")
        nil))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
