;;; Tests for the tray: the StatusNotifierWatcher service side and
;;; the item registry. The DBus layer's live integration tests
;;; (tests/dbus.lisp) prove the wire; these stub the connection and
;;; drive the watcher logic: registration, deduplication, owner-loss
;;; removal via NameOwnerChanged, the exposed properties, and
;;; click/scroll dispatch.
(ql:quickload :rhizome :silent t)

(in-package #:rhizome)

(let ((pass 0) (fail 0) (emitted nil))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass) (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    ;; stubs: no real bus, no real icon loads
    (setf (symbol-function 'dbus-emit-signal)
          (lambda (conn path iface member &key signature values)
            (declare (ignore conn path signature))
            (push (list member iface values) emitted)))
    (setf (symbol-function 'tray-refresh-item)
          (lambda (item)
            (setf (getf item :title) "stubbed")))
    (setf *tray-items* nil
          *tray-conn* :fake
          *tray-changed* 0
          emitted nil)

    ;; --- Registration ---

    (tray-register-item ":1.42" "/StatusNotifierItem")
    (ok "item registered" (length *tray-items*) 1)
    (ok "registered signal emitted"
        (first (first emitted))
        "StatusNotifierItemRegistered")
    (ok "registration signal carries the name"
        (third (first emitted)) '(":1.42"))

    ;; duplicate registration is a no-op
    (tray-register-item ":1.42" "/StatusNotifierItem")
    (ok "duplicate registration ignored" (length *tray-items*) 1)

    ;; --- Properties ---

    (let ((props (funcall (tray-property-values))))
      (ok "watcher advertises the host bit"
          (cdr (assoc "IsStatusNotifierHostRegistered" props
                      :test #'string=))
          (cons "b" t))
      (ok "watcher lists registered items"
          (cdr (assoc "RegisteredStatusNotifierItems" props
                      :test #'string=))
          (cons "as" '(":1.42/StatusNotifierItem"))))

    ;; --- Owner loss (NameOwnerChanged with empty new owner) ---

    (setf emitted nil)
    (tray-signal
      (make-dbus-message
        :type +dbus-msg-signal+
        :fields '((2 . "org.freedesktop.DBus")
                  (3 . "NameOwnerChanged"))
        :body '(":1.42" ":1.99" "")))
    (ok "owner loss removes the item" *tray-items* nil)
    (ok "unregistered signal emitted"
        (first (first emitted))
        "StatusNotifierItemUnregistered")

    ;; a name GAINING an owner must not touch anything
    (tray-register-item ":1.7" "/StatusNotifierItem")
    (setf emitted nil)
    (tray-signal
      (make-dbus-message
        :type +dbus-msg-signal+
        :fields '((2 . "org.freedesktop.DBus")
                  (3 . "NameOwnerChanged"))
        :body '(":1.7" "" ":1.99")))
    (ok "owner gain does not remove the item"
        (length *tray-items*) 1)
    (ok "owner gain emits nothing" emitted nil)

    ;; --- Click / scroll dispatch (the dbus call is stubbed) ---

    (let ((clicked nil))
      (setf (symbol-function 'tray-click)
            (lambda (item &optional secondary)
              (setf clicked (list (getf item :name) secondary))))
      (let ((item (first *tray-items*)))
        ;; simulate the bar's click routing: hitbox carries the item
        (let ((*bar-hitboxes* (list (list item 100 116))))
          (bar-handle-click 108)))
      (ok "bar click routes to tray-click"
          clicked '(":1.7" nil)))

    ;; --- The tray module's geometry helpers ---

    (ok "tray width scales with items"
        (bar-tray-width)
        (+ (+ *bar-icon-size* 4) *bar-module-gap*))
    (setf *tray-items* nil)
    (ok "empty tray renders zero width" (bar-tray-width) 0)

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
