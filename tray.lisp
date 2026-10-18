;;; The system tray for Stumpway — org.kde.StatusNotifierWatcher
;;;
;;; The watcher OWNS the well-known bus name; tray apps (Variety,
;;; flameshot, anything SNI-aware) register their items with it and
;;; listen for its signals. The bar process hosts the watcher (the
;;; tray IS a bar module — dbus.lisp is river-independent, so the
;;; bar speaks DBus directly): registrations arrive as method calls
;;; on the bar's DBus connection, item loss arrives as the bus's
;;; NameOwnerChanged signal, and clicks go back out as Activate /
;;; SecondaryActivate calls on the item's own bus name.
;;;
;;; Item icons: IconName (theme name — reuse the bar's existing
;;; icon resolution) or IconPixmap (raw ARGB32, width, height,
;;; bytes-per-row packed in an int32 array — drawn directly into
;;; the bar's shm buffer).

(in-package #:stumpway)

(defparameter *tray-items* nil
  "Registered SNI items, newest last: alists of
(:name bus-name :path object-path :id id :title title :status
status :icon-name icon-name :icon icon-pixmap-or-nil :menu
menu-path-or-nil).")
(defparameter *tray-conn* nil
  "The tray's own DBus connection (the bar process's).")
(defparameter *tray-changed* 0
  "Bumped whenever the item list changes so the bar re-renders.")

(defparameter +snw-name+ "org.kde.StatusNotifierWatcher")
(defparameter +snw-path+ "/StatusNotifierWatcher")
(defparameter +sni-iface+ "org.kde.StatusNotifierItem")

(defun tray-item-p (bus-name)
  (find bus-name *tray-items*
        :test #'string= :key (lambda (item) (getf item :name))))

(defun tray-register-item (bus-name path)
  "RegisterStatusNotifierItem: an app offered an item."
  (unless (tray-item-p bus-name)
    (let ((item (list :name bus-name :path path
                      :id nil :title nil :status nil
                      :icon-name nil :icon nil :menu nil)))
      (tray-refresh-item item)
      (push item *tray-items*)
      (setf *tray-items* (nreverse *tray-items*))
      (incf *tray-changed*)
      (format t "~&; Stumpway: tray: registered ~A~A~%" bus-name path)
      (dbus-emit-signal *tray-conn* +snw-path+
                        +snw-name+ "StatusNotifierItemRegistered"
                        :signature "s" :values (list bus-name)))))

(defun tray-remove-item (bus-name)
  "Drop an item (its owner left the bus or unregistered)."
  (when (tray-item-p bus-name)
    (setf *tray-items* (remove bus-name *tray-items*
                                :test #'string=
                                :key (lambda (item)
                                       (getf item :name))))
    (incf *tray-changed*)
    (format t "~&; Stumpway: tray: removed ~A~%" bus-name)
    (dbus-emit-signal *tray-conn* +snw-path+
                      +snw-name+ "StatusNotifierItemUnregistered"
                      :signature "s" :values (list bus-name))))

(defun tray-item-prop (props name)
  "The DATUM of property NAME from a Properties.GetAll dict:
entries arrive as (KEY VARIANT) and each VARIANT is (SIG . DATUM)."
  (let ((entry (assoc name props :test #'string=)))
    (when (and entry (rest entry))
      (cdr (second entry)))))

(defun tray-refresh-item (item)
  "Pull an item's identity and icon via Properties.GetAll. Items
are bus names; the SNI object lives at the registered path."
  (handler-case
      (multiple-value-bind (reply body)
          (dbus-call *tray-conn* (getf item :name)
                     (getf item :path)
                     "org.freedesktop.DBus.Properties" "GetAll"
                     :signature "s"
                     :values (list +sni-iface+)
                     :timeout 2)
        (let ((props (first body)))
        (when (and reply (eq (dbus-message-type reply)
                              +dbus-msg-return+))
          (setf (getf item :id)
                (tray-item-prop props "Id")
                (getf item :title)
                (tray-item-prop props "Title")
                (getf item :status)
                (tray-item-prop props "Status")
                (getf item :icon-name)
                (tray-item-prop props "IconName")
                (getf item :menu)
                (tray-item-prop props "Menu"))
          ;; IconPixmap: array of struct (width, height, bytes)
          (let* ((px (tray-item-prop props "IconPixmap"))
                 (first-px (first px)))
            (when (and (listp first-px)
                       (numberp (first first-px))
                       (plusp (first first-px))
                       (numberp (second first-px))
                       (plusp (second first-px))
                       (third first-px))
              (setf (getf item :icon)
                    (list (first first-px)
                          (second first-px)
                          (third first-px)))))
          (incf *tray-changed*))))
    (error (e)
      (format t "~&; Stumpway: tray: refresh failed: ~A~%" e))))

(defun tray-property-values ()
  "The watcher's exposed properties (Properties.GetAll)."
  (lambda ()
    (list
      (cons "IsStatusNotifierHostRegistered"
            (cons "b" t))
      (cons "ProtocolVersion" (cons "s" "0.2"))
      (cons "IsStatusNotifierHostRegistered"
            (cons "b" t))
      (cons "RegisteredStatusNotifierItems"
            (cons "as"
                  (mapcar (lambda (item)
                            (format nil "~A~A"
                                    (getf item :name)
                                    (getf item :path)))
                          *tray-items*))))))

(defun tray-start (&key (path (dbus-bus-path)))
  "Become the StatusNotifierWatcher: own the well-known name,
serve RegisterStatusNotifierItem and Properties, and watch for
item owners vanishing from the bus."
  (when *tray-conn*
    (return-from tray-start *tray-conn*))
  (let ((conn (dbus-connect :path path)))
    (unless conn
      (format t "~&; Stumpway: tray: no session bus; tray disabled~%")
      (return-from tray-start nil))
    (setf *tray-conn* conn)
    ;; Service the standard watcher surface
    (setf (dbus-handler "org.kde.StatusNotifierWatcher"
                        "RegisterStatusNotifierItem")
          (lambda (msg conn)
            (declare (ignore conn))
            (let ((arg (first (dbus-message-body msg))))
              ;; New SNI sends a bus name; legacy sends a path
              (if (and (> (length arg) 0)
                       (char= (aref arg 0) #\/))
                  ;; legacy: the caller's own name is the item owner
                  (tray-register-item
                    (or (message-field msg 7) arg) arg)
                  (tray-register-item arg "/StatusNotifierItem")))
            (values "" nil)))
    (setf (gethash "org.kde.StatusNotifierWatcher"
                   *dbus-property-getters*)
          (tray-property-values))
    ;; Watch for owners vanishing (crash, quit): NameOwnerChanged
    (dbus-add-match
      conn "type='signal',sender='org.freedesktop.DBus',
interface='org.freedesktop.DBus',member='NameOwnerChanged'")
    ;; Register the name; apps find the host through it
    (let ((result (dbus-request-name conn +snw-name+ 4)))
      (if (= result 1)
          (progn
            (format t "~&; Stumpway: tray: watcher active as ~A~%"
                    +snw-name+)
            conn)
          (progn
            (format t "~&; Stumpway: tray: ~A already owned (~A); \
tray disabled~%" +snw-name+ result)
            (setf *tray-conn* nil)
            nil)))))

(defun tray-signal (msg)
  "Route one incoming signal. Only NameOwnerChanged matters: an
item's owner losing its name means the item is gone."
  (when (and (string= (or (message-field msg 2) "")
                       "org.freedesktop.DBus")
             (string= (or (message-field msg 3) "")
                      "NameOwnerChanged"))
    (let ((body (dbus-message-body msg)))
      (when (and (listp body) (= (length body) 3))
        (let ((name (first body))
              (old (second body))
              (new (third body)))
          (when (and (plusp (length old))
                     (zerop (length new)))
            (tray-remove-item name)))))))

(defun tray-drain ()
  "One drain pass on the bar's tick: dispatch watcher method calls
(method call -> reply via the handler table) and watch for
NameOwnerChanged item loss. Redefines the dbus-drain stub."
  (let ((conn *tray-conn*))
    (when conn
      (loop
        (let ((msg (bt:with-lock-held
                       ((dbus-connection-queue-lock conn))
                     (pop (dbus-connection-queue conn)))))
          (unless msg (return))
          (when (eq msg :closed)
            (setf *tray-conn* nil)
            (return))
          (cond
            ((eq (dbus-message-type msg) +dbus-msg-call+)
             (handler-case
                 (dispatch-method-call msg conn)
               (error (e)
                 (format t "~&; Stumpway: tray: dispatch ~A.~A(~A): ~A~%"
                         (message-field msg 2) (message-field msg 3)
                         (message-field msg 8) e))))
            ((eq (dbus-message-type msg) +dbus-msg-signal+)
             (handler-case (tray-signal msg)
               (error (e)
                 (format t "~&; Stumpway: tray: signal: ~A~%" e))))))))))

(defun tray-click (item &optional (secondary nil))
  "Left/secondary-click an item: Activate / SecondaryActivate at
its current geometry (SNI takes x,y in item coordinates; 0,0 is
what most apps actually use for 'the icon was clicked')."
  (when *tray-conn*
    (handler-case
        (dbus-call *tray-conn* (getf item :name) (getf item :path)
                   +sni-iface+
                   (if secondary "SecondaryActivate" "Activate")
                   :signature "ii" :values (list 0 0)
                   :timeout 2)
      (error (e)
        (format t "~&; Stumpway: tray: click failed: ~A~%" e)))))

(defun tray-scroll (item delta)
  "Scroll over an item: Scroll(delta, 'vertical')."
  (when *tray-conn*
    (handler-case
        (dbus-call *tray-conn* (getf item :name) (getf item :path)
                   +sni-iface+ "Scroll"
                   :signature "is" :values (list delta "vertical")
                   :timeout 2)
      (error (e)
        (format t "~&; Stumpway: tray: scroll failed: ~A~%" e)))))

;;; --- dbusmenu: right-click menus mapped onto select-from-menu ---

(defun menu-prop (props name)
  "The DATUM of a dbusmenu property from an alist of
(NAME (SIG . VALUE)) pairs."
  (cdr (second (assoc name props :test #'string=))))

(defun menu-strip-accel (label)
  "Remove _ accelerator marks from LABEL (dbusmenu convention)."
  (remove #\_ label))

(defun menu-unwrap (node)
  "A dbusmenu layout node: either (id props children) — the
GetLayout root — or a VARIANT (sig id props children) — the
recursive children, which arrive as variants with signature
"ia{sv}av". Returns (id props children)."
  (if (stringp (first node))
      (rest node)                     ; variant: strip the signature
      node))

(defun menu-entries (node &optional (prefix ""))
  "Flatten a dbusmenu layout NODE into select-from-menu entries:
(LABEL . ID) pairs. Separators, invisible and disabled entries are
skipped; submenu children are indented with a prefix."
  (destructuring-bind (id props children) (menu-unwrap node)
    (declare (ignorable id))
    (flet ((prop-false (name)
             ;; present AND false: a missing prop means visible/enabled
             (let ((entry (assoc name props :test #'string=)))
               (and entry
                    (null (cdr (second entry)))))))
      (let ((type (menu-prop props "type"))
            (label (menu-prop props "label"))
            (submenu (string= (or (menu-prop props "children-display") "")
                              "submenu")))
        (append
          (unless (or (and type (string= type "separator"))
                      (prop-false "visible")
                      (prop-false "enabled"))
          (when (and label (not submenu))
            (list (cons (concatenate 'string prefix
                                      (menu-strip-accel label))
                        id))))
          (when (and submenu children)
            ;; The root menu node also carries children-display=submenu,
            ;; but it is a container, not an entry: only LABELLED
            ;; submenus indent their children
            (let ((child-prefix (if label
                                    (concatenate 'string prefix "  ")
                                    prefix)))
              (loop for child in children
                    append (menu-entries (menu-unwrap child)
                                          child-prefix)))))))))

(defun tray-menu-activate (conn bus-name menu-path id)
  "Click dbusmenu entry ID: Event(id, clicked, (), timestamp)."
  (dbus-call conn bus-name menu-path
             "com.canonical.dbusmenu" "Event"
             :signature "isvu"
             :values (list id "clicked"
                            (cons "i" 0)
                            0)
             :timeout 2))

(defun tray-show-menu (bus-name menu-path)
  "Fetch an item's dbusmenu and present it as a selection prompt.
WM-side: the bar's right-click invokes this via IPC; the menu
renders with the WM's prompt machinery (filter, navigation,
expiry) and clicking an entry sends Event back over DBus."
  (let ((conn (or *tray-conn* (dbus-connect))))
    (unless conn
      (message "tray: no session bus")
      (return-from tray-show-menu nil))
    ;; AboutToShow: some apps refresh their menu state on this
    (ignore-errors
      (dbus-call conn bus-name menu-path
                 "com.canonical.dbusmenu" "AboutToShow"
                 :signature "i" :values (list 0)
                 :timeout 2))
    (multiple-value-bind (reply body)
        (dbus-call conn bus-name menu-path
                   "com.canonical.dbusmenu" "GetLayout"
                   :signature "iias"
                   :values (list 0 -1 '())
                   :timeout 3)
      (unless (and reply (typep reply 'dbus-message)
                   (eq (dbus-message-type reply) +dbus-msg-return+))
        (message "tray: no menu from ~A" bus-name)
        (return-from tray-show-menu nil))
      (let ((layout (second body)))
        (let ((ids (menu-entries layout)))
          (if (null ids)
              (message "tray: empty menu")
              (select-from-menu "tray menu"
                (loop for (label . id) in ids
                      collect (cons label
                                    (lambda ()
                                      (tray-menu-activate
                                        conn bus-name menu-path id))))
                :on-select (lambda (entry)
                             (declare (ignore entry))
                             nil))))))))
