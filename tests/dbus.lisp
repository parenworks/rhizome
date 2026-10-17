;;; Tests for the pure-CL DBus wire layer: marshalling roundtrips
;;; plus LIVE integration against the real session bus (auth, Hello,
;;; RequestName, GetNameOwner, Properties.Get). The live section
;;; guards on the bus socket existing.

(ql:quickload :stumpway :silent t)

(in-package #:stumpway)

(let ((pass 0) (fail 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass) (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    ;; --- marshalling roundtrips ---
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:byte) 200)
      (ok "byte roundtrip" (nth-value 0 (dbus-unmarshal buf '(:byte) 0)) 200))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:boolean) t)
      (ok "boolean roundtrip"
          (nth-value 0 (dbus-unmarshal buf '(:boolean) 0)) t))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:string) "hello wörld")
      (ok "utf8 string roundtrip"
          (nth-value 0 (dbus-unmarshal buf '(:string) 0)) "hello wörld"))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:int32) -7)
      (ok "int32 roundtrip"
          (nth-value 0 (dbus-unmarshal buf '(:int32) 0))
          (ldb (byte 32 0) -7)))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:variant) (cons "s" "a variant"))
      (ok "variant roundtrip"
          (nth-value 0 (dbus-unmarshal buf '(:variant) 0))
          (cons "s" "a variant")))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:double) 1.5d0)
      (ok "double roundtrip"
          (nth-value 0 (dbus-unmarshal buf '(:double) 0)) 1.5d0))
    (let ((buf (make-dbus-buffer)))
      ;; the header-fields signature: array of (byte, variant).
      ;; The last element carries NO trailing padding inside the
      ;; array length (pinned against gdbus's Hello wire bytes;
      ;; dbus-broker rejects the padded form)
      (dbus-marshal buf '(:array (:struct (:byte) (:variant)))
                    (list (list 1 (cons "o" "/a/path"))
                          (list 3 (cons "s" "Member"))))
      (ok "a(yv) roundtrip"
          (nth-value 0
            (dbus-unmarshal buf '(:array (:struct (:byte) (:variant))) 0))
          (list (list 1 (cons "o" "/a/path"))
                (list 3 (cons "s" "Member")))))

    ;; --- the live broker ---
    (let ((conn (when (probe-file (dbus-bus-path)) (dbus-connect))))
      (ok "connects to the real session bus" (and conn t) t)
      (when conn
        (ok "Hello returned a unique name"
            (and (search ":1." (dbus-connection-unique-name conn)
                         :test #'string=)
                 t)
            t)
        (let ((names (dbus-list-names conn)))
          (ok "ListNames sees the broker's own name"
              (and (member "org.freedesktop.DBus" names :test #'string=) t)
              t))
        (let ((result (dbus-request-name conn "org.stumpway.Test")))
          (ok "RequestName succeeds (1 = primary owner)" result 1))
        (multiple-value-bind (reply body)
            (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
                       "org.freedesktop.DBus" "GetNameOwner"
                       :signature "s"
                       :values (list "org.stumpway.Test"))
          (ok "GetNameOwner returns our unique name"
              (and reply (first body)
                   (string= (first body)
                            (dbus-connection-unique-name conn)))
              t))
        (multiple-value-bind (reply body)
            (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
                       "org.freedesktop.DBus.Properties" "Get"
                       :signature "ss"
                       :values (list "org.freedesktop.DBus" "Features"))
          (ok "Properties.Get returns a variant (broker's Features)"
              (and reply (consp (first body))
                   (string= (car (first body)) "as"))
              t))
        (dbus-request-name conn "org.stumpway.Test" 0))
      (when conn
        (ok "reader thread running"
            (and (bt:thread-alive-p (dbus-connection-thread conn)) t)
            t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
