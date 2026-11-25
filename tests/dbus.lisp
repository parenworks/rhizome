;;; Tests for the pure-CL DBus wire layer: marshalling roundtrips
;;; plus LIVE integration against the real session bus (auth, Hello,
;;; RequestName, GetNameOwner, Properties.Get). The live section
;;; guards on the bus socket existing.

(ql:quickload :rhizome :silent t)

(in-package #:rhizome)

;;; notify-send 0.8.9 Notify calls captured from a session bus
;;; (dbus-monitor --binary), one whole little-endian message each.
(defparameter *notify-send-test-wire*
  "6c0100016c000000090000009600000001016f001e0000002f6f72672f667265
656465736b746f702f4e6f74696669636174696f6e730000020173001d000000
6f72672e667265656465736b746f702e4e6f74696669636174696f6e73000000
06017300050000003a312e3232000000080167000d73757373736173617b7376
7d6900000000000003017300060000004e6f7469667900000701730005000000
3a312e38330000000b0000006e6f746966792d73656e64000000000000000000
0000000004000000746573740000000000000000000000000000000030000000
07000000757267656e637900017900010a00000073656e6465722d7069640001
780000000000000083d501000000000020030000")

(defparameter *notify-send-critical-wire*
  "6c0100016c000000090000009600000001016f001e0000002f6f72672f667265
656465736b746f702f4e6f74696669636174696f6e730000020173001d000000
6f72672e667265656465736b746f702e4e6f74696669636174696f6e73000000
06017300050000003a312e3232000000080167000d73757373736173617b7376
7d6900000000000003017300060000004e6f7469667900000701730005000000
3a312e38350000000b0000006e6f746966792d73656e64000000000000000000
0000000004000000746573740000000000000000000000000000000030000000
07000000757267656e637900017900020a00000073656e6465722d7069640001
7800000000000000cad501000000000000000000")

(defparameter *notify-send-probe-wire*
  "6c0100017c000000090000009600000001016f001e0000002f6f72672f667265
656465736b746f702f4e6f74696669636174696f6e730000020173001d000000
6f72672e667265656465736b746f702e4e6f74696669636174696f6e73000000
06017300050000003a312e3232000000080167000d73757373736173617b7376
7d6900000000000003017300060000004e6f7469667900000701730005000000
3a312e38370000000b0000006e6f746966792d73656e64000000000000000000
00000000100000007268697a6f6d652d70726f62652d72310000000000000000
0000000000000000300000000000000007000000757267656e63790001790001
0a00000073656e6465722d706964000178000000000000000ad6010000000000
20030000")

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
    ;; an INT64 aligns to 8: after an INT32 it starts at offset 8,
    ;; not 4
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:struct (:int32) (:int64))
                    (list 7 #x0102030405060708))
      (ok "(ix) roundtrip, int64 after an int32"
          (nth-value 0
            (dbus-unmarshal buf '(:struct (:int32) (:int64)) 0))
          (list 7 #x0102030405060708)))
    (let ((buf (make-dbus-buffer)))
      (dbus-marshal buf '(:int32) 7)
      (dbus-marshal buf '(:uint64) #x1122334455667788)
      (ok "uint64 read from offset 4 decodes the 8-aligned value"
          (nth-value 0
            (dbus-unmarshal buf '(:uint64)
                            (nth-value 1 (dbus-unmarshal buf '(:int32) 0))))
          #x1122334455667788))
    (let ((buf (make-dbus-buffer)))
      ;; the array length excludes the 4 bytes of padding between
      ;; the length word and the first 8-aligned element; the last
      ;; element here is 4 bytes long and starts on the boundary
      ;; where the first one ends
      (dbus-marshal buf '(:array (:struct (:variant)))
                    (list (list (cons "s" "abcdefg"))
                          (list (cons "y" 5))))
      (ok "a(v) roundtrip keeps a short last element"
          (nth-value 0
            (dbus-unmarshal buf '(:array (:struct (:variant))) 0))
          (list (list (cons "s" "abcdefg"))
                (list (cons "y" 5)))))

    ;; --- recorded Notify calls ---
    (flet ((wire-octets (hex)
             (let ((digits (remove-if-not
                            (lambda (c) (digit-char-p c 16)) hex)))
               (coerce (loop for i from 0 below (length digits) by 2
                             collect (parse-integer digits :start i
                                                           :end (+ i 2)
                                                           :radix 16))
                       '(vector (unsigned-byte 8))))))
      (loop for (label hex summary urgency pid expire)
              in (list (list "notify-send -t 800 test"
                             *notify-send-test-wire* "test" 1 120195 800)
                       (list "notify-send -u critical -t 0 test"
                             *notify-send-critical-wire* "test" 2 120266 0)
                       (list "notify-send -t 800 rhizome-probe-r1"
                             *notify-send-probe-wire* "rhizome-probe-r1"
                             1 120330 800))
            do (let* ((msg (ignore-errors (parse-message (wire-octets hex))))
                      (body (and msg (dbus-message-body msg)))
                      (hints (seventh body)))
                 (flet ((hint (key)
                          (cdr (second (assoc key hints :test #'equal)))))
                   (ok (format nil "~A: parses" label) (and msg t) t)
                   (ok (format nil "~A: summary" label)
                       (fourth body) summary)
                   (ok (format nil "~A: urgency hint" label)
                       (hint "urgency") urgency)
                   (ok (format nil "~A: sender-pid hint" label)
                       (hint "sender-pid") pid)
                   (ok (format nil "~A: expire timeout" label)
                       (eighth body) expire)))))

    ;; --- the live broker ---
    (let ((conn (when (probe-file (dbus-bus-path)) (dbus-connect))))
      (if (probe-file (dbus-bus-path))
          (ok "connects to the real session bus" (and conn t) t)
          (format t "~&SKIP: no D-Bus session bus at ~A; live broker assertions skipped~%"
                  (dbus-bus-path)))
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
        (let ((result (dbus-request-name conn "org.rhizome.Test")))
          (ok "RequestName succeeds (1 = primary owner)" result 1))
        (multiple-value-bind (reply body)
            (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
                       "org.freedesktop.DBus" "GetNameOwner"
                       :signature "s"
                       :values (list "org.rhizome.Test"))
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
        (dbus-request-name conn "org.rhizome.Test" 0))
      (when conn
        (ok "reader thread running"
            (and (bt:thread-alive-p (dbus-connection-thread conn)) t)
            t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
