;;; DBus wire protocol for Rhizome — pure Common Lisp
;;;
;;; The system tray (an org.kde.StatusNotifierWatcher host) needs a
;;; DBus connection, and rhizome stays a pure-CL application: this
;;; file speaks the DBus wire protocol directly over a Unix socket
;;; (minimal CFFI shims, the same approach ipc.lisp takes), with no C
;;; library, no foreign bindings, and no quicklisp dependencies —
;;; not even babel or ieee-floats (UTF-8 and IEEE-754 doubles are
;;; implemented locally, below).
;;;
;;; Layout:
;;;   - socket shims (AF_UNIX SOCK_STREAM, standalone)
;;;   - UTF-8 string <-> octets, IEEE-754 double encode/decode
;;;   - signature parsing and alignment-aware marshalling
;;;   - message framing (fixed header + a(yv) fields + body)
;;;   - connection: SASL EXTERNAL auth, serial bookkeeping, the
;;;     org.freedesktop.DBus bootstrap (Hello, RequestName, AddMatch,
;;;     ListNames), a reader thread that completes pending calls,
;;;     and a main-loop drain for incoming method calls and signals.

(in-package #:rhizome)

;;; --- Socket shims (standalone; same foreign-funcall style as ipc.lisp) ---

(defun %sock-socket ()
  (cffi:foreign-funcall "socket" :int +af-unix+ :int +sock-stream+
                         :int 0 :int))
(defun %sock-connect (fd addr len)
  (cffi:foreign-funcall "connect" :int fd :pointer addr :int len :int))
(defun %sock-read (fd buf len)
  (cffi:foreign-funcall "read" :int fd :pointer buf :long len :long))
(defun %sock-write (fd buf len)
  (cffi:foreign-funcall "write" :int fd :pointer buf :long len :long))
(defun %sock-close (fd)
  (cffi:foreign-funcall "close" :int fd :int))
(defun %sock-getuid ()
  (cffi:foreign-funcall "getuid" :int))

(defconstant +af-unix+ 1)
(defconstant +sock-stream+ 1)
(defconstant +sun-path-max+ 108)

(defun sock-connect-unix (path)
  "Connect a fresh socket fd to the Unix socket at PATH, or NIL."
  (let ((fd (%sock-socket)))
    (when (minusp fd)
      (return-from sock-connect-unix nil))
    (cffi:with-foreign-object (addr :char (+ 2 +sun-path-max+))
      (setf (cffi:mem-ref addr :uint16 0) +af-unix+)
      (let ((n (min +sun-path-max+ (length path))))
        (dotimes (i n)
          (setf (cffi:mem-ref addr :char (+ 2 i)) (char-code (aref path i))))
        (setf (cffi:mem-ref addr :char (+ 2 n)) 0)
        (if (minusp (%sock-connect fd addr (+ 2 n)))
            (progn (%sock-close fd) nil)
            fd)))))

(defun sock-write-all (fd bytes)
  "Write the whole byte vector BYTES to FD; T or NIL."
  (let ((sent 0) (len (length bytes)))
    (cffi:with-foreign-pointer (buf len %size)
      (dotimes (i len)
        (setf (cffi:mem-ref buf :unsigned-char i) (aref bytes i)))
      (loop
        (when (>= sent len) (return t))
        (let ((w (%sock-write fd buf (- len sent))))
          (when (<= w 0) (return-from sock-write-all nil))
          (incf sent w))))))

(defun sock-read-exact (fd length)
  "Read exactly LENGTH bytes from FD into a fresh vector; NIL at EOF."
  (let ((out (make-array length :element-type '(unsigned-byte 8)))
        (got 0))
    (loop
      (when (>= got length) (return out))
      (cffi:with-foreign-pointer (buf (- length got) %size)
        (let ((r (%sock-read fd buf (- length got))))
          (when (<= r 0) (return-from sock-read-exact nil))
          (dotimes (i r)
            (setf (aref out (+ got i))
                  (cffi:mem-ref buf :unsigned-char i)))
          (incf got r))))))

;;; --- Local UTF-8 and IEEE-754 (no babel, no ieee-floats) ---

(defun string->utf8 (string)
  "UTF-8 octets of STRING (no terminator)."
  (let ((out (make-array (max 8 (* 4 (length string)))
                         :element-type '(unsigned-byte 8)
                         :fill-pointer 0 :adjustable t)))
    (flet ((emit (byte) (vector-push-extend byte out)))
      (loop for ch across string
            for code = (char-code ch)
            do (cond ((< code #x80) (emit code))
                     ((< code #x800)
                      (emit (logior #b11000000 (ash code -6)))
                      (emit (logior #b10000000 (logand code #x3f))))
                     ((< code #x10000)
                      (emit (logior #b11100000 (ash code -12)))
                      (emit (logior #b10000000 (logand (ash code -6) #x3f)))
                      (emit (logior #b10000000 (logand code #x3f))))
                     (t
                      (emit (logior #b11110000 (ash code -18)))
                      (emit (logior #b10000000 (logand (ash code -12) #x3f)))
                      (emit (logior #b10000000 (logand (ash code -6) #x3f)))
                      (emit (logior #b10000000 (logand code #x3f)))))))
    out))

(defun utf8->string (bytes &optional (start 0) (end (length bytes)))
  "Decode UTF-8 octets [START, END) of BYTES into a string."
  (with-output-to-string (s)
    (let ((i start))
      (loop while (< i end)
            do (let ((b (aref bytes i)))
                 (cond ((< b #x80)
                        (write-char (code-char b) s) (incf i))
                       ((< b #xE0)
                        (write-char
                          (code-char (logior
                                       (ash (logand b #x1F) 6)
                                       (logand (aref bytes (+ i 1)) #x3F)))
                          s)
                        (incf i 2))
                       ((< b #xF0)
                        (write-char
                          (code-char (logior
                                       (ash (logand b #x0F) 12)
                                       (ash (logand (aref bytes (+ i 1)) #x3F) 6)
                                       (logand (aref bytes (+ i 2)) #x3F)))
                          s)
                        (incf i 3))
                       (t
                        (write-char
                          (code-char (logior
                                       (ash (logand b #x07) 18)
                                       (ash (logand (aref bytes (+ i 1)) #x3F) 12)
                                       (ash (logand (aref bytes (+ i 2)) #x3F) 6)
                                       (logand (aref bytes (+ i 3)) #x3F)))
                          s)
                        (incf i 4))))))))

(defun encode-double64 (value)
  "IEEE-754 bits of a double as an unsigned integer."
  (multiple-value-bind (mantissa exponent sign)
      (integer-decode-float (coerce value 'double-float))
    (let* ((k (integer-length mantissa))
           (biased (+ exponent 1022 k))
           ;; integer-decode-float's mantissa includes the implied
           ;; leading bit; strip it and shift the remainder into
           ;; the 52-bit fraction field
           (fraction (ash (- mantissa (ash 1 (1- k)))
                          (- 52 (1- k)))))
      (logior (ash (if (minusp sign) 1 0) 63)
              (ash (ldb (byte 11 0) biased) 52)
              (ldb (byte 52 0) fraction)))))

(defun decode-double64 (bits)
  "A double from IEEE-754 little-endian BITS."
  (let ((sign (if (logbitp 63 bits) -1.0d0 1.0d0))
        (exponent (ldb (byte 11 52) bits))
        (mantissa (ldb (byte 52 0) bits)))
    (cond ((zerop exponent)
           (if (zerop mantissa)
               0.0d0
               (* sign mantissa (expt 2 -1074))))
          ((= exponent 2047)
           (if (zerop mantissa)
               (* sign sb-ext:double-float-positive-infinity)
               sb-ext:double-float-positive-infinity)) ; NaN-ish
          (t (* sign (coerce (1+ (/ mantissa (expt 2 52))) 'double-float)
                 (expt 2 (- exponent 1023)))))))

;;; --- Signature parsing ---
;;;
;;; A signature string maps to a tree of types:
;;;   "a(yv)" -> ((:ARRAY (:STRUCT (:BYTE) (:VARIANT))))

(defun parse-dbus-signature (signature &optional (start 0))
  "Parse one complete type from SIGNATURE at START.
Returns (values type-tree next-index)."
  (let ((ch (and (< start (length signature)) (aref signature start))))
    (case ch
      ((#\y #\b #\n #\q #\i #\u #\x #\t #\d #\s #\o #\g #\h #\v)
       (values (list (ecase ch
                      (#\y :byte) (#\b :boolean) (#\n :int16) (#\q :uint16)
                      (#\i :int32) (#\u :uint32) (#\x :int64) (#\t :uint64)
                      (#\d :double) (#\s :string) (#\o :object-path)
                      (#\g :signature) (#\h :unix-fd) (#\v :variant)))
                    (1+ start)))
      (#\a
       (multiple-value-bind (elem next)
           (parse-dbus-signature signature (1+ start))
         (values (list :array elem) next)))
      (#\(
       (loop with fields = nil with pos = (1+ start)
             do (multiple-value-bind (type next)
                    (parse-dbus-signature signature pos)
                  (push type fields)
                  (setf pos next))
             while (< pos (length signature))
             until (char= (aref signature pos) #\))
             finally (return (values (cons :struct (nreverse fields))
                                     (1+ pos)))))
      (#\{
       (multiple-value-bind (key next1)
           (parse-dbus-signature signature (1+ start))
         (multiple-value-bind (val next2)
             (parse-dbus-signature signature next1)
           (values (list :dict-entry key val) (1+ next2)))))
      (t (error "bad signature char ~S at ~D" ch start)))))

(defun parse-dbus-signatures (signature)
  "Parse ALL complete types in SIGNATURE."
  (loop with pos = 0
        while (< pos (length signature))
        collect (multiple-value-bind (type next)
                    (parse-dbus-signature signature pos)
                  (setf pos next)
                  type)))

(defun type-alignment (type)
  "Wire alignment (byte offset the value must sit at)."
  (ecase (first type)
    (:byte 1) (:boolean 4) (:int16 2) (:uint16 2) (:int32 4)
    (:uint32 4) (:int64 8) (:uint64 8) (:double 8) (:string 4)
    (:object-path 4) (:signature 1) (:unix-fd 4)
    (:array 4) (:struct 8) (:dict-entry 8) (:variant 1)))

;;; --- Marshalling ---
;;;
;;; The buffer holds the whole message from byte 0, so a value's
;;; absolute offset is the fill-pointer.
;;;
;;; Lisp representations:
;;;   byte / ints / double : integers / double-float
;;;   boolean             : T or NIL
;;;   string / path / sig : string
;;;   array               : list of element values
;;;   struct              : list of field values
;;;   dict-entry          : (list key value)
;;;   variant             : (cons signature-string datum)

(defun make-dbus-buffer ()
  "A fresh growable message buffer."
  (make-array 64 :element-type '(unsigned-byte 8)
                :fill-pointer 0 :adjustable t :initial-element 0))

(defun dbus-buffer-p (buffer alignment)
  "Pad BUFFER to ALIGNMENT."
  (let ((pad (mod (- alignment (mod (fill-pointer buffer) alignment))
                  alignment)))
    (dotimes (i pad) (vector-push-extend 0 buffer))))

(defun dbus-marshal (buffer type value)
  "Append VALUE marshalled as TYPE to BUFFER."
  (ecase (first type)
    (:byte
     (vector-push-extend (ldb (byte 8 0) value) buffer))
    ((:boolean :int32 :uint32 :unix-fd)
     (dbus-buffer-p buffer 4)
     (let ((n (ecase (first type)
                (:boolean (if value 1 0))
                (:int32 (ldb (byte 32 0) value))
                (:uint32 (ldb (byte 32 0) value))
                (:unix-fd (ldb (byte 32 0) value)))))
       (dotimes (i 4)
         (vector-push-extend (ldb (byte 8 (* 8 i)) n) buffer))))
    ((:int16 :uint16)
     (dbus-buffer-p buffer 2)
     (let ((n (ldb (byte 16 0) value)))
       (dotimes (i 2)
         (vector-push-extend (ldb (byte 8 (* 8 i)) n) buffer))))
    ((:int64 :uint64)
     (dbus-buffer-p buffer 8)
     (let ((n (ldb (byte 64 0) value)))
       (dotimes (i 8)
         (vector-push-extend (ldb (byte 8 (* 8 i)) n) buffer))))
    (:double
     (dbus-buffer-p buffer 8)
     (let ((n (encode-double64 value)))
       (dotimes (i 8)
         (vector-push-extend (ldb (byte 8 (* 8 i)) n) buffer))))
    ((:string :object-path)
     (dbus-buffer-p buffer 4)
     (let ((octets (string->utf8 value)))
       (dotimes (i 4)
         (vector-push-extend
           (ldb (byte 8 (* 8 i)) (length octets)) buffer))
       (loop for b across octets do (vector-push-extend b buffer))
       (vector-push-extend 0 buffer)))
    (:signature
     (vector-push-extend (length value) buffer)
     (loop for c across value do (vector-push-extend (char-code c) buffer))
     (vector-push-extend 0 buffer))
    (:array
     ;; u32 length placeholder, element padding BETWEEN elements (each
     ;; element starts aligned), elements — then the length fixup.
     ;; NO trailing padding: the last element's trailing alignment
     ;; padding belongs to the enclosing message, not the array
     ;; (pinned byte-for-byte against gdbus's Hello: array length 110,
     ;; message-level pad 2 after it; dbus-broker rejects the padded
     ;; form)
     (dbus-buffer-p buffer 4)
     (let ((len-pos (fill-pointer buffer)))
       (dotimes (i 4) (vector-push-extend 0 buffer))
       (dbus-buffer-p buffer (type-alignment (second type)))
       (let ((start (fill-pointer buffer))
             (first t))
         (dolist (elem value)
           (unless first
             (dbus-buffer-p buffer (type-alignment (second type))))
           (setf first nil)
           (dbus-marshal buffer (second type) elem))
         (dotimes (i 4)
           (setf (aref buffer (+ len-pos i))
                 (ldb (byte 8 (* 8 i))
                      (- (fill-pointer buffer) start)))))))
    ((:struct :dict-entry)
     (dbus-buffer-p buffer 8)
     (loop for f in (rest type)
           for v in value
           do (dbus-marshal buffer f v)))
    (:variant
     (let ((sig (car value))
           (datum (cdr value)))
       (vector-push-extend (length sig) buffer)
       (loop for c across sig do (vector-push-extend (char-code c) buffer))
       (vector-push-extend 0 buffer)
       (multiple-value-bind (vt) (parse-dbus-signature sig)
         (dbus-marshal buffer vt datum))))))

;;; --- Unmarshalling ---

(defun dbus-unmarshal (bytes type &optional (offset 0))
  "Read a value of TYPE from BYTES at OFFSET.
Returns (values value new-offset)."
  (macrolet ((aligned (alignment)
              `(let ((pad (mod (- ,alignment (mod offset ,alignment))
                               ,alignment)))
                 (incf offset pad)))
             (u8 () `(progn (prog1 (aref bytes offset) (incf offset))))
             (u16 ()
               `(progn
                  (aligned 2)
                  (prog1 (+ (aref bytes offset)
                            (ash (aref bytes (+ offset 1)) 8))
                    (incf offset 2))))
             (u32 ()
               `(progn
                  (aligned 4)
                  (prog1 (+ (aref bytes offset)
                            (ash (aref bytes (+ offset 1)) 8)
                            (ash (aref bytes (+ offset 2)) 16)
                            (ash (aref bytes (+ offset 3)) 24))
                    (incf offset 4))))
             (u32v ()
               `(let ((n (u32))) n))
             (sized-string (prefix-align terminator)
                 `(progn
                    (aligned ,prefix-align)
                    (let* ((len (u32))
                           (start offset))
                      (incf offset (+ len ,terminator))
                      (utf8->string bytes start (+ start len))))))
    (ecase (first type)
      (:byte (values (u8) offset))
      (:boolean (let ((n (u32v))) (values (not (zerop n)) offset)))
      ((:int32 :uint32 :unix-fd) (values (u32v) offset))
      ((:int16 :uint16) (values (u16) offset))
      ((:int64 :uint64)
       (let ((lo (u32v)) (hi (u32v)))
         (values (+ lo (ash hi 32)) offset)))
      (:double
       (aligned 8)
       (let ((bits (loop for i from 7 downto 0
                         sum (ash (aref bytes (+ offset i)) (* 8 i)))))
         (incf offset 8)
         (values (decode-double64 (ldb (byte 64 0) bits)) offset)))
      ((:string :object-path) (values (sized-string 4 1) offset))
      (:signature
       (aligned 1)
       (let* ((len (u8))
              (start offset))
         (incf offset (1+ len))
         (values (utf8->string bytes start (+ start len)) offset)))
      (:array
       (let* ((len (u32v))
              (elem-align (type-alignment (second type)))
              (pad (mod (- elem-align (mod offset elem-align))
                        elem-align)))
         (decf len pad)
         (incf offset pad)
         (let ((end (+ offset len))
               (elems nil))
           (loop while (< offset end)
                 do (multiple-value-bind (v new)
                        (dbus-unmarshal bytes (second type) offset)
                      (push v elems)
                      (setf offset new)))
           (values (nreverse elems) offset))))
      ((:struct :dict-entry)
       (aligned 8)
       (let ((vals nil))
         (dolist (f (rest type))
           (multiple-value-bind (v new) (dbus-unmarshal bytes f offset)
             (push v vals)
             (setf offset new)))
         (values (nreverse vals) offset)))
      (:variant
       (multiple-value-bind (sig new)
           (dbus-unmarshal bytes '(:signature) offset)
         (setf offset new)
         (multiple-value-bind (vt) (parse-dbus-signature sig)
           (multiple-value-bind (v new2) (dbus-unmarshal bytes vt offset)
             (values (cons sig v) new2))))))))

;;; --- Messages ---

(defconstant +dbus-msg-call+ 1)
(defconstant +dbus-msg-return+ 2)
(defconstant +dbus-msg-error+ 3)
(defconstant +dbus-msg-signal+ 4)

(defstruct dbus-message
  "One framed DBus message. FIELDS is an alist of
(header-field-code . value); BODY holds decoded Lisp values."
  type serial fields body)

(defun message-field (message code)
  "The value of header field CODE, or NIL."
  (cdr (assoc code (dbus-message-fields message))))

(defun (setf message-field) (value message code)
  (let ((entry (assoc code (dbus-message-fields message))))
    (if entry
        (setf (cdr entry) value)
        (push (cons code value) (dbus-message-fields message)))
    value))

(defstruct (outgoing-message (:conc-name om-))
  "A message being built. TYPES are parsed from SIGNATURE; the
header FIELD-CODES parallel the wire field codes."
  type destination path interface member error-name reply-serial
  signature values)

(defun build-outgoing (out &optional (serial 1))
  "Serialize an OUTGOING-MESSAGE into a byte vector."
  (let ((body (make-dbus-buffer)))
    (when (om-signature out)
      (loop for type in (parse-dbus-signatures (om-signature out))
            for value in (om-values out)
            do (dbus-marshal body type value)))
    (let ((buffer (make-dbus-buffer))
          (fields nil))
      (vector-push-extend (char-code #\l) buffer) ; little endian
      (vector-push-extend (om-type out) buffer)
      (vector-push-extend 0 buffer)               ; flags
      (vector-push-extend 1 buffer)               ; protocol version
      (dotimes (i 4)
        (vector-push-extend
          (ldb (byte 8 (* 8 i)) (fill-pointer body)) buffer))
      (dotimes (i 4)
        (vector-push-extend (ldb (byte 8 (* 8 i)) serial) buffer))
      (when (om-path out) (push (cons 1 (cons "o" (om-path out))) fields))
      (when (om-interface out)
        (push (cons 2 (cons "s" (om-interface out))) fields))
      (when (om-member out) (push (cons 3 (cons "s" (om-member out))) fields))
      (when (om-error-name out)
        (push (cons 4 (cons "s" (om-error-name out))) fields))
      (when (om-reply-serial out)
        (push (cons 5 (cons "u" (om-reply-serial out))) fields))
      (when (om-destination out)
        (push (cons 6 (cons "s" (om-destination out))) fields))
      (when (om-signature out)
        (push (cons 8 (cons "g" (om-signature out))) fields))
      (dbus-marshal buffer '(:array (:struct (:byte) (:variant)))
                    (mapcar (lambda (entry)
                              (destructuring-bind (code . (sig . value))
                                  entry
                                (list code (cons sig value))))
                            (nreverse fields)))
      (dbus-buffer-p buffer 8)
      (loop for b across body do (vector-push-extend b buffer))
      buffer)))

(defun parse-message (bytes)
  "Parse a complete message from BYTES."
  (let* ((type (aref bytes 1))
         (body-len (+ (aref bytes 4) (ash (aref bytes 5) 8)
                      (ash (aref bytes 6) 16) (ash (aref bytes 7) 24)))
         (serial (+ (aref bytes 8) (ash (aref bytes 9) 8)
                    (ash (aref bytes 10) 16) (ash (aref bytes 11) 24))))
    (multiple-value-bind (fields-raw offset)
        (dbus-unmarshal bytes '(:array (:struct (:byte) (:variant))) 12)
      (let* ((fields (loop for (code variant) in fields-raw
                           collect (cons code (cdr variant))))
             (msg (make-dbus-message :type type :serial serial
                                     :fields fields))
             (sig (message-field msg 8)))
        (when (and sig (plusp (length sig)))
          ;; the body starts at the next 8-byte boundary
          (let* ((body-start (* 8 (ceiling offset 8)))
                 (decoded nil)
                 (pos body-start))
            (dolist (type-tree (parse-dbus-signatures sig))
              (multiple-value-bind (v new)
                  (dbus-unmarshal bytes type-tree pos)
                (push v decoded)
                (setf pos new)))
            (setf (dbus-message-body msg) (nreverse decoded))))
        msg))))

;;; --- Connection ---

(defparameter *dbus-connection* nil
  "The WM's session-bus connection, or NIL.")

(defstruct dbus-connection
  fd lock pending (next-serial 0) queue queue-lock queue-cond
  running thread unique-name)

(defun dbus-bus-path ()
  "The session bus socket path (environment or the usual place)."
  (let ((addr (uiop:getenv "DBUS_SESSION_BUS_ADDRESS"))
        (prefix "unix:path="))
    (or (and addr
             (>= (length addr) (length prefix))
             (string= addr prefix :end2 (length prefix))
             (subseq addr (length prefix)))
        (format nil "/run/user/~D/bus" (%sock-getuid)))))

(defun dbus-send (conn bytes)
  "Write BYTES to the connection."
  (bt:with-lock-held ((dbus-connection-lock conn))
    (sock-write-all (dbus-connection-fd conn) bytes)))

(defun dbus-send-outgoing (conn out)
  "Assign a serial, serialize and send OUT; returns the serial (not
the socket-write result — callers match replies on this)."
  (let ((serial (incf (dbus-connection-next-serial conn))))
    (dbus-send conn (build-outgoing out serial))
    serial))

(defun dbus-read-line (fd)
  "Read a \\r\\n-terminated ASCII line from FD (byte at a time)."
  (with-output-to-string (s)
    (loop
      (let ((bytes (sock-read-exact fd 1)))
        (cond ((null bytes) (return-from dbus-read-line nil))
              ((= (aref bytes 0) (char-code #\Newline)) (return s))
              ((= (aref bytes 0) (char-code #\Return)))
              (t (write-char (code-char (aref bytes 0)) s)))))))

(defun dbus-auth (fd)
  "SASL EXTERNAL authentication over FD."
  (let* (;; EXTERNAL's initial response is the hex encoding of the
         ;; DECIMAL UID STRING's bytes ("1000" -> "31303030"),
         ;; not the number in hex
         (uid-hex (format nil "~(~{~X~}~)"
                          (map 'list 'char-code
                               (format nil "~D" (%sock-getuid)))))
         (cmd (concatenate 'string "AUTH EXTERNAL " uid-hex))
         (octets (make-array (+ 2 (length cmd))
                              :element-type '(unsigned-byte 8)
                              :fill-pointer 0)))
    (vector-push-extend 0 octets)               ; the leading NUL byte
    (loop for c across cmd do (vector-push-extend (char-code c) octets))
    (vector-push-extend (char-code #\Return) octets)
    (vector-push-extend (char-code #\Linefeed) octets)
    (unless (sock-write-all fd octets)
      (return-from dbus-auth nil))
    (let ((line (dbus-read-line fd)))
      (unless (and line (>= (length line) 2) (string= line "OK" :end1 2))
        (format t "~&; Rhizome: dbus: auth rejected: ~S~%" line)
        (return-from dbus-auth nil))
      (sock-write-all
        fd (string->utf8 (format nil "BEGIN~C~C" #\Return #\Newline)))
      t)))

(defun dbus-read-message (fd)
  "Read one complete message from FD; NIL on EOF/error."
  (let ((fixed (sock-read-exact fd 16)))
    (unless fixed
      (return-from dbus-read-message nil))
    (let* ((body-len (+ (aref fixed 4) (ash (aref fixed 5) 8)
                        (ash (aref fixed 6) 16) (ash (aref fixed 7) 24)))
           (fields-len (+ (aref fixed 12) (ash (aref fixed 13) 8)
                          (ash (aref fixed 14) 16)
                          (ash (aref fixed 15) 24)))
           ;; the fields array starts at offset 12; its u32 length is
           ;; included in the 16 fixed bytes we already have
           (header-size (* 8 (ceiling (+ 16 fields-len) 8))))
      (let ((rest-header (if (> header-size 16)
                             (sock-read-exact fd (- header-size 16))
                             #())))
        (when (and (> header-size 16) (null rest-header))
          (return-from dbus-read-message nil))
        (let* ((header (if (> header-size 16)
                           (let ((all (make-array header-size
                                                  :element-type
                                                  '(unsigned-byte 8))))
                             (replace all fixed)
                             (replace all rest-header :start1 16)
                             all)
                           fixed))
               (body (if (plusp body-len)
                         (sock-read-exact fd body-len)
                         #())))
          (when (and (plusp body-len) (null body))
            (return-from dbus-read-message nil))
          (let ((all (make-array (+ header-size (length body))
                                 :element-type '(unsigned-byte 8))))
            (replace all header)
            (replace all body :start1 header-size)
            (parse-message all)))))))

(defun dbus-connect (&key (path (dbus-bus-path)))
  "Connect and authenticate to the session bus. Returns a
DBUS-CONNECTION, or NIL on failure."
  (let ((fd (sock-connect-unix path)))
    (unless fd
      (format t "~&; Rhizome: dbus: cannot connect to ~A~%" path)
      (return-from dbus-connect nil))
    (unless (dbus-auth fd)
      (%sock-close fd)
      (return-from dbus-connect nil))
    (let ((conn (make-dbus-connection
                  :fd fd
                  :lock (bt:make-lock "dbus")
                  :pending (make-hash-table)
                  :queue nil
                  :queue-lock (bt:make-lock "dbus-queue")
                  :queue-cond (bt:make-condition-variable)
                  :running t)))
      ;; The reader thread must be running BEFORE any call goes
      ;; out: replies arrive asynchronously and someone has to be
      ;; reading the socket when they do
      (setf (dbus-connection-thread conn)
            (bt:make-thread
              (lambda () (dbus-reader-loop conn))
              :name "rhizome-dbus-reader"))
      ;; Hello: the bus assigns our unique name
      (let* ((serial (dbus-send-outgoing
                       conn
                       (make-outgoing-message
                         :type +dbus-msg-call+
                         :destination "org.freedesktop.DBus"
                         :path "/org/freedesktop/DBus"
                         :interface "org.freedesktop.DBus"
                         :member "Hello"
                         :signature "" :values nil)))
             (reply (dbus-await-reply conn serial 2)))
        (unless (and reply (eq (dbus-message-type reply)
                               +dbus-msg-return+))
          (format t "~&; Rhizome: dbus: Hello failed: ~A~%"
                  (and reply (first (dbus-message-body reply))))
          (%sock-close fd)
          (return-from dbus-connect nil))
        (setf (dbus-connection-unique-name conn)
              (first (dbus-message-body reply))))
      (format t "~&; Rhizome: dbus: connected as ~A~%"
              (dbus-connection-unique-name conn))
      conn)))

(defun dbus-reader-loop (conn)
  "The reader thread: read messages, complete pending calls,
enqueue everything else for the main-loop drain. Exits when the
socket closes, marking the connection down and waking the drain."
  (let ((fd (dbus-connection-fd conn)))
    (loop
      (let ((msg (dbus-read-message fd)))
        (when (null msg)
          (setf (dbus-connection-running conn) nil)
          (bt:with-lock-held ((dbus-connection-queue-lock conn))
            (push :closed (dbus-connection-queue conn)))
          (bt:condition-notify (dbus-connection-queue-cond conn))
          (return))
        (cond
          ((and (member (dbus-message-type msg)
                        (list +dbus-msg-return+ +dbus-msg-error+))
                (message-field msg 5))
           (bt:with-lock-held ((dbus-connection-lock conn))
             (setf (gethash (message-field msg 5)
                            (dbus-connection-pending conn))
                   msg)))
          (t
           (bt:with-lock-held ((dbus-connection-queue-lock conn))
             (push msg (dbus-connection-queue conn)))))))))

(defun dbus-await-reply (conn serial &optional (timeout 5))
  "Block until the reply to SERIAL arrives (or TIMEOUT seconds)."
  (let ((deadline (+ (get-universal-time) timeout)))
    (loop
      (let ((reply (bt:with-lock-held ((dbus-connection-lock conn))
                     (gethash serial (dbus-connection-pending conn)))))
        (when reply
          (bt:with-lock-held ((dbus-connection-lock conn))
            (remhash serial (dbus-connection-pending conn)))
          (return reply))
        (when (> (get-universal-time) deadline)
          (return nil))
        (sleep 0.01)))))

(defun dbus-call (conn destination path interface member
                 &key (signature "") values (timeout 5))
  "Send a method call; returns (values reply decoded-body)."
  (let ((serial (dbus-send-outgoing
                  conn
                  (make-outgoing-message
                    :type +dbus-msg-call+
                    :destination destination :path path
                    :interface interface :member member
                    :signature signature :values values))))
    (let ((reply (dbus-await-reply conn serial timeout)))
      (values reply (and reply (dbus-message-body reply))))))

(defun dbus-request-name (conn name &optional (flags 4))
  "Request a well-known name; 1 on success. Flags 4 = DO_NOT_QUEUE."
  (multiple-value-bind (reply body)
      (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
                 "org.freedesktop.DBus" "RequestName"
                 :signature "su" :values (list name flags))
    (declare (ignore reply))
    (first body)))

(defun dbus-add-match (conn rule)
  "Subscribe to signals matching RULE."
  (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
             "org.freedesktop.DBus" "AddMatch"
             :signature "s" :values (list rule))
  t)

(defun dbus-list-names (conn)
  "All names on the bus (unique and well-known)."
  (multiple-value-bind (reply body)
      (dbus-call conn "org.freedesktop.DBus" "/org/freedesktop/DBus"
                 "org.freedesktop.DBus" "ListNames"
                 :signature "" :values nil)
    (declare (ignore reply))
    (first body)))

;;; --- Main-loop integration (stub pattern: wm-protocol loads
;;; earlier and calls DBUS-DRAIN in its event loop) ---

(defun dbus-drain ()
  "Dispatch queued incoming messages on the WM main thread. The
default drops nothing: the tray (the first consumer) redefines
this with real method-call and signal dispatch."
  nil)

;;; --- Service side: replies, signals, handler dispatch ---

(defun dbus-reply (conn msg &key (signature "") values)
  "Reply to an incoming METHOD_CALL message with VALUES."
  (when (message-field msg 7)               ; sender
    (dbus-send-outgoing
      conn
      (make-outgoing-message
        :type +dbus-msg-return+
        :destination (message-field msg 7)
        :reply-serial (dbus-message-serial msg)
        :signature signature
        :values values))))

(defun dbus-error-reply (conn msg error-name text)
  "Reply to an incoming METHOD_CALL with a DBus error."
  (when (message-field msg 7)
    (dbus-send-outgoing
      conn
      (make-outgoing-message
        :type +dbus-msg-error+
        :destination (message-field msg 7)
        :error-name error-name
        :reply-serial (dbus-message-serial msg)
        :signature "s"
        :values (list text)))))

(defun dbus-emit-signal (conn path interface member &key (signature "")
                                                  values)
  "Emit a broadcast SIGNAL (every subscriber with a match rule
hears it)."
  (dbus-send-outgoing
    conn
    (make-outgoing-message
      :type +dbus-msg-signal+
      :path path :interface interface :member member
      :signature signature :values values)))

(defparameter *dbus-handlers* (make-hash-table :test 'equal)
  "Service-side method-call handlers: (INTERFACE . MEMBER) ->
function (MESSAGE CONN) returning (values signature values), or
:ERROR (NAME TEXT). Populated by the service (the tray watcher)
at startup.")

(defparameter *dbus-property-getters* (make-hash-table :test 'equal)
  "Interface name -> function returning an alist of
(PROPERTY-NAME . (cons signature value)) for Properties.GetAll.")

(defun (setf dbus-handler) (fn interface member)
  (setf (gethash (cons interface member) *dbus-handlers*) fn))

(defun dispatch-method-call (msg conn)
  "Handle one incoming METHOD_CALL: the org.freedesktop.DBus
standard interfaces (Properties) plus the registered handlers."
  (let ((interface (message-field msg 2))
        (member (message-field msg 3)))
    (cond
      ((and (string= interface "org.freedesktop.DBus.Properties")
            (string= member "GetAll"))
       (let ((getter (gethash (first (dbus-message-body msg))
                              *dbus-property-getters*)))
         (if getter
             (dbus-reply conn msg
                         :signature "a{sv}"
                         :values (list
                                   (loop for (name . (sig . value))
                                           in (funcall getter)
                                         collect (list name
                                                       (cons sig value)))))
             (dbus-error-reply conn msg
                               "org.freedesktop.DBus.Error.UnknownInterface"
                               (format nil "unknown interface ~A"
                                       (first (dbus-message-body msg)))))))
      ((and (string= interface "org.freedesktop.DBus.Properties")
            (string= member "Get"))
       ;; single-property Get: serve it from the GetAll collection
       (let ((getter (gethash (first (dbus-message-body msg))
                              *dbus-property-getters*)))
         (if getter
             (let* ((want (second (dbus-message-body msg)))
                    (entry (assoc want (funcall getter)
                                  :test #'string=)))
               (if entry
                   (destructuring-bind (name . (sig . value)) entry
                     (declare (ignore name))
                     (dbus-reply conn msg
                                 :signature "v"
                                 :values (list (cons sig value))))
                   (dbus-error-reply
                     conn msg
                     "org.freedesktop.DBus.Error.InvalidArgs"
                     (format nil "no such property ~A" want))))
             (dbus-error-reply conn msg
                               "org.freedesktop.DBus.Error.UnknownInterface"
                               "unknown interface"))))
      (t
       (let ((handler (gethash (cons interface member)
                               *dbus-handlers*)))
         (if handler
             (multiple-value-bind (signature values)
                 (funcall handler msg conn)
               (dbus-reply conn msg :signature signature
                           :values values))
             (dbus-error-reply
               conn msg
               "org.freedesktop.DBus.Error.UnknownMethod"
               (format nil "no handler for ~A.~A"
                       interface member))))))))

(defun dbus-pump (conn)
  "Drain every queued incoming message: method calls dispatch to
handlers (which reply themselves), signals go to the
signal-handler table. Runs on a tick; safe on an empty queue."
  (loop
    (let ((msg (bt:with-lock-held ((dbus-connection-queue-lock conn))
                 (pop (dbus-connection-queue conn)))))
      (unless msg (return))
      (when (eq msg :closed) (return))
      (when (eq (dbus-message-type msg) +dbus-msg-call+)
        (handler-case
            (dispatch-method-call msg conn)
          (error (e)
            (format t "~&; Rhizome: dbus: dispatch error: ~A~%" e)))))))
