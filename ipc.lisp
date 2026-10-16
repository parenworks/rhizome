;;; IPC for Stumpway — the stumpish equivalent
;;;
;;; A Unix domain socket (default $XDG_RUNTIME_DIR/stumpway.sock) that
;;; accepts one command per line and replies with the result:
;;;
;;;   stumpway-cmd switch-workspace 3
;;;   stumpway-cmd current-workspace
;;;   stumpway-cmd "(+ 1 2)"            ; Lisp forms are evaluated
;;;
;;; Safety model: the socket thread only reads and writes bytes.
;;; Commands are enqueued and executed by the WM's main thread (see
;;; the poll loop in wm-protocol.lisp), so no Wayland request is ever
;;; made from a foreign thread.

(in-package #:stumpway)

;;; --- Command queue (drained by the main loop) ---

(defvar *ipc-queue* nil
  "Pending command thunks, executed by the WM's main thread.")
(defvar *ipc-lock* (bt:make-lock "stumpway-ipc")
  "Protects *ipc-queue*.")

(defun ipc-enqueue (thunk)
  "Queue THUNK for execution by the WM's main thread."
  (bt:with-lock-held (*ipc-lock*)
    (push thunk *ipc-queue*)))

(defun ipc-drain ()
  "Execute all queued commands on the calling (main) thread."
  (loop
    (let ((thunk nil))
      (bt:with-lock-held (*ipc-lock*)
        (setf thunk (pop *ipc-queue*)))
      (unless thunk (return))
      (handler-case
          (funcall thunk)
        (error (e)
          (format t "~&; Stumpway: ipc command error: ~A~%" e))))))

;;; --- Command dispatch ---

(defun ipc-dispatch-line (line)
  "Execute one command LINE and return the result as a string.
A line starting with ( is evaluated as a Lisp form in the STUMPWAY
package; otherwise it is NAME ARGS..., looked up in *command-table*."
  (let ((line (string-trim " " (remove #\newline line))))
    (cond
      ((zerop (length line)) "")
      ((char= (char line 0) #\()
       (let ((result
               (handler-case
                   (let ((*package* (find-package "STUMPWAY")))
                     (format nil "~{~A~^ ~}"
                             (multiple-value-list
                               (eval (read-from-string line)))))
                 (error (e)
                   (format nil "error: ~A" e)))))
         (show-message result 1)
         result))
      (t
       (let* ((tokens (cl-ppcre:split "\\s+" line))
              (command (intern (string-upcase (first tokens)) :keyword))
              (args (mapcar (lambda (token)
                              (or (ignore-errors (parse-integer token))
                                  token))
                            (rest tokens)))
              (handler (gethash command *command-table*)))
         (cond
           (handler
             (let ((result
                     (handler-case
                         (let ((r (funcall handler (first (all-outputs))
                                                args)))
                           (format nil "~A" (or r "")))
                       (error (e)
                         (format nil "error: ~A" e)))))
               (show-message result 1)
               result))
           (t
             (format nil "unknown command: ~A (try: ~{~A~^ ~})"
                     command (sort (loop for k being the hash-keys of *command-table*
                                         collect k)
                                   #'string<)))))))))

;;; --- Unix domain socket via CFFI (no extra dependencies) ---

(defun ipc-socket-path ()
  "Path of the IPC socket."
  (or (and (uiop:getenv "XDG_RUNTIME_DIR")
           (concatenate 'string (uiop:getenv "XDG_RUNTIME_DIR")
                        "/stumpway.sock"))
      (format nil "/tmp/stumpway-~A.sock"
              (cffi:foreign-funcall "getuid" :int))))

(defparameter *ipc-thread* nil
  "The accept-thread for the IPC socket.")
(defparameter *ipc-fd* nil
  "Listening socket fd.")

(defun %ipc-socket ()
  (cffi:foreign-funcall "socket" :int 1 :int 1 :int 0 :int)) ; AF_UNIX, SOCK_STREAM

(defun %ipc-bind (fd path)
  "Bind FD to PATH (a unix socket address)."
  (cffi:with-foreign-object (addr :uint8 110)
    (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
    (setf (cffi:mem-aref addr :uint16 0) 1) ; AF_UNIX
    (cffi:with-foreign-string (cpath path)
      (loop for i below (min 107 (length path))
            do (setf (cffi:mem-aref addr :uint8 (+ 2 i))
                     (cffi:mem-aref cpath :uint8 i))))
    (cffi:foreign-funcall "bind" :int fd :pointer addr :uint 110 :int)))

(defun %ipc-write-string (fd string)
  "Write STRING (utf-8) to FD, newline-terminated."
  (cffi:with-foreign-string (buf (concatenate 'string string (string #\newline)))
    (cffi:foreign-funcall "write" :int fd :pointer buf :long
                          (1+ (length string)) :long)))

(defun ipc-handle-connection (client-fd)
  "Read newline-terminated commands from CLIENT-FD until EOF,
queueing each for the main thread. Results are written back from the
main thread by the queued thunk."
  (cffi:with-foreign-object (buf :uint8 4096)
    (let ((acc ""))
      (loop
        (let ((n (cffi:foreign-funcall "read" :int client-fd :pointer buf
                                       :long 4096 :long)))
          (cond
            ((<= n 0) (return))
            (t
             (let ((chunk (cffi:foreign-string-to-lisp buf :count n)))
               (setf acc (concatenate 'string acc chunk))
               (let ((lines nil))
                 ;; split complete lines off the accumulator
                 (loop
                  (let ((pos (position #\newline acc)))
                    (unless pos (return))
                    (push (subseq acc 0 pos) lines)
                    (setf acc (subseq acc (1+ pos)))))
                 (dolist (line (nreverse lines))
                   (ipc-enqueue
                     (lambda ()
                       (%ipc-write-string client-fd (ipc-dispatch-line line))))))))))))
    (cffi:foreign-funcall "close" :int client-fd :int)))

(defun ipc-accept-loop (fd)
  "Accept connections until the socket closes."
  (handler-case
      (loop
        (let ((client (cffi:foreign-funcall "accept" :int fd
                                            :pointer (cffi:null-pointer)
                                            :pointer (cffi:null-pointer) :int)))
          (when (< client 0) (return))
          (ipc-handle-connection client)))
    (error (e)
      (format t "~&; Stumpway: ipc accept loop ended: ~A~%" e))))

(defun start-ipc-server (&optional (path (ipc-socket-path)))
  "Start the IPC command socket on PATH (the default location unless
given)."
  (when *ipc-thread* (return-from start-ipc-server))
  (let ((fd (%ipc-socket)))
    (when (< fd 0)
      (format t "~&; Stumpway: ipc: cannot create socket~%")
      (return-from start-ipc-server))
    (cffi:foreign-funcall "unlink" :string path :int)
    (when (< (%ipc-bind fd path) 0)
      (format t "~&; Stumpway: ipc: cannot bind ~A~%" path)
      (cffi:foreign-funcall "close" :int fd :int)
      (return-from start-ipc-server))
    (cffi:foreign-funcall "listen" :int fd :int 8 :int)
    (setf *ipc-fd* fd
          *ipc-thread*
          (bt:make-thread (lambda () (ipc-accept-loop fd))
                          :name "stumpway-ipc"))
    (format t "~&; Stumpway: ipc listening on ~A~%" path)))

(defun stop-ipc-server ()
  "Stop the IPC server and remove the socket."
  (when *ipc-fd*
    (cffi:foreign-funcall "shutdown" :int *ipc-fd* :int 2 :int)
    (cffi:foreign-funcall "close" :int *ipc-fd* :int)
    (setf *ipc-fd* nil))
  (when (and *ipc-thread* (bt:thread-alive-p *ipc-thread*))
    (handler-case (bt:destroy-thread *ipc-thread*)
      (error ())))
  (setf *ipc-thread* nil))

;;; --- Client side (used by tests; stumpway-cmd uses the shell) ---

(defun ipc-send-command (path line)
  "Connect to the IPC socket at PATH, send LINE, return the reply."
  (let ((fd (%ipc-socket)))
    (when (< fd 0) (error "cannot create socket"))
    (unwind-protect
         (progn
           (cffi:with-foreign-object (addr :uint8 110)
             (cffi:foreign-funcall "memset" :pointer addr :int 0 :long 110 :pointer)
             (setf (cffi:mem-aref addr :uint16 0) 1)
             (cffi:with-foreign-string (cpath path)
               (loop for i below (min 107 (length path))
                     do (setf (cffi:mem-aref addr :uint8 (+ 2 i))
                              (cffi:mem-aref cpath :uint8 i))))
             (when (< (cffi:foreign-funcall "connect" :int fd :pointer addr
                                            :uint 110 :int) 0)
               (error "cannot connect to ~A" path)))
           (%ipc-write-string fd line)
           ;; Read the reply (up to newline)
           (cffi:with-foreign-object (buf :uint8 4096)
             (let ((acc ""))
               (loop
                 (let ((n (cffi:foreign-funcall "read" :int fd :pointer buf
                                                :long 4096 :long)))
                   (when (<= n 0) (return))
                   (setf acc (concatenate 'string
                                          acc
                                          (cffi:foreign-string-to-lisp buf :count n)))
                   (when (position #\newline acc) (return))))
               (string-trim " " (subseq acc 0 (or (position #\newline acc)
                                                  (length acc)))))))
      (cffi:foreign-funcall "close" :int fd :int))))
