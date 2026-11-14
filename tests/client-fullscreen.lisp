;;; Tests for client-requested fullscreen: the fullscreen_requested
;;; and exit_fullscreen_requested window events queue the same
;;; manage-pass request as C-t F11, with the requested output honoured
;;; when river knows it. River requests are stubbed and recorded, so no
;;; compositor is needed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (sent '()) (dirty 0))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (flush ()
           (setf sent '())
           (flush-fullscreen-requests)
           (reverse sent)))

    ;; The stubs below take any arguments, so check the real binding:
    ;; river_window_v1.fullscreen needs a window and an output.
    (ok "river fullscreen request takes a window and an output"
        (length (sb-introspect:function-lambda-list
                 #'rwm:river-window-v1.fullscreen))
        2)

    ;; Record river requests instead of sending them
    (setf (symbol-function 'request-manage-dirty)
          (lambda () (incf dirty) nil))
    (dolist (spec '((rwm:river-window-v1.fullscreen . :fullscreen)
                    (rwm:river-window-v1.exit-fullscreen . :exit-fullscreen)
                    (rwm:river-window-v1.inform-fullscreen . :inform-fullscreen)
                    (rwm:river-window-v1.inform-not-fullscreen
                     . :inform-not-fullscreen)))
      (let ((tag (cdr spec)))
        (setf (symbol-function (car spec))
              (lambda (&rest args) (push (cons tag args) sent) nil))))

    ;; Fresh global state: two outputs, one window on the first
    (setf *windows* (make-hash-table)
          *outputs* (make-hash-table)
          *pending-fullscreen-requests* nil)

    (let* ((left (make-instance 'rhizome-output :proxy :left-proxy
                                :width 1920 :height 1080))
           (right (make-instance 'rhizome-output :proxy :right-proxy
                                 :x 1920 :width 2560 :height 1440))
           (video (make-instance 'rhizome-window :proxy :video-proxy)))
      (setf (gethash :left-proxy *outputs*) left
            (gethash :right-proxy *outputs*) right
            (gethash :video-proxy *windows*) video
            (window-output video) left)

      ;; --- enter, no output preference ---

      (setf dirty 0)
      (handle-window-event video :fullscreen-requested nil)
      (ok "client request sets the fullscreen flag"
          (window-fullscreen-p video) t)
      (ok "client request queues a fullscreen request"
          (and (member video *pending-fullscreen-requests*) t) t)
      (ok "client request asks for a manage pass" (plusp dirty) t)
      (ok "flush sends fullscreen on the window's output and informs it"
          (flush)
          '((:fullscreen :video-proxy :left-proxy)
            (:inform-fullscreen :video-proxy)))
      (ok "flush drains the queue" *pending-fullscreen-requests* nil)

      ;; --- leave ---

      (setf dirty 0)
      (handle-window-event video :exit-fullscreen-requested)
      (ok "client exit clears the fullscreen flag"
          (window-fullscreen-p video) nil)
      (ok "client exit queues a request"
          (and (member video *pending-fullscreen-requests*) t) t)
      (ok "client exit asks for a manage pass" (plusp dirty) t)
      (ok "flush sends exit-fullscreen and informs the window"
          (flush)
          '((:exit-fullscreen :video-proxy)
            (:inform-not-fullscreen :video-proxy)))

      ;; --- enter on a requested output ---

      (handle-window-event video :fullscreen-requested :right-proxy)
      (ok "a known requested output is honoured"
          (flush)
          '((:fullscreen :video-proxy :right-proxy)
            (:inform-fullscreen :video-proxy)))
      (handle-window-event video :exit-fullscreen-requested)
      (flush)

      ;; --- enter on an output river no longer reports ---

      (handle-window-event video :fullscreen-requested :gone-proxy)
      (ok "an unknown requested output falls back to the window's output"
          (flush)
          '((:fullscreen :video-proxy :left-proxy)
            (:inform-fullscreen :video-proxy)))
      (handle-window-event video :exit-fullscreen-requested)
      (flush)

      ;; --- the requested output does not outlive the request ---

      (handle-window-event video :fullscreen-requested :right-proxy)
      (handle-window-event video :exit-fullscreen-requested)
      (flush)
      (toggle-fullscreen video)
      (ok "a later C-t F11 uses the window's output, not a stale request"
          (flush)
          '((:fullscreen :video-proxy :left-proxy)
            (:inform-fullscreen :video-proxy)))
      (toggle-fullscreen video)
      (flush)

      ;; --- parity with C-t F11 ---

      (handle-window-event video :fullscreen-requested nil)
      (let ((client (flush)))
        (handle-window-event video :exit-fullscreen-requested)
        (flush)
        (toggle-fullscreen video)
        (ok "client and C-t F11 send the same fullscreen requests"
            (flush) client))
      (toggle-fullscreen video)
      (flush)

      ;; --- a window without an output, while outputs exist ---

      (let ((orphan (make-instance 'rhizome-window :proxy :orphan-proxy)))
        (setf (gethash :orphan-proxy *windows*) orphan)
        (handle-window-event orphan :fullscreen-requested nil)
        (ok "a window without an output goes fullscreen on a known output"
            (let ((req (first (flush))))
              (and (eq (first req) :fullscreen)
                   (eq (second req) :orphan-proxy)
                   (member (third req) '(:left-proxy :right-proxy))
                   t))
            t)
        (remhash :orphan-proxy *windows*))

      ;; --- no outputs at all ---

      (clrhash *outputs*)
      (setf (window-output video) nil)
      (handle-window-event video :fullscreen-requested nil)
      (ok "with no output, nothing is sent" (flush) nil)
      (ok "with no output, the queue still drains"
          *pending-fullscreen-requests* nil)

      ;; --- a dropped request leaves the window not fullscreen ---

      (handle-window-event video :fullscreen-requested :right-proxy)
      (ok "a dropped request sends nothing" (flush) nil)
      (ok "a dropped request clears the fullscreen flag and output"
          (list (window-fullscreen-p video)
                (window-fullscreen-request-output video))
          '(nil nil))
      (setf (gethash :left-proxy *outputs*) left
            (window-output video) left)
      (toggle-fullscreen video)
      (ok "after a dropped request, C-t F11 enters fullscreen"
          (flush)
          '((:fullscreen :video-proxy :left-proxy)
            (:inform-fullscreen :video-proxy)))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
