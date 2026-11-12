;;; Tests for river_layer_shell_seat_v1 focus events, delivered as
;;; wayflan reads them: built by the readers wayflan generates from the
;;; protocol file and dispatched through its own hook dispatch.
;;; River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (manage-requests 0) (river '()) (hook-count 0)
      (seat-names '()) (ls-seats '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    (flet ((seat-name-of (seat-proxy)
             (cdr (assoc (gethash seat-proxy *seats*) seat-names))))
      ;; Record river requests instead of sending them
      (dolist (fn '(message show-message message-hide))
        (setf (symbol-function fn) (lambda (&rest args)
                                     (declare (ignore args))
                                     nil)))
      (setf (symbol-function 'request-manage-dirty)
            (lambda (&rest args)
              (declare (ignore args))
              (incf manage-requests)
              nil)
            (symbol-function 'rwm:zwlr-layer-surface-v1.destroy)
            (lambda (x)
              (push (list :destroy-layer x) river)
              nil)
            (symbol-function 'wl:wl-surface.destroy)
            (lambda (x)
              (push (list :destroy-surface x) river)
              nil)
            (symbol-function 'rwm:river-seat-v1.set-xcursor-theme)
            (lambda (&rest args)
              (declare (ignore args))
              nil)
            (symbol-function 'rwm:river-seat-v1.get-pointer-binding)
            (lambda (&rest args)
              (declare (ignore args))
              (make-instance 'wl:wl-proxy))
            ;; The generated readers declare the layer shell seat type
            (symbol-function 'rwm:river-layer-shell-v1.get-seat)
            (lambda (layer-shell seat-proxy)
              (declare (ignore layer-shell))
              (let ((ls (make-instance 'rwm:river-layer-shell-seat-v1)))
                (push (cons seat-proxy ls) ls-seats)
                ls))
            (symbol-function 'rwm:river-seat-v1.focus-window)
            (lambda (seat-proxy window-proxy)
              (push (list :focus (seat-name-of seat-proxy) window-proxy) river)
              nil)
            (symbol-function 'rwm:river-seat-v1.clear-focus)
            (lambda (seat-proxy)
              (push (list :clear (seat-name-of seat-proxy)) river)
              nil)))

    (labels ((reset ()
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *seats-with-pointer-bindings* (make-hash-table)
                     *pending-focus-requests* nil
                     *pending-close-requests* nil
                     *pending-pointer-warps* nil
                     *pointer-bindings* nil
                     *prefix-state* nil
                     *expose-active* nil
                     *river-layer-shell* :layer-shell
                     manage-requests 0
                     hook-count 0
                     river nil
                     seat-names nil
                     ls-seats nil))
             (readers ()
               ;; Indexed by opcode: 0 focus_exclusive,
               ;; 1 focus_non_exclusive, 2 focus_none
               (slot-value (find-class 'rwm:river-layer-shell-seat-v1)
                           'wl::%event-table))
             (new-seat (name)
               (let ((proxy (make-instance 'rwm:river-seat-v1)))
                 (with-output-to-string (*standard-output*)
                   (register-seat proxy))
                 (let ((seat (gethash proxy *seats*)))
                   (push (cons seat name) seat-names)
                   (values seat (cdr (assoc proxy ls-seats))))))
             (deliver (ls-seat opcode)
               (with-output-to-string (*standard-output*)
                 (handler-case
                     (let ((event (funcall (aref (readers) opcode) ls-seat nil)))
                       (wl::%dispatch-event ls-seat event))
                   (error (e)
                     (format t "~&DELIVERY ERROR: ~A~%" e)))))
             (make-win (proxy)
               (setf (gethash proxy *windows*)
                     (make-instance 'rhizome-window :proxy proxy)))
             (give-focus (win seat)
               (setf (seat-focused-window seat) win
                     (window-focused-p win) t))
             (proxy-of (x)
               (and x (window-proxy x)))
             (pending ()
               (mapcar (lambda (r)
                         (list (proxy-of (car r))
                               (cdr (assoc (cdr r) seat-names))))
                       *pending-focus-requests*))
             (close-prefix ()
               (let ((h (make-hash-table)))
                 (setf (gethash :layer-surface h) :lf-ls
                       (gethash :surface h) :lf-surface
                       *prefix-state* h))
               (exit-prefix-mode))
             (start ()
               ;; One seat focused on a live :la
               (reset)
               (multiple-value-bind (seat ls) (new-seat :seat-1)
                 (let ((a (make-win :la)))
                   (give-focus a seat)
                   (values seat ls a)))))

      ;; Wayflan's own reading of the three events
      (reset)
      (multiple-value-bind (seat ls) (new-seat :seat-1)
        (declare (ignore seat))
        (ok "wayflan reads the layer shell seat events as dashed keywords"
            (loop for i below 3 collect (first (funcall (aref (readers) i) ls nil)))
            '(:focus-exclusive :focus-non-exclusive :focus-none))
        (ok "registering a seat hooks its layer shell seat"
            (length (wl:wl-proxy-hooks ls))
            1))

      ;; focus_none hands the keyboard back to the focused window
      (multiple-value-bind (seat ls a) (start)
        (let* ((handler (lambda (&rest args)
                          (declare (ignore args))
                          (incf hook-count)))
               (text (progn (add-hook :focus-changed handler)
                            (deliver ls 2))))
          (ok "focus_none queues focus for the focused window"
              (pending)
              '((:la :seat-1)))
          (ok "focus_none asks for a manage sequence"
              (plusp manage-requests)
              t)
          (ok "focus_none is logged"
              (and (search "layer shell focus none" text) t)
              t)
          (ok "focus_none leaves the seat focus unchanged"
              (list (proxy-of (seat-focused-window seat))
                    (and (window-focused-p a) t))
              '(:la t))
          (ok "focus_none runs no focus-changed hook"
              hook-count
              0)
          (remove-hook :focus-changed handler)
          (setf river nil)
          (flush-focus-requests)
          (ok "the manage pass after focus_none sends focus to the focused window"
              (remove-if-not (lambda (e) (eq (first e) :focus)) river)
              '((:focus :seat-1 :la)))))

      ;; focus_exclusive only logs
      (multiple-value-bind (seat ls) (start)
        (declare (ignore seat))
        (let ((text (deliver ls 0)))
          (ok "focus_exclusive is logged"
              (and (search "layer shell exclusive focus" text) t)
              t)
          (ok "focus_exclusive queues no focus request"
              (list (pending) manage-requests)
              '(nil 0))))

      ;; focus_non_exclusive only logs
      (multiple-value-bind (seat ls) (start)
        (declare (ignore seat))
        (let ((text (deliver ls 1)))
          (ok "focus_non_exclusive is logged"
              (and (search "layer shell non-exclusive focus" text) t)
              t)
          (ok "focus_non_exclusive queues no focus request"
              (list (pending) manage-requests)
              '(nil 0))))

      ;; A seat with no focused window
      (reset)
      (multiple-value-bind (seat ls) (new-seat :seat-1)
        (declare (ignore seat))
        (make-win :la)
        (deliver ls 2)
        (ok "focus_none with no focused window queues nothing"
            (list (pending) manage-requests)
            '(nil 0)))

      ;; A seat whose focused window is no longer registered
      (reset)
      (multiple-value-bind (seat ls) (new-seat :seat-1)
        (make-win :la)
        (give-focus (make-instance 'rhizome-window :proxy :ld) seat)
        (deliver ls 2)
        (ok "focus_none with a closed focused window queues nothing"
            (pending)
            nil))

      ;; The teardown fallback and focus_none together
      (multiple-value-bind (seat ls) (start)
        (declare (ignore seat))
        (close-prefix)
        (deliver ls 2)
        (ok "a prefix teardown and focus_none queue one request per seat"
            (pending)
            '((:la :seat-1))))

      ;; focus_none on one seat covers every seat
      (multiple-value-bind (seat-1 ls-1) (start)
        (declare (ignore seat-1))
        (let ((seat-2 (new-seat :seat-2))
              (b (make-win :lb)))
          (give-focus b seat-2)
          (deliver ls-1 2)
          (ok "focus_none on one seat re-queues every seat's focused window"
              (sort (pending) #'string< :key #'prin1-to-string)
              '((:la :seat-1) (:lb :seat-2))))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
