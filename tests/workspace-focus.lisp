;;; Tests for keyboard focus across workspace changes: switching to an
;;; empty workspace, switching back, and moving marked or focused
;;; windows away (workspaces.lisp). River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))
;; No slide-in animation in a test run
(configure '(:animations (:enabled nil)))

(let ((pass 0) (fail 0) (manage-requests 0) (river '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; Record river requests instead of sending them
    (setf (symbol-function 'message) (lambda (&rest args)
                                       (declare (ignore args))
                                       nil)
          (symbol-function 'show-message) (lambda (&rest args)
                                            (declare (ignore args))
                                            nil)
          (symbol-function 'request-manage-dirty) (lambda (&rest args)
                                                    (declare (ignore args))
                                                    (incf manage-requests)
                                                    nil)
          (symbol-function 'rwm:river-seat-v1.focus-window)
          (lambda (seat-proxy window-proxy)
            (push (list :focus seat-proxy window-proxy) river)
            nil)
          (symbol-function 'rwm:river-seat-v1.clear-focus)
          (lambda (seat-proxy)
            (push (list :clear seat-proxy) river)
            nil)
          (symbol-function 'rwm:river-seat-v1.pointer-warp)
          (lambda (&rest args) (declare (ignore args)) nil))

    (labels ((reset ()
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *marked-windows* (make-hash-table)
                     *window-number-map* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *scroll-states* (make-hash-table :test 'equal)
                     *workspace-last-focus* (make-hash-table :test 'equal)
                     *pending-focus-requests* nil
                     *pending-close-requests* nil
                     *pending-pointer-warps* nil
                     *last-registered-window* nil
                     *pending-autofocus* nil
                     manage-requests 0
                     river nil))
             (make-output (proxy workspace)
               (let ((out (make-instance 'rhizome-output :proxy proxy
                                         :width 1920 :height 1080)))
                 (setf (gethash out *output-workspace*) workspace)
                 out))
             (make-seat (proxy)
               (setf (gethash proxy *seats*)
                     (make-instance 'rhizome-seat :proxy proxy)))
             (make-win (proxy output workspace)
               (let ((win (make-instance 'rhizome-window :proxy proxy)))
                 (setf (gethash proxy *windows*) win
                       (window-output win) output
                       *last-registered-window* win)
                 (assign-window-workspace win workspace)
                 win))
             (settle ()
               (setf *pending-focus-requests* nil
                     manage-requests 0
                     river nil))
             (proxy-of (x)
               (and x (window-proxy x)))
             (pending ()
               (mapcar (lambda (r)
                         (list (proxy-of (car r)) (seat-proxy (cdr r))))
                       *pending-focus-requests*))
             (focused-proxies ()
               (sort (mapcar #'window-proxy
                             (remove-if-not #'window-focused-p
                                            (all-windows)))
                     #'string< :key #'symbol-name))
             (frame-of (out workspace windows)
               ;; WINDOWS in frame-windows order: push them last first
               (let ((root (frame-tree-for out workspace)))
                 (dolist (w (reverse windows))
                   (frame-push-window w root))
                 root)))

      ;; --- Switching to an empty workspace clears focus ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1)))
        (declare (ignorable a))
        (focus-window b seat)
        (settle)
        (switch-workspace 2 out)
        (ok "empty workspace: the view moves" (current-workspace out) 2)
        (ok "empty workspace: no seat keeps a window of the old one"
            (seat-focused-window seat) nil)
        (ok "empty workspace: no window stays marked focused"
            (focused-proxies) '())
        (ok "empty workspace: a focus clear is queued for the seat"
            (pending) '((nil :seat-1)))
        (ok "empty workspace: a manage sequence is requested"
            (plusp manage-requests) t)
        (setf river nil)
        (flush-focus-requests)
        (ok "empty workspace: the manage pass sends clear_focus"
            river '((:clear :seat-1))))

      ;; --- Switching to a populated workspace focuses its window ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (d (make-win :d out 2)))
        (declare (ignorable d))
        (focus-window a seat)
        (settle)
        (switch-workspace 2 out)
        (ok "populated workspace: its window takes focus"
            (proxy-of (seat-focused-window seat)) :d)
        (ok "populated workspace: only that window is marked focused"
            (focused-proxies) '(:d)))

      ;; --- Coming back focuses the window last focused there ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (declare (ignorable a c))
        (focus-window b seat)
        (switch-workspace 2 out)
        (settle)
        (switch-workspace 1 out)
        (ok "return: the window last focused there takes focus"
            (proxy-of (seat-focused-window seat)) :b)
        (ok "return: the focus request is queued"
            (pending) '((:b :seat-1))))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (declare (ignorable a c))
        (focus-window b seat)
        (switch-workspace 2 out)
        (handle-window-event b :closed)
        (switch-workspace 1 out)
        (ok "return: a closed remembered window falls back to the newest"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (declare (ignorable a c))
        (focus-window b seat)
        (switch-workspace 2 out)
        (assign-window-workspace b 3)
        (switch-workspace 1 out)
        (ok "return: a remembered window now elsewhere falls back"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (declare (ignorable a c))
        (focus-window b seat)
        (switch-workspace 2 out)
        (setf (window-minimized-p b) t)
        (switch-workspace 1 out)
        (ok "return: a minimized remembered window falls back"
            (proxy-of (seat-focused-window seat)) :c))

      ;; --- With nothing remembered, the newest window there wins ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 2))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1))
             (x (make-win :x out 2)))
        (declare (ignorable a b c))
        (focus-window x seat)
        (settle)
        (switch-workspace 1 out)
        (ok "fallback: the newest window on the workspace takes focus"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 2))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1))
             (x (make-win :x out 2)))
        (declare (ignorable a b))
        (setf (window-minimized-p c) t)
        (focus-window x seat)
        (settle)
        (switch-workspace 1 out)
        (ok "fallback: a minimized newest window is passed over"
            (proxy-of (seat-focused-window seat)) :b))

      ;; --- Moving the focused marked window off the visible workspace ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out 1 (list a b c))
        (focus-window c seat)
        (setf (gethash c *marked-windows*) t)
        (settle)
        (move-marked-to-workspace 3)
        (ok "move-marked: the view stays" (current-workspace out) 1)
        (ok "move-marked: the window moved" (window-workspace c) 3)
        (ok "move-marked: focus takes the close successor on the view"
            (proxy-of (seat-focused-window seat)) :a)
        (ok "move-marked: the moved window is not marked focused"
            (window-focused-p c) nil)
        (ok "move-marked: the successor's focus request is queued"
            (pending) '((:a :seat-1))))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out 1 (list a b c))
        (focus-window b seat)
        (setf (gethash b *marked-windows*) t
              (gethash c *marked-windows*) t)
        (settle)
        (move-marked-to-workspace 3)
        (ok "move-marked: a successor is never another moved window"
            (proxy-of (seat-focused-window seat)) :a))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1)))
        (frame-of out 1 (list a b))
        (focus-window a seat)
        (setf (gethash a *marked-windows*) t
              (gethash b *marked-windows*) t)
        (settle)
        (move-marked-to-workspace 3)
        (ok "move-marked: emptying the view clears focus"
            (seat-focused-window seat) nil)
        (ok "move-marked: the clear is queued" (pending) '((nil :seat-1)))
        (ok "move-marked: nothing stays marked focused"
            (focused-proxies) '()))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out 1 (list a b c))
        (focus-window a seat)
        (setf (gethash b *marked-windows*) t
              (gethash c *marked-windows*) t)
        (settle)
        (move-marked-to-workspace 3)
        (ok "move-marked: moving unfocused windows leaves focus alone"
            (proxy-of (seat-focused-window seat)) :a)
        (ok "move-marked: and queues no focus request" (pending) '()))

      ;; --- move-focused-window-to-workspace still follows ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (c (make-win :c out 1))
             (e (make-win :e out 3)))
        (declare (ignorable a e))
        (focus-window e seat)
        (focus-window c seat)
        (settle)
        (move-focused-window-to-workspace 3)
        (ok "move-focused: the view follows" (current-workspace out) 3)
        (ok "move-focused: the moved window keeps focus"
            (proxy-of (seat-focused-window seat)) :c)))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (uiop:quit (if (zerop fail) 0 1))))
