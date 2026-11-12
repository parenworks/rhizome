;;; Tests for focus recovery when a window closes (window.lisp :closed,
;;; seat.lisp clear-focus). River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

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
            nil))

    (labels ((reset ()
               (setf *seats* (make-hash-table)
                     *windows* (make-hash-table)
                     *output-workspace* (make-hash-table)
                     *window-workspace* (make-hash-table)
                     *minimized-windows* (make-hash-table)
                     *window-number-map* (make-hash-table)
                     *frame-trees* (make-hash-table :test 'equal)
                     *current-frames* (make-hash-table :test 'equal)
                     *scroll-states* (make-hash-table :test 'equal)
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
                       (window-output win) output)
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
             (close-win (win)
               (handle-window-event win :closed))
             (frame-of (out windows)
               ;; WINDOWS in frame-windows order: push them last first
               (let ((root (frame-tree-for out 1)))
                 (dolist (w (reverse windows))
                   (frame-push-window w root))
                 root))
             (ribbon (out windows)
               (setf (gethash (frame-key out 1) *scroll-states*)
                     (list :columns (copy-list windows) :scroll-x 0
                           :column-width 960 :prev-layout nil))))

      ;; --- Block 1: two-window frame, the focused one closes ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1)))
        (frame-of out (list a b))
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "closing the focused window focuses the next window in its frame"
            (proxy-of (seat-focused-window seat)) :b)
        (ok "the next window is marked focused"
            (and (window-focused-p b) t) t)
        (ok "the closed window is no longer marked focused"
            (window-focused-p a) nil)
        (ok "the refocus queues one focus request for the next window"
            (pending) '((:b :seat-1)))
        (ok "the refocus asks for a manage sequence"
            (plusp manage-requests) t)
        (ok "the closed window leaves the window list"
            (values (gethash :a *windows*)) nil)
        (setf river nil)
        (flush-focus-requests)
        (ok "the manage pass sends focus to the next window"
            river '((:focus :seat-1 :b))))

      ;; --- Block 2: frame order follows cycle-frame-window ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out (list a b c))
        (focus-window b seat)
        (settle)
        (close-win b)
        (ok "closing a middle window focuses the one after it in frame order"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out (list a b c))
        (focus-window c seat)
        (settle)
        (close-win c)
        (ok "closing the last window in frame order wraps to the first"
            (proxy-of (seat-focused-window seat)) :a))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out (list a b c))
        (setf (window-minimized-p b) t)
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "a minimized window in the frame is skipped"
            (proxy-of (seat-focused-window seat)) :c))

      ;; --- Block 3: the workspace fallback ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (root (frame-tree-for out 1))
             (second (split-frame root :horizontal 1/2)))
        (frame-push-window a (frame-first-child root))
        (frame-push-window b second)
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "an emptied frame hands focus to another window on the workspace"
            (proxy-of (seat-focused-window seat)) :b)
        (ok "the workspace fallback queues a focus request"
            (pending) '((:b :seat-1))))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1))
             (root (frame-tree-for out 1))
             (right (split-frame root :horizontal 1/2))
             (lower (split-frame right :vertical 1/2)))
        (frame-push-window a (frame-first-child root))
        (frame-push-window b (frame-first-child right))
        (frame-push-window c lower)
        (setf *last-registered-window* c)
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "the workspace fallback prefers the newest registered window"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1)))
        (setf (window-floating-p a) t)
        (frame-push-window b (frame-tree-for out 1))
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "closing a focused float hands focus to a window on its workspace"
            (proxy-of (seat-focused-window seat)) :b))

      ;; --- Block 4: scroll ribbon columns ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (ribbon out (list a b c))
        (focus-window b seat)
        (settle)
        (close-win b)
        (ok "closing the focused column in a scroll ribbon focuses the column after it"
            (proxy-of (seat-focused-window seat)) :c))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (ribbon out (list a b c))
        (focus-window c seat)
        (settle)
        (close-win c)
        (ok "closing the last column in a scroll ribbon focuses the column before it"
            (proxy-of (seat-focused-window seat)) :b))

      ;; --- Block 5: nothing left to focus; clear-focus is queued ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1)))
        (frame-push-window a (frame-tree-for out 1))
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "closing the last window leaves the seat focused on nothing"
            (proxy-of (seat-focused-window seat)) nil)
        (ok "closing the last window queues a focus clear"
            (pending) '((nil :seat-1)))
        (ok "closing the last window asks for a manage sequence"
            (plusp manage-requests) t)
        (setf river nil)
        (flush-focus-requests)
        (ok "the manage pass sends the queued clear to river"
            river '((:clear :seat-1)))
        (ok "the manage pass sends no focus to the closed window"
            (remove :clear river :key #'first) nil))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (b (make-win :b out 1)))
        (frame-push-window b (frame-tree-for out 1))
        (focus-window b seat)
        (settle)
        (clear-focus seat)
        (ok "clear-focus sends nothing to river outside a manage pass"
            river nil)
        (ok "clear-focus leaves the seat focused on nothing"
            (proxy-of (seat-focused-window seat)) nil)
        (setf river nil)
        (flush-focus-requests)
        (ok "the manage pass sends the clear-focus request"
            river '((:clear :seat-1))))

      ;; --- Block 6: an unfocused window closes ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1)))
        (frame-of out (list a b))
        (focus-window a seat)
        (settle)
        (close-win b)
        (ok "closing an unfocused window leaves focus where it was"
            (proxy-of (seat-focused-window seat)) :a)
        (ok "closing an unfocused window queues no focus request"
            (pending) nil)
        (ok "closing an unfocused window keeps the focused window marked"
            (and (window-focused-p a) t) t))

      ;; --- Block 7: other workspaces and outputs never take focus ---

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out-1 (make-output :out-1 1))
             (out-2 (make-output :out-2 1))
             (a (make-win :a out-1 1))
             (b (make-win :b out-1 2))
             (c (make-win :c out-2 1)))
        (frame-push-window a (frame-tree-for out-1 1))
        (frame-push-window b (frame-tree-for out-1 2))
        (frame-push-window c (frame-tree-for out-2 1))
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "focus clears when only other workspaces and outputs hold windows"
            (proxy-of (seat-focused-window seat)) nil)
        (ok "a window on another workspace is not chosen"
            (list (and (window-focused-p b) t)
                  (and (find :b (pending) :key #'first) t))
            '(nil nil))
        (ok "a window on another output is not chosen"
            (list (and (window-focused-p c) t)
                  (and (find :c (pending) :key #'first) t))
            '(nil nil)))

      (reset)
      (let* ((seat (make-seat :seat-1))
             (out-1 (make-output :out-1 1))
             (a (make-win :a out-1 2))
             (b (make-win :b out-1 2))
             (root (frame-tree-for out-1 2)))
        (frame-push-window b root)
        (frame-push-window a root)
        (focus-window a seat)
        (settle)
        (close-win a)
        (ok "a window on a hidden workspace does not take focus when its neighbour closes"
            (list (proxy-of (seat-focused-window seat))
                  (and (window-focused-p b) t))
            '(nil nil)))

      ;; --- Block 8: two seats ---

      (reset)
      (let* ((seat-1 (make-seat :seat-1))
             (seat-2 (make-seat :seat-2))
             (out (make-output :out-1 1))
             (a (make-win :a out 1))
             (b (make-win :b out 1))
             (c (make-win :c out 1)))
        (frame-of out (list a b c))
        (focus-window a seat-1)
        (focus-window c seat-2)
        (settle)
        (close-win a)
        (ok "the seat on the closed window moves to the next window"
            (proxy-of (seat-focused-window seat-1)) :b)
        (ok "a seat focused on another window keeps its focus"
            (proxy-of (seat-focused-window seat-2)) :c)
        (ok "only the seat on the closed window gets a focus request"
            (pending) '((:b :seat-1)))))

    (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
    (unless (zerop fail) (uiop:quit 1))))

(uiop:quit 0)
