;;; Tests for the StumpWM quick-wins batch: window numbering,
;;; pull/exchange/only/balance/fclear, lastmsg/time, quit-confirm,
;;; fullscreen requests, F-keys.
(ql:quickload :stumpway)

(in-package #:stumpway)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (calls '()))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (called? (win tag)
           (and (member (list tag win) calls :test #'equal) t))
         (tag-called? (tag)
           (and (find tag calls :key #'car) t)))

    ;; Record calls instead of touching river
    (dolist (fn '(propose-window-dimensions set-window-tiled-state
                  make-window-node set-window-position
                  set-window-borders show-window hide-window
                  message request-manage-dirty))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (if (and args (typep (first args) 'stumpway-window))
                    (progn
                      (push (list (intern (string fn-sym) :keyword)
                                  (first args))
                            calls)
                      (when (eq fn-sym 'show-window)
                        (setf (window-hidden-p (first args)) nil))
                      (when (eq fn-sym 'hide-window)
                        (setf (window-hidden-p (first args)) t)))
                    (push (list (intern (string fn-sym) :keyword) args)
                          calls))
                nil))))

    ;; Fresh global state
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *window-number-map* (make-hash-table)
          *window-number-overflow* 10
          *pending-fullscreen-requests* nil
          *last-message* nil
          *quit-armed-at* 0)

    ;; --- Window numbering ---

    (let* ((out (make-instance 'stumpway-output :proxy :out-proxy
                              :width 1920 :height 1080))
           (wins (loop for i below 11
                       for proxy = (intern (format nil "W~D" i) :keyword)
                       collect (make-instance 'stumpway-window
                                               :proxy proxy))))
      (setf (gethash out *output-workspace*) 1)
      (dolist (win wins)
        (setf (gethash (window-proxy win) *windows*) win
              (window-output win) out)
        (assign-window-number win)
        (assign-window-workspace win 1))
      (ok "first ten windows get 1..9 then 0"
          (mapcar #'window-number (subseq wins 0 10))
          '(1 2 3 4 5 6 7 8 9 0))
      (ok "eleventh window overflows past 10"
          (window-number (nth 10 wins)) 10)
      (ok "numbers are unique"
          (= (hash-table-count *window-number-map*) 11) t)

      ;; Closing a window frees its number for reuse
      (release-window-number (first wins))
      (let ((fresh (make-instance 'stumpway-window :proxy :fresh)))
        (assign-window-number fresh)
        (ok "freed number is reused"
            (window-number fresh) 1)
        ;; The fresh window is not on the workspace; drop its stale
        ;; number so renumber expectations below are exact
        (release-window-number fresh))

      ;; Renumbering compacts 1..N
      (renumber-windows out 1)
      (ok "renumber assigns 1..N in order"
          (sort (loop for w being the hash-keys of *window-number-map*
                      collect (window-number w))
                #'<)
          (loop for i from 1 to 11 collect i))

      ;; find/select by number
      (setf (window-focused-p (second wins)) nil)
      (ok "find-window-by-number finds it"
          (find-window-by-number (window-number (second wins))
                                 out 1)
          (second wins))
      (ok "unknown number finds nothing"
          (find-window-by-number 42 out 1) nil)

      ;; --- Fullscreen ---

      (setf calls '())
      (toggle-fullscreen (second wins))
      (ok "fullscreen flag set" (window-fullscreen-p (second wins)) t)
      (ok "fullscreen request queued"
          (and (member (second wins) *pending-fullscreen-requests*) t) t)
      (ok "toggle requested relayout"
          (tag-called? :request-manage-dirty) t)
      (toggle-fullscreen (second wins))
      (ok "second toggle unsets the flag"
          (window-fullscreen-p (second wins)) nil)
      (toggle-fullscreen (second wins))
      ;; Flush: the rwm request errors on the fake proxy, but the
      ;; queue must drain either way
      (flush-fullscreen-requests)
      (ok "fullscreen queue drained"
          *pending-fullscreen-requests* nil)
      (toggle-fullscreen nil)                    ; no window: no-op
      (ok "toggle with no window is a no-op"
          *pending-fullscreen-requests* nil)

      ;; --- pull / exchange / only / balance / fclear ---

      ;; Two frames; three windows: one visible in each, one hidden
      (setf (gethash :w2 *windows*) (second wins))
      (setf (window-focused-p (second wins)) nil)
      (let ((a (first wins)) (b (third wins)) (c (fourth wins)))
        (dolist (win (list a b c))
          (setf (window-floating-p win) nil))
        (set-workspace-tree out 1
                             '((0 0 960 1080) (960 0 960 1080))
                             (list a b))
        (let ((leaves (frame-leaves (frame-tree-for out 1))))
          ;; c is frameless (hidden); pull it into the current frame
          (setf (gethash (frame-key out 1) *current-frames*)
                (first leaves))
          (frame-remove-window c)
          (setf calls '())
          (pull-window-into-current-frame c out 1)
          (ok "pull moves the window into the current frame"
              (window-frame c) (first leaves))
          ;; focused window (make one focused) is a
          (setf (window-focused-p a) t)
          (setf calls '())
          ;; exchange current frame's a with target frame's b
          (exchange-window-direction out 1 :right)
          (ok "exchange swaps the frames' windows"
              (list (window-frame a) (window-frame b))
              (list (second leaves) (first leaves)))
          ;; balance: resize splits unevenly first, then equalize
          (resize-current-split out 1 100)
          (frame-balance out 1)
          (ok "balance equalizes the split"
              (frame-width (first leaves)) 960)
          ;; only: collapse everything into the current frame
          (setf (gethash (frame-key out 1) *current-frames*)
                (first leaves))
          (frame-only out 1)
          (ok "only collapses the tree to one frame"
              (length (frame-leaves (frame-tree-for out 1))) 1)
          (ok "only merged all windows into the current frame"
              (sort (mapcar #'window-proxy
                            (frame-windows (frame-tree-for out 1)))
                    #'string<)
              '(:W0 :W2 :W3))
          ;; fclear with no other frame: polite refusal
          (setf calls '())
          (frame-clear out 1)
          (ok "fclear refuses without another frame"
              (tag-called? :message) t))))

    ;; --- Messages: lastmsg / time ---

    (let ((shown '()))
      (setf (symbol-function 'message)
            (lambda (fmt &rest args)
              (let ((text (apply #'format nil fmt args)))
                (push text shown)
                (setf *last-message* text))))
      (setf (symbol-function 'show-message)
            (lambda (text &optional (seconds 3))
              (declare (ignore seconds))
              (push text shown)))
      (setf *last-message* nil)
      (message "hello ~A" 'world)
      (message "second message")
      (setf shown '())
      (lastmsg)
      (ok "lastmsg rediscovers the last message"
          (first shown) "second message")
      (lastmsg)
      (ok "lastmsg does not consume the message"
          (first shown) "second message")
      (setf shown '())
      (time-message)
      (ok "time shows a nonempty message"
          (and (first shown) (plusp (length (first shown))) t) t))

    ;; --- quit-confirm ---

    (let ((stopped nil) (msgs 0))
      (setf (symbol-function 'stop) (lambda () (setf stopped t) nil))
      (setf (symbol-function 'message)
            (lambda (fmt &rest args)
              (declare (ignore fmt args))
              (incf msgs)))
      (setf *quit-armed-at* 0)
      (quit-confirm)
      (ok "first quit arms confirmation"
          (and (plusp msgs) (not stopped)) t)
      (quit-confirm)
      (ok "second quick press stops"
          stopped t)
      (setf stopped nil msgs 0)
      (setf *quit-armed-at* (- (get-universal-time) 10))
      (quit-confirm)
      (ok "stale arm re-arms instead of quitting"
          (and (plusp msgs) (not stopped)) t))

    ;; --- F-keys in evdev-to-xkb ---

    (ok "F1 keycode maps to F1 keysym" (evdev-to-xkb 59) 65470)
    (ok "F11 keycode maps to F11 keysym" (evdev-to-xkb 87) 65480)
    (ok "F12 keycode maps to F12 keysym" (evdev-to-xkb 88) 65481))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
