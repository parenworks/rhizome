;;; Tests for the interaction trio: which-key hints, iresize, and
;;; command mode (sticky prefix). River-touching functions are
;;; stubbed; prefix mode is driven directly (the capture surface
;;; needs a compositor, which tests don't have).
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
         (tag-called? (tag)
           (and (find tag calls :key #'car) t)))

    ;; Record calls instead of touching river
    (dolist (fn '(message request-manage-dirty))
      (let ((fn-sym fn))
        (setf (symbol-function fn)
              (lambda (&rest args)
                (push (list (intern (string fn-sym) :keyword) args)
                      calls)
                nil))))

    ;; Fresh global state
    (setf *windows* (make-hash-table)
          *output-workspace* (make-hash-table)
          *window-workspace* (make-hash-table)
          *minimized-windows* (make-hash-table)
          *frame-trees* (make-hash-table :test 'equal)
          *current-frames* (make-hash-table :test 'equal)
          *prefix-mode-active* nil
          *prefix-current-map* nil
          *prefix-command-mode* nil
          *iresize-active* nil
          *prefix-describe-key-pending* nil
          *last-message* nil
          *prefix-expiry* 0)

    ;; --- Multi-line messages ---

    (ok "single line stays single"
        (message-lines "hello") '("hello"))
    (ok "newlines split lines"
        (message-lines (format nil "one~%two~%three")) '("one" "two" "three"))
    (ok "height for one line"
        (message-height-for "hello") 24)
    (ok "height grows with lines"
        (message-height-for (format nil "a~%b~%c")) 64)

    ;; --- which-key text ---

    (setf *prefix-map* (make-keymap)
          *prefix-key-docs* (make-hash-table))
    (setup-default-prefix-bindings)
    (let ((text (which-key-text "C-t" *prefix-map*)))
      (ok "which-key titled with the chain"
          (and (search "C-t:" text) t) t)
      (ok "which-key lists windowlist"
          (and (search "windowlist" text) t) t)
      (ok "which-key lists the help map"
          (and (search "help" text) t) t)
      (ok "which-key is multi-line"
          (<= 3 (length (message-lines text))) t))
    ;; nested maps list their (docless) keys
    (let* ((help-map (gethash 104 *prefix-map*))
           (text (which-key-text "C-t h" help-map)))
      (ok "nested which-key titled with the chain"
          (and (search "C-t h:" text) t) t)
      (ok "nested which-key lists the describe keys"
          (count-if (lambda (k) (and (search k text) t))
                    '("f" "v" "k" "b" "c" "w"))
          6))

    ;; --- iresize ---

    (let* ((out (make-instance 'stumpway-output :proxy :resize-out
                              :width 1920 :height 1080))
           (root (frame-tree-for out 8)))
      (setf (gethash out *output-workspace*) 8
            (gethash :resize-out *outputs*) out)
      ;; a split: left 960 | right 960
      (setf (gethash (frame-key out 8) *current-frames*)
            (frame-first-child root))
      (split-current-frame out 8 :horizontal)
      (let ((left (frame-first-child root))
            (right (frame-second-child root)))
        ;; drive iresize directly (surface needs a compositor)
        (setf *iresize-active* t
              *prefix-mode-active* t
              *prefix-expiry* (+ (get-universal-time) 60))
        (setf calls '())
        (prefix-handle-key 65363)          ; Right: grow the split
        (ok "iresize right grows the left frame"
            (list (frame-width left) (frame-x right)) '(990 990))
        (ok "iresize keeps the mode active"
            *iresize-active* t)
        (ok "iresize ignores other keys but stays"
            (progn (prefix-handle-key 119) *iresize-active*) t)
        (setf calls '())
        (prefix-handle-key 65393)          ; Enter commits
        (ok "Enter leaves iresize"
            (list *iresize-active* *prefix-mode-active*) '(nil nil))
        (ok "resize kept its geometry"
            (frame-width left) 990)
        ;; again, this time escaping
        (setf *iresize-active* t
              *prefix-mode-active* t
              *prefix-expiry* (+ (get-universal-time) 60))
        (prefix-handle-key 65307)          ; Escape
        (ok "Escape leaves iresize"
            (list *iresize-active* *prefix-mode-active*) '(nil nil))
        ;; expiry path also clears the flag (prefix-drain)
        (setf *iresize-active* t
              *prefix-mode-active* t
              *prefix-expiry* (- (get-universal-time) 1))
        (prefix-drain)
        (ok "timeout drains iresize"
            (list *iresize-active* *prefix-mode-active*) '(nil nil))))

    ;; --- Command mode: sticky prefix ---

    (let* ((ran 0)
           (scratch (make-keymap)))
      (define-keymap-key scratch 97 (lambda () (incf ran)))  ; a
      (setf *prefix-command-mode* t
            *prefix-mode-active* t
            *prefix-current-map* scratch
            *prefix-expiry* (+ (get-universal-time) 60)
            calls '())
      (prefix-handle-key 97)              ; run the binding
      (ok "command mode runs the binding"
          ran 1)
      (ok "command mode stays armed"
          *prefix-mode-active* t)
      (ok "command mode resets to the root map"
          *prefix-current-map* *prefix-map*)
      (ok "command mode extends the expiry"
          (>= *prefix-expiry* (+ (get-universal-time) 50)) t)
      (ok "command mode keeps its flag"
          *prefix-command-mode* t)
      ;; a second command without re-arming: we're at the root map
      ;; now, so C-t a (time) shows the time — a message fires
      (setf calls '())
      (prefix-handle-key 97)              ; 'a' is time at root
      (ok "second command in a row still runs"
          (tag-called? :message) t)
      ;; Escape leaves command mode
      (setf calls '())
      (prefix-handle-key 65307)
      (ok "Escape leaves command mode"
          (list *prefix-mode-active* *prefix-command-mode*) '(nil nil))
      ;; normal (non-command) bindings still exit after one command
      (setf *prefix-mode-active* t
            *prefix-command-mode* nil
            *prefix-current-map* scratch
            *prefix-expiry* (+ (get-universal-time) 60))
      (prefix-handle-key 97)
      (ok "normal chains exit after one command"
          (list *prefix-mode-active* ran) '(nil 2))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
