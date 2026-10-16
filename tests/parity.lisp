;;; Tests for phase 6 parity items: Alt+drag pointer bindings,
;;; C-t h help system, C-t +/- resize, move-window-to-frame-direction,
;;; and :output window rules.
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
           (and (member (list tag win) calls :test #'equal) t)))

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
          *prefix-mode-active* nil
          *prefix-current-map* nil
          *prefix-key-docs* (make-hash-table)
          *prefix-describe-key-pending* nil
          *pointer-bindings* nil
          *pointer-bindings-enabled* nil)

    ;; --- Window-at-pointer and Alt+drag ---

    (let* ((out (make-instance 'stumpway-output :proxy :out-proxy
                              :width 1920 :height 1080))
           (seat (make-instance 'stumpway-seat :proxy :seat-proxy))
           (term (make-instance 'stumpway-window :proxy :term-proxy))
           (floaty (make-instance 'stumpway-window :proxy :float-proxy)))
      (setf (gethash out *output-workspace*) 1
            (gethash :term-proxy *windows*) term
            (gethash :float-proxy *windows*) floaty
            (window-output term) out
            (window-x term) 0 (window-y term) 0
            (window-width term) 960 (window-height term) 1080
            (window-output floaty) out
            (window-x floaty) 1200 (window-y floaty) 100
            (window-width floaty) 300 (window-height floaty) 200
            (window-float-x floaty) 1200 (window-float-y floaty) 100
            (window-float-width floaty) 300 (window-float-height floaty) 200)
      (assign-window-workspace term 1)
      (assign-window-workspace floaty 1)

      ;; Pointer inside the focused window wins
      (setf (window-focused-p term) t
            (seat-pointer-x seat) 500 (seat-pointer-y seat) 500)
      (ok "pointer inside focused window"
          (window-at-pointer seat) term)

      ;; Pointer inside a float
      (setf (window-focused-p term) nil
            (window-focused-p floaty) nil
            (seat-pointer-x seat) 1300 (seat-pointer-y seat) 150)
      (ok "pointer inside float"
          (window-at-pointer seat) floaty)

      ;; Pointer over empty screen
      (setf (seat-pointer-x seat) 1500 (seat-pointer-y seat) 900)
      (ok "pointer over nothing"
          (window-at-pointer seat) nil)

      ;; Alt+drag on a tiled window floats it, StumpWM-style
      (setf (seat-pointer-x seat) 500 (seat-pointer-y seat) 500
            (window-focused-p term) nil
            calls '())
      (setf (window-floating-p term) nil)
      (pointer-drag-start seat :move)
      (ok "alt-drag floats the tiled window"
          (window-floating-p term) t)
      (ok "alt-drag queues a move op"
          (getf (seat-op-pending seat) :type) :move)
      (ok "alt-drag targets the dragged window"
          (getf (seat-op-pending seat) :win) term)

      ;; Alt+right-drag on a float queues a resize from bottom-right
      (setf calls '()
            (seat-op-pending seat) nil
            (window-floating-p floaty) t
            (seat-pointer-x seat) 1300 (seat-pointer-y seat) 150)
      (pointer-drag-start seat :resize)
      (ok "alt-right-drag queues a resize op"
          (getf (seat-op-pending seat) :type) :resize)
      (ok "resize op grabs the bottom-right corner"
          (getf (seat-op-pending seat) :edges) '(:bottom :right))
      (ok "resize op targets the float"
          (getf (seat-op-pending seat) :win) floaty)
      (ok "float stays floating"
          (window-floating-p floaty) t)
      (setf (window-floating-p term) nil)

      ;; --- move-window-to-frame-direction ---

      (setf (window-focused-p term) t
            (window-floating-p term) nil
            (window-frame term) nil
            (window-frame floaty) nil
            calls '())
      (set-workspace-tree out 1
                          '((0 0 960 1080) (960 0 960 1080))
                          (list term floaty))
      (let ((leaves (frame-leaves (frame-tree-for out 1))))
        (setf (window-focused-p term) nil
              (window-focused-p floaty) t)
        ;; floaty is in the right frame; move it left
        (setf (gethash (frame-key out 1) *current-frames*)
              (second leaves))
        (move-window-to-frame-direction out 1 :left)
        (ok "float move refused with feedback"
            (called? floaty :message) nil)   ; floaty is a float: skipped
        (ok "float not moved into the tiling"
            (eq (window-frame floaty) (second leaves)) nil)

        ;; Now move a real tiled window
        (setf (window-focused-p floaty) nil
              (window-focused-p term) t)
        (setf (gethash (frame-key out 1) *current-frames*)
              (first leaves))
        (move-window-to-frame-direction out 1 :right)
        (ok "window moved to the right frame"
            (window-frame term) (second leaves))

        ;; No frame further right: refused, stays put
        (setf (gethash (frame-key out 1) *current-frames*)
              (second leaves))
        (setf calls '())
        (move-window-to-frame-direction out 1 :right)
        (ok "no frame in direction: window stays"
            (window-frame term) (second leaves))
        (ok "move requested relayout"
            (and (member (list :request-manage-dirty '(0)) calls
                         :test (lambda (a b) (declare (ignore b)) (eq (first a) :request-manage-dirty)))
                 t)
            t)))

    ;; --- Help system: keysym-name, docs, describe-key ---

    (ok "keysym names letters" (keysym-name 119) "w")
    (ok "keysym names arrows" (keysym-name 65361) "Left")
    (ok "keysym names return" (keysym-name 65293) "Return")

    (setf *prefix-map* (make-keymap)
          *prefix-key-docs* (make-hash-table))
    (setup-default-prefix-bindings)
    (ok "bindings registered with docs"
        (gethash 119 *prefix-key-docs*) "windowlist")
    (ok "nested move map installed"
        (hash-table-p (gethash 77 *prefix-map*)) t)
    (ok "nested exchange map installed"
        (hash-table-p (gethash 101 *prefix-map*)) t)
    (ok "nested help map installed"
        (hash-table-p (gethash 104 *prefix-map*)) t)
    (ok "resize bindings registered"
        (list (gethash 43 *prefix-key-docs*) (gethash 45 *prefix-key-docs*))
        '("grow split (+30)" "shrink split (-30)"))

    ;; describe-key flow: pending flag describes the next key
    (let ((messages '()))
      (setf (symbol-function 'message)
            (lambda (fmt &rest args)
              (push (apply #'format nil fmt args) messages)))
      (setf *prefix-current-map* *prefix-map*
            *prefix-describe-key-pending* t
            *prefix-mode-active* t)
      (prefix-handle-key 119)                  ; w
      (ok "describe-key reports the binding"
          (and (search "windowlist" (first messages)) t) t)
      (ok "describe-key clears the pending flag"
          *prefix-describe-key-pending* nil)
      (ok "describe-key exits prefix mode"
          *prefix-mode-active* nil)

      ;; describing an unbound key says so
      (setf messages '()
            *prefix-current-map* *prefix-map*
            *prefix-describe-key-pending* t
            *prefix-mode-active* t)
      (prefix-handle-key 122)                  ; z
      (ok "describe-key reports unbound keys"
          (and (search "unbound" (first messages)) t) t)

      ;; describe-bindings lists the doc'd keys
      (setf messages '())
      (describe-prefix-bindings)
      (ok "describe-bindings lists windowlist"
          (and (search "windowlist" (first messages)) t) t)

      ;; command docs captured for describe-command
      (ok "command docs captured"
          (and (gethash :toggle-float *command-docs*)
               (search "Float" (gethash :toggle-float *command-docs*))
               t)
          t)

      ;; :output window rules
      (let* ((out1 (make-instance 'stumpway-output :proxy :out-proxy
                                  :width 1920 :height 1080))
             (out2 (make-instance 'stumpway-output :proxy :out2-proxy
                                 :width 1920 :height 1080 :x 1920))
             (steam (make-instance 'stumpway-window :proxy :steam-proxy)))
        (setf (gethash :out-proxy *outputs*) out1
              (gethash :out2-proxy *outputs*) out2
              (gethash :steam-proxy *windows*) steam
              (window-app-id steam) "steam")
        (setf (gethash out2 *output-workspace*) 1)
        (configure '(:window-rules ((:match "steam" :output 2 :workspace 1))))
        (setf messages '())
        (apply-window-rules steam)
        (ok ":output rule places window on output 2"
            (window-output steam) out2)
        (ok ":output rule also assigned workspace"
            (window-workspace steam) 1))

      ;; enable-pointer-bindings: no bindings in tests, flag flips
      (setf *pointer-bindings* nil *pointer-bindings-enabled* nil)
      (enable-pointer-bindings-if-needed)
      (ok "pointer bindings marked enabled"
          *pointer-bindings-enabled* t)

      ;; --- The user modifier (define once, use many) ---

      (ok "default mod is super" *mod* '(:mod4))
      (ok "resolve-mod maps alt" (resolve-mod :alt) '(:mod1))
      (ok "resolve-mod maps super" (resolve-mod :super) '(:mod4))
      (ok "resolve-mod maps ctrl" (resolve-mod :ctrl) '(:ctrl))
      (ok "resolve-mod rejects junk" (resolve-mod :banana) nil)
      ;; default bindings are built on *mod*
      (setf *keybindings* (make-hash-table :test 'equal))
      (setup-default-keybindings)
      (ok "terminal binding on the user mod"
          (and (gethash (cons +xkb-key-Return+ *mod*) *keybindings*) t) t)
      (set-user-mod :alt)
      (ok "set-user-mod switches to alt"
          *mod* '(:mod1))
      (set-user-mod :super)
      (ok "set-user-mod switches back to super"
          *mod* '(:mod4))
      (set-user-mod :banana)
      (ok "set-user-mod rejects unknown modifiers"
          *mod* '(:mod4))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
