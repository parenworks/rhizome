;;; Tests for key sequences: kbd parsing, keymaps bound to direct keys,
;;; modifiers on every key of a sequence, the prefix key as an ordinary
;;; binding, the modifier map and grab skipping, and the calling
;;; convention for bindings. Key presses are simulated at the two
;;; points river delivers them: a grab firing (direct keys) and a
;;; wl_keyboard key event on the capture surface (evdev keycode plus
;;; the depressed xkb modifier mask).
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defvar *key-sequence-test-ran* nil
  "What the last test binding recorded.")

(let ((pass 0) (fail 0) (messages '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (call-if (name &rest args)
             (if (fboundp name) (apply name args) :missing))
           (special (name)
             (if (boundp name) (symbol-value name) :missing))
           (press-direct (keysym river-mods)
             ;; A river grab firing for KEYSYM with RIVER-MODS
             (handler-case
                 (if (fboundp 'press-key)
                     (funcall 'press-key (cons keysym river-mods))
                     (let ((cb (key-binding keysym river-mods)))
                       (when (functionp cb) (funcall cb))))
               (error (e) (format t "~&; press error: ~A~%" e))))
           (press-captured (keycode &optional (mask 0))
             ;; A key on the capture surface: evdev KEYCODE, xkb MASK
             (when *prefix-mode-active*
               (if (fboundp 'keyboard-key)
                   (let ((key (funcall 'keyboard-key keycode mask)))
                     (prefix-handle-key (car key) (cdr key)))
                   (prefix-handle-key (evdev-to-xkb keycode (logbitp 0 mask))))))
           (recorder (tag)
             (lambda () (setf *key-sequence-test-ran* tag)))
           (reset ()
             (setf *key-sequence-test-ran* nil messages '()
                   *prefix-describe-key-pending* nil)
             (when *prefix-mode-active* (exit-prefix-mode))))

    ;; River-touching stubs
    (dolist (fn '(show-message message-hide request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (push (apply #'format nil fmt args) messages)))
    ;; The capture surface needs a compositor: arm the sequence state
    ;; without it
    (if (fboundp 'open-prefix-capture-surface)
        (setf (symbol-function 'prefix-capture-available-p) (lambda () t)
              (symbol-function 'open-prefix-capture-surface)
              (lambda () (setf *prefix-mode-active* t)))
        (setf (symbol-function 'enter-prefix-mode)
              (lambda (&rest args)
                (declare (ignore args))
                (setf *prefix-current-map* *prefix-map*
                      *prefix-mode-active* t))))

    (setf *xkb-bindings-proxy* nil
          *prefix-mode-active* nil
          *prefix-state* nil
          *minor-mode-stack* nil)
    (clear-keymap *top-map*)
    (clear-keymap *prefix-map*)
    (setup-default-keybindings)

    ;; --- kbd ---
    (ok "kbd C-M-s-x" (call-if 'kbd "C-M-s-x") '((120 :control :meta :super)))
    (ok "kbd H-a" (call-if 'kbd "H-a") '((97 :hyper)))
    (ok "kbd A-Return" (call-if 'kbd "A-Return") '((65293 :alt)))
    (ok "kbd s-;" (call-if 'kbd "s-;") '((59 :super)))
    (ok "kbd sequence" (call-if 'kbd "s-x C-f") '((120 :super) (102 :control)))
    (ok "kbd keeps case" (call-if 'kbd "T") '((84)))
    (ok "kbd modifier order is canonical"
        (call-if 'kbd "S-H-C-x") '((120 :control :hyper :shift)))
    (ok "kbd rejects junk"
        (handler-case (progn (call-if 'kbd "C-banana") :accepted)
          (error () :rejected))
        :rejected)
    (ok "key-description round-trips"
        (if (fboundp 'key-description)
            (mapcar 'key-description (kbd "C-M-s-H-A-S-x s-Return C-- F5"))
            :missing)
        '("C-M-s-H-A-S-x" "s-Return" "C--" "F5"))
    (ok "parse-key-spec keeps its river form"
        (multiple-value-list (parse-key-spec "C-M-t")) '((:ctrl :mod1) 116))

    ;; --- A direct key bound to a keymap ---
    (let ((s-x-map (make-keymap)))
      (define-keymap-key s-x-map 102 (recorder :f))                     ; f
      (define-keymap-key s-x-map '(102 :control :meta) (recorder :c-m-f))
      (define-key 120 '(:mod4) s-x-map)                                 ; s-x
      (reset)
      (press-direct 120 '(:mod4))
      (ok "s-x enters its keymap" *prefix-mode-active* t)
      (press-captured 33)                                               ; f
      (ok "s-x f runs the bound function" *key-sequence-test-ran* :f)
      (reset)
      (press-direct 120 '(:mod4))
      (press-captured 33 #b1100)                                        ; C-M-f
      (ok "s-x C-M-f sees the modifiers on the second key"
          *key-sequence-test-ran* :c-m-f)
      (ok "the which-key label names the entry key"
          (special '*prefix-label*) "s-x")
      ;; describe-key names the sequence in key spec syntax
      (reset)
      (press-direct 120 '(:mod4))
      (setf *prefix-describe-key-pending* t)
      (press-captured 33 #b1100)
      (ok "describe-key prints the sequence in kbd syntax"
          (and (search "s-x C-M-f" (or (first messages) "")) t) t))

    ;; --- The prefix key is an ordinary binding ---
    (define-prefix-key 119 (recorder :user-w) "user w")               ; C-t w
    (reset)
    (press-direct 116 '(:ctrl))
    (press-captured 17)                                                 ; w
    (ok "C-t w runs through the prefix binding" *key-sequence-test-ran* :user-w)
    (reset)
    (press-direct 116 '(:ctrl))
    (press-captured 17 #b100)                                           ; w, Ctrl held
    (ok "Ctrl still held from C-t does not change the next key"
        *key-sequence-test-ran* :user-w)
    (define-key 39 '(:ctrl) '*prefix-map*)                              ; C-'
    (reset)
    (press-direct 39 '(:ctrl))
    (press-captured 17)
    (ok "C-' bound to the prefix map works like C-t"
        *key-sequence-test-ran* :user-w)
    (define-key 39 '(:ctrl) nil)

    ;; --- set-prefix-key moves the binding; Super as the root ---
    (set-prefix-key "s-t")
    (reset)
    (press-direct 116 '(:ctrl))
    (ok "C-t no longer enters after the move" *prefix-mode-active* nil)
    (press-direct 116 '(:mod4))
    (press-captured 17)
    (ok "s-t w runs after set-prefix-key \"s-t\"" *key-sequence-test-ran* :user-w)
    (set-prefix-key "C-t")

    ;; --- Calling convention ---
    (let ((seen nil))
      (handler-case
          (progn
            (define-key *top-map* "s-y"
              (lambda ()
                (setf seen (list (key-event-key (special '*current-key-event*))
                                 (special '*prefix-argument*)
                                 (special '*current-seat*)))))
            (define-key *top-map* "s-u"
              (lambda (event) (setf seen (list :event (key-event-key event))))
              :pass-event t))
        (error (e) (format t "~&; define error: ~A~%" e)))
      (reset)
      (press-direct 121 '(:mod4))
      (ok "zero-arg binding runs with the key event bound"
          seen '((121 :super) nil nil))
      (setf seen nil)
      (press-direct 117 '(:mod4))
      (ok "event binding receives the key event" seen '(:event (117 :super))))
    (let ((got nil) (seq-map (make-keymap)))
      (handler-case
          (progn
            (define-key seq-map "g" (lambda (event) (setf got event)) :pass-event t)
            (define-key 120 '(:mod4) seq-map))
        (error (e) (format t "~&; define error: ~A~%" e)))
      (reset)
      (press-direct 120 '(:mod4))
      (press-captured 34)                                               ; g
      (ok "event binding in a sequence sees the whole sequence"
          (and got (funcall 'key-event-sequence got)) '((120 :super) (103))))
    (setf (symbol-function 'key-sequence-test-late) (recorder :early))
    (handler-case (define-key *top-map* "s-l" 'key-sequence-test-late)
      (error (e) (format t "~&; define error: ~A~%" e)))
    (setf (symbol-function 'key-sequence-test-late) (recorder :late))
    (reset)
    (press-direct 108 '(:mod4))
    (ok "a symbol binding is called late" *key-sequence-test-ran* :late)
    (handler-case (define-key *top-map* "s-e" (lambda () (error "boom")))
      (error (e) (format t "~&; define error: ~A~%" e)))
    (reset)
    (ok "an error in a binding is caught"
        (handler-case (progn (press-direct 101 '(:mod4)) :caught)
          (error () :escaped))
        :caught)
    (ok "the error is shown" (and (search "boom" (or (first messages) "")) t) t)

    ;; --- Sticky keymap ---
    (let ((count 0) (sticky-map (make-keymap)))
      (define-keymap-key sticky-map 97 (lambda () (incf count)))       ; a
      (handler-case
          (define-key *top-map* "s-r" (funcall 'sticky-keymap sticky-map "sticky"))
        (error (e) (format t "~&; define error: ~A~%" e)))
      (reset)
      (press-direct 114 '(:mod4))
      (press-captured 30)
      (press-captured 30)
      (ok "a sticky keymap stays armed between commands"
          (list count *prefix-mode-active*) '(2 t))
      (press-captured 1)                                                ; Esc
      (ok "Escape leaves the sticky keymap" *prefix-mode-active* nil))

    ;; --- Modifier map and grabs ---
    (let ((grabs '()))
      (setf (symbol-function 'grab-key)
            (lambda (seat key &optional river)
              (push (list seat key river) grabs)
              (setf (gethash (cons seat key) *grabbed-keys*) :grab)))
      (release-key-grabs)
      (setf *xkb-bindings-proxy* :proxy
            *seats* (make-hash-table))
      (setf (gethash :seat-proxy *seats*) :seat)
      (define-key 97 '(:hyper) (recorder :h-a))
      (ok "installing grabs with an unmapped modifier does not signal"
          (handler-case (progn (install-keybindings) :ok)
            (error () :error))
          :ok)
      (ok "a key with unmapped Hyper is not grabbed"
          (and (find '(97 :hyper) grabs :key #'second :test #'equal) t) nil)
      (ok "other keys are grabbed with river modifiers"
          (third (find '(65293 :super) grabs :key #'second :test #'equal))
          '(:mod4))
      (setf grabs '())
      (call-if 'set-modifier :hyper :mod3)
      (install-keybindings)
      (ok "once Hyper is mapped the key is grabbed on its river modifier"
          (third (find '(97 :hyper) grabs :key #'second :test #'equal))
          '(:mod3))
      (call-if 'set-modifier :hyper nil)
      (define-key 97 '(:hyper) nil)
      (setf *xkb-bindings-proxy* nil)
      (release-key-grabs)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
