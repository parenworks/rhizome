;;; Tests for key names and keysym printing: kbd names for ISO_Left_Tab
;;; and the XF86 keys, key-description round trips, XKB's
;;; keysym-to-character rule, and unnamed keysyms in which-key text and
;;; the key-pressed log. River requests are stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (ran nil))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (attempt (thunk)
             ;; THUNK's value, or :error when it signals
             (handler-case (funcall thunk)
               (error () :error)))
           (parse (spec)
             (handler-case (kbd spec)
               (error () :rejected)))
           (describe-all (spec)
             (handler-case (mapcar #'key-description (kbd spec))
               (error () :rejected)))
           (which-key-lists-p (keysym text)
             ;; True when which-key text for a keymap binding KEYSYM
             ;; contains TEXT
             (handler-case
                 (let ((map (make-keymap)))
                   (define-keymap-key map keysym (lambda () nil) "test key")
                   (and (search text (which-key-text "s-x" map)) t))
               (error () :error)))
           (press-grabbed (key)
             ;; Bind KEY in *top-map*, grab it through the real grab-key,
             ;; and fire the grab's hooks with :pressed as wayflan
             ;; dispatches river's pressed event. Returns the output.
             (setf *seats* (make-hash-table)
                   *windows* (make-hash-table)
                   *grabbed-keys* (make-hash-table :test 'equal)
                   *active-bindings* nil
                   *xkb-bindings-proxy* :xkb-bindings
                   ran nil)
             (define-key *top-map* key (lambda () (setf ran :ran)))
             (let* ((out *standard-output*)
                    (text
                      (with-output-to-string (*standard-output*)
                        (handler-case
                            (let ((binding (grab-key :seat-proxy key nil)))
                              (dolist (hook (wl:wl-proxy-hooks binding))
                                (funcall hook :pressed)))
                          (error (e)
                            (format t "~&; press signalled ~A~%" (type-of e))
                            (format out "~&; press signalled ~A~%"
                                    (type-of e)))))))
               (define-key *top-map* key nil)
               (setf *xkb-bindings-proxy* nil)
               text)))

    ;; River-touching stubs
    (dolist (fn '(message show-message message-hide request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'rwm:river-xkb-bindings-v1.get-xkb-binding)
          (lambda (&rest args)
            (declare (ignore args))
            (make-instance 'wl:wl-proxy)))
    (setf (symbol-function 'rwm:river-xkb-binding-v1.enable)
          (lambda (&rest args) (declare (ignore args)) nil))

    ;; kbd names
    (ok "kbd reads s-ISO_Left_Tab"
        (parse "s-ISO_Left_Tab") '((65056 :super)))
    (ok "kbd reads XF86MonBrightnessUp"
        (parse "XF86MonBrightnessUp") '((269025026)))
    (ok "kbd reads XF86MonBrightnessDown"
        (parse "XF86MonBrightnessDown") '((269025027)))
    (ok "kbd reads Shift with a brightness key"
        (parse "S-XF86MonBrightnessUp") '((269025026 :shift)))
    (ok "kbd reads the volume and mute keys"
        (parse "XF86AudioRaiseVolume XF86AudioLowerVolume XF86AudioMute XF86AudioMicMute")
        '((269025043) (269025041) (269025042) (269025202)))

    ;; key-description round trips
    (ok "key-description round-trips ISO_Left_Tab and the brightness keys"
        (describe-all "s-ISO_Left_Tab XF86MonBrightnessUp S-XF86MonBrightnessDown")
        '("s-ISO_Left_Tab" "XF86MonBrightnessUp" "S-XF86MonBrightnessDown"))
    (ok "key-description round-trips the volume and mute keys"
        (describe-all "XF86AudioRaiseVolume XF86AudioLowerVolume XF86AudioMute XF86AudioMicMute")
        '("XF86AudioRaiseVolume" "XF86AudioLowerVolume" "XF86AudioMute" "XF86AudioMicMute"))
    (ok "the ISO_Left_Tab keysym prints by name"
        (attempt (lambda () (key-description (make-key 65056 '(:super)))))
        "s-ISO_Left_Tab")

    ;; Keysym printing: name, character, or #N
    (ok "keysym-name prints an unnamed XF86 keysym as #N"
        (attempt (lambda () (keysym-name 269025124)))
        "#269025124")
    (ok "key-description prints an unnamed XF86 keysym with its modifiers"
        (attempt (lambda () (key-description (make-key 269025124 '(:super :shift)))))
        "s-S-#269025124")
    (ok "keysym-name prints an unnamed function keysym as #N"
        (attempt (lambda () (keysym-name 65505)))
        "#65505")
    (ok "keysym-name prints a Unicode keysym as its character"
        (attempt (lambda () (keysym-name 16785580)))
        (string (code-char 8364)))
    (ok "keysym-name prints an ASCII keysym as its character"
        (keysym-name 120) "x")
    (ok "keysym-name prints a Latin-1 letter as itself"
        (keysym-name 233) (string (code-char 233)))
    (ok "the euro sign kbd reads still prints as itself"
        (attempt (lambda ()
                   (let ((keys (kbd (string (code-char 8364)))))
                     (list keys (key-description (first keys))))))
        (list (list (list 8364)) (string (code-char 8364))))
    (ok "keysym-name prints a control keysym as #N"
        (keysym-name 7) "#7")
    (ok "keysym-name prints a named keysym by its first name"
        (keysym-name 65293) "Return")

    ;; which-key text
    (ok "which-key text lists an unnamed XF86 keysym"
        (which-key-lists-p 269025124 "#269025124") t)
    (ok "which-key text lists a brightness key by name"
        (which-key-lists-p 269025026 "XF86MonBrightnessUp") t)

    ;; A grabbed press logs the key and runs its binding
    (let ((text (press-grabbed (make-key 269025124))))
      (ok "a grabbed press of an unnamed XF86 keysym runs its binding"
          ran :ran)
      (ok "the key-pressed log names an unnamed XF86 keysym"
          (and (search "key pressed: #269025124" text) t) t))
    (let ((text (press-grabbed (make-key 269025026 '(:shift)))))
      (ok "the key-pressed log names Shift with a brightness key"
          (and (search "key pressed: S-XF86MonBrightnessUp" text) t) t)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
