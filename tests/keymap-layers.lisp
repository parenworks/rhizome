;;; Tests for layered keymaps: user bindings win over the defaults and
;;; survive manage sequences, set-user-mod, and the order in which
;;; user and default bindings are made; the old define-key /
;;; define-prefix-key / define-keymap-key API keeps working.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (direct-binding (keysym mods)
             ;; What a press of KEYSYM+MODS runs
             (key-binding keysym mods))
           (manage-cycles (n)
             (dotimes (i n) (on-manage-sequence)))
           (press-prefix-chain (&rest keysyms)
             ;; Simulate C-t followed by KEYSYMS through the capture
             ;; surface dispatcher.
             (setf *prefix-mode-active* t
                   *prefix-current-map* *prefix-map*)
             (dolist (k keysyms) (prefix-handle-key k))))

    ;; River-touching stubs
    (dolist (fn '(message show-message message-hide request-manage-dirty))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))

    (setf *xkb-bindings-proxy* nil
          *prefix-mode-active* nil
          *prefix-current-map* nil
          *prefix-state* nil
          *minor-mode-stack* nil)

    ;; --- The keymap object: parents, shadowing, enumeration ---
    (let* ((base (make-keymap :name :base))
           (extra (make-keymap :name :extra))
           (child (make-keymap :name :child :parents (list base extra)))
           (seen '()))
      (keymap-bind base :a :base-a "base a")
      (keymap-bind base :b :base-b)
      (keymap-bind extra :b :extra-b)
      (keymap-bind extra :c :extra-c)
      (keymap-bind child :a :child-a)
      (ok "child binding shadows the parent's" (keymap-lookup child :a) :child-a)
      (ok "earlier parent wins over a later one" (keymap-lookup child :b) :base-b)
      (ok "later parent still contributes" (keymap-lookup child :c) :extra-c)
      (ok "lookup reports the keymap holding the binding"
          (eq (nth-value 1 (keymap-lookup child :c)) extra) t)
      (ok "unbound key is NIL with no holder"
          (multiple-value-list (keymap-lookup child :z)) '(nil nil))
      (ok "doc follows the effective binding"
          (list (keymap-binding-doc base :a) (keymap-binding-doc child :a))
          '("base a" nil))
      (map-keymap (lambda (k b) (push (cons k b) seen)) child)
      (ok "map-keymap visits each key once with its effective binding"
          (sort (copy-list seen) #'string< :key (lambda (e) (string (car e))))
          '((:a . :child-a) (:b . :base-b) (:c . :extra-c)))
      (keymap-bind child :a nil)
      (ok "unbinding in the child uncovers the parent" (keymap-lookup child :a) :base-a))

    (let ((user-return (lambda () :user-return))
          (user-z (lambda () :user-z))
          (late-g (lambda () :late-g))
          (ran nil))

      ;; --- A user binding on a default's key, made at config time ---
      ;; (user config loads before the first manage sequence)
      (define-key +xkb-key-Return+ *mod* user-return)
      (manage-cycles 3)
      (ok "user binding on a default key wins after manage sequences"
          (direct-binding +xkb-key-Return+ *mod*) user-return)
      (ok "defaults are present alongside the user layer"
          (functionp (direct-binding +xkb-key-Q+ *mod*)) t)

      ;; --- A user binding made after the defaults ---
      (define-key +xkb-key-G+ *mod* late-g)
      (manage-cycles 3)
      (ok "user binding made after the defaults wins and survives"
          (direct-binding +xkb-key-G+ *mod*) late-g)

      ;; --- Prefix map: user entries win over default entries ---
      (define-prefix-key 119 (lambda () (setf ran :user-w)) "user w")
      (manage-cycles 3)
      (setf ran nil)
      (press-prefix-chain 119)
      (ok "user C-t w survives manage sequences" ran :user-w)
      (ok "an unshadowed C-t entry resolves to the default layer"
          (eq (nth-value 1 (keymap-lookup *prefix-map* 103))
              *default-prefix-map*)
          t)

      ;; --- set-user-mod keeps user bindings ---
      (define-key 122 '(:mod4) user-z)
      (set-user-mod :alt)
      (manage-cycles 2)
      (ok "set-user-mod keeps a user direct binding"
          (direct-binding 122 '(:mod4)) user-z)
      (ok "set-user-mod keeps a user binding on a default key"
          (direct-binding +xkb-key-Return+ '(:mod4)) user-return)
      (ok "set-user-mod moves the defaults to the new modifier"
          (functionp (direct-binding +xkb-key-Q+ '(:mod1))) t)
      (setf ran nil)
      (press-prefix-chain 119)
      (ok "set-user-mod keeps user prefix entries" ran :user-w)
      (set-user-mod :super)
      (ok "user binding still wins after switching back"
          (direct-binding +xkb-key-Return+ *mod*) user-return)

      ;; --- Old API: define-keymap-key with a nested make-keymap ---
      (let ((nested (make-keymap)))
        (define-keymap-key nested 99 (lambda () (setf ran :nested-c)))
        (define-prefix-key 121 nested "nested y"))
      (setf ran nil)
      (press-prefix-chain 121 99)
      (ok "define-prefix-key with a nested keymap dispatches" ran :nested-c)
      (define-key 120 '(:mod4 :ctrl) (lambda () :plain))
      (ok "define-key binds an unbound key"
          (funcall (direct-binding 120 '(:mod4 :ctrl))) :plain)

      ;; --- set-prefix-key moves the prefix ---
      (set-prefix-key "C-z")
      (ok "set-prefix-key binds the new prefix"
          (functionp (direct-binding 122 '(:ctrl))) t)
      (ok "set-prefix-key unbinds the old prefix"
          (direct-binding 116 '(:ctrl)) nil)
      (manage-cycles 2)
      (ok "the moved prefix survives manage sequences"
          (list (functionp (direct-binding 122 '(:ctrl)))
                (direct-binding 116 '(:ctrl)))
          '(t nil))
      (set-prefix-key "C-t")

      ;; --- reload-config replaces the user layer ---
      (let ((reloaded (lambda () :reloaded)))
        (setf (symbol-function 'load-user-config)
              (lambda () (define-key 104 '(:mod4) reloaded)))
        (setf (symbol-function 'apply-config) (lambda () nil))
        (reload-config)
        (ok "reload-config drops user bindings the file no longer makes"
            (direct-binding 122 '(:mod4)) nil)
        (ok "reload-config restores the default under a dropped override"
            (and (functionp (direct-binding +xkb-key-Return+ *mod*))
                 (not (eq (direct-binding +xkb-key-Return+ *mod*)
                          user-return)))
            t)
        (ok "reload-config installs what the file binds"
            (direct-binding 104 '(:mod4)) reloaded)
        (setf ran nil)
        (press-prefix-chain 119)
        (ok "reload-config drops user prefix entries"
            (eq ran :user-w) nil))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
