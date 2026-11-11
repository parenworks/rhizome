;;; Tests for key grab sync: river grabs exactly the first keys the
;;; active top-level keymaps bind, and each sync sends only the
;;; difference. River's xkb-bindings requests are stubbed to record
;;; create/enable/disable/destroy against fake binding objects.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (requests '()) (live (make-hash-table))
      (manage-requests 0) (messages '()))
  (labels ((ok (name actual expected)
             (if (equal actual expected)
                 (progn (incf pass)
                        (format t "~&PASS: ~A~%" name))
                 (progn (incf fail)
                        (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                name actual expected))))
           (call-if (name &rest args)
             (if (fboundp name) (apply name args) :missing))
           (record (kind proxy)
             (push (cons kind (gethash proxy live)) requests))
           (sync ()
             ;; What on-manage-sequence runs for keys
             (install-keybindings-if-needed))
           (grabbed-p (keysym river-mods)
             (loop for k being the hash-values of live
                   thereis (equal k (list keysym river-mods))))
           (grab-set-has (key)
             ;; :missing when key-grab-set does not exist or returns no list
             (let ((set (call-if 'key-grab-set)))
               (if (listp set)
                   (and (member key set :test #'equal) t)
                   :missing)))
           (count-kind (kind)
             (count kind requests :key #'car))
           (by-text (list)
             (sort (copy-list list) #'string< :key #'prin1-to-string))
           (fresh ()
             (setf requests '() manage-requests 0)))

    ;; River-touching stubs
    (setf (symbol-function 'message)
          (lambda (fmt &rest args)
            (push (apply #'format nil fmt args) messages)
            nil))
    (dolist (fn '(show-message message-hide))
      (setf (symbol-function fn)
            (lambda (&rest args) (declare (ignore args)) nil)))
    (setf (symbol-function 'request-manage-dirty)
          (lambda () (incf manage-requests)))
    (setf (symbol-function 'rwm:river-xkb-bindings-v1.get-xkb-binding)
          (lambda (manager seat keysym modifiers)
            (declare (ignore manager seat))
            (let ((proxy (make-instance 'rwm:river-xkb-binding-v1)))
              (setf (gethash proxy live) (list keysym modifiers))
              (record :create proxy)
              proxy)))
    (setf (symbol-function 'rwm:river-xkb-binding-v1.enable)
          (lambda (proxy) (record :enable proxy)))
    (setf (symbol-function 'rwm:river-xkb-binding-v1.disable)
          (lambda (proxy) (record :disable proxy)))
    (setf (symbol-function 'rwm:river-xkb-binding-v1.destroy)
          (lambda (proxy) (record :destroy proxy) (remhash proxy live)))

    (setf *xkb-bindings-proxy* :xkb-bindings
          *seats* (make-hash-table)
          *prefix-mode-active* nil
          *prefix-state* nil)
    (setf (gethash :seat-proxy *seats*) :seat)
    (clear-keymap *top-map*)
    (clear-keymap *prefix-map*)
    (setup-default-keybindings)
    (release-key-grabs)
    (sync)

    ;; --- The grab set is what the maps bind ---
    (ok "the first sync grabs the defaults"
        (grabbed-p 65293 '(:mod4)) t)
    (ok "an unbound Super chord is not grabbed"
        (grabbed-p 117 '(:mod4)) nil)
    (ok "the grab set lists the bound first keys"
        (let ((ret (grab-set-has '(65293 :super)))
              (prefix (grab-set-has '(116 :control))))
          (if (or (eq ret :missing) (eq prefix :missing))
              :missing
              (and ret prefix)))
        t)
    (ok "the grab set leaves out an unbound chord"
        (grab-set-has '(117 :super)) nil)
    (fresh)
    (sync)
    (sync)
    (ok "syncs with nothing changed send no requests" requests '())

    ;; --- Binding and unbinding at runtime ---
    (define-key 117 '(:mod4) (lambda () :user-u))                 ; s-u
    (ok "a binding asks for a manage sequence" (plusp manage-requests) t)
    (fresh)
    (sync)
    (ok "binding s-u adds exactly one grab"
        (reverse requests) '((:create 117 (:mod4)) (:enable 117 (:mod4))))
    (fresh)
    (define-key 117 '(:mod4) nil)
    (sync)
    (ok "unbinding s-u destroys exactly its grab"
        requests '((:destroy 117 (:mod4))))
    (ok "s-u is released to the client" (grabbed-p 117 '(:mod4)) nil)

    ;; --- Unbinding a default ---
    (fresh)
    (call-if 'undefine-key *top-map* "s-e")
    (sync)
    (ok "undefining a default destroys its grab"
        requests '((:destroy 101 (:mod4))))
    (ok "the undefined default is not bound" (key-binding 101 '(:mod4)) nil)
    (fresh)
    (define-key 101 '(:mod4) nil)
    (sync)
    (ok "clearing the user entry restores the default grab"
        (reverse requests) '((:create 101 (:mod4)) (:enable 101 (:mod4))))

    ;; --- Pressing an undefined key ---
    (let ((ran nil))
      (define-key *top-map* "s-o" (lambda () (setf ran t)))
      (call-if 'undefine-key *top-map* "s-o")
      (let ((pressed (press-key "s-o")))
        (ok "pressing an undefined key runs nothing"
            (list (and pressed t) ran) '(nil nil)))
      (define-key *top-map* "s-o" nil)
      (sync))

    ;; --- Seats coming and going ---
    (let ((n (hash-table-count live)))
      (setf (gethash :seat-proxy-2 *seats*) :seat-2)
      (fresh)
      (sync)
      (ok "a new seat gets grabs without touching the first seat"
          (list (count-kind :destroy) (count-kind :create)
                (hash-table-count live))
          (list 0 n (* 2 n)))
      (remhash :seat-proxy-2 *seats*)
      (fresh)
      (sync)
      (ok "a removed seat has its grabs released"
          (list (count-kind :destroy) (count-kind :create)
                (hash-table-count live))
          (list n 0 n)))

    ;; --- set-user-mod moves defaults, not literal user keys ---
    (define-key 121 '(:mod4) (lambda () :user-y))                 ; s-y
    (sync)
    (fresh)
    (set-user-mod :alt)
    (ok "set-user-mod asks for a manage sequence" (plusp manage-requests) t)
    (sync)
    (ok "set-user-mod grabs the defaults on the new modifier"
        (list (grabbed-p 65293 '(:mod1)) (grabbed-p 65293 '(:mod4)))
        '(t nil))
    (ok "set-user-mod leaves the user's literal s- grab untouched"
        (remove-if-not (lambda (r) (equal (cdr r) '(121 (:mod4)))) requests)
        '())
    (ok "the user's s-y is still grabbed" (grabbed-p 121 '(:mod4)) t)
    (set-user-mod :super)
    (sync)
    (define-key 121 '(:mod4) nil)
    (sync)

    ;; --- An unmapped modifier stays skipped ---
    (define-key 97 '(:hyper) (lambda () :h-a))                    ; H-a
    (fresh)
    (sync)
    (ok "a key with unmapped Hyper is not grabbed"
        (remove-if-not (lambda (r) (eql (second r) 97)) requests) '())
    (fresh)
    (sync)
    (ok "it stays skipped with no requests" requests '())
    (call-if 'set-modifier :hyper :mod3)
    (ok "changing the modifier map asks for a manage sequence"
        (plusp manage-requests) t)
    (fresh)
    (sync)
    (ok "mapping Hyper grabs exactly that key"
        (reverse requests) '((:create 97 (:mod3)) (:enable 97 (:mod3))))
    (fresh)
    (call-if 'set-modifier :hyper nil)
    (sync)
    (ok "unmapping Hyper destroys exactly that grab"
        requests '((:destroy 97 (:mod3))))
    (define-key 97 '(:hyper) nil)
    (sync)

    ;; --- Moving the prefix key ---
    (fresh)
    (set-prefix-key "C-z")
    (sync)
    (ok "set-prefix-key moves exactly the prefix grab"
        (by-text requests)
        (by-text '((:destroy 116 (:ctrl)) (:create 122 (:ctrl))
                   (:enable 122 (:ctrl)))))
    (set-prefix-key "C-t")
    (sync)

    ;; --- Clearing the user keymaps ---
    (define-key 117 '(:mod4) (lambda () :user-u))                 ; s-u
    (sync)
    (fresh)
    (clear-user-keymaps)
    (sync)
    (ok "clearing the user keymaps releases exactly the dropped grab"
        requests '((:destroy 117 (:mod4))))

    ;; --- Undefined keys inside a sequence ---
    (let ((ran nil))
      (define-prefix-key 119 (lambda () (setf ran t)))
      (call-if 'undefine-key *prefix-map* "w")
      (ok "an undefined key is not enumerated"
          (find (make-key 119) (keymap-keys *prefix-map*) :test #'equal)
          nil)
      (setf *prefix-mode-active* t
            *prefix-current-map* *prefix-map*)
      (setf messages '())
      (prefix-handle-key 119)
      (ok "an undefined key in a sequence runs nothing" ran nil)
      (ok "and ends the sequence" *prefix-mode-active* nil)
      (ok "an undefined key in a sequence is reported unbound"
          (and (first messages) (search "is unbound" (first messages)) t)
          t)
      (ok "which-key does not list it"
          (search (format nil "~%w ") (which-key-text "C-t" *prefix-map*))
          nil))

    (sync)
    (fresh)
    (sync)
    (sync)
    (ok "syncs after every change above send no requests" requests '())

    (setf *xkb-bindings-proxy* nil))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (unless (zerop fail) (uiop:quit 1)))

(uiop:quit 0)
