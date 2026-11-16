;;; module-sources.lisp - named sources own what they register:
;;; a module's keybindings survive reload-config (#25), its window
;;; rules survive too, the :config source is replaced wholesale by
;;; re-reading the file, and set-window-rules replaces a source's
;;; whole set with no duplicates.
(ql:quickload :rhizome)

(in-package #:rhizome)

(configure '(:bar (:enabled nil)))

(defparameter *ms-pass* 0)
(defparameter *ms-fail* 0)

(defun ms-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *ms-pass*)
             (format t "~&PASS: ~A~%" name))
      (progn (incf *ms-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                     name actual expected))))

(let ((old-load (symbol-function 'load-user-config))
      (old-apply (symbol-function 'apply-config)))
  (flet ((stub-load () nil)
         (stub-apply () nil))
    (setf (symbol-function 'load-user-config) #'stub-load
          (symbol-function 'apply-config) #'stub-apply)
    (unwind-protect
         (progn
           ;; clean slate: no sources
           (clrhash *window-rule-sources*)
           (clrhash *source-key-bindings*)
           (setf *window-rule-source-order* nil)

           ;; --- a module binds a key and sets rules ---
           (let ((*current-source* :rhizome-test))
             (define-prefix-key 58 (lambda () nil) "test key")
             (set-window-rules
               '((:match "testapp" :float t))
               :source :rhizome-test))
           (ms-ok "module key noted under its source"
                  (gethash :rhizome-test *source-key-bindings*)
                  (list (cons *prefix-map* 58)))
           (ms-ok "module rules registered"
                  (gethash :rhizome-test *window-rule-sources*)
                  '((:match "testapp" :float t)))
           (ms-ok "effective rules include the module's"
                  (getf *config* :window-rules)
                  '((:match "testapp" :float t)))

           ;; --- reload-config: only the :config source is replaced ---
           (reload-config)
           (ms-ok "module key SURVIVES reload-config (#25)"
                  (gethash :rhizome-test *source-key-bindings*)
                  (list (cons *prefix-map* 58)))
           (ms-ok "module rules SURVIVE reload-config"
                  (gethash :rhizome-test *window-rule-sources*)
                  '((:match "testapp" :float t)))
           (ms-ok "effective rules still include the module's"
                  (getf *config* :window-rules)
                  '((:match "testapp" :float t)))

           ;; --- :config rules replace wholesale ---
           (let ((*current-source* :config))
             (configure '(:window-rules ((:match "cfg" :workspace 2)))))
           (ms-ok "config rules registered under :config"
                  (gethash :config *window-rule-sources*)
                  '((:match "cfg" :workspace 2)))
           (let ((*current-source* :config))
             (configure '(:window-rules ((:match "cfg2" :workspace 3)))))
           (ms-ok "second configure REPLACES (no merge, no duplicates)"
                  (gethash :config *window-rule-sources*)
                  '((:match "cfg2" :workspace 3)))
           (ms-ok "effective order: config first, then module"
                  (getf *config* :window-rules)
                  '((:match "cfg2" :workspace 3)
                    (:match "testapp" :float t)))

           ;; --- reload-config drops :config rules (file re-adds its own) ---
           (reload-config)
           (ms-ok "config rules dropped by reload"
                  (gethash :config *window-rule-sources*) nil)
           (ms-ok "module rules still the only ones left"
                  (getf *config* :window-rules)
                  '((:match "testapp" :float t)))

           ;; --- set-window-rules nil empties a source ---
           (set-window-rules nil :source :rhizome-test)
           (ms-ok "set-window-rules nil empties the set"
                  (gethash :rhizome-test *window-rule-sources*) nil)

           ;; --- remove-source-key-bindings undoes bindings ---
           (remove-source-key-bindings :rhizome-test)
           (ms-ok "source keybindings removed"
                  (gethash :rhizome-test *source-key-bindings*) nil))

      (setf (symbol-function 'load-user-config) old-load
            (symbol-function 'apply-config) old-apply))))

(format t "~&=== ~A passed, ~A failed ===~%" *ms-pass* *ms-fail*)
(when (plusp *ms-fail*)
  (uiop:quit 1))
