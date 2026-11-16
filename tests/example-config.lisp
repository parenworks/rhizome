;;; example-config.lisp: examples/config.lisp loads without signalling,
;;; the values it marks "; default" are the defaults, and every
;;; "rhizome-cmd NAME" it mentions names a command.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

;; The example names rhizome-display's commands: load that module from
;; the repo's modules/ directory, the way a user loads it.
(let ((*module-directory*
        (make-pathname
          :directory (append (pathname-directory
                               (asdf:system-source-directory :rhizome))
                              '("modules")))))
  (load-module "rhizome-display"))

(let ((pass 0) (fail 0)
      (example (asdf:system-relative-pathname :rhizome "examples/config.lisp"))
      (defaults (default-config)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))

    ;; --- loading signals nothing ---
    (let ((conditions '()))
      (handler-bind ((condition (lambda (c) (push (princ-to-string c) conditions))))
        (load example))
      (ok "the example loads without signalling" conditions '()))

    ;; --- values marked as the default are the default ---
    (ok "the example's :font-size is the default"
        (config-getf '(:theme) :font-size)
        (getf (getf defaults :theme) :font-size))
    (ok "the example's :font-family is the default"
        (config-getf '(:theme) :font-family)
        (getf (getf defaults :theme) :font-family))

    ;; --- every rhizome-cmd it names exists ---
    (let ((text (uiop:read-file-string example))
          (missing '()))
      (cl-ppcre:do-register-groups (name) ("rhizome-cmd ([a-z][a-z-]*)" text)
        (unless (gethash (intern (string-upcase name) :keyword)
                         *command-table*)
          (pushnew name missing :test #'string=)))
      (ok "every rhizome-cmd command the example names exists"
          missing '())))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
