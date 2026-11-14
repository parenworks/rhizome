;;; modules.lisp — load-module: directory discovery, ASDF
;;; registration, idempotent loading. Uses a throwaway module under
;;; /tmp; nothing here touches the user's own module directory.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(let ((pass 0) (fail 0) (tmp (format nil "/tmp/rhizome-test-mod-~A"
                                     (get-universal-time))))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                          name actual expected)))))
    (unwind-protect
         (progn
           ;; --- fixture: one real module, one dir without an .asd ---
           (ensure-directories-exist
             (format nil "~A/hello-module/" tmp))
           (with-open-file (s (format nil "~A/hello-module/hello-module.asd" tmp)
                              :direction :output :if-exists :supersede)
             (format s "(asdf:defsystem #:hello-module~@
                        :depends-on ()~@
                        :components ((:file \"thing\"))~%)"))
           (with-open-file (s (format nil "~A/hello-module/thing.lisp" tmp)
                              :direction :output :if-exists :supersede)
             (format s "(defpackage #:hello-module (:use #:cl))~@
                        (in-package #:hello-module)~@
                        (defparameter *thing* 42)~%"))
           ;; a directory with no .asd must be skipped, not registered
           (ensure-directories-exist (format nil "~A/not-a-module/" tmp))

           (let ((*module-directory*
                   (make-pathname :directory
                                  (list :absolute
                                        (format nil "~A/" tmp)))))
             ;; directory listing: only asd-carrying dirs, sorted
             (ok "module-subdirectories finds only .asd-carrying dirs"
                 (mapcar (lambda (d) (car (last (pathname-directory d))))
                         (module-subdirectories))
                 '("hello-module"))

             ;; loading an unknown module fails quietly
             (ok "unknown module returns NIL"
                 (load-module "no-such-module") nil)

             ;; first load: loads, system visible
             (ok "first load returns :LOADED"
                 (load-module "hello-module") :loaded)
             (ok "system actually loaded"
                 (and (asdf:find-system "hello-module" nil)
                      (asdf:component-loaded-p
                        (asdf:find-system "hello-module" nil))
                      t)
                 t)

             ;; second load: idempotent no-op
             (ok "second load returns :ALREADY-LOADED"
                 (load-module "hello-module") :already-loaded)

             ;; registration is additive and does not duplicate
             (register-module-directories)
             (ok "registry holds one entry per module dir"
                 (count (first (module-subdirectories)) asdf:*central-registry*
                        :test #'equal)
                 1)))

      ;; --- cleanup ---
      (ignore-errors (uiop:delete-directory-tree
                       (pathname tmp) :validate t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
