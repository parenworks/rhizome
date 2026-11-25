;;; window-rules-reload.lisp: window rules across real config and module
;;; reloads. Each reload of a source leaves exactly one copy of that
;;; source's rules: a removed rule is gone, a changed rule is replaced,
;;; and other sources keep their sets and their place in the matching
;;; order. Uses a throwaway config file and module under /tmp; the
;;; user's own config and module directory are never read.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun reload-test-rules ()
  "The effective rule list that apply-window-rules matches against."
  (config-get '(:window-rules)))

(defun reload-test-matches ()
  "The :match strings of the effective rule list, in order."
  (mapcar (lambda (rule) (getf rule :match)) (reload-test-rules)))

(let ((pass 0) (fail 0)
      (tmp (format nil "/tmp/rhizome-test-rules-~A-~A/"
                   (get-universal-time) (random 100000)))
      (real-config-pathname (symbol-function 'config-pathname)))
  (labels ((ok (name thunk expected)
             (let ((actual (handler-case (funcall thunk)
                             (error (e) (list :error (princ-to-string e))))))
               (if (equal actual expected)
                   (progn (incf pass)
                          (format t "~&PASS: ~A~%" name))
                   (progn (incf fail)
                          (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                                  name actual expected)))))
           (config-file ()
             (format nil "~Aconfig.lisp" tmp))
           (write-config (text)
             (with-open-file (s (config-file) :direction :output
                                              :if-exists :supersede)
               (write-string text s)))
           (write-module (rule-text)
             (with-open-file (s (format nil "~Amodules/rules-module/rules-module.lisp" tmp)
                                :direction :output :if-exists :supersede)
               (format s "(in-package #:rhizome)~%(set-window-rules '(~A))~%"
                       rule-text)))
           (quiet (thunk)
             (let ((*standard-output* (make-broadcast-stream)))
               (funcall thunk)))
           (quiet-reload ()
             (quiet #'reload-config))
           (fresh-sources (thunk)
             ;; Each scenario starts with no source holding rules
             (let ((*window-rule-sources* (make-hash-table))
                   (*window-rule-source-order* nil)
                   (*source-key-bindings* (make-hash-table)))
               (setf (getf *config* :window-rules) nil)
               (funcall thunk)))
           (window-with-app-id (proxy app-id)
             (let ((win (make-instance 'rhizome-window :proxy proxy)))
               (setf (gethash proxy *windows*) win
                     (window-app-id win) app-id)
               win)))
    (unwind-protect
         (progn
           (ensure-directories-exist
             (format nil "~Amodules/rules-module/" tmp))
           (with-open-file (s (format nil "~Amodules/rules-module/rules-module.asd" tmp)
                              :direction :output :if-exists :supersede)
             (format s "(asdf:defsystem #:rules-module~@
                        :components ((:file \"rules-module\")))~%"))
           (write-module "(:match \"from-module\" :workspace 8)")
           (setf (symbol-function 'config-pathname)
                 (lambda () (pathname (config-file))))

           ;; --- the config file loaded at startup, then reloaded ---
           (fresh-sources
             (lambda ()
               (write-config
                 "(rhizome:configure '(:window-rules ((:match \"boot\" :workspace 1))))")
               (quiet #'load-user-config)
               (ok "the config file loaded at startup sets its rules once"
                   #'reload-test-matches '("boot"))
               (quiet-reload)
               (ok "a reload after the startup load leaves one copy"
                   #'reload-test-matches '("boot"))
               (write-config "")
               (quiet-reload)
               (ok "a rule removed from the file after startup is gone on reload"
                   #'reload-test-matches '())))

           ;; --- the config file's rules, set through configure ---
           (fresh-sources
             (lambda ()
               (write-config
                 "(rhizome:configure '(:window-rules ((:match \"keep\" :workspace 1)
                                                      (:match \"change\" :workspace 2)
                                                      (:match \"drop\" :workspace 3))))")
               (quiet-reload)
               (quiet-reload)
               (ok "unchanged config rules stay one copy over two reloads"
                   #'reload-test-matches '("keep" "change" "drop"))
               (write-config
                 "(rhizome:configure '(:window-rules ((:match \"keep\" :workspace 1)
                                                      (:match \"change\" :workspace 5))))")
               (quiet-reload)
               (ok "a rule removed from the config file is gone after reload"
                   #'reload-test-matches '("keep" "change"))
               (ok "a changed config rule is replaced, not duplicated"
                   (lambda ()
                     (mapcar (lambda (rule) (getf rule :workspace))
                             (remove "change" (reload-test-rules)
                                     :key (lambda (rule) (getf rule :match))
                                     :test-not #'equal)))
                   '(5))))

           ;; --- the config file's rules, set through set-window-rules ---
           (fresh-sources
             (lambda ()
               (write-config
                 "(rhizome:set-window-rules '((:match \"a\" :workspace 1)
                                              (:match \"b\" :workspace 2)))")
               (quiet-reload)
               (quiet-reload)
               (ok "set-window-rules in the config file: one copy over two reloads"
                   #'reload-test-matches '("a" "b"))
               (write-config
                 "(rhizome:set-window-rules '((:match \"a\" :workspace 4)))")
               (quiet-reload)
               (ok "set-window-rules in the config file: removed rule gone, changed rule replaced"
                   #'reload-test-rules '((:match "a" :workspace 4)))
               (write-config "")
               (quiet-reload)
               (ok "a config file that sets no rules leaves none after reload"
                   #'reload-test-rules '())))

           ;; --- another source beside the config file ---
           (fresh-sources
             (lambda ()
               (write-config
                 "(rhizome:configure '(:window-rules ((:match \"cfg\" :workspace 1))))")
               (quiet-reload)
               (set-window-rules '((:match "other" :workspace 6)) :source :other)
               (set-window-rules '((:match "other" :workspace 7)) :source :other)
               (ok "setting a source twice replaces its set"
                   #'reload-test-rules
                   '((:match "cfg" :workspace 1) (:match "other" :workspace 7)))
               (quiet-reload)
               (quiet-reload)
               (ok "another source's rules survive config reloads as one copy, after the config's"
                   #'reload-test-rules
                   '((:match "cfg" :workspace 1) (:match "other" :workspace 7)))
               (set-window-rules nil :source :other)
               (ok "setting a source to NIL removes its rules"
                   #'reload-test-matches '("cfg"))))

           ;; --- a module loaded with load-module from the config file ---
           (fresh-sources
             (lambda ()
               (write-config
                 "(rhizome:configure '(:window-rules ((:match \"cfg\" :workspace 1))))
                  (rhizome:load-module \"rules-module\")")
               (let ((*module-directory* (pathname (format nil "~Amodules/" tmp))))
                 (quiet-reload)
                 (ok "a module's top-level rules land under the module's name"
                     (lambda () (gethash :rules-module *window-rule-sources*))
                     '((:match "from-module" :workspace 8)))
                 (quiet-reload)
                 (quiet-reload)
                 (ok "module rules survive config reloads as one copy"
                     #'reload-test-matches '("cfg" "from-module"))
                 (write-module "(:match \"from-module\" :workspace 9)")
                 (quiet (lambda ()
                          (let ((*current-source* :rules-module))
                            (asdf:load-system "rules-module"
                                              :force '("rules-module")))))
                 (ok "reloading the module replaces its rule and keeps the config's"
                     #'reload-test-rules
                     '((:match "cfg" :workspace 1)
                       (:match "from-module" :workspace 9))))
               ;; matching uses the effective list, config first
               (set-window-rules '((:match "shared" :workspace 6)
                                   (:match "only-module" :workspace 7))
                                 :source :rules-module)
               (let ((*current-source* :config))
                 (configure '(:window-rules ((:match "shared" :workspace 2)))))
               (let ((win (window-with-app-id :reload-w1 "shared")))
                 (quiet (lambda () (apply-window-rules win)))
                 (ok "a config rule wins over a module's rule"
                     (lambda () (window-workspace win)) 2))
               (let ((win (window-with-app-id :reload-w2 "only-module")))
                 (quiet (lambda () (apply-window-rules win)))
                 (ok "a module's rule applies when no config rule matches"
                     (lambda () (window-workspace win)) 7))))

           ;; --- sources keep their place in the matching order ---
           (fresh-sources
             (lambda ()
               (set-window-rules '((:match "first" :workspace 1)) :source :first)
               (set-window-rules '((:match "second" :workspace 2)) :source :second)
               (ok "sources match in the order they first set rules"
                   #'reload-test-matches '("first" "second"))
               (set-window-rules nil :source :first)
               (set-window-rules '((:match "first" :workspace 3)) :source :first)
               (ok "a source emptied and set again holds one copy of its set"
                   (lambda ()
                     (count "first" (reload-test-matches) :test #'equal))
                   1)
               (ok "a source emptied and set again keeps its place"
                   #'reload-test-matches '("first" "second")))))

      ;; --- cleanup ---
      (setf (symbol-function 'config-pathname) real-config-pathname)
      (remhash :reload-w1 *windows*)
      (remhash :reload-w2 *windows*)
      (ignore-errors (uiop:delete-directory-tree
                       (pathname tmp) :validate t))))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
