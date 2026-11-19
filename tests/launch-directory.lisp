;;; launch-directory.lisp: programs the WM launches for the user start
;;; in the home directory, whatever the WM process's own working
;;; directory is. Each check runs inside a temporary working directory
;;; and records what the launched process prints for `pwd`.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defun strip-trailing-slash (string)
  "STRING without a trailing slash (kept when STRING is \"/\") and
without trailing newlines and spaces."
  (string-right-trim '(#\Newline #\Space)
                     (if (and (> (length string) 1)
                              (char= (char string (1- (length string))) #\/))
                         (subseq string 0 (1- (length string)))
                         string)))

(defun file-text (path)
  "The contents of PATH with surrounding whitespace removed, or NIL."
  (when (probe-file path)
    (string-trim '(#\Newline #\Space) (uiop:read-file-string path))))

(defun launched-pwd (launch out)
  "Call LAUNCH with a pwd argv and an :output file OUT, wait for the
process, and return what it printed."
  (uiop:wait-process (funcall launch (list "/bin/sh" "-c" "pwd")
                              :output out :if-output-exists :supersede))
  (file-text out))

(defun wait-for-text (path &key (seconds 5))
  "Poll PATH until it holds text or SECONDS elapse; return the text."
  (loop repeat (* seconds 20)
        for text = (file-text path)
        when (and text (plusp (length text))) return text
        do (sleep 0.05)))

(let* ((pass 0) (fail 0)
       (home (strip-trailing-slash (namestring (user-homedir-pathname))))
       (scratch (uiop:ensure-directory-pathname
                 (format nil "/tmp/rhizome-launch-dir-~A-~A"
                         (get-universal-time) (random 100000))))
       (scratch-name (strip-trailing-slash (namestring scratch)))
       (out (merge-pathnames "pwd.out" scratch))
       (site-out (merge-pathnames "site.out" scratch)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (ensure-directories-exist scratch)
    (unwind-protect
         (uiop:with-current-directory (scratch)
           ;; Control: a bare launch inherits the WM's directory, so
           ;; the checks below can tell the two apart
           (ok "bare uiop:launch-program inherits the caller's directory"
               (launched-pwd #'uiop:launch-program out)
               scratch-name)
           (ok "launch-user-program starts the program in the home directory"
               (and (fboundp 'launch-user-program)
                    (launched-pwd 'launch-user-program out))
               home)
           ;; A real call site: the bar's click-command runner
           (ok "bar-run-command starts its shell in the home directory"
               (progn
                 (bar-run-command (format nil "pwd > ~A"
                                          (namestring site-out)))
                 (wait-for-text site-out))
               home))
      (uiop:delete-directory-tree scratch :validate t
                                          :if-does-not-exist :ignore)))
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
