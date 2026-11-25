;;; bar-launch-core.lisp: the bar process runs on the WM's own SBCL
;;; core, whatever the WM's working directory holds. With no core
;;; named, the SBCL runtime loads ./sbcl.core from the working
;;; directory in preference to its installed core. Each run check
;;; starts SBCL in a directory holding a planted sbcl.core that
;;; defines CL-USER::*STRAY-MARKER*, and records whether the marker is
;;; bound in the started image.
;;;
;;; RHIZOME_STRAY_CORE_DIR names a directory that already holds such a
;;; core; otherwise the suite saves one into a temporary directory.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Never start the bar from a test run
(configure '(:bar (:enabled nil)))

(defparameter *probe-form*
  "(format t \"~&STRAY=~A~%\" (boundp 'cl-user::*stray-marker*))"
  "Printed by each probe image: whether the planted core was loaded.")

(defun plant-stray-core (directory)
  "Save an SBCL image defining CL-USER::*STRAY-MARKER* as
DIRECTORY/sbcl.core. Signal an error when no core file results."
  (let ((core (merge-pathnames "sbcl.core" directory)))
    (ensure-directories-exist directory)
    (uiop:run-program
     (list "sbcl" "--core" (sb-ext:native-namestring sb-ext:*core-pathname*)
           "--noinform" "--no-sysinit" "--no-userinit" "--non-interactive"
           "--eval" "(defvar cl-user::*stray-marker* t)"
           "--eval" (format nil "(sb-ext:save-lisp-and-die ~S)"
                            (sb-ext:native-namestring core)))
     :output nil :error-output nil :ignore-error-status t)
    (unless (probe-file core)
      (error "No stray core saved at ~A; check that sbcl runs." core))
    core))

(defun probe-stray (runtime-argv directory)
  "Run RUNTIME-ARGV plus the probe form in DIRECTORY and return the
probe's STRAY= value as a string, or NIL when it printed none."
  (let* ((output (uiop:run-program
                  (append runtime-argv
                          (list "--noinform" "--non-interactive"
                                "--eval" *probe-form*))
                  :directory directory
                  :output :string :error-output :output
                  :ignore-error-status t))
         (start (search "STRAY=" output :from-end t)))
    (when start
      (string-trim '(#\Newline #\Space #\Return)
                   (subseq output (+ start 6)
                           (or (position #\Newline output :start start)
                               (length output)))))))

(let* ((pass 0) (fail 0)
       (given (uiop:getenv "RHIZOME_STRAY_CORE_DIR"))
       (directory (uiop:ensure-directory-pathname
                   (if (and given (plusp (length given)))
                       given
                       (format nil "/tmp/rhizome-stray-core-~A-~A/"
                               (get-universal-time) (random 100000)))))
       (own-core (sb-ext:native-namestring sb-ext:*core-pathname*)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected)))))
    (unwind-protect
         (let ((argv (and (fboundp 'bar-launch-argv)
                          (funcall 'bar-launch-argv))))
           (unless (probe-file (merge-pathnames "sbcl.core" directory))
             (plant-stray-core directory))
           (ok "bar argv names the WM's own core as its first option"
               (subseq argv 0 (min 3 (length argv)))
               (list "sbcl" "--core" own-core))
           (ok "bar argv still runs the bar"
               (and (member "(rhizome:run-bar)" argv :test #'equal) t)
               t)
           ;; Control: a bare sbcl in that directory loads the planted
           ;; core, so the check below can fail
           (ok "bare sbcl in the directory loads the planted core"
               (probe-stray (list "sbcl") directory)
               "T")
           (ok "the bar argv's runtime part ignores the planted core"
               (and argv (probe-stray (subseq argv 0 3) directory))
               "NIL"))
      (unless given
        (uiop:delete-directory-tree directory :validate t
                                              :if-does-not-exist :ignore))))
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (uiop:quit (if (zerop fail) 0 1)))
