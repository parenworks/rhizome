;;; Bar hide and show: toggle-bar hides and shows the running bar
;;; process instead of stopping and restarting it, the WM reserves the
;;; bar's space only while river reports an exclusive zone at least as
;;; tall as the bar, a configure that leaves the :bar settings alone
;;; keeps the bar, and concurrent start-bar calls launch one process.
;;;
;;; Process launch, termination and pkill are stubbed: no bar process
;;; or SBCL child is ever started. The snapshot write is stubbed so the
;;; user's ~/.cache/rhizome/snapshot.sexp is never touched, and the bar
;;; log path points into a temporary directory.
(ql:quickload :rhizome)

(in-package #:rhizome)

(defvar *bh-pass* 0)
(defvar *bh-fail* 0)

(defun bh-ok (name actual expected)
  (if (equal actual expected)
      (progn (incf *bh-pass*) (format t "~&PASS: ~A~%" name))
      (progn (incf *bh-fail*)
             (format t "~&FAIL: ~A (got ~S, wanted ~S)~%" name actual expected))))

(defmacro bh-case (name &body body)
  "Run BODY; an error in it is recorded as a failure of NAME."
  `(handler-case (progn ,@body)
     (error (e)
       (incf *bh-fail*)
       (format t "~&FAIL: ~A (error: ~A)~%" ,name e))))

;;; --- Effect stubs ---

(defvar *bh-launched* 0)
(defvar *bh-terminated* 0)
(defvar *bh-launch-delay* 0)
(defvar *bh-alive-during-launch* :unset)
(defvar *bh-snapshots* nil)
(defvar *bh-retiles* 0)

(setf (symbol-function 'uiop:launch-program)
      (lambda (&rest args)
        (declare (ignore args))
        (setf *bh-alive-during-launch* (bar-alive-p))
        (sleep *bh-launch-delay*)
        (incf *bh-launched*)
        (list :fake-bar *bh-launched*)))
(setf (symbol-function 'uiop:process-alive-p)
      (lambda (process) (and (consp process) (eq (first process) :fake-bar))))
(setf (symbol-function 'uiop:terminate-process)
      (lambda (&rest args) (declare (ignore args)) (incf *bh-terminated*)))
(setf (symbol-function 'uiop:wait-process)
      (lambda (&rest args) (declare (ignore args)) nil))
(setf (symbol-function 'uiop:run-program)
      (lambda (&rest args) (declare (ignore args)) nil))
(let ((dir (uiop:ensure-directory-pathname
            (format nil "/tmp/rhizome-bar-hide-test-~D" (random 1000000)))))
  (setf (symbol-function 'bar-log-path)
        (lambda () (merge-pathnames "bar.log" dir))))
(setf (symbol-function 'bar-write-snapshot)
      (lambda (snapshot) (push snapshot *bh-snapshots*)))
(setf (symbol-function 'request-manage-dirty)
      (lambda () (incf *bh-retiles*)))
(setf (symbol-function 'font-apply-config) (lambda () nil))
(setf (symbol-function 'output-apply-config) (lambda () nil))

(defun bh-reset (&key (enabled t))
  (setf *bh-launched* 0 *bh-terminated* 0 *bh-launch-delay* 0
        *bh-snapshots* nil *bh-retiles* 0
        *bar-process* nil *bar-process-mode* nil *wm-process-mode* t
        *wm-proxy* :fake-wm *wm-snapshot* nil
        *config* (list :bar (list :enabled enabled :height 28 :margin 8)))
  (when (boundp '*bar-non-exclusive-heights*)
    (clrhash (symbol-value '*bar-non-exclusive-heights*))))

(defun bh-last-hidden ()
  (getf (first *bh-snapshots*) :bar-hidden :absent))

;;; --- Toggle hides and shows the running bar ---

(bh-case "toggle off keeps the bar process"
  (bh-reset)
  (bar-apply-config)
  (let ((process *bar-process*))
    (toggle-bar)
    (bh-ok "toggle off: config disabled"
           (config-getf '(:bar) :enabled :default t) nil)
    (bh-ok "toggle off: process neither stopped nor launched again"
           (list *bh-terminated* *bh-launched* (eq *bar-process* process))
           '(0 1 t))
    (bh-ok "toggle off: snapshot tells the bar to hide"
           (bh-last-hidden) t)))

(bh-case "toggle on shows the same bar"
  (let ((process *bar-process*))
    (toggle-bar)
    (bh-ok "toggle on: config enabled"
           (config-getf '(:bar) :enabled :default t) t)
    (bh-ok "toggle on: same process, nothing launched or stopped"
           (list *bh-terminated* *bh-launched* (eq *bar-process* process))
           '(0 1 t))
    (bh-ok "toggle on: snapshot tells the bar to show"
           (bh-last-hidden) nil)))

;;; --- Configure without a bar change keeps the bar ---

(bh-case "configure without a bar change"
  (bh-reset)
  (bar-apply-config)
  (configure '(:theme (:gaps 4)))
  (configure '(:bar (:enabled t :height 28)))
  (bh-ok "configure leaving :bar alone: no restart"
         (list *bh-terminated* *bh-launched*) '(0 1)))

(bh-case "configure with a bar change"
  (configure '(:bar (:height 30)))
  (bh-ok "configure changing :bar height: one restart"
         (list *bh-terminated* *bh-launched*) '(1 2)))

;;; --- The bar process follows the snapshot ---

(bh-case "bar process opens and closes its surface"
  (let ((opened 0) (closed 0) (hidden nil)
        (old-open (and (fboundp 'bar-open-surface) (symbol-function 'bar-open-surface)))
        (old-close (and (fboundp 'bar-close-surface) (symbol-function 'bar-close-surface)))
        (old-snapshot (symbol-function 'bar-snapshot)))
    (setf (symbol-function 'bar-open-surface)
          (lambda (state) (incf opened) (setf (gethash :surface state) :fake-surface))
          (symbol-function 'bar-close-surface)
          (lambda (state) (incf closed) (setf (gethash :surface state) nil))
          (symbol-function 'bar-snapshot)
          (lambda () (list :bar-hidden hidden)))
    (unwind-protect
         (let ((state (make-hash-table)))
           (bar-sync-visibility state)
           (bh-ok "shown, no surface: opens one" (list opened closed) '(1 0))
           (bar-sync-visibility state)
           (bh-ok "shown, surface open: nothing to do" (list opened closed) '(1 0))
           (setf hidden t)
           (bar-sync-visibility state)
           (bh-ok "hidden: closes the surface" (list opened closed) '(1 1))
           (bar-sync-visibility state)
           (bh-ok "hidden, no surface: nothing to do" (list opened closed) '(1 1)))
      (setf (symbol-function 'bar-snapshot) old-snapshot)
      (if old-open
          (setf (symbol-function 'bar-open-surface) old-open)
          (fmakunbound 'bar-open-surface))
      (if old-close
          (setf (symbol-function 'bar-close-surface) old-close)
          (fmakunbound 'bar-close-surface)))))

;;; --- Reserved space follows river's exclusive zone ---

(bh-case "reserved space"
  (bh-reset)
  (let ((out (make-instance 'rhizome-output :height 1080)))
    (bh-ok "no area reported yet: config decides" (bar-reserved-height) 44)
    (bar-note-non-exclusive-area out 1080)
    (bh-ok "river reports the full output: nothing reserved"
           (bar-reserved-height) 0)
    (bh-ok "space released: re-tile requested" (plusp *bh-retiles*) t)
    (setf *bh-retiles* 0)
    (bar-note-non-exclusive-area out (- 1080 44))
    (bh-ok "river reports the bar's zone: space reserved"
           (bar-reserved-height) 44)
    (bh-ok "space reserved: re-tile requested" (plusp *bh-retiles*) t)
    (setf *bh-retiles* 0)
    (bar-note-non-exclusive-area out (- 1080 44))
    (bh-ok "same area again: no re-tile" *bh-retiles* 0)
    (setf *config* (list :bar (list :enabled nil :height 28 :margin 8)))
    (bh-ok "bar disabled: nothing reserved even with a zone"
           (bar-reserved-height) 0)))

(bh-case "toggle on reserves nothing until the zone appears"
  (bh-reset :enabled nil)
  (let ((out (make-instance 'rhizome-output :height 1080)))
    (bar-note-non-exclusive-area out 1080)
    (toggle-bar)
    (bh-ok "enabled, no zone yet: nothing reserved" (bar-reserved-height) 0)))

;;; --- Concurrent starts launch one bar ---

(bh-case "concurrent start-bar"
  (bh-reset)
  (setf *bh-launch-delay* 0.3)
  (let ((threads (loop repeat 2
                       collect (bt:make-thread (lambda () (start-bar))))))
    (mapc #'bt:join-thread threads))
  (bh-ok "two concurrent starts launch one bar" *bh-launched* 1)
  (bh-ok "a start in flight counts as alive" *bh-alive-during-launch* t))

(format t "~&=== ~A passed, ~A failed ===~%" *bh-pass* *bh-fail*)
(uiop:quit (if (zerop *bh-fail*) 0 1))
