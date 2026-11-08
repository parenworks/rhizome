;;; display.lisp — the display configurator
;;;
;;; zwlr_output_management_unstable_v1: the WM tracks every output
;;; head (name, description, modes, current mode, scale, position)
;;; and applies configurations through the manager. This is what
;;; the display-menu bar module drives. River 0.5 removed the old
;;; riverctl interface — this standard wlroots protocol is the way.
;;;
;;; Configuration lifecycle: create_configuration carries the last
;;; done serial; every head must be RESTATED (enable_head + current
;;; mode/scale/position) because the protocol disables unmentioned
;;; heads — a partial config blanks the other outputs. Overrides
;;; land on the target head only. River answers with
;;; succeeded/failed; we surface that via message.

(in-package #:rhizome)

(defvar *output-manager* nil
  "The bound zwlr_output_manager_v1, or NIL before the registry
offers it (or after the manager says finished).")
(defvar *display-serial* 0
  "The manager's last done serial — configurations must carry it.")
(defvar *display-heads* (make-hash-table)
  "Head proxy → state plist: :head :name :description
:physical-size (w h) :enabled :modes (mode proxies, newest first)
:current-mode :position (x . y) :scale.")
(defvar *display-modes* (make-hash-table)
  "Mode proxy → plist: :width :height :refresh (millihertz).")

(defun display-manager-init (registry global-name version)
  "Bind zwlr_output_manager_v1 and start tracking heads."
  (let ((manager (wl:wl-registry.bind registry global-name
                                       (wl:find-interface-named
                                         "zwlr_output_manager_v1")
                                       (min version 4))))
    (setf *output-manager* manager)
    (push (function display-manager-event) (wl:wl-proxy-hooks manager))
    (format t "~&; Rhizome: display: output manager bound (v~D)~%"
            version)))

(defun display-manager-event (event-name &rest args)
  (case event-name
    (:head
     (destructuring-bind (head) args
       (setf (gethash head *display-heads*) (list :head head))
       (push (lambda (event-name &rest args)
               (apply #'display-head-event head event-name args))
             (wl:wl-proxy-hooks head))))
    (:done
     (destructuring-bind (serial) args
       (setf *display-serial* serial)))
    (:finished
     (setf *output-manager* nil))))

(defun display-head-event (head event-name &rest args)
  (case event-name
    (:name (display-head-put head :name (first args)))
    (:description (display-head-put head :description (first args)))
    (:physical-size (display-head-put head :physical-size args))
    (:enabled (display-head-put head :enabled (plusp (first args))))
    (:mode
     (destructuring-bind (mode) args
       (let ((state (gethash head *display-heads*)))
         (display-head-put head :modes (cons mode (getf state :modes))))
       (push (lambda (event-name &rest args)
               (apply #'display-mode-event mode event-name args))
             (wl:wl-proxy-hooks mode))))
    (:current-mode (display-head-put head :current-mode (first args)))
    (:position
     (display-head-put head :position (cons (first args) (second args))))
    (:scale (display-head-put head :scale (first args)))
    (:finished (remhash head *display-heads*))))

(defun display-mode-event (mode event-name &rest args)
  (case event-name
    (:size
     (display-mode-put mode :width (first args))
     (display-mode-put mode :height (second args)))
    (:refresh (display-mode-put mode :refresh (first args)))
    (:finished (remhash mode *display-modes*))))

(defun display-head-put (head key value)
  "Threaded plist update — setf-getf rebinds locals instead of
mutating the list (the decorations.lisp lesson): copy, set, write
the result back to the hash."
  (let ((state (copy-list (gethash head *display-heads*))))
    (setf (getf state key) value)
    (setf (gethash head *display-heads*) state)))

(defun display-mode-put (mode key value)
  (let ((state (copy-list (gethash mode *display-modes*))))
    (setf (getf state key) value)
    (setf (gethash mode *display-modes*) state)))

(defun display-heads ()
  "All tracked heads as state plists, ordered by name."
  (sort (loop for state being the hash-values of *display-heads*
              when (getf state :name)
              collect state)
        #'string< :key (lambda (state) (getf state :name))))

(defun display-mode-label (mode)
  "Human label for a mode proxy: \"1920x1080 @ 60Hz\"."
  (let ((info (and mode (gethash mode *display-modes*))))
    (if (and info (getf info :width) (getf info :refresh))
        (format nil "~Ax~A @ ~DHz"
                (getf info :width) (getf info :height)
                (round (/ (getf info :refresh) 1000)))
        "?")))

(defun display-apply (target &key mode scale)
  "Send a configuration: every tracked head is restated at its
current state (the protocol disables unmentioned heads — a partial
config blanks the other outputs); TARGET gets the MODE/SCALE
override. Returns T when the apply was sent."
  (unless *output-manager*
    (message "display: no output manager")
    (return-from display-apply nil))
  (let ((config (rwm:zwlr-output-manager-v1.create-configuration
                  *output-manager* *display-serial*)))
    (push (lambda (event-name &rest args)
            (declare (ignore args))
            (case event-name
              (:succeeded (message "display: applied"))
              (:failed (message "display: configuration rejected"))
              (:cancelled (message "display: cancelled"))))
          (wl:wl-proxy-hooks config))
    (dolist (state (display-heads))
      (let* ((head (getf state :head))
             (targetp (eq head target))
             (conf-head (rwm:zwlr-output-configuration-v1.enable-head
                          config head)))
        (unless (and targetp mode)
          (let ((current (getf state :current-mode)))
            (when current
              (rwm:zwlr-output-configuration-head-v1.set-mode
                conf-head current))))
        (unless (and targetp scale)
          (let ((current (getf state :scale)))
            (when current
              (rwm:zwlr-output-configuration-head-v1.set-scale
                conf-head current))))
        (let ((pos (getf state :position)))
          (when pos
            (rwm:zwlr-output-configuration-head-v1.set-position
              conf-head (car pos) (cdr pos))))
        (when targetp
          (when mode
            (rwm:zwlr-output-configuration-head-v1.set-mode
              conf-head mode))
          (when scale
            (rwm:zwlr-output-configuration-head-v1.set-scale
              conf-head scale)))))
    (rwm:zwlr-output-configuration-v1.apply config)
    t))
