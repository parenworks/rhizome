;;; Window management for Rhizome
;;;
;;; Each river_window_v1 is wrapped in a rhizome-window CLOS instance
;;; that tracks the window's state (dimensions, title, app_id, focus,
;;; tiling frame assignment, etc.)

(in-package #:rhizome)

(defclass rhizome-window ()
  ((proxy       :initarg :proxy       :accessor window-proxy)
   (node        :initarg :node        :accessor window-node :initform nil)
   (title       :initarg :title       :accessor window-title :initform nil)
   (app-id      :initarg :app-id      :accessor window-app-id :initform nil)
   (width       :initarg :width       :accessor window-width :initform 0)
   (height      :initarg :height      :accessor window-height :initform 0)
   (x           :initarg :x           :accessor window-x :initform 0)
   (y           :initarg :y           :accessor window-y :initform 0)
   (output      :initarg :output      :accessor window-output :initform nil)
   (frame       :initarg :frame       :accessor window-frame :initform nil)
   (fullscreen  :initarg :fullscreen  :accessor window-fullscreen-p :initform nil)
   (hidden      :initarg :hidden      :accessor window-hidden-p :initform nil)
   (focused     :initarg :focused     :accessor window-focused-p :initform nil)
   (floating    :initarg :floating    :accessor window-floating-p :initform nil)
   (float-x     :initarg :float-x     :accessor window-float-x :initform nil)
   (float-y     :initarg :float-y     :accessor window-float-y :initform nil)
   (float-width :initarg :float-width :accessor window-float-width :initform nil)
   (float-height :initarg :float-height :accessor window-float-height :initform nil)
   (hint-min-width  :initarg :hint-min-width  :accessor window-hint-min-width :initform nil)
   (hint-min-height :initarg :hint-min-height :accessor window-hint-min-height :initform nil)
   (hint-max-width  :initarg :hint-max-width  :accessor window-hint-max-width :initform nil)
   (hint-max-height :initarg :hint-max-height :accessor window-hint-max-height :initform nil))
  (:documentation "Represents a managed window in the river compositor."))

(defvar *last-registered-window* nil
  "The most recently registered window, for auto-focus.")

;;; --- Window numbering (StumpWM-style 1..9, then 0) ---

(defvar *window-number-map* (make-hash-table)
  "Maps windows to their numbers (window-number / C-t ' prompt).")
(defvar *window-number-overflow* 10
  "Numbers handed out when 1..9 and 0 are all taken.")

(defun window-number (win)
  "WIN's number, or NIL."
  (gethash win *window-number-map*))

(defun assign-window-number (win)
  "Give WIN the lowest free number (1..9, then 0), or a fresh
overflow number beyond 10 when they're all taken."
  (let* ((taken (loop for n being the hash-values of *window-number-map*
                      collect n))
         (n (loop for c in '(1 2 3 4 5 6 7 8 9 0)
                  thereis (and (not (member c taken)) c))))
    (setf (gethash win *window-number-map*)
          (or n (prog1 *window-number-overflow*
                  (incf *window-number-overflow*))))))

(defun release-window-number (win)
  "Forget WIN's number (window closed)."
  (remhash win *window-number-map*))

(defun find-window-by-number (n &optional (output (first (all-outputs)))
                                      (workspace (current-workspace output)))
  "The window with number N on OUTPUT's WORKSPACE, if any."
  (find-if (lambda (win)
             (and (eql (window-number win) n)
                  (eq (window-output win) output)
                  (eql (window-workspace win) workspace)))
           (all-windows)))

(defun renumber-windows (&optional (output (first (all-outputs)))
                             (workspace (current-workspace output)))
  "Renumber the workspace's windows from 1 in number order
(StumpWM's renumber)."
  (let* ((wins (sort (windows-on-workspace output workspace)
                     #'<
                     :key (lambda (w) (or (window-number w) 999)))))
    (loop for win in wins
          for n from 1
          do (setf (gethash win *window-number-map*) n))))

(defun repack-window-numbers (&optional (output (first (all-outputs)))
                               (workspace (current-workspace output)))
  "Fill gaps in the workspace's window numbers, keeping relative
order (StumpWM's repack-window-numbers)."
  (renumber-windows output workspace))

(defun select-window-by-number (n)
  "Focus the window with number N on the current workspace, moving
frame focus there with it (StumpWM's select-window-by-number)."
  (let* ((out (first (all-outputs)))
         (ws (current-workspace out))
         (win (find-window-by-number n out ws)))
    (cond ((null win)
           (message "no window ~D" n))
          ((window-floating-p win)
           (dolist (seat (all-seats)) (focus-window win seat))
           (request-manage-dirty))
          (t
           (when (window-frame win)
             (setf (gethash (frame-key out ws) *current-frames*)
                   (window-frame win)))
           (dolist (seat (all-seats)) (focus-window win seat))
           (request-manage-dirty)))))

;;; --- Window marks (StumpWM: mark / gmove-marked) ---

(defvar *marked-windows* (make-hash-table)
  "Windows tagged with C-t #; commands can act on all of them at once
(move-marked, clear-window-marks).")

(defun window-marked-p (win)
  "Whether WIN carries a mark."
  (gethash win *marked-windows*))

(defun toggle-window-mark (win)
  "Tag (or untag) WIN so mark commands act on it (StumpWM's mark,
C-t #)."
  (if (gethash win *marked-windows*)
      (progn
        (remhash win *marked-windows*)
        (message "unmarked: ~A"
                 (or (window-title win) (window-app-id win) "window")))
      (progn
        (setf (gethash win *marked-windows*) t)
        (message "marked: ~A"
                 (or (window-title win) (window-app-id win) "window")))))

(defun toggle-mark-focused ()
  "Mark/unmark the focused window."
  (let ((win (find-if #'window-focused-p (all-windows))))
    (if win
        (toggle-window-mark win)
        (message "no focused window"))))

(defun clear-window-marks ()
  "Remove every window mark."
  (clrhash *marked-windows*)
  (message "marks cleared"))

(defun marked-windows ()
  "All marked windows."
  (loop for win being the hash-keys of *marked-windows* collect win))

(defun release-window-mark (win)
  "Drop WIN's mark (window closed)."
  (remhash win *marked-windows*))

;;; --- Fullscreen (WM-initiated) ---

(defvar *pending-fullscreen-requests* nil
  "Windows whose fullscreen request is waiting for the next manage
sequence (river_window_v1.fullscreen is a manage-sequence request).")

(defun flush-fullscreen-requests ()
  "Send pending fullscreen requests (called during a manage
sequence)."
  (dolist (win *pending-fullscreen-requests*)
    (handler-case
        (if (window-fullscreen-p win)
            (rwm:river-window-v1.exit-fullscreen (window-proxy win))
            (rwm:river-window-v1.fullscreen (window-proxy win)))
      (error (e)
        (format t "~&; Rhizome: fullscreen request failed: ~A~%" e))))
  (setf *pending-fullscreen-requests* nil))

(defun toggle-fullscreen (win)
  "Toggle WIN's fullscreen (StumpWM's C-t F11). The fullscreen
request itself is flushed by the manage pass."
  (when win
    (setf (window-fullscreen-p win) (not (window-fullscreen-p win)))
    (pushnew win *pending-fullscreen-requests*)
    (request-manage-dirty)))

(defun register-window (proxy)
  "Register a new river_window_v1 proxy and set up event handlers."
  (let ((win (make-instance 'rhizome-window :proxy proxy)))
    (setf (gethash proxy *windows*) win
          *last-registered-window* win
          *pending-autofocus* win)
    (assign-window-number win)
    (push (lambda (event-name &rest args)
            (apply #'handle-window-event win event-name args))
          (wl:wl-proxy-hooks proxy))
    (run-hook :window-created win)
    (format t "~&; Rhizome: new window registered~%")))

(defun handle-window-event (win event-name &rest args)
  "Dispatch river_window_v1 events for WIN."
  (handler-case
      (case event-name
        (:dimensions
         (destructuring-bind (width height) args
           (setf (window-width win) width
                 (window-height win) height)
           ;; A fresh floating window has no float size yet: adopt the
           ;; dimensions the window chose for itself (e.g. a GIMP
           ;; toolbox) so subsequent manage passes re-propose them.
           (when (and (window-floating-p win)
                      (not (window-float-width win)))
             (setf (window-float-width win) width
                   (window-float-height win) height))))

        (:dimensions-hint
         (destructuring-bind (min-w min-h max-w max-h) args
           (setf (window-hint-min-width win) min-w
                 (window-hint-min-height win) min-h
                 (window-hint-max-width win) max-w
                 (window-hint-max-height win) max-h)))

        (:title
         (destructuring-bind (title) args
           (setf (window-title win) title)
           (format t "~&; Rhizome: window title: ~A~%" title)
           ;; Window rules may match on title
           (apply-window-rules win)))

        (:app-id
         (destructuring-bind (app-id) args
           (setf (window-app-id win) app-id)
           ;; Window rules may match on app-id
           (apply-window-rules win)))

        (:closed
         (format t "~&; Rhizome: window closed~%")
         (run-hook :window-closed win)
         (remhash (window-proxy win) *windows*)
         (remhash win *window-workspace*)
         (remhash win *minimized-windows*)
         (release-window-number win)
         (release-window-mark win))

        (:fullscreen-requested
         (destructuring-bind (output) args
           (declare (ignore output))
           (setf (window-fullscreen-p win) t)
           (request-manage-dirty)))

        (:exit-fullscreen-requested
         (setf (window-fullscreen-p win) nil)
           (request-manage-dirty))

        (:maximize-requested
         (request-manage-dirty))

        (:unmaximize-requested
         (request-manage-dirty))

        (:minimize-requested
         (setf (window-hidden-p win) t)
         (setf (window-minimized-p win) t)
         (request-manage-dirty))

        (:pointer-move-requested
         (destructuring-bind (seat-proxy) args
           (let ((seat (gethash seat-proxy *seats*)))
             (when seat
               ;; float-op-request is redefined by floating.lisp
               (float-op-request win seat :move)))))

        (:pointer-resize-requested
         (destructuring-bind (seat-proxy edges) args
           (let ((seat (gethash seat-proxy *seats*)))
             (when seat
               (float-op-request win seat :resize edges)))))

        (t
         (format t "~&; Rhizome: unhandled window event ~A~%" event-name)))
    (error (e)
      (format t "~&; Rhizome: error handling window event ~A: ~A~%" event-name e))))

(defun all-windows ()
  "Return a list of all registered windows."
  (loop for win being the hash-values of *windows* collect win))

(defun windows-on-output (output)
  "Return windows assigned to OUTPUT, including hidden ones."
  (remove-if (lambda (win)
               (not (eq (window-output win) output)))
             (all-windows)))

(defun focus-window (win seat)
  "Give keyboard focus to WIN on SEAT."
  (when (and (window-proxy win) (seat-proxy seat))
    ;; Clear focus on all other windows
    (dolist (w (all-windows))
      (unless (eq w win)
        (setf (window-focused-p w) nil)))
    (rwm:river-seat-v1.focus-window (seat-proxy seat) (window-proxy win))
    (setf (window-focused-p win) t
          (seat-focused-window seat) win)
    (run-hook :focus-changed win)))

(defun close-window (win)
  "Request that WIN be closed."
  (rwm:river-window-v1.close (window-proxy win)))

(defun propose-window-dimensions (win width height)
  "Propose dimensions for WIN during a manage sequence."
  (rwm:river-window-v1.propose-dimensions (window-proxy win) width height))

(defun set-window-position (win x y)
  "Set the render position of WIN during a render sequence.
Requires that the window's node has been created."
  (when (window-node win)
    (rwm:river-node-v1.set-position (window-node win) x y)))

(defun set-window-borders (win edges width r g b a)
  "Set borders on WIN during a render sequence."
  (rwm:river-window-v1.set-borders (window-proxy win) edges width r g b a))

(defun show-window (win)
  "Show WIN (un-hide it) during a render sequence."
  (rwm:river-window-v1.show (window-proxy win))
  (setf (window-hidden-p win) nil))

(defun hide-window (win)
  "Hide WIN during a render sequence."
  (rwm:river-window-v1.hide (window-proxy win))
  (setf (window-hidden-p win) t))

(defun make-window-node (win)
  "Create the render list node for WIN. Must be called once per window."
  (unless (window-node win)
    (let ((node (rwm:river-window-v1.get-node (window-proxy win))))
      (setf (window-node win) node))))

(defun set-window-tiled-state (win edges)
  "Tell WIN which EDGES it is tiled on (manage-sequence request).
An empty/none list means the window is not part of the tiling, so
clients draw their own decorations — what floating windows want."
  (rwm:river-window-v1.set-tiled (window-proxy win) edges))

(defun place-node-above (node other-node)
  "Place render NODE directly above OTHER-NODE (z-order,
render-sequence request)."
  (rwm:river-node-v1.place-above node other-node))

(defun float-op-request (win seat type &optional edges)
  "Record a client-requested interactive move/resize of WIN on SEAT.
Placeholder: redefined by floating.lisp, which loads later."
  (declare (ignore win seat type edges))
  nil)
