;;; Workspaces for Stumpway
;;;
;;; StumpWM-style workspaces (groups): each output has N workspaces
;;; (configurable via *workspace-count*), windows are stamped with the
;;; workspace they were opened on, and switching workspaces changes
;;; which windows the tiling engine lays out (windows on other
;;; workspaces are hidden by the render pass, like monocle overflow).
;;;
;;; The current workspace per output is exported to the bar via the
;;; WM snapshot file (see update-wm-snapshot in wm-protocol.lisp).

(in-package #:stumpway)

(defparameter *workspace-count* 9
  "Number of workspaces per output.")

(defvar *output-workspace* (make-hash-table)
  "Maps outputs to their current workspace index (1..*workspace-count*).")

(defvar *window-workspace* (make-hash-table)
  "Maps windows to their workspace index.")

(defvar *minimized-windows* (make-hash-table)
  "Windows the user minimized. Distinguished from windows hidden
because they are on another workspace.")

;;; --- Accessors ---

(defun current-workspace (output)
  "The current workspace index of OUTPUT (1-based)."
  (let ((ws (gethash output *output-workspace*)))
    (if (and ws (<= 1 ws *workspace-count*))
        ws
        1)))

(defun window-workspace (win)
  "The workspace index of WIN, or NIL if unassigned."
  (gethash win *window-workspace*))

(defun window-minimized-p (win)
  "Whether WIN was minimized by the user."
  (gethash win *minimized-windows*))

(defun (setf window-minimized-p) (value win)
  (setf (gethash win *minimized-windows*) value))

(defun windows-on-workspace (output workspace)
  "Windows on OUTPUT assigned to WORKSPACE (including hidden ones)."
  (remove-if (lambda (win)
               (or (not (eq (window-output win) output))
                   (not (eql (window-workspace win) workspace))))
             (all-windows)))

;;; --- Workspace operations ---

(defun assign-window-workspace (win workspace)
  "Stamp WIN as belonging to WORKSPACE."
  (setf (gethash win *window-workspace*) workspace))

(defun switch-workspace (workspace &optional (output (first (all-outputs))))
  "Switch OUTPUT to WORKSPACE (1-based), focusing its newest window."
  (unless (and (<= 1 workspace *workspace-count*) output)
    (return-from switch-workspace))
  (setf (gethash output *output-workspace*) workspace)
  (format t "~&; Stumpway: switched to workspace ~D~%" workspace)
  (message "workspace ~D" workspace)
  (run-hook :workspace-changed workspace output)
  ;; Focus the most recently registered window on the new workspace
  (let* ((wins (windows-on-workspace output workspace))
         (newest (if (and *last-registered-window*
                          (member *last-registered-window* wins))
                     *last-registered-window*
                     (first wins))))
    (when newest
      (dolist (seat (all-seats))
        (focus-window newest seat))))
  ;; The explicit focus stands; don't let a pending auto-focus override it
  (setf *pending-autofocus* nil)
  (request-manage-dirty))

(defun move-window-to-workspace (win workspace &optional output)
  "Move WIN to WORKSPACE (1-based), switching to it if OUTPUT is given."
  (unless (<= 1 workspace *workspace-count*)
    (return-from move-window-to-workspace))
  ;; Leave the old workspace's frame tree; adopted into the target
  ;; workspace's current frame on its next manage pass
  (frame-remove-window win)
  (assign-window-workspace win workspace)
  ;; Moving a minimized window restores it
  (setf (window-minimized-p win) nil)
  (format t "~&; Stumpway: moved window to workspace ~D~%" workspace)
  (when output
    (switch-workspace workspace output)))

(defun move-focused-window-to-workspace (workspace)
  "Move the focused window to WORKSPACE and follow it."
  (let* ((seats (all-seats))
         (focused (and seats (seat-focused-window (first seats)))))
    (when focused
      (move-window-to-workspace focused workspace
                                (window-output focused)))))

;;; --- Window rules ---
;;;
;;; Configure applications to open on specific workspaces, optionally
;;; switching the output's layout and following the window there.
;;; Rules live in the config and match a regex against the window's
;;; app-id (default) or title:
;;;
;;;   (stumpway:configure
;;;     '(:window-rules ((:match "vivaldi" :workspace 2 :follow t)
;;;                      (:match "emacs" :workspace 3 :layout :monocle)
;;;                      (:match "gimp" :float t))))

(defun window-rule-matches-p (win rule)
  "Whether WIN matches RULE's :match regex."
  (let* ((pattern (getf rule :match))
         (key (getf rule :key :app-id))
         (candidates
           (case key
             (:app-id (list (window-app-id win)))
             (:title (list (window-title win)))
             (:any (list (window-app-id win) (window-title win)))
             (t (list (window-app-id win))))))
    (and pattern
         (some (lambda (candidate)
                 (and candidate
                      (cl-ppcre:scan (concatenate 'string
                                                  "(?i)" pattern)
                                     candidate)))
               candidates))))

(defun apply-window-rules (win)
  "Apply the first matching window rule to WIN: assign its workspace,
optionally switch the output's layout and/or follow the window.
Returns the matched rule, if any."
  (dolist (rule (config-get '(:window-rules)))
    (when (window-rule-matches-p win rule)
      (let ((workspace (getf rule :workspace))
            (layout (getf rule :layout))
            (follow (getf rule :follow))
            (float (getf rule :float)))
        (when float
          ;; GIMP-style: never in the tiling (see floating.lisp)
          (float-window win))
        (when workspace
          (assign-window-workspace win workspace)
          (setf (window-minimized-p win) nil))
        (when layout
          (let ((outs (all-outputs)))
            (when (and outs (gethash (find-layout-symbol layout) *layouts*))
              (setf (output-current-layout (first outs))
                    (find-layout-symbol layout)))))
        (when (and follow workspace)
          (let ((outs (all-outputs)))
            (when outs
              (switch-workspace workspace (first outs)))))
        (request-manage-dirty)
        (format t "~&; Stumpway: window rule ~S applied to ~A~%"
                rule (or (window-app-id win) (window-title win) win))
        (return rule)))))

(defun find-layout-symbol (layout)
  "Normalize a rule's :layout value (keyword or string) to a layout name."
  (typecase layout
    (string (intern (string-upcase layout) :keyword))
    (t layout)))
