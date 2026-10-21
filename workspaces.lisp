;;; Workspaces for Rhizome
;;;
;;; StumpWM-style workspaces (groups): each output has N workspaces
;;; (configurable via *workspace-count*), windows are stamped with the
;;; workspace they were opened on, and switching workspaces changes
;;; which windows the tiling engine lays out (windows on other
;;; workspaces are hidden by the render pass, like monocle overflow).
;;;
;;; The current workspace per output is exported to the bar via the
;;; WM snapshot file (see update-wm-snapshot in wm-protocol.lisp).

(in-package #:rhizome)

(defparameter *workspace-count* 9
  "Number of workspaces per output.")

(defparameter *workspace-layouts* (make-hash-table :test 'equal)
  "Frame-key → the workspace's tiling layout name (master-stack,
even, monocle). Per-workspace: switching layouts no longer
clobbers every workspace on the output. DEFVAR: reloading must
not forget layouts.")

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

(defun workspace-layout (output workspace)
  "The workspace's tiling layout name: the per-workspace override
if one was set, else the configured default
((:workspaces :default-layout), :master-stack). The output's
current layout is only a mirror of whatever workspace is showing —
using it as a fallback would let one workspace's layout leak into
every unconfigured one."
  (or (gethash (frame-key output workspace) *workspace-layouts*)
      (config-getf '(:workspaces) :default-layout :default :master-stack)))

(defun (setf workspace-layout) (layout output workspace)
  (setf (gethash (frame-key output workspace) *workspace-layouts*)
        layout))

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
  (let ((old-workspace (gethash output *output-workspace*)))
    (setf (gethash output *output-workspace*) workspace)
    ;; The target workspace's layout (or its scroll ribbon) is what
    ;; the output now shows — layouts are per-workspace
    (setf (output-current-layout output)
          (if (scroll-workspace-p output workspace)
              :scroll
              (workspace-layout output workspace)))
    ;; Slide-in: the new workspace's windows glide in from the edge
    ;; (direction = old → new). Pure rendering polish — the layout
    ;; math is untouched; the animation decays the offset to zero.
    (when (and (animations-enabled-p) old-workspace)
      (let ((dir (if (> workspace old-workspace) 1 -1))
            (slide (output-width output)))
        (dolist (win (windows-on-workspace output workspace))
          (unless (or (window-minimized-p win) (window-floating-p win))
            (animation-start win :dx (- (* dir slide)) :duration 160)))))
    (format t "~&; Rhizome: switched to workspace ~D~%" workspace)
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
    ;; Warp the pointer to the output center so it lands on the new
    ;; workspace's windows (river_seat_v1.pointer_warp, protocol >= 3).
    ;; pointer_warp MODIFIES WINDOW MANAGEMENT STATE — river 0.5
    ;; terminates the client for sending it outside a manage
    ;; sequence (the "invalid modification" death), so the request
    ;; is queued and flushed from the manage pass, like focus.
    ;; Config: (:pointer (:warp-on-switch t)).
    (when (config-getf '(:pointer) :warp-on-switch :default t)
      (let ((cx (+ (output-x output) (floor (output-width output) 2)))
            (cy (+ (output-y output) (floor (output-height output) 2))))
        (push (list :warp cx cy)
              *pending-pointer-warps*)))
    (request-manage-dirty)))

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
  (format t "~&; Rhizome: moved window to workspace ~D~%" workspace)
  (when output
    (switch-workspace workspace output)))

(defun move-focused-window-to-workspace (workspace)
  "Move the focused window to WORKSPACE and follow it."
  (let* ((seats (all-seats))
         (focused (and seats (seat-focused-window (first seats)))))
    (when focused
      (move-window-to-workspace focused workspace
                                (window-output focused)))))

(defun move-marked-to-workspace (workspace)
  "Move every marked window to WORKSPACE and clear the marks
(StumpWM's gmove-marked)."
  (let ((marked (marked-windows)))
    (if (null marked)
        (message "no marked windows")
        (progn
          (dolist (win marked)
            (when (gethash (window-proxy win) *windows*)
              (move-window-to-workspace win workspace)))
          (clear-window-marks)
          (message "~D moved to workspace ~D"
                   (length marked) workspace)))))

;;; --- Window rules ---
;;;
;;; Configure applications to open on specific workspaces, optionally
;;; switching the output's layout and following the window there.
;;; Rules live in the config and match a regex against the window's
;;; app-id (default) or title:
;;;
;;;   (rhizome:configure
;;;     '(:window-rules ((:match "vivaldi" :workspace 2 :follow t)
;;;                      (:match "emacs" :workspace 3 :layout :monocle)
;;;                      (:match "gimp" :float t)
;;;                      (:match "steam" :output 2))))

(defun window-rule-matches-p (win rule)
  "Whether WIN matches RULE's :wse expression (if given), else its
:match regex."
  (let ((wse (getf rule :wse)))
    (if wse
        (wse-matches-p win wse)
        (let* ((pattern (getf rule :match))
               (key (getf rule :key :app-id))
               (candidates
                 (case key
                   (:app-id (list (window-app-id win)))
                   (:title (list (window-title win)))
                   (:identifier (list (window-identifier win)))
                   (:any (list (window-app-id win) (window-title win)))
                   (t (list (window-app-id win))))))
              (and pattern
                   (some (lambda (candidate)
                           (and candidate
                                (cl-ppcre:scan (concatenate 'string
                                                            "(?i)" pattern)
                                               candidate)))
                         candidates))))))

(defun all-outputs-sorted ()
  "Outputs in stable left-to-right, top-to-bottom order."
  (sort (all-outputs)
        #'<
        :key (lambda (o) (+ (* (output-y o) 65536) (output-x o)))))

(defun apply-window-rules (win)
  "Apply the first matching window rule to WIN: assign its workspace
or output, optionally switch the output's layout and/or follow the
window. Returns the matched rule, if any."
  (dolist (rule (config-get '(:window-rules)))
    (when (window-rule-matches-p win rule)
      (let ((workspace (getf rule :workspace))
            (layout (getf rule :layout))
            (follow (getf rule :follow))
            (float (getf rule :float))
            (output-index (getf rule :output)))
        (when float
          ;; GIMP-style: never in the tiling (see floating.lisp)
          (float-window win))
        (when (and output-index
                   (nth (1- output-index) (all-outputs-sorted)))
          ;; Place on a specific output (1-based index)
          (setf (window-output win)
                (nth (1- output-index) (all-outputs-sorted))))
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
        (format t "~&; Rhizome: window rule ~S applied to ~A~%"
                rule (or (window-app-id win) (window-title win) win))
        (return rule)))))

(defun find-layout-symbol (layout)
  "Normalize a rule's :layout value (keyword or string) to a layout name."
  (typecase layout
    (string (intern (string-upcase layout) :keyword))
    (t layout)))

;;; --- Window state persistence (identifier-based) ---
;;;
;;; river v4 gives each window a stable identifier, sent once at
;;; creation. Rhizome remembers where such windows were placed
;;; (workspace, floating state) and restores the placement after a
;;; WM restart or relogin — the "rules survive restarts" mechanism.
;;; Config rules still win: restore runs first, rules second.

(defparameter *window-state* (make-hash-table :test 'equal)
  "Identifier → plist (:workspace N :float T) for remembered
windows. DEFVAR: reloading this file must not forget placements.")

(defparameter *window-state-path*
  (merge-pathnames #P".local/share/rhizome/window-state.sexp"
                   (user-homedir-pathname))
  "Where the remembered window placements live. LET-bindable for
tests.")

(defun window-state-changed-p ()
  "Whether (:window-state :enabled) is on."
  (config-getf '(:window-state) :enabled :default t))

(defun save-window-state ()
  "Persist remembered placements (atomic write, like the snapshot)."
  (when (window-state-changed-p)
    (handler-case
        (let ((entries
                (loop for id being the hash-keys of *window-state*
                      collect (cons id (gethash id *window-state*)))))
          (ensure-directories-exist *window-state-path*)
          (with-open-file (s *window-state-path*
                             :direction :output :if-exists :supersede)
            (write entries :stream s)))
      (error (e)
        (format t "~&; Rhizome: window state save failed: ~A~%" e)))))

(defun load-window-state ()
  "Load remembered placements. Missing file = nothing remembered."
  (when (window-state-changed-p)
    (handler-case
        (when (probe-file *window-state-path*)
          (clrhash *window-state*)
          (with-open-file (s *window-state-path*)
            (dolist (entry (read s))
              (setf (gethash (car entry) *window-state*) (cdr entry)))))
      (error (e)
        (format t "~&; Rhizome: window state load failed: ~A~%" e)))))

(defun remember-window-state (win)
  "Record WIN's placement under its identifier (v4 windows only).
Called when the window closes — one hook point captures both the
final workspace and the floating state."
  (when (and (window-identifier win) (window-state-changed-p))
    (setf (gethash (window-identifier win) *window-state*)
          (list :workspace (window-workspace win)
                :float (window-floating-p win)))
    (save-window-state)))

(defun restore-window-state (win)
  "Restore WIN's remembered placement, if any. Runs before config
rules so explicit rules override what was remembered. The output
is stamped too: without it the manage pass treats the window as
unassigned and re-adopts it onto the CURRENT workspace — the
takeover/relogin clustering bug."
  (when (and (window-identifier win) (window-state-changed-p))
    (let ((state (gethash (window-identifier win) *window-state*)))
      (when state
        (when (and (getf state :float)
                   (not (window-floating-p win)))
          (float-window win :animate nil))
        (when (getf state :workspace)
          (assign-window-workspace win (getf state :workspace))
          (setf (window-minimized-p win) nil)
          ;; Stamp the output so the manage pass adopts the window
          ;; onto ITS remembered workspace, not the current one
          (unless (window-output win)
            (setf (window-output win) (first (all-outputs)))))
        (request-manage-dirty)
        (format t "~&; Rhizome: restored placement for ~A (workspace ~A)~%"
                (window-identifier win) (getf state :workspace))))))
