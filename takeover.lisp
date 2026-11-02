;;; WM hot-swap: takeover state for the river stop/finished handoff
;;;
;;; detach-wm (session.lisp) completes river's stop/finished
;;; handshake and exits; a supervisor loop in river-init relaunches
;;; rhizome; the fresh WM binds (river re-registers every output,
;;; seat, and window with their stable identifiers) and THIS module
;;; puts the session back: per-window placements, per-output
;;; current workspaces, per-workspace frame trees, and focus.
;;;
;;; Numbers are TRANSLATION KEYS, not identities: river may
;;; re-register windows in a different order after the swap, so
;;; the new WM assigns different numbers — trees are rewritten
;;; through the identifier → old-number → new-number map before
;;; restore. The takeover file is consumed (deleted) once applied:
;;; a later fresh boot must not resurrect stale session state.

(in-package #:rhizome)

(defparameter *takeover-state-path*
  (merge-pathnames #P".local/share/rhizome/takeover.sexp"
                   (user-homedir-pathname))
  "Where the hot-swap session state lives. LET-bindable for tests.")

(defvar *takeover-state* nil
  "The loaded takeover state, or NIL (fresh boot / consumed).")

(defvar *takeover-manage-passes* 0
  "Manage passes seen while waiting for takeover windows to
settle — bounds the wait when some windows never return.")

;;; --- Writing (the outgoing WM, at detach) ---

(defun frame-tree-has-windows (tree)
  "Whether TREE's frames hold any window at all (empty workspaces
are not worth serializing)."
  (labels ((walk (frame)
             (if (frame-first-child frame)
                 (or (walk (frame-first-child frame))
                     (walk (frame-second-child frame)))
                 (and (frame-windows frame) t))))
    (and tree (walk tree))))

(defun takeover-window-entry (win)
  "WIN's takeover entry, keyed by its stable identifier."
  (cons (window-identifier win)
        (list :workspace (window-workspace win)
              :float (window-floating-p win)
              :float-x (window-float-x win)
              :float-y (window-float-y win)
              :float-width (window-float-width win)
              :float-height (window-float-height win)
              :minimized (window-minimized-p win)
              :number (window-number win))))

(defun write-takeover-state ()
  "Serialize the whole session for the successor WM. Called from
detach-wm BEFORE the stop request — windows still exist, so
geometry and trees are live values."
  (let ((state
          (list
            :outputs
            (loop for out in (all-outputs-sorted)
                  collect (cons (output-wl-output-name out)
                                (gethash out *output-workspace*)))
            :windows
            (loop for win in (all-windows)
                  when (window-identifier win)
                    collect (takeover-window-entry win))
            :focused
            (let ((f (find-if #'window-focused-p (all-windows))))
              (and f (window-identifier f)))
            :trees
            (loop for out in (all-outputs-sorted)
                  append (loop for ws from 1 to *workspace-count*
                                for tree = (ignore-errors
                                             (frame-tree-for out ws))
                                when (and tree
                                          (frame-tree-has-windows tree))
                                  collect (cons (cons (output-wl-output-name out)
                                                      ws)
                                                (frame-tree-to-sexp tree)))))))
    (handler-case
        (progn
          (ensure-directories-exist *takeover-state-path*)
          ;; atomic: write beside, rename over (SBCL's rename-file
          ;; does not overwrite — delete the old target first)
          (let ((tmp (merge-pathnames "takeover.sexp.tmp"
                                      *takeover-state-path*)))
            (with-open-file (s tmp :direction :output
                               :if-exists :supersede)
              (write state :stream s :readably t))
            (when (probe-file *takeover-state-path*)
              (delete-file *takeover-state-path*))
            (rename-file tmp *takeover-state-path*))
          (format t "~&; Rhizome: takeover state written (~D windows)~%"
                  (length (getf state :windows))))
      (error (e)
        (format t "~&; Rhizome: takeover state write failed: ~A~%" e)))))

;;; --- Loading (the incoming WM, at boot) ---

(defun load-takeover-state ()
  "Read the takeover file at boot. Missing or unreadable means a
fresh boot (or a crash) — normal behavior, no takeover."
  (setf *takeover-state* nil
        *takeover-manage-passes* 0)
  (handler-case
      (when (probe-file *takeover-state-path*)
        (with-open-file (s *takeover-state-path*)
          (setf *takeover-state* (read s)))
        (format t "~&; Rhizome: takeover state found (~D windows) — hot swap~%"
                (length (getf *takeover-state* :windows))))
    (error (e)
      (format t "~&; Rhizome: takeover state unreadable: ~A~%" e)
      (setf *takeover-state* nil))))

(defun consume-takeover-state ()
  "Forget the takeover entirely — applied, or too old to matter.
The file is deleted so a later fresh boot sees nothing."
  (setf *takeover-state* nil
        *takeover-manage-passes* 0)
  (ignore-errors (delete-file *takeover-state-path*)))

(defun find-output-by-wl-name (name)
  "The output whose wl_output name is NAME (the stable key across
a swap; proxies are not)."
  (find name (loop for out being the hash-values of *outputs*
                   collect out)
        :key #'output-wl-output-name :test #'equal))

(defun takeover-restore-output-workspaces ()
  "Boot-time: put every output back on the workspace it was
showing, before the first layout renders. Missing outputs (a
monitor unplugged mid-swap) are skipped."
  (when *takeover-state*
    (dolist (pair (getf *takeover-state* :outputs))
      (let ((out (find-output-by-wl-name (car pair))))
        (when (and out (cdr pair)
                   (<= 1 (cdr pair) *workspace-count*))
          (setf (gethash out *output-workspace*) (cdr pair))
          (format t "~&; Rhizome: takeover: output ~A → workspace ~D~%"
                  (car pair) (cdr pair)))))))

(defun restore-takeover-placement (win)
  "Per-window restore, called from the :identifier handler AFTER
the remembered placements and config rules (the live session
arrangement wins over both). Applies workspace, floating
geometry, and minimized state."
  (when (and *takeover-state* (window-identifier win))
    (let* ((entry (cdr (assoc (window-identifier win)
                               (getf *takeover-state* :windows)
                               :test #'equal))))
      (when entry
        (handler-case
            (progn
              (when (and (getf entry :workspace)
                         (<= 1 (getf entry :workspace) *workspace-count*))
                (assign-window-workspace win (getf entry :workspace)))
              ;; Stamp the output so the manage pass adopts the
              ;; window onto its remembered workspace, not the
              ;; current one (the takeover clustering bug)
              (unless (window-output win)
                (setf (window-output win) (first (all-outputs))))
              (when (getf entry :minimized)
                (setf (window-minimized-p win) t))
              (when (getf entry :float)
                (unless (window-floating-p win)
                  (float-window win :animate nil))
                (when (getf entry :float-x)
                  (setf (window-float-x win) (getf entry :float-x)))
                (when (getf entry :float-y)
                  (setf (window-float-y win) (getf entry :float-y)))
                (when (getf entry :float-width)
                  (setf (window-float-width win) (getf entry :float-width)))
                (when (getf entry :float-height)
                  (setf (window-float-height win) (getf entry :float-height))))
              (request-manage-dirty)
              (format t "~&; Rhizome: takeover: ~A → workspace ~A~%"
                      (window-identifier win) (getf entry :workspace)))
          (error (e)
            (format t "~&; Rhizome: takeover placement failed: ~A~%" e)))))))

;;; --- Settle detection and tree/focus application ---

(defun takeover-settled-p ()
  "Whether every takeover window has re-registered (or enough
manage passes passed — windows closed during the gap never
return)."
  (incf *takeover-manage-passes*)
  (let ((ids (loop for pair in (getf *takeover-state* :windows)
                   collect (car pair))))
    (or (> *takeover-manage-passes* 30)
        (every (lambda (id)
                 (find id (loop for w being the hash-values of *windows*
                                when (window-identifier w)
                                  collect (window-identifier w))
                       :test #'equal))
               ids))))

(defun translate-tree-numbers (sexp number-map)
  "Rewrite a dump sexp's window numbers through NUMBER-MAP
(old-number → new-number). Windows the map doesn't know (closed
during the gap) are dropped from their frames; restore skips
them anyway, but dropping keeps the trees honest."
  (flet ((map-nums (nums)
           (loop for n in nums
                 for new = (gethash n number-map)
                 when new collect new)))
    (if (eq (first sexp) :frame)
        (list* :frame (second sexp)
               (map-nums (cddr sexp)))
        (list :split (second sexp) (third sexp)
              (translate-tree-numbers (fourth sexp) number-map)
              (translate-tree-numbers (fifth sexp) number-map)))))

(defun apply-takeover-trees ()
  "The settled moment: rebuild every workspace's frame tree and
restore focus, then consume the takeover state."
  (when *takeover-state*
    (let ((number-map (make-hash-table)))
      ;; old-number → new-number, through identifiers
      (loop for pair in (getf *takeover-state* :windows)
            for id = (car pair)
            for old-number = (getf (cdr pair) :number)
            for win = (find id (loop for w being the hash-values of *windows*
                                     when (window-identifier w)
                                       collect w)
                             :key #'window-identifier :test #'equal)
            when (and old-number win)
              do (setf (gethash old-number number-map)
                       (window-number win)))
      (dolist (tree-entry (getf *takeover-state* :trees))
        (let* ((key (car tree-entry))
               (out (find-output-by-wl-name (car key)))
               (ws (cdr key)))
          (when (and out ws)
            (handler-case
                (progn
                  (restore-workspace-from-sexp
                    out ws (translate-tree-numbers (cdr tree-entry)
                                                   number-map))
                  (format t "~&; Rhizome: takeover: tree restored ~A ws ~D~%"
                          (car key) ws))
              (error (e)
                (format t "~&; Rhizome: takeover tree restore failed: ~A~%"
                        e))))))
      ;; Focus the remembered window, if it returned
      (let ((focused-id (getf *takeover-state* :focused)))
        (let ((win (find focused-id
                         (loop for w being the hash-values of *windows*
                               when (window-identifier w)
                                 collect w)
                         :key #'window-identifier :test #'equal)))
          (when win
            (dolist (seat (all-seats))
              (focus-window win seat)))))
      (request-manage-dirty)
      (consume-takeover-state)
      (format t "~&; Rhizome: takeover complete~%"))))
