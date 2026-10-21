;;; Niri-style scrollable tiling for Rhizome
;;;
;;; A workspace view mode: an infinite horizontal ribbon where every
;;; window owns a full-height column. New windows enter next to the
;;; focused column; focus moves with C-t comma/period (the viewport
;;; follows to keep focus visible, niri-style); MOD+comma/period
;;; scrolls the viewport alone. C-t v toggles the ribbon on the
;;; current workspace — on exit the dynamic tiler re-adopts every
;;; window, so the two modes compose over the same window set.
;;;
;;; State lives per (output . workspace) in *scroll-states*; while a
;;; workspace is scrolled its windows are frameless (the ribbon owns
;;; placement) and the dispatch in tiling.lisp routes manage/render
;;; passes here instead of the frame tree.

(in-package #:rhizome)

(defparameter *scroll-states* (make-hash-table :test 'equal)
  "Scroll-ribbon state per workspace key, or no entry when the
workspace is frame-tiled. Entry: (:columns (win ...) one window per
column, left to right; :scroll-x viewport offset into the ribbon;
:column-width uniform column width; :prev-layout the layout name to
restore when the ribbon exits).")

(defun scroll-state (output workspace)
  "The ribbon state for OUTPUT/WORKSPACE, or NIL when frame-tiled."
  (gethash (frame-key output workspace) *scroll-states*))

(defun scroll-workspace-p (output workspace)
  "T while OUTPUT/WORKSPACE shows the scroll ribbon."
  (and (scroll-state output workspace) t))

(defun scroll-content-height (output)
  "Usable column height: below the bar."
  (max 0 (- (output-height output) (bar-reserved-height))))

(defun scroll-total-width (state)
  "Total ribbon width: columns times the column width."
  (* (getf state :column-width) (length (getf state :columns))))

(defun scroll-clamp (output workspace)
  "Keep the viewport inside the ribbon: scroll-x in
[0, max(0, total-width - content-width)]."
  (let ((state (scroll-state output workspace)))
    (when state
      (setf (getf state :scroll-x)
            (min (max 0 (- (scroll-total-width state)
                           (output-width output)))
                 (max 0 (getf state :scroll-x)))))))

(defun scroll-layout-columns (output workspace)
  "Proposed rectangles (WIN x y w h) for every column, in screen
coordinates: the ribbon scrolls under the viewport."
  (let* ((state (scroll-state output workspace))
         (cw (getf state :column-width))
         (sx (getf state :scroll-x))
         (gx (output-x output))
         (gy (+ (output-y output) (bar-reserved-height)))
         (ch (scroll-content-height output)))
    (loop for win in (getf state :columns)
          for i from 0
          collect (list win
                        (+ gx (- (* i cw) sx))
                        gy
                        cw
                        ch))))

(defun scroll-ensure-focused-visible (output workspace)
  "Scroll the viewport the minimum distance that brings the focused
column fully into view (niri keeps the focused column on screen)."
  (let* ((state (scroll-state output workspace))
         (columns (getf state :columns))
         (focused (find-if #'window-focused-p columns))
         (pos (and focused (position focused columns :test #'eq))))
    (when pos
      (let* ((cw (getf state :column-width))
             (col-x (* pos cw))
             (sx (getf state :scroll-x))
             (content (output-width output)))
        (cond ((< col-x sx) (setf (getf state :scroll-x) col-x))
              ((> (+ col-x cw) (+ sx content))
               (setf (getf state :scroll-x) (- (+ col-x cw) content))))))
    (scroll-clamp output workspace)))

(defun scroll-enter (output workspace)
  "Switch OUTPUT/WORKSPACE to the scroll ribbon. Windows keep their
frame-tree order (left-to-right becomes column order) and leave the
frame tree; the dynamic tiler resumes on exit."
  (unless (scroll-state output workspace)
    (let* ((framed (remove nil
                           (mapcar #'frame-current-window
                                   (frame-leaves
                                     (frame-tree-for output workspace)))))
           (framed (remove-if (lambda (win)
                                (or (window-minimized-p win)
                                    (window-floating-p win)))
                              framed))
           (rest (remove-if (lambda (win)
                              (or (window-minimized-p win)
                                  (window-floating-p win)
                                  (member win framed :test #'eq)))
                            (windows-on-workspace output workspace)))
           (wins (append framed rest)))
      (dolist (win wins)
        (frame-remove-window win))
      (setf (gethash (frame-key output workspace) *scroll-states*)
            (list :columns wins
                  :scroll-x 0
                  :column-width (floor (output-width output) 2)
                  :prev-layout (output-current-layout output)))
      (setf (output-current-layout output) :scroll)
      (scroll-ensure-focused-visible output workspace)
      (message "scroll layout: ~D column~:P" (length wins))
      (run-hook :layout-changed output :scroll)
      (request-manage-dirty))))

(defun scroll-exit (output workspace)
  "Leave the ribbon: windows become frameless and the dynamic tiler
re-adopts them (each gets its own frame, splitting as needed)."
  (let* ((key (frame-key output workspace))
         (state (gethash key *scroll-states*)))
    (when state
      (remhash key *scroll-states*)
      (let ((prev (getf state :prev-layout)))
        (dolist (win (getf state :columns))
          (setf (window-frame win) nil))
        (setf (output-current-layout output) prev)
        (message "tiling restored")
        (run-hook :layout-changed output prev)
        (request-manage-dirty)))))

(defun scroll-exit-if-active (output)
  "Exit the ribbon on OUTPUT's current workspace, if active (so the
one-shot layout templates keep working)."
  (let ((ws (current-workspace output)))
    (when (scroll-workspace-p output ws)
      (scroll-exit output ws))))

(defun scroll-toggle (output)
  "Toggle the scroll ribbon on OUTPUT's current workspace."
  (let ((ws (current-workspace output)))
    (if (scroll-workspace-p output ws)
        (scroll-exit output ws)
        (scroll-enter output ws))))

(defun scroll-manage-decisions (output workspace wins)
  "Manage-sequence decisions for the ribbon: prune columns whose
windows died, minimized, floated or moved, and adopt frameless
arrivals as new columns right after the focused one."
  (let ((state (scroll-state output workspace)))
    (when state
      (let* ((columns (getf state :columns))
             (focused-pos (position-if #'window-focused-p columns)))
        (setf (getf state :columns)
              (remove-if (lambda (win)
                           (or (null (gethash (window-proxy win) *windows*))
                               (window-minimized-p win)
                               (window-floating-p win)
                               (not (eq (window-output win) output))
                               (not (eql (window-workspace win) workspace))))
                         columns))
        (dolist (win wins)
          (when (and (not (window-frame win))
                     (not (window-floating-p win))
                     (not (member win (getf state :columns) :test #'eq)))
            (let* ((columns (getf state :columns))
                   (focused (find-if #'window-focused-p columns))
                   (pos (or (and focused
                                 (1+ (position focused columns :test #'eq)))
                            (length columns))))
              (setf (getf state :columns)
                    (append (subseq columns 0 pos)
                            (list win)
                            (subseq columns pos))))))
        ;; Propose every column its rectangle: river sizes windows
        ;; from manage-sequence proposals, so without these the
        ;; columns keep their old frame-mode size and overlap
        (let ((ch (scroll-content-height output))
              (cw (getf state :column-width)))
          (dolist (win (getf state :columns))
            (propose-window-dimensions win cw ch)))
        (let ((columns (getf state :columns)))
          (when columns
            ;; Focus recovery: when the focused column closed, its
            ;; slot (or the last one) takes focus
            (unless (find-if #'window-focused-p columns)
              (let ((pos (min (or focused-pos 0) (1- (length columns)))))
                (dolist (seat (all-seats))
                  (focus-window (nth pos columns) seat))))))
        (scroll-ensure-focused-visible output workspace)))))

(defun scroll-render-decisions (output workspace wins)
  "Render-sequence decisions for the ribbon: place every column's
window at its scrolled rectangle, focused column highlighted.
Returns the rendered windows so the render pass can exclude them
from its hide logic."
  (declare (ignore wins))
  (let ((rendered nil))
    (dolist (p (scroll-layout-columns output workspace))
      (destructuring-bind (win x y w h) p
        (when (and win
                   (gethash (window-proxy win) *windows*)
                   (not (window-minimized-p win))
                   (not (window-floating-p win)))
          (make-window-node win)
          (set-window-position win x y)
          (setf (window-x win) x
                (window-y win) y)
          (multiple-value-bind (r g b) (theme-border-rgb (window-focused-p win))
            (set-window-borders win '(:top :bottom :left :right)
                                (if (window-focused-p win) 2 1)
                                r g b #xff))
          (when (window-hidden-p win)
            (show-window win))
          (push win rendered))))
    (nreverse rendered)))

(defun scroll-focus (output workspace delta)
  "Focus the column DELTA positions away (niri: focus-column-left /
focus-column-right); the viewport follows to keep it visible."
  (let* ((state (scroll-state output workspace))
         (columns (getf state :columns)))
    (when columns
      (let* ((focused (or (find-if #'window-focused-p columns)
                          (first columns)))
             (pos (or (position focused columns :test #'eq) 0))
             (next (nth (min (max 0 (+ pos delta)) (1- (length columns)))
                        columns)))
        (dolist (seat (all-seats))
          (focus-window next seat))
        (scroll-ensure-focused-visible output workspace)
        (request-manage-dirty)))))

(defun scroll-viewport (output workspace delta)
  "Scroll the ribbon viewport by half a screen (niri: MOD+comma /
MOD+period); focus stays where it was."
  (let* ((state (scroll-state output workspace))
         (step (floor (output-width output) 2)))
    (when state
      (incf (getf state :scroll-x) (* delta step))
      (scroll-clamp output workspace)
      (message "scroll-x: ~D" (getf state :scroll-x))
      (request-manage-dirty))))

;;; --- IPC/binding wrappers (resolve the current output) ---

(defun scroll-focus-left (&optional (output (first (all-outputs))))
  "Focus the column to the left; the viewport follows."
  (when output
    (scroll-focus output (current-workspace output) -1)))

(defun scroll-focus-right (&optional (output (first (all-outputs))))
  "Focus the column to the right; the viewport follows."
  (when output
    (scroll-focus output (current-workspace output) 1)))

(defun scroll-viewport-left (&optional (output (first (all-outputs))))
  "Scroll the ribbon viewport left (half a screen)."
  (when output
    (scroll-viewport output (current-workspace output) -1)))

(defun scroll-viewport-right (&optional (output (first (all-outputs))))
  "Scroll the ribbon viewport right (half a screen)."
  (when output
    (scroll-viewport output (current-workspace output) 1)))
