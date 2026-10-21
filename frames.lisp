;;; Frame trees for Rhizome — StumpWM's core data structure
;;;
;;; StumpWM doesn't have "layouts" in the i3 sense: the screen is a
;;; recursive binary tree of FRAMES (rectangles) that the user carves
;;; up interactively. Internal nodes are splits (a direction and a
;;; dividing line), leaves are frames holding windows — one visible at
;;; a time, cycled with C-t n/p. New windows open into the current
;;; frame; closed windows leave the frame behind; master-stack, even
;;; and monocle are all just particular trees.
;;;
;;;   C-t s / C-t S    split the current frame (side-by-side / stacked)
;;;   C-t R            remove the current split (merge into sibling)
;;;   C-t arrows       move focus between frames
;;;   C-t n / C-t p    cycle the windows in the current frame
;;;
;;; Each output keeps one tree per workspace (the StumpWM per-group
;;; layout). Trees are pure client-side state: the manage pass proposes
;;; each frame's visible window its rectangle, the render pass places
;;; it — the same river requests the preset layouts used.

(in-package #:rhizome)

(defparameter *frame-min-size* 50
  "Smallest frame dimension a split or resize may produce.")

;;; --- Frame structure ---

(defstruct frame
  "A rectangular region of an output that can hold windows."
  (x 0 :type integer)
  (y 0 :type integer)
  (width 0 :type integer)
  (height 0 :type integer)
  (parent nil :type (or null frame))
  (first-child nil :type (or null frame))
  (second-child nil :type (or null frame))
  (split-direction nil :type (or null (member :horizontal :vertical)))
  (windows nil :type list))

(defvar *frame-trees* (make-hash-table :test 'equal)
  "Maps (OUTPUT . WORKSPACE) to the root frame of its tree.")
(defvar *current-frames* (make-hash-table :test 'equal)
  "Maps (OUTPUT . WORKSPACE) to the frame commands act on.")

(defun frame-key (output workspace)
  (cons output workspace))

(defun frame-tree-for (output workspace)
  "The workspace's frame tree, created on first use as a single
full-screen frame (the StumpWM default — split from there)."
  (or (gethash (frame-key output workspace) *frame-trees*)
      (setf (gethash (frame-key output workspace) *frame-trees*)
            (make-frame
              :width (output-width output)
              :height (max 0 (- (output-height output)
                                (bar-reserved-height)))))))

(defun frame-leaves (frame)
  "All leaf frames of the tree rooted at FRAME, in layout order."
  (if (frame-first-child frame)
      (append (frame-leaves (frame-first-child frame))
              (frame-leaves (frame-second-child frame)))
      (list frame)))

(defun frame-sibling (frame)
  "FRAME's sibling under its parent, or NIL at the root."
  (let ((parent (frame-parent frame)))
    (and parent
         (if (eq (frame-first-child parent) frame)
             (frame-second-child parent)
             (frame-first-child parent)))))

(defun frame-current-window (frame)
  "The window FRAME shows: its focused window if any, else the first."
  (or (find-if #'window-focused-p (frame-windows frame))
      (first (frame-windows frame))))

(defun frame-push-window (win frame)
  "Put WIN in FRAME, making it the frame's visible window."
  (push win (frame-windows frame))
  (setf (window-frame win) frame))

(defun frame-remove-window (win)
  "Take WIN out of whatever frame holds it."
  (let ((frame (window-frame win)))
    (when frame
      (setf (frame-windows frame)
            (remove win (frame-windows frame)))
      (setf (window-frame win) nil))))

(defun current-frame-for (output workspace)
  "The frame commands act on: the focused window's frame when its
window lives on this workspace, else the stored pointer, else the
first leaf."
  (let* ((key (frame-key output workspace))
         (root (frame-tree-for output workspace))
         (stored (gethash key *current-frames*))
         (focused (find-if #'window-focused-p (all-windows))))
    (or (and focused
             (window-frame focused)
             (eq (window-output focused) output)
             (eql (window-workspace focused) workspace)
             (window-frame focused))
        (and stored
             (member stored (frame-leaves root))
             stored)
        (first (frame-leaves root)))))

;;; --- Tree editing ---

(defun split-frame (frame direction ratio)
  "Split FRAME in DIRECTION (:horizontal side-by-side, :vertical
stacked) at RATIO (0.0-1.0). Returns the new (second) child frame."
  (let ((first (make-frame :parent frame))
        (second (make-frame :parent frame)))
    (setf (frame-split-direction frame) direction
          (frame-first-child frame) first
          (frame-second-child frame) second)
    (ecase direction
      (:horizontal
       (let ((split-x (+ (frame-x frame)
                          (floor (* (frame-width frame) ratio)))))
         (setf (frame-x first) (frame-x frame)
               (frame-y first) (frame-y frame)
               (frame-width first) (- split-x (frame-x frame))
               (frame-height first) (frame-height frame)
               (frame-x second) split-x
               (frame-y second) (frame-y frame)
               (frame-width second) (- (+ (frame-x frame) (frame-width frame))
                                       split-x)
               (frame-height second) (frame-height frame))))
      (:vertical
       (let ((split-y (+ (frame-y frame)
                          (floor (* (frame-height frame) ratio)))))
         (setf (frame-x first) (frame-x frame)
               (frame-y first) (frame-y frame)
               (frame-width first) (frame-width frame)
               (frame-height first) (- split-y (frame-y frame))
               (frame-x second) (frame-x frame)
               (frame-y second) split-y
               (frame-width second) (frame-width frame)
               (frame-height second) (- (+ (frame-y frame) (frame-height frame))
                                        split-y)))))
    second))

(defun split-current-frame (output workspace direction &optional (ratio 1/2))
  "Split the current frame; focus moves to the new (second) frame so
the next window opened lands there. The current window stays in the
first child."
  (let ((frame (current-frame-for output workspace)))
    (split-frame frame direction ratio)
    ;; Windows move down to the first child; the new frame is empty
    (let ((first (frame-first-child frame)))
      (dolist (win (frame-windows frame))
        (setf (window-frame win) first))
      (setf (frame-windows first) (frame-windows frame)
            (frame-windows frame) nil))
    (setf (gethash (frame-key output workspace) *current-frames*)
          (frame-second-child frame))
    (message "split")
    (request-manage-dirty)))

(defun remove-current-split (output workspace)
  "Remove the current frame's split: its windows merge into the
sibling frame, which takes over the parent's rectangle. No-op at the
root."
  (let* ((frame (current-frame-for output workspace))
         (parent (frame-parent frame)))
    (unless parent
      (message "no split to remove")
      (return-from remove-current-split))
    (let ((sibling (frame-sibling frame)))
      ;; Windows merge in, behind the sibling's own current window
      (setf (frame-windows sibling)
            (append (frame-windows sibling) (frame-windows frame)))
      (dolist (win (frame-windows sibling))
        (setf (window-frame win) sibling))
      ;; The sibling takes over the parent's rectangle and position
      (setf (frame-x sibling) (frame-x parent)
            (frame-y sibling) (frame-y parent)
            (frame-width sibling) (frame-width parent)
            (frame-height sibling) (frame-height parent)
            (frame-parent sibling) (frame-parent parent))
      (let ((grand (frame-parent parent)))
        (cond (grand
               (if (eq (frame-first-child grand) parent)
                   (setf (frame-first-child grand) sibling)
                   (setf (frame-second-child grand) sibling)))
              (t
               (setf (gethash (frame-key output workspace) *frame-trees*)
                     sibling))))
      (setf (gethash (frame-key output workspace) *current-frames*) sibling)
      (message "split removed")
      (request-manage-dirty))))

(defun resize-current-split (output workspace delta)
  "Move the dividing line between the current frame and its sibling by
DELTA pixels along the split axis (clamped so both sides stay
usable). No-op at the root."
  (let* ((frame (current-frame-for output workspace))
         (parent (frame-parent frame)))
    (unless parent
      (message "no split to resize")
      (return-from resize-current-split))
    (let ((first (frame-first-child parent))
          (second (frame-second-child parent))
          (x2 (+ (frame-x parent) (frame-width parent)))
          (y2 (+ (frame-y parent) (frame-height parent))))
      (ecase (frame-split-direction parent)
        (:horizontal
         (let ((line (max (+ (frame-x parent) *frame-min-size*)
                          (min (- x2 *frame-min-size*)
                               (+ (frame-x first) (frame-width first) delta)))))
           (setf (frame-width first) (- line (frame-x first))
                 (frame-x second) line
                 (frame-width second) (- x2 line))))
        (:vertical
         (let ((line (max (+ (frame-y parent) *frame-min-size*)
                          (min (- y2 *frame-min-size*)
                               (+ (frame-y first) (frame-height first) delta)))))
           (setf (frame-height first) (- line (frame-y first))
                 (frame-y second) line
                 (frame-height second) (- y2 line)))))
      (request-manage-dirty))))

;;; --- Focus ---

(defun frame-at-direction (frame direction leaves)
  "The leaf nearest FRAME in DIRECTION (:left :right :up :down), or
NIL. Prefers frames that share an edge band; falls back to the
nearest in the raw direction."
  (let ((fx1 (frame-x frame)) (fy1 (frame-y frame))
        (fx2 (+ (frame-x frame) (frame-width frame)))
        (fy2 (+ (frame-y frame) (frame-height frame))))
    (flet ((in-dir-p (f)
             (let ((x1 (frame-x f)) (y1 (frame-y f))
                   (x2 (+ (frame-x f) (frame-width f)))
                   (y2 (+ (frame-y f) (frame-height f))))
               (ecase direction
                 (:right (>= x1 fx2))
                 (:left  (<= x2 fx1))
                 (:down  (>= y1 fy2))
                 (:up    (<= y2 fy1)))))
           (distance (f)
             (let ((x1 (frame-x f)) (y1 (frame-y f))
                   (x2 (+ (frame-x f) (frame-width f)))
                   (y2 (+ (frame-y f) (frame-height f))))
               (ecase direction
                 (:right (- x1 fx2))
                 (:left  (- fx1 x2))
                 (:down  (- y1 fy2))
                 (:up    (- fy1 y2)))))
           (overlaps-p (f)
             (let ((x1 (frame-x f)) (y1 (frame-y f))
                   (x2 (+ (frame-x f) (frame-width f)))
                   (y2 (+ (frame-y f) (frame-height f))))
               (ecase direction
                 ((:right :left) (> (min y2 fy2) (max y1 fy1)))
                 ((:down :up)    (> (min x2 fx2) (max x1 fx1)))))))
      (let ((cands (remove-if-not #'in-dir-p
                                  (remove frame leaves))))
        (let ((sorted (sort cands #'< :key #'distance)))
          (or (find-if #'overlaps-p sorted)
              (first sorted)))))))

(defun focus-frame-direction (output workspace direction)
  "Move frame focus one step in DIRECTION. If the target frame holds
windows, the nearest one takes keyboard focus."
  (let* ((root (frame-tree-for output workspace))
         (target (frame-at-direction (current-frame-for output workspace)
                                     direction (frame-leaves root))))
    (when target
      (setf (gethash (frame-key output workspace) *current-frames*) target)
      (let ((win (frame-current-window target)))
        (when win
          (dolist (seat (all-seats))
            (focus-window win seat))))
      (request-manage-dirty))))

(defun move-window-to-frame-direction (output workspace direction)
  "Move the focused window to the frame in DIRECTION (StumpWM's
move-window): it becomes the target frame's visible window and keeps
keyboard focus. Floats stay put (they are not in the tiling)."
  (let* ((root (frame-tree-for output workspace))
         (leaves (frame-leaves root))
         (target (frame-at-direction (current-frame-for output workspace)
                                     direction leaves))
         (win (find-if #'window-focused-p (all-windows))))
    (cond ((null win) (message "no focused window"))
          ((window-floating-p win) (message "floats are not in the tiling"))
          ((null target) (message "no frame ~A" direction))
          ((eq (window-frame win) target) (message "already in that frame"))
          (t
           (frame-remove-window win)
           (frame-push-window win target)
           (dolist (seat (all-seats))
             (focus-window win seat))
           (request-manage-dirty)))))

(defun cycle-frame-window (output workspace &optional (delta 1))
  "Cycle the visible window of the current frame by DELTA (+1/-1).
With a single window this is a no-op."
  (let* ((frame (current-frame-for output workspace))
         (wins (remove-if #'window-minimized-p (frame-windows frame))))
    (when (cdr wins)
      (let* ((cur (frame-current-window frame))
             (pos (or (position cur wins :test #'eq) 0))
             (next (nth (mod (+ pos delta) (length wins)) wins)))
        (setf (frame-windows frame)
              (append (list next) (remove next (frame-windows frame))))
        (dolist (seat (all-seats))
          (focus-window next seat))
        (request-manage-dirty)))))

;;; --- Pull / exchange / frame tree collapse (StumpWM commands) ---

(defun pull-window-into-current-frame (win output workspace)
  "Pull WIN into the current frame, making it the visible window
(StumpWM's pull). Floats stay put."
  (cond ((null win) (message "no window"))
        ((window-floating-p win) (message "floats are not pulled into frames"))
        (t
         (frame-remove-window win)
         (frame-push-window win (current-frame-for output workspace))
         (dolist (seat (all-seats))
           (focus-window win seat))
         (request-manage-dirty))))

(defun pull-window-by-number (n &optional (output (first (all-outputs)))
                                   (workspace (current-workspace output)))
  "Pull the window with number N into the current frame
(StumpWM's pull N)."
  (pull-window-into-current-frame (find-window-by-number n output workspace)
                                  output workspace))

(defun visible-frame-windows (output workspace)
  "The windows currently displayed by the workspace's frames."
  (remove nil
          (mapcar #'frame-current-window
                  (frame-leaves (frame-tree-for output workspace)))))

(defun pull-hidden-next (output workspace &optional (delta 1))
  "Pull the next (or previous, DELTA -1) window not displayed in any
frame into the current frame, in number order (StumpWM's
pull-hidden-next/previous)."
  (let* ((visible (visible-frame-windows output workspace))
         (hidden (remove-if (lambda (w)
                              (or (window-floating-p w)
                                  (member w visible :test #'eq)))
                            (windows-on-workspace output workspace)))
         (focused (find-if #'window-focused-p (all-windows))))
    (cond ((null hidden)
           (message "no hidden windows"))
          (t
           ;; Number order; the focused window's number anchors the walk
           (let* ((cur-n (and focused (window-number focused)))
                  (next (or (if (plusp delta)
                                (find-if (lambda (w)
                                           (and cur-n
                                                (> (window-number w) cur-n)))
                                         hidden)
                                (find-if (lambda (w)
                                           (and cur-n
                                                (< (window-number w) cur-n)))
                                         hidden :from-end t))
                            (if (plusp delta)
                                (first hidden)
                                (first (last hidden))))))
             (pull-window-into-current-frame next output workspace))))))

(defun exchange-window-direction (output workspace direction)
  "Swap the current frame's window with the one in the frame in
DIRECTION (StumpWM's exchange-direction). Focus follows the window
that arrives in the current frame."
  (let* ((root (frame-tree-for output workspace))
         (cur (current-frame-for output workspace))
         (target (frame-at-direction cur direction (frame-leaves root)))
         (w1 (frame-current-window cur))
         (w2 (and target (frame-current-window target))))
    (cond ((null w1) (message "no window in the current frame"))
          ((null target) (message "no frame ~A" direction))
          ((eq target cur) (message "already there"))
          (t
           (frame-remove-window w1)
           (when w2 (frame-remove-window w2))
           (frame-push-window w1 target)
           (when w2 (frame-push-window w2 cur))
           (when w2
             (dolist (seat (all-seats))
               (focus-window w2 seat)))
           (request-manage-dirty)))))

(defun frame-only (output workspace)
  "Collapse the workspace to a single frame: every window merges into
the current frame, which becomes the whole content area (StumpWM's
only, C-t Q)."
  (let* ((root (frame-tree-for output workspace))
         (leaves (frame-leaves root))
         (cur (current-frame-for output workspace))
         (wins (loop for leaf in leaves append (frame-windows leaf))))
    (if (= (length leaves) 1)
        (message "already the only frame")
        (progn
          (dolist (win wins)
            (setf (window-frame win) nil))
          (setf (frame-windows cur) wins)
          (dolist (win wins)
            (setf (window-frame win) cur))
          ;; The current frame becomes the root
          (setf (frame-x cur) 0
                (frame-y cur) 0
                (frame-width cur) (output-width output)
                (frame-height cur) (max 0 (- (output-height output)
                                              (bar-reserved-height)))
                (frame-parent cur) nil
                (frame-first-child cur) nil
                (frame-second-child cur) nil
                (frame-split-direction cur) nil)
          (setf (gethash (frame-key output workspace) *frame-trees*) cur
                (gethash (frame-key output workspace) *current-frames*) cur)
          (request-manage-dirty)))))

(defun frame-balance (output workspace)
  "Equalize every split in the workspace's tree (StumpWM's
balance-frames)."
  (labels ((balance (frame)
             (when (frame-first-child frame)
               (let ((first (frame-first-child frame))
                     (second (frame-second-child frame))
                     (x (frame-x frame))
                     (y (frame-y frame))
                     (w (frame-width frame))
                     (h (frame-height frame)))
                 (ecase (frame-split-direction frame)
                   (:horizontal
                    (let ((half (floor w 2)))
                      (frame-set-rect first x y half h)
                      (frame-set-rect second (+ x half) y (- w half) h)))
                   (:vertical
                    (let ((half (floor h 2)))
                      (frame-set-rect first x y w half)
                      (frame-set-rect second x (+ y half) w (- h half)))))
                 (balance first)
                 (balance second)))))
    (balance (frame-tree-for output workspace))
    (request-manage-dirty)))

(defun frame-clear (output workspace)
  "Clear the current frame: its windows move out to the other frames,
round-robin (StumpWM's fclear)."
  (let* ((root (frame-tree-for output workspace))
         (leaves (frame-leaves root))
         (cur (current-frame-for output workspace))
         (others (remove cur leaves)))
    (cond ((null others)
           (message "no other frame to move windows to"))
          ((null (frame-windows cur))
           (message "frame is already empty"))
          (t
           (let ((rest (frame-windows cur)))
             (setf (frame-windows cur) nil)
             (loop for win in rest
                   for i from 0
                   do (frame-push-window win
                                          (nth (mod i (length others))
                                               others))))
           (request-manage-dirty)))))

;;; --- Frame dump/restore (StumpWM's fdump) ---
;;;
;;; A workspace's frame tree serializes to a readable sexp:
;;;
;;;   (:split :horizontal (0 0 1920 1060)
;;;     (:frame (0 0 960 1060) 1 2)
;;;     (:split :vertical (960 0 960 1060) (:frame ...) (:frame ...)))
;;;
;;; Windows are recorded by number; windows that no longer exist when
;;; the dump is restored are skipped, and tiled windows not mentioned
;;; in the dump pile onto the last frame. Rectangles scale
;;; proportionally when the output's content area changed since the
;;; dump.

(defun frame-dump-pathname (name)
  "Pathname of the frame dump NAME (rhizome-cmd dump-frame <name>)."
  (merge-pathnames
    (make-pathname :name (string-downcase (string name))
                   :type "lisp"
                   :directory '(:relative "frame-dumps"))
    (merge-pathnames #P".local/share/rhizome/"
                     (user-homedir-pathname))))

(defun frame-tree-to-sexp (root)
  "Serialize the frame tree ROOT to a readable sexp (window numbers
per frame, the visible window first)."
  (labels ((walk (frame)
             (let ((rect (list :rect (frame-x frame) (frame-y frame)
                                   (frame-width frame)
                                   (frame-height frame))))
               (if (frame-first-child frame)
                   (list :split (frame-split-direction frame) rect
                         (walk (frame-first-child frame))
                         (walk (frame-second-child frame)))
                   (list* :frame rect
                          (loop for w in (frame-windows frame)
                                collect (window-number w)))))))
    (walk root)))

(defun dump-rect (sexp)
  "The rectangle of a dump node, as (x y w h)."
  (rest (if (eq (first sexp) :split)
            (third sexp)
            (second sexp))))

(defun sexp-to-frame-tree (sexp sx sy)
  "Rebuild a frame tree from a dump sexp, scaling x/width by SX and
y/height by SY. The dump's number lists are ignored here; callers
assign windows after the tree is built."
  (let* ((rect (dump-rect sexp))
         (frame (make-frame :x (round (* (first rect) sx))
                            :y (round (* (second rect) sy))
                            :width (max 1 (round (* (third rect) sx)))
                            :height (max 1 (round (* (fourth rect) sy))))))
    (if (eq (first sexp) :split)
        (let* ((direction (second sexp))
               (first (sexp-to-frame-tree (fourth sexp) sx sy))
               (second (sexp-to-frame-tree (fifth sexp) sx sy)))
          (setf (frame-split-direction frame) direction
                (frame-first-child frame) first
                (frame-second-child frame) second
                (frame-parent first) frame
                (frame-parent second) frame)
          frame)
        frame)))

(defun dump-frame (output workspace file)
  "Write the workspace's frame tree to FILE (a printable sexp)."
  (ensure-directories-exist file)
  (with-open-file (s file :direction :output :if-exists :supersede)
    (write (frame-tree-to-sexp (frame-tree-for output workspace))
           :stream s :readably t))
  (message "dumped to ~A" file))

(defun dump-leaf-numbers (sexp)
  "The per-frame window number lists of a dump sexp, in leaf order."
  (if (eq (first sexp) :frame)
      (list (cddr sexp))
      (append (dump-leaf-numbers (fourth sexp))
              (dump-leaf-numbers (fifth sexp)))))

(defun restore-frame (output workspace file)
  "Rebuild the workspace's frame tree from a dump FILE."
  (let ((sexp (with-open-file (s file :direction :input)
                (read s))))
    (restore-workspace-from-sexp output workspace sexp)))

(defun restore-workspace-from-sexp (output workspace sexp)
  "Rebuild the workspace's frame tree from a dump sexp (see
frame-tree-to-sexp). Windows are placed by number; missing ones are
skipped, unmentioned tiled windows pile onto the last frame, and
rectangles scale when the output's content area changed since the
dump."
  (let* ((root-rect (dump-rect sexp))
         (content-w (output-width output))
         (content-h (max 0 (- (output-height output)
                              (bar-reserved-height))))
         (sx (if (plusp (third root-rect))
                 (/ content-w (third root-rect)) 1))
         (sy (if (plusp (fourth root-rect))
                 (/ content-h (fourth root-rect)) 1))
         (tree (sexp-to-frame-tree sexp sx sy))
         (leaves (frame-leaves tree))
         (number-lists (dump-leaf-numbers sexp))
         (assigned '()))
    ;; Place windows by number, in frame order
    ;; Numbers are pushed in reverse so the first dumped window ends
    ;; up the frame's visible window (frame-push pushes front)
    (loop for leaf in leaves
          for numbers in number-lists
          do (dolist (n (reverse numbers))
                (let ((win (find-window-by-number n output workspace)))
                  (when (and win
                             (not (window-floating-p win))
                             (not (member win assigned :test #'eq)))
                    (frame-remove-window win)
                    (frame-push-window win leaf)
                    (push win assigned)))))
    ;; Tiled windows the dump didn't mention pile onto the last frame
    (let ((last (first (last leaves))))
      (dolist (win (windows-on-workspace output workspace))
        (when (and (not (window-floating-p win))
                   (not (member win assigned :test #'eq)))
          (frame-remove-window win)
          (frame-push-window win last)
          (push win assigned))))
    ;; Commit: swap the tree in; current frame = the first frame
    ;; that has windows, else the first leaf
    (setf (gethash (frame-key output workspace) *frame-trees*) tree
          (gethash (frame-key output workspace) *current-frames*)
          (or (find-if (lambda (l) (frame-windows l)) leaves)
              (first leaves)))
    (request-manage-dirty)
    tree))

;;; --- Tree construction from rectangles ---
;;;
;;; Layout templates (master-stack, even, monocle, custom user
;;; layouts) produce rectangles; these build a frame tree whose
;;; leaves are exactly those rectangles — when they form a valid
;;; guillotine partition. Otherwise the workspace falls back to a
;;; single frame (custom layouts with gaps/overlaps).

(defun frame-find-cut (rects)
  "Find a full cut through RECTS: a vertical line x=POS that no rect
crosses (→ :horizontal split) or a horizontal line y=POS (→ :vertical
split). Returns (DIRECTION POS LEFT/ABOVE RIGHT/BELOW), or NIL."
  (flet ((x2 (r) (+ (first r) (third r)))
         (y2 (r) (+ (second r) (fourth r))))
    (let ((min-x (reduce #'min (mapcar #'first rects)))
          (max-x (reduce #'max (mapcar #'x2 rects)))
          (min-y (reduce #'min (mapcar #'second rects)))
          (max-y (reduce #'max (mapcar #'y2 rects))))
      (dolist (pos (sort (remove-duplicates (mapcar #'first rects)) #'<))
        (when (and (> pos min-x) (< pos max-x))
          (let ((a (remove-if (lambda (r) (>= (first r) pos)) rects))
                (b (remove-if (lambda (r) (<= (x2 r) pos)) rects)))
            (when (= (+ (length a) (length b)) (length rects))
              (return-from frame-find-cut
                (list :horizontal pos a b))))))
      (dolist (pos (sort (remove-duplicates (mapcar #'second rects)) #'<))
        (when (and (> pos min-y) (< pos max-y))
          (let ((a (remove-if (lambda (r) (>= (second r) pos)) rects))
                (b (remove-if (lambda (r) (<= (y2 r) pos)) rects)))
            (when (= (+ (length a) (length b)) (length rects))
              (return-from frame-find-cut
                (list :vertical pos a b))))))
      nil)))

(defun build-tree-from-rects (rects)
  "Build a frame tree whose leaves are RECTS ((x y w h) each), or NIL
when they don't form a guillotine partition."
  (labels ((bounds (rs)
             (list (reduce #'min (mapcar #'first rs))
                   (reduce #'min (mapcar #'second rs))
                   (reduce #'max (mapcar (lambda (r) (+ (first r) (third r))) rs))
                   (reduce #'max (mapcar (lambda (r) (+ (second r) (fourth r))) rs))))
           (build (rs)
             (if (= 1 (length rs))
                 (destructuring-bind (x y w h) (first rs)
                   (make-frame :x x :y y :width w :height h))
                 (let ((cut (frame-find-cut rs)))
                   (when cut
                     (destructuring-bind (dir _pos a b) cut
                       (let* ((bnd (bounds rs))
                              (frame (make-frame
                                       :x (first bnd) :y (second bnd)
                                       :width (- (third bnd) (first bnd))
                                       :height (- (fourth bnd) (second bnd))
                                       :split-direction dir))
                              (fa (build a))
                              (fb (build b)))
                         (setf (frame-first-child frame) fa
                               (frame-second-child frame) fb
                               (frame-parent fa) frame
                               (frame-parent fb) frame)
                         frame)))))))
    (when rects (build rects))))

(defun set-workspace-tree (output workspace rects windows)
  "Rebuild the workspace's frame tree with leaves RECTS, distributing
WINDOWS one per leaf in order (extras pile onto the last leaf).
Falls back to a single frame when RECTS aren't a valid partition."
  (let* ((key (frame-key output workspace))
         (tree (build-tree-from-rects rects))
         (width (output-width output))
         (height (max 0 (- (output-height output)
                          (bar-reserved-height)))))
    ;; Floats are not part of the tiling: take everyone out of the
    ;; old tree, then distribute the tiled windows across the new one
    (dolist (win windows) (frame-remove-window win))
    (let ((tiled (remove-if #'window-floating-p windows)))
      (cond (tree
             (let ((leaves (frame-leaves tree)))
               (let ((rest tiled))
                 (dolist (leaf leaves)
                   (when rest
                     (frame-push-window (pop rest) leaf)))
                 (when rest
                   (let ((last (first (last leaves))))
                     (setf (frame-windows last)
                           (append (frame-windows last) rest))
                     (dolist (win rest) (setf (window-frame win) last)))))
               (setf (gethash key *frame-trees*) tree
                     (gethash key *current-frames*) (first leaves))))
            (t
             ;; Not a guillotine partition (gaps/overlaps in a custom
             ;; layout): one frame holding everything.
             (let ((root (make-frame :width width :height height)))
               (dolist (win tiled)
                 (frame-push-window win root))
               (setf (gethash key *frame-trees*) root
                     (gethash key *current-frames*) root)))))))

(defun frame-set-rect (frame x y width height)
  "Rescale FRAME (and its whole subtree) to a new rectangle,
preserving each split's dividing line as a ratio of the parent."
  (setf (frame-x frame) x
        (frame-y frame) y
        (frame-width frame) width
        (frame-height frame) height)
  (when (frame-first-child frame)
    (let* ((first (frame-first-child frame))
           (second (frame-second-child frame))
           (x2 (+ x width))
           (y2 (+ y height)))
      (ecase (frame-split-direction frame)
        (:horizontal
         (let* ((total (+ (frame-width first) (frame-width second)))
                (split-x (if (plusp total)
                             (+ x (floor (* width (/ (frame-width first)
                                                      total))))
                             (+ x (floor width 2)))))
           (frame-set-rect first x y (- split-x x) height)
           (frame-set-rect second split-x y (- x2 split-x) height)))
        (:vertical
         (let* ((total (+ (frame-height first) (frame-height second)))
                (split-y (if (plusp total)
                             (+ y (floor (* height (/ (frame-height first)
                                                       total))))
                             (+ y (floor height 2)))))
           (frame-set-rect first x y width (- split-y y))
           (frame-set-rect second x split-y width (- y2 split-y))))))))

(defun refit-output-frame-trees (output)
  "Rescale every existing frame tree of OUTPUT to the output's current
content area (output size minus the bar). Frame trees cache their
geometry at creation, so this runs whenever the reservation or the
output dimensions change."
  (let ((width (output-width output))
        (height (max 0 (- (output-height output)
                          (bar-reserved-height)))))
    (maphash (lambda (key root)
               (when (eq (car key) output)
                 (frame-set-rect root 0 0 width height)))
             *frame-trees*)))

;;; --- Manage/render sequence integration ---

;;; --- Group-type stubs (redefined by groups.lisp, which loads later) ---

(defun floating-workspace-p (output workspace)
  "T while the workspace's group type floats every window.
Placeholder: redefined by groups.lisp."
  (declare (ignore output workspace))
  nil)

(defun float-adopt-window (win output workspace)
  "Placeholder: redefined by groups.lisp."
  (declare (ignore win output workspace))
  nil)

;; Keep the compiler from deriving the stubs' NULL return type into
;; the adoption dispatch (a stub defined earlier in the same file
;; would otherwise bake an impossible-branch check into the compiled
;; caller, and the real definition's T return would trip it).
(declaim (ftype (function (t t) t) floating-workspace-p)
         (ftype (function (t t t) t) float-adopt-window))

(defun frame-adopt-window (win output workspace)
  "Adopt a frameless WIN into the tiling. Dynamic tiling by default:
if the current frame is occupied, split it first so every new window
gets its own frame (dwm-style) — (:tiling (:auto-split nil)) opts
back into strict StumpWM (one window per frame, hidden behind)."
  (let ((frame (current-frame-for output workspace)))
    (if (and (config-getf '(:tiling) :auto-split :default t)
             (frame-windows frame)
             (>= (frame-width frame) (* 2 *frame-min-size*))
             (>= (frame-height frame) (* 2 *frame-min-size*)))
        ;; Split along the longer axis so frames stay usable; the
        ;; existing windows move to the first child (split-frame
        ;; leaves them on the now-internal node otherwise), and the
        ;; new window takes the fresh half with the current pointer
        (let ((second (split-frame frame
                                   (if (>= (frame-width frame)
                                           (frame-height frame))
                                       :horizontal
                                       :vertical)
                                   1/2))
              (first (frame-first-child frame)))
          (dolist (win (frame-windows frame))
            (setf (window-frame win) first))
          (setf (frame-windows first) (frame-windows frame)
                (frame-windows frame) nil)
          (frame-push-window win second)
          (setf (gethash (frame-key output workspace) *current-frames*)
                second))
        (frame-push-window win frame))))

(defun frames-preserve-empty-p ()
  "T when empty frames must be kept for a pending restore: expose
floats every window off its frame and puts them back afterwards, so
pruning mid-expose would collapse the tree under the restore."
  (and (boundp '*expose-active*) (symbol-value '*expose-active*)))

(defun prune-empty-frames (output workspace)
  "Absorb empty leaf frames into their siblings, dwm-style: when a
window closes, floats away, or moves workspaces, its neighbors grow
to fill the vacated space. Leaves merge pairwise — the survivor
(internal nodes rescaled to keep their split ratios) takes over the
parent's rectangle and slot. The root frame always survives, so new
windows always have a place to land."
  (let ((key (frame-key output workspace)))
    (loop
      (let* ((root (gethash key *frame-trees*))
             (empty (when (and root (frame-first-child root))
                      (find-if (lambda (leaf)
                                 (and (null (frame-windows leaf))
                                      (frame-parent leaf)))
                               (frame-leaves root)))))
        (unless empty
          (return))
        (let* ((parent (frame-parent empty))
               (sibling (if (eq (frame-first-child parent) empty)
                            (frame-second-child parent)
                            (frame-first-child parent)))
               (grand (frame-parent parent)))
          ;; The survivor takes over the parent's rectangle (children
          ;; rescaled, split ratios preserved) and the parent's slot
          (frame-set-rect sibling
                          (frame-x parent) (frame-y parent)
                          (frame-width parent) (frame-height parent))
          (setf (frame-parent sibling) grand)
          (cond (grand
                 (if (eq (frame-first-child grand) parent)
                     (setf (frame-first-child grand) sibling)
                     (setf (frame-second-child grand) sibling)))
                (t
                 (setf (gethash key *frame-trees*) sibling)))
          ;; The current-frame pointer survives: aim it at the survivor
          (when (eq (gethash key *current-frames*) empty)
            (setf (gethash key *current-frames*) sibling)))))))

(defun frames-manage-decisions (output workspace wins)
  "Manage-sequence decisions for the workspace's frame tree: clean up
windows that died or moved, prune frames emptied by closes (their
neighbors grow), adopt frameless windows into the current frame, and
propose each leaf's visible window its rectangle."
  (let* ((root (frame-tree-for output workspace))
         (leaves (frame-leaves root)))
    ;; Drop dead windows and windows whose frame moved elsewhere
    (dolist (leaf leaves)
      (setf (frame-windows leaf)
            (remove-if-not (lambda (win)
                             (and (gethash (window-proxy win) *windows*)
                                  (eq (window-frame win) leaf)))
                           (frame-windows leaf))))
    ;; Empty frames reclaim: neighbors absorb them (dwm-style) — but
    ;; not while expose is mid-flight with everything floated off
    (unless (frames-preserve-empty-p)
      (prune-empty-frames output workspace))
    ;; New windows (and frameless ones after a hot reload) open into
    ;; the current frame — splitting it when occupied (dynamic
    ;; tiling) — or cascade-float on a floating group-type workspace
    (dolist (win wins)
      (unless (window-frame win)
        (if (floating-workspace-p output workspace)
            (float-adopt-window win output workspace)
            (frame-adopt-window win output workspace))))
    ;; Each frame proposes its rectangle for its visible window,
    ;; inset by the configured gap (:gaps :inner)
    (dolist (leaf leaves)
      (let ((win (frame-current-window leaf)))
        (when (and win (not (window-minimized-p win)))
          (multiple-value-bind (_x _y w h)
              (gaps-inset-rect (frame-x leaf) (frame-y leaf)
                               (frame-width leaf) (frame-height leaf))
            (declare (ignore _x _y))
            (propose-window-dimensions win w h)))))))

(defun frames-render-decisions (output workspace wins)
  "Render-sequence decisions for the frame tree: place each leaf's
visible window at its rectangle, show it if hidden, and stamp its
position. Returns the list of rendered (visible) windows so the
render pass can exclude them from its hide logic."
  (let* ((root (frame-tree-for output workspace))
         (leaves (frame-leaves root))
         (rendered nil))
    ;; Adopt frameless windows even in a bare render pass (e.g. the
    ;; first pass after a hot reload, before any manage sequence)
    (dolist (win wins)
      (unless (window-frame win)
        (frame-push-window win (current-frame-for output workspace))))
    (dolist (leaf leaves)
      (let* ((win (frame-current-window leaf))
             (i (min (gaps-inner)
                     (floor (frame-width leaf) 2)
                     (floor (frame-height leaf) 2)))
             (x (+ (frame-x leaf) i (output-x output)))
             (y (+ (frame-y leaf) i (output-y output)
                    (bar-reserved-height))))
        (when (and win (not (window-minimized-p win)))
          (make-window-node win)
          ;; The animation engine interpolates from a recorded
          ;; start toward this final position, per frame
          (multiple-value-bind (ax ay) (animation-position win x y)
            (set-window-position win ax ay))
          (setf (window-x win) x
                (window-y win) y)
          (multiple-value-bind (r g b) (theme-border-rgb (window-focused-p win))
            (set-window-borders win '(:top :bottom :left :right)
                                (if (window-focused-p win) 2 1)
                                r g b #xff))
          ;; Show only on transition: river treats every show/hide
          ;; as a state change and starts a new sequence for it.
          (when (window-hidden-p win)
            (show-window win))
          (push win rendered))))
    (nreverse rendered)))
