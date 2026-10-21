;;; Per-workspace group types for Rhizome
;;;
;;; StumpWM's workspace "kinds" (tile-windows, floating-windows,
;;; dynamic-windows) as one per-workspace setting with a live switch:
;;;   :tile     the frame trees (dynamic splitting is the default tiling)
;;;   :scroll   the niri-style ribbon (scroll.lisp)
;;;   :floating every window floats; new arrivals float automatically
;;;
;;; C-t l cycles tile -> scroll -> floating -> tile; each switch takes
;;; over ALL the workspace's windows (entering scroll unfloats, entering
;;; floating un-ribbons and floats, entering tile unfloats and un-ribbons
;;; so the dynamic tiler re-adopts everything).

(in-package #:rhizome)

(defparameter *workspace-types* (make-hash-table :test 'equal)
  "Explicit per-workspace group types, keyed like frame trees. Only
:floating is stored here; :scroll lives in *scroll-states* (scroll.lisp)
and :tile is the absence of both.")

(defun workspace-type (output workspace)
  "The workspace's group type: :SCROLL, :FLOATING or :TILE. The
in-session hash wins; config-declared defaults come next via
(:workspaces (:types (2 :floating 5 :scroll)))."
  (cond ((scroll-workspace-p output workspace) :scroll)
        ((gethash (frame-key output workspace) *workspace-types*)
         :floating)
        ((let ((types (config-getf '(:workspaces) :types)))
           (getf types workspace)))
        (t :tile)))

(defun unfloat-all (output workspace)
  "Return every window on the workspace to the tiler."
  (dolist (win (windows-on-workspace output workspace))
    (when (window-floating-p win)
      (setf (window-floating-p win) nil
            (window-frame win) nil))))

(defun float-all (output workspace)
  "Float every tiled window on the workspace in a cascade."
  (let* ((base-x (output-x output))
         (base-y (+ (output-y output) (bar-reserved-height)))
         (i 0))
    (dolist (win (windows-on-workspace output workspace))
      (unless (window-minimized-p win)
        (unless (window-floating-p win)
          (let ((x (+ base-x 60 (* 40 i)))
                (y (+ base-y 60 (* 30 i))))
            (float-window win :x x :y y)
            (incf i)))))))

(defun set-workspace-type (output workspace type)
  "Switch the workspace's group type, taking over every window:
:tile hands everything to the dynamic tiler, :scroll unfloats into
the ribbon, :floating un-ribbons into a cascade of floats."
  (case type
    (:scroll
     (unfloat-all output workspace)
     (scroll-enter output workspace))
    (:floating
     (when (scroll-workspace-p output workspace)
       (scroll-exit output workspace))
     (setf (gethash (frame-key output workspace) *workspace-types*)
           :floating)
     (float-all output workspace)
     (message "workspace ~D: floating" workspace)
     (run-hook :layout-changed output :floating)
     (request-manage-dirty))
    (:tile
     (remhash (frame-key output workspace) *workspace-types*)
     (when (scroll-workspace-p output workspace)
       (scroll-exit output workspace))
     (unfloat-all output workspace)
     (message "workspace ~D: tiling" workspace)
     (run-hook :layout-changed output :tile)
     (request-manage-dirty))
    (t (message "group type must be :tile, :scroll or :floating"))))

(defun cycle-workspace-type (&optional (output (first (all-outputs))))
  "Cycle the current workspace's group type:
tile -> scroll -> floating -> tile (C-t l)."
  (let ((workspace (current-workspace output)))
    (case (workspace-type output workspace)
      (:tile (set-workspace-type output workspace :scroll))
      (:scroll (set-workspace-type output workspace :floating))
      (:floating (set-workspace-type output workspace :tile)))))

;;; --- Floating-workspace adoption (redefines the frames.lisp stubs,
;;; which load earlier and see these through the symbol cell) ---

(defun floating-workspace-p (output workspace)
  "T while every window on the workspace floats on arrival."
  (eql (workspace-type output workspace) :floating))

(defun float-adopt-window (win output workspace)
  "Adopt a new arrival on a floating workspace: cascade it below the
existing floats instead of tiling it."
  (let* ((floats (remove-if-not #'window-floating-p
                                (windows-on-workspace output workspace)))
         (i (mod (length floats) 8)))
    (float-window win
                  :x (+ (output-x output) 60 (* 40 i))
                  :y (+ (output-y output)
                        (bar-reserved-height)
                        60
                        (* 30 i)))))
