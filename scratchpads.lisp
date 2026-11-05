;;; Scratchpads — the StumpWM C-t ; toggle
;;;
;;; A scratchpad is a hidden floating window (typically a terminal)
;;; that toggles on a keybinding: show it centered and focused, or
;;; hide it (the process stays alive — your session persists).
;;; Multiple scratchpads can exist; each is named and has its own
;;; launch command. The default is a terminal ("foot").
;;;
;;; Keybinding: C-t ; toggles the default scratchpad.
;;; IPC: rhizome-cmd scratchpad toggle
;;; Config: (:scratchpads ((:name "term" :command "foot")
;;;                        (:name "music" :command "ncmpcpp")))

(in-package #:rhizome)

(defvar *scratchpads* (make-hash-table :test #'equal)
  "Name → plist (:window WIN :command CMD :visible T/NIL).")

(defun scratchpad-config ()
  "The configured scratchpads, as ((:name N :command C) ...)."
  (config-getf '(:scratchpads) :entries
               :default '((:name "term" :command "foot"))))

(defun scratchpad-find (name)
  "The scratchpad's state plist, or NIL."
  (gethash name *scratchpads*))

(defun scratchpad-window-alive-p (state)
  "Whether the scratchpad's window still exists in *windows*."
  (and state
       (getf state :window)
       (gethash (window-proxy (getf state :window)) *windows*)))

(defun scratchpad-hide (name state)
  "Hide the scratchpad: minimize it (the process stays alive)."
  (let ((win (getf state :window)))
    (when (and win (gethash (window-proxy win) *windows*))
      (setf (window-minimized-p win) t
            (window-hidden-p win) t)
      (setf (gethash win *minimized-windows*) t)
      (setf (getf state :visible) nil)
      (setf (gethash name *scratchpads*) state)
      (request-manage-dirty)
      (format t "~&; Rhizome: scratchpad ~A hidden~%" name))))

(defun scratchpad-show (name state)
  "Show the scratchpad: un-minimize, float, center, focus."
  (let ((win (getf state :window)))
    (when (and win (gethash (window-proxy win) *windows*))
      ;; un-minimize
      (setf (window-minimized-p win) nil
            (window-hidden-p win) nil)
      (remhash win *minimized-windows*)
      ;; float centered
      (unless (window-floating-p win)
        (let* ((ow 1920) (oh 1080)
               (w (or (window-float-width win) (window-width win) 800))
               (h (or (window-float-height win) (window-height win) 500)))
          (float-window win :x (- (floor ow 2) (floor w 2))
                              :y (+ (bar-reserved-height) 50))))
      ;; focus it (queued through the manage pass)
      (dolist (seat (all-seats))
        (focus-window win seat))
      (setf (getf state :visible) t)
      (setf (gethash name *scratchpads*) state)
      (request-manage-dirty)
      (format t "~&; Rhizome: scratchpad ~A shown~%" name))))

(defun scratchpad-create (name command)
  "Launch the scratchpad's process; the :window-created hook
registers the first window that appears within 10 seconds."
  (setf (gethash name *scratchpads*)
        (list :window nil :command command :visible nil))
  (uiop:launch-program (list "bash" "-c" command))
  (let ((created-at (get-universal-time)))
    (add-hook :window-created
              (lambda (win)
                (let ((state (gethash name *scratchpads*)))
                  ;; claim this window if the scratchpad has none yet
                  (when (and state
                             (null (getf state :window))
                             (gethash (window-proxy win) *windows*)
                             (< (- (get-universal-time) created-at) 10))
                    (setf (getf state :window) win)
                    (setf (gethash name *scratchpads*) state)
                    (format t "~&; Rhizome: scratchpad ~A claimed ~A~%"
                            name (window-app-id win))
                    (scratchpad-show name state)))))))

(defun toggle-scratchpad (&optional (name "term"))
  "Toggle the scratchpad NAME (default: the terminal). Creates it
on first use; hides it if visible; shows it if hidden or dead."
  (let ((state (scratchpad-find name)))
    (cond
      ;; doesn't exist — create it
      ((null state)
       (let ((entry (find name (scratchpad-config)
                          :key (lambda (e) (getf e :name))
                          :test #'equal)))
         (let ((command (or (and entry (getf entry :command)) name)))
           (scratchpad-create name command)
           (format t "~&; Rhizome: scratchpad ~A launching~%" name))))
      ;; window is dead — relaunch
      ((not (scratchpad-window-alive-p state))
       (let ((entry (find name (scratchpad-config)
                          :key (lambda (e) (getf e :name))
                          :test #'equal)))
         (let ((command (or (and entry (getf entry :command))
                           (getf state :command) name)))
           (remhash name *scratchpads*)
           (scratchpad-create name command)
           (format t "~&; Rhizome: scratchpad ~A relaunching~%" name))))
      ;; visible — hide
      ((getf state :visible)
       (scratchpad-hide name state))
      ;; hidden — show
      (t
       (scratchpad-show name state)))))

(defun list-scratchpads ()
  "The scratchpad states, for IPC/debug."
  (loop for name being the hash-keys of *scratchpads*
        for state = (gethash name *scratchpads*)
        collect (list :name name
                      :visible (getf state :visible)
                      :alive (scratchpad-window-alive-p state)
                      :command (getf state :command))))

;; Hook cleanup when a scratchpad window closes
(add-hook :window-closed
          (lambda (win)
            (loop for name being the hash-keys of *scratchpads*
                  for state = (gethash name *scratchpads*)
                  when (eq (getf state :window) win)
                    do (setf (getf state :window) nil
                             (getf state :visible) nil))))

(defun scratchpad-debug ()
  "Debug the scratchpad state (IPC: rhizome-cmd scratchpad list)."
  (loop for name being the hash-keys of *scratchpads*
        for state = (gethash name *scratchpads*)
        collect (list :name name
                     :visible (getf state :visible)
                     :window (and (getf state :window)
                                 (gethash (window-proxy (getf state :window))
                                          *windows*)
                                 t)
                     :minimized (and (getf state :window)
                                    (window-minimized-p (getf state :window)))
                     :floating (and (getf state :window)
                                   (window-floating-p (getf state :window))))))
