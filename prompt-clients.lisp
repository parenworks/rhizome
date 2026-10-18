;;; Interactive prompt clients: windowlist, workspace picker,
;;; command palette. Bound to Alt+W, Alt+G, Alt+X.

(in-package #:rhizome)

(defun windowlist ()
  "Interactive window list: pick by title, jump to its workspace and
focus it."
  (let* ((wins (all-windows))
         (candidates
           (mapcar
             (lambda (win)
               (format nil "~D: [~D] ~A~A"
                       (or (window-number win) ?-)
                       (or (window-workspace win) 0)
                       (or (window-title win)
                           (window-app-id win)
                           "?")
                       (if (window-marked-p win) " *" "")))
             wins)))
    (if (null wins)
        (message "No windows")
        (prompt-open "Window:"
                     :candidates candidates
                     :on-complete
                     (lambda (choice)
                       (let ((pos (position choice candidates :test #'string=)))
                         (when pos
                           (let ((win (nth pos wins)))
                             (let ((out (window-output win)))
                               (when (and out (window-workspace win))
                                 (switch-workspace (window-workspace win) out)))
                             (dolist (seat (all-seats))
                               (focus-window win seat))
                             (request-manage-dirty)))))))))

(defun workspace-list ()
  "Interactive workspace picker."
  (let* ((snapshot (bar-snapshot))
         (current (getf snapshot :workspace 1))
         (count (getf snapshot :workspace-count *workspace-count*))
         (candidates
           (loop for i from 1 to count
                 collect (format nil "~A~A" i
                                 (if (= i current) " *" "")))))
    (prompt-open "Workspace:"
                 :candidates candidates
                 :on-complete
                 (lambda (choice)
                   (let ((n (parse-integer choice :junk-allowed t)))
                     (when n
                       (switch-workspace n)))))))

(defun command-palette ()
  "Run a command from the command table by name."
  (let ((commands
          (sort (loop for k being the hash-keys of *command-table*
                      collect (string-downcase k))
                #'string<)))
    (prompt-open "Command:"
                 :candidates commands
                 :on-complete
                 (lambda (choice)
                   (message "~A"
                            (ipc-dispatch-line choice))))))

(defun run-shell-command-prompt ()
  "Prompt for a shell command and run it in the background
(StumpWM's C-t ! equivalent)."
  (prompt-open "Shell:"
               :candidates nil
               :on-complete
               (lambda (command)
                 (message "running: ~A" command)
                 (uiop:launch-program (list "bash" "-c" command)))))

(defun select-from-menu (title items &key on-select)
  "Pop a type-to-filter selection menu and act on the choice —
StumpWM's scripting menu (select-from-menu).

ITEMS is a list of labels or (LABEL . ACTION) entries:
- a cons with a FUNCTION cdr runs the action on selection
- any other entry (including data payloads) is passed to ON-SELECT
  if given, otherwise the label is echoed

  (rhizome:select-from-menu \"Workspace?\"
    (list (cons \"Web\"  (lambda () (switch-workspace 2)))
          (cons \"Code\" (lambda () (switch-workspace 3)))))"
  (let* ((labels (mapcar (lambda (item)
                          (if (consp item)
                              (string (car item))
                              (string item)))
                        items)))
    (prompt-open title
                 :candidates labels
                 :on-complete
                 (lambda (choice)
                   (let ((pos (position choice labels :test #'string=)))
                     (when pos
                       (let* ((item (nth pos items))
                              (action (and (consp item) (cdr item))))
                         (cond ((functionp action) (funcall action))
                               (on-select (funcall on-select item))
                               (t (message "~A" choice))))))))))

(defun menu (title items)
  "Alias for select-from-menu (StumpWM's menu macro flavor)."
  (select-from-menu title items))
