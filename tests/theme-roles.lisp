;;; Tests for colour themes: the role table, the per-element table,
;;; theme files and the loader, per-element and per-role overrides,
;;; and the deprecated colour keys. Under the default theme every
;;; element resolves to the colour rhizome drew before themes existed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; Bar off first: configures below must never spawn a bar process.
(configure '(:bar (:enabled nil)))

(defparameter *pre-theme-palette*
  '(:border-focused "#7aa2f7"
    :border-unfocused "#404040"
    :decoration-tint "#7aa2f7"
    :decoration-shadow "#1e1e24"
    :bar-background "#1a1b26"
    :bar-foreground "#c0caf5"
    :bar-accent "#7aa2f7"
    :bar-muted "#565f89"
    :bar-border "#3b4261"
    :bar-recording "#f7768e"
    :bar-workspace-current "#7aa2f7"
    :bar-workspace-occupied "#c0caf5"
    :bar-workspace-empty "#565f89"
    :bar-clock-date "#565f89"
    :bar-clock-time "#7aa2f7"
    :bar-hover "#7aa2f7"
    :message-background "#1a1b26"
    :message-text "#7aa2f7"
    :message-border "#7aa2f7"
    :prompt-background "#1a1b26"
    :prompt-border "#7aa2f7"
    :prompt-title "#7aa2f7"
    :prompt-query "#c0caf5"
    :prompt-candidate "#c0caf5"
    :prompt-selection "#7aa2f7"
    :eval-background "#1a1b26"
    :eval-label "#7aa2f7"
    :eval-query "#c0caf5"
    :eval-result "#7aa2f7"
    :launcher-background "#23142d"
    :launcher-border "#4a3a66"
    :launcher-prompt "#8c64aa"
    :launcher-query "#dcc8f0"
    :launcher-cursor "#b4a0d2"
    :launcher-placeholder "#6b5a8a"
    :launcher-entry "#aa96c8"
    :launcher-selection "#503278"
    :launcher-selected "#f0e6ff"
    :launcher-match "#7aa2f7"
    :menu-background "#1f1633"
    :menu-border "#4a3a72"
    :menu-item "#c9b8e8"
    :menu-hover-background "#4b3a78"
    :menu-hover-foreground "#f0e6ff")
  "Every element's colour as drawn before themes: the default-config
values and the defaults written at each drawing site.")

(defun collect-warnings (thunk)
  "Call THUNK; return the text of every warning it signals."
  (let ((texts '()))
    (handler-bind ((warning (lambda (w)
                              (push (princ-to-string w) texts)
                              (muffle-warning w))))
      (funcall thunk))
    (nreverse texts)))

(defun write-scratch-theme (directory name text)
  "Write TEXT as DIRECTORY/NAME.theme; return the pathname."
  (let ((path (merge-pathnames (format nil "~A.theme" name) directory)))
    (ensure-directories-exist path)
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string text s))
    path))

(let ((pass 0) (fail 0)
      (scratch (uiop:ensure-directory-pathname
                 (format nil "/tmp/rhizome-theme-test-~D/" (sb-posix:getpid)))))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                              name actual expected))))
         (fresh-config ()
           (setf *config* (default-config))
           (configure '(:bar (:enabled nil)))
           (clear-theme-cache)))

    ;; --- the default theme draws what rhizome drew before themes ---

    (fresh-config)
    (loop for (element colour) on *pre-theme-palette* by #'cddr
          do (ok (format nil "default theme: ~(~A~) is ~A" element colour)
                 (theme-color element) colour))
    (ok "every element has a pre-theme colour"
        (sort (mapcar #'string (theme-element-names)) #'string<)
        (sort (loop for (e) on *pre-theme-palette* by #'cddr
                    collect (string e))
              #'string<))
    (multiple-value-bind (r g b) (theme-rgb :bar-background)
      (ok "theme-rgb parses the resolved colour" (list r g b) '(26 27 38)))
    (multiple-value-bind (r g b) (theme-border-rgb t)
      (ok "focused border unchanged" (list r g b) '(122 162 247)))
    (multiple-value-bind (r g b) (theme-border-rgb nil)
      (ok "unfocused border unchanged" (list r g b) '(64 64 64)))

    ;; --- the shipped theme file is the default theme ---

    (let ((file (read-theme-file
                  (asdf:system-relative-pathname :rhizome
                                                 "themes/tokyo-night.theme"))))
      (ok "tokyo-night.theme is format 1" (getf file :format) 1)
      (ok "tokyo-night.theme names itself" (getf file :name) "tokyo-night")
      (ok "the default theme is the shipped file"
          (getf *default-theme* :roles) (getf file :roles)))
    (ok "the default theme name" (config-getf '(:theme) :name) "tokyo-night")
    (ok "the role table names every role the default theme sets"
        (every (lambda (role) (member role (theme-role-names)))
               (loop for (r) on (getf *default-theme* :roles) by #'cddr
                     collect r))
        t)
    (ok "every element and role resolves to #rrggbb"
        (every (lambda (name) (theme-colour-string-p (theme-color name)))
               (append (theme-element-names) (theme-role-names)))
        t)

    ;; --- drawing sites resolve through the theme ---

    (let ((real-snapshot (symbol-function 'bar-snapshot)))
      (setf (symbol-function 'bar-snapshot)
            (lambda () '(:workspace 3 :workspace-count 4 :occupied (2 3)
                         :recording t)))
      (let ((parts (bar-module-parts :workspaces)))
        (ok "workspaces: current, occupied, empty under the default theme"
            (mapcar #'second parts)
            '("#565f89" "#c0caf5" "#7aa2f7" "#565f89")))
      (ok "recording marker under the default theme"
          (bar-module-recording) '(("REC " . "#f7768e")))
      (configure '(:theme (:roles (:accent "#ff0000"))))
      (ok "an accent role change reaches the current workspace"
          (second (third (bar-module-parts :workspaces))) "#ff0000")
      (setf (symbol-function 'bar-snapshot) real-snapshot))
    (ok "an accent role change reaches the focused border"
        (theme-color :border-focused) "#ff0000")
    (ok "an accent role change reaches the launcher match"
        (theme-color :launcher-match) "#ff0000")
    (ok "an accent role change reaches the prompt title"
        (theme-color :prompt-title) "#ff0000")
    (ok "an accent role change leaves the launcher prompt alone"
        (theme-color :launcher-prompt) "#8c64aa")
    (fresh-config)

    (ok "no drawing site writes its own colour default"
        (loop for file in (directory (asdf:system-relative-pathname
                                       :rhizome "*.lisp"))
              when (search ":default \"#" (uiop:read-file-string file))
                collect (pathname-name file))
        nil)

    ;; --- per-element overrides ---

    (configure '(:theme (:elements (:bar-workspace-empty "#123456"))))
    (ok "an element override takes effect"
        (theme-color :bar-workspace-empty) "#123456")
    (ok "an element override leaves its role alone"
        (theme-color :bar-muted) "#565f89")
    (configure '(:theme (:elements (:message-text :bar-recording))))
    (ok "an element override may name another element"
        (theme-color :message-text) "#f7768e")
    (fresh-config)
    (configure '(:theme (:elements (:prompt-title :prompt-title))))
    (ok "a self-referencing override falls back to the default theme"
        (theme-color :prompt-title) "#7aa2f7")
    (fresh-config)

    ;; --- (:theme "name") and theme files ---

    (let ((themes (merge-pathnames "themes/" scratch)))
      (write-scratch-theme themes "scratch-red"
        ";; a test theme
(:format 1 :name \"scratch-red\"
 :roles (:accent \"#ff0000\" :background \"#000000\"))")
      (write-scratch-theme themes "scratch-eval"
        "(:format 1 :name \"scratch-eval\" :roles (:accent #.(progn \"#00ff00\")))")
      (write-scratch-theme themes "scratch-bad"
        "(:format 1 :name \"scratch-bad\" :roles (:accent \"blue\"))")
      (let ((*theme-search-path* (list themes)))
        (configure '(:theme (:font-family "Scratch Mono")))
        (configure '(:theme "scratch-red"))
        (ok "(:theme \"name\") selects the theme"
            (config-getf '(:theme) :name) "scratch-red")
        (ok "(:theme \"name\") keeps the font settings"
            (config-getf '(:theme) :font-family) "Scratch Mono")
        (ok "a theme file sets a role" (theme-color :bar-accent) "#ff0000")
        (ok "a theme file's roles reach their elements"
            (theme-color :prompt-background) "#000000")
        (ok "a role the theme file omits comes from the default theme"
            (theme-color :bar-foreground) "#c0caf5")
        (ok "the default theme's element values stay with the default theme"
            (theme-color :launcher-prompt) "#ff0000")
        (configure '(:theme (:roles (:accent "#0000ff"))))
        (ok "a Lisp form in config overrides the theme file"
            (theme-color :bar-accent) "#0000ff")
        (fresh-config)
        (let ((warnings
                (collect-warnings
                  (lambda ()
                    (configure '(:theme "scratch-eval"))
                    (ok "a theme file cannot evaluate at read time"
                        (theme-color :bar-accent) "#7aa2f7")))))
          (ok "an unreadable theme warns" (plusp (length warnings)) t))
        (fresh-config)
        (collect-warnings
          (lambda ()
            (configure '(:theme "scratch-bad"))
            (ok "a theme with a malformed colour falls back"
                (theme-color :bar-accent) "#7aa2f7")))
        (fresh-config)
        (let ((warnings
                (collect-warnings
                  (lambda ()
                    (configure '(:theme "no-such-theme"))
                    (ok "an unknown theme falls back to the default"
                        (theme-color :bar-accent) "#7aa2f7")))))
          (ok "an unknown theme warns with its name"
              (and (some (lambda (w) (search "no-such-theme" w)) warnings) t)
              t))
        (fresh-config)
        (collect-warnings
          (lambda ()
            (configure '(:theme "../scratch-red"))
            (ok "a theme name cannot leave the theme directories"
                (theme-color :bar-accent) "#7aa2f7"))))
      (fresh-config)
      (configure '(:theme (:format 1
                           :roles (:accent "#00ffff")
                           :elements (:bar-hover "#ffff00"))))
      (ok "a full theme form in config sets roles"
          (theme-color :bar-accent) "#00ffff")
      (ok "a full theme form in config sets elements"
          (theme-color :bar-hover) "#ffff00"))
    (fresh-config)

    ;; --- deprecated colour keys ---

    (let ((warnings
            (collect-warnings
              (lambda ()
                (configure '(:bar (:accent "#abcdef")))))))
      (ok "an old key still sets its element"
          (theme-color :bar-accent) "#abcdef")
      (ok "an old key still reaches the elements derived from it"
          (theme-color :prompt-title) "#abcdef")
      (ok "an old key warns once, naming its replacement"
          (and (= 1 (length warnings))
               (search ":BAR-ACCENT" (string-upcase (first warnings)))
               t)
          t))
    (ok "an old key warns only once per session"
        (collect-warnings (lambda () (configure '(:bar (:accent "#abcdef")))))
        nil)
    (configure '(:theme (:elements (:bar-accent "#111111"))))
    (ok "a new element override wins over the old key"
        (theme-color :bar-accent) "#111111")
    (fresh-config)
    (loop for (path element) in '(((:theme :focused-border) :border-focused)
                                  ((:theme :unfocused-border) :border-unfocused)
                                  ((:bar :background) :bar-background)
                                  ((:bar :foreground) :bar-foreground)
                                  ((:bar :muted) :bar-muted)
                                  ((:bar :border-color) :bar-border)
                                  ((:bar :recording-color) :bar-recording)
                                  ((:launcher :background) :launcher-background)
                                  ((:launcher :accent) :launcher-match)
                                  ((:launcher :prompt-color) :launcher-prompt)
                                  ((:launcher :query-color) :launcher-query)
                                  ((:launcher :cursor-color) :launcher-cursor)
                                  ((:launcher :selection-background)
                                   :launcher-selection)
                                  ((:launcher :selected-color) :launcher-selected)
                                  ((:launcher :entry-color) :launcher-entry)
                                  ((:launcher :border-color) :launcher-border)
                                  ((:launcher :placeholder-color)
                                   :launcher-placeholder)
                                  ((:menus :background) :menu-background)
                                  ((:menus :item-color) :menu-item)
                                  ((:menus :hover-background)
                                   :menu-hover-background)
                                  ((:menus :hover-color) :menu-hover-foreground)
                                  ((:menus :border-color) :menu-border))
          do (fresh-config)
             (let ((warnings
                     (collect-warnings
                       (lambda ()
                         (configure (list (first path)
                                          (list (second path) "#010203")))))))
               (ok (format nil "old key ~S sets ~(~A~)" path element)
                   (theme-color element) "#010203")
               (ok (format nil "old key ~S names ~(~A~)" path element)
                   (and (some (lambda (w)
                                (search (string element) (string-upcase w)))
                              warnings)
                        t)
                   t)))
    (fresh-config)
    (loop for key in '(:accent :disabled)
          do (let ((warnings
                     (collect-warnings
                       (lambda () (configure (list :menus (list key "#010203")))))))
               (ok (format nil "dead key (:menus ~S) warns" key)
                   (= 1 (length warnings)) t)))
    (ok "the defaults carry no old colour keys"
        (deprecated-colour-keys-in (default-config))
        nil)
    (fresh-config)

    ;; --- the :underline part flag is gone ---

    (ok "bar-draw-module no longer reads :underline"
        (search ":underline"
                (uiop:read-file-string
                  (asdf:system-relative-pathname :rhizome "bar.lisp")))
        nil))

  (uiop:delete-directory-tree scratch :validate t :if-does-not-exist :ignore)
  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
