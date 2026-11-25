;;; Rhizome colour themes
;;;
;;; Every colour rhizome draws belongs to a named ELEMENT: the bar's
;;; background, the launcher's match highlight, the focused window
;;; border. Each element derives from a ROLE (background, accent,
;;; focus, ...) or from another element, so one role change reaches
;;; every element built on it, and any element can be set on its own.
;;;
;;; A theme is an inert plist, normally read from themes/NAME.theme:
;;;
;;;   (:format 1
;;;    :name "NAME"
;;;    :roles (:accent "#7aa2f7" :focus :accent ...)
;;;    :elements (:launcher-prompt "#8c64aa" ...))
;;;
;;; Each value is "#rrggbb" or the keyword of another role or element.
;;; The config selects and adjusts a theme under :theme:
;;;
;;;   (configure '(:theme "tokyo-night"))                    ; by name
;;;   (configure '(:theme (:roles (:accent "#ff9e64"))))     ; a role
;;;   (configure '(:theme (:elements (:bar-hover :muted))))  ; an element
;;;
;;; Colours resolve at draw time. For each name the first of these
;;; that gives a value wins:
;;;   1. the config's (:theme :elements ...);
;;;   2. the deprecated colour key the element replaces;
;;;   3. the theme's :elements;
;;;   4. the element's source in *theme-elements*;
;;;   5. the config's (:theme :roles ...);
;;;   6. the theme's :roles, completed from the default theme.

(in-package #:rhizome)

;;; --- Roles and elements ---

(defparameter *theme-roles*
  '((:background "Bar, prompt, and message surfaces.")
    (:foreground "Text on the background.")
    (:accent "The highlight colour.")
    (:muted "Secondary text on the background.")
    (:focus "The focused window's border and decorations.")
    (:unfocus "Unfocused window borders.")
    (:hairline "The bar's outline.")
    (:surface "Launcher and menu backgrounds.")
    (:surface-foreground "Text on the surface.")
    (:surface-border "Launcher and menu outlines.")
    (:selection "The selected launcher row and the hovered menu row.")
    (:selection-foreground "Text on the selection.")
    (:match "Typed text found inside a launcher entry.")
    (:error "Urgent states, such as the recording marker.")
    (:shadow "The base colour of a floating window's shadow."))
  "Every theme role as (ROLE DESCRIPTION).")

(defparameter *theme-elements*
  '((:border-focused :focus (:theme :focused-border))
    (:border-unfocused :unfocus (:theme :unfocused-border))
    (:decoration-tint :border-focused nil)
    (:decoration-shadow :shadow nil)
    (:bar-background :background (:bar :background))
    (:bar-foreground :foreground (:bar :foreground))
    (:bar-accent :accent (:bar :accent))
    (:bar-muted :muted (:bar :muted))
    (:bar-border :hairline (:bar :border-color))
    (:bar-recording :error (:bar :recording-color))
    (:bar-workspace-current :bar-accent nil)
    (:bar-workspace-occupied :bar-foreground nil)
    (:bar-workspace-empty :bar-muted nil)
    (:bar-clock-date :bar-muted nil)
    (:bar-clock-time :bar-accent nil)
    (:bar-hover :bar-accent nil)
    (:message-background :bar-background nil)
    (:message-text :bar-accent nil)
    (:message-border :bar-accent nil)
    (:prompt-background :bar-background nil)
    (:prompt-border :bar-accent nil)
    (:prompt-title :bar-accent nil)
    (:prompt-query :bar-foreground nil)
    (:prompt-candidate :bar-foreground nil)
    (:prompt-selection :bar-accent nil)
    (:eval-background :bar-background nil)
    (:eval-label :bar-accent nil)
    (:eval-query :bar-foreground nil)
    (:eval-result :bar-accent nil)
    (:launcher-background :surface (:launcher :background))
    (:launcher-border :surface-border (:launcher :border-color))
    (:launcher-prompt :accent (:launcher :prompt-color))
    (:launcher-query :selection-foreground (:launcher :query-color))
    (:launcher-cursor :surface-foreground (:launcher :cursor-color))
    (:launcher-placeholder :muted (:launcher :placeholder-color))
    (:launcher-entry :surface-foreground (:launcher :entry-color))
    (:launcher-selection :selection (:launcher :selection-background))
    (:launcher-selected :selection-foreground (:launcher :selected-color))
    (:launcher-match :match (:launcher :accent))
    (:menu-background :surface (:menus :background))
    (:menu-border :surface-border (:menus :border-color))
    (:menu-item :surface-foreground (:menus :item-color))
    (:menu-hover-background :selection (:menus :hover-background))
    (:menu-hover-foreground :selection-foreground (:menus :hover-color)))
  "Every element as (ELEMENT SOURCE OLD-KEY). SOURCE is the role or
element it derives from. OLD-KEY is the config path of the colour key
the element replaces, still read for one release, or NIL.")

(defparameter *dead-colour-keys*
  '((:menus :accent) (:menus :disabled))
  "Config paths of colour keys that never had a reader.")

(defun theme-role-names ()
  "The names of every theme role."
  (mapcar #'first *theme-roles*))

(defun theme-element-names ()
  "The names of every theme element."
  (mapcar #'first *theme-elements*))

;;; --- Colour values ---

(defun theme-colour-string-p (value)
  "True when VALUE is a \"#rrggbb\" string."
  (and (stringp value)
       (= (length value) 7)
       (char= (char value 0) #\#)
       (every (lambda (c) (digit-char-p c 16)) (subseq value 1))))

(defun normalize-theme-colour (value)
  "VALUE as \"#rrggbb\", or NIL. Accepts \"rrggbb\", as the colour
keys before themes did."
  (cond ((theme-colour-string-p value) (string-downcase value))
        ((and (stringp value) (= (length value) 6))
         (normalize-theme-colour (concatenate 'string "#" value)))
        (t nil)))

;;; --- Theme files ---

(define-condition theme-file-error (error)
  ((path :initarg :path :reader theme-file-error-path)
   (reason :initarg :reason :reader theme-file-error-reason))
  (:report (lambda (c stream)
             (format stream "theme file ~A: ~A"
                     (theme-file-error-path c)
                     (theme-file-error-reason c)))))

(defun theme-table-p (table)
  "True when TABLE is a plist of keywords to colours or keywords."
  (and (listp table)
       (evenp (length table))
       (loop for (key value) on table by #'cddr
             always (and (keywordp key)
                         (or (keywordp value)
                             (theme-colour-string-p value))))))

(defun read-theme-file (path)
  "Read and check the theme at PATH; return its plist. Reading interns
symbols only as keywords and never evaluates. Signals
THEME-FILE-ERROR when the file is unreadable, holds more than one
form, is not format 1, or holds a value that is neither \"#rrggbb\"
nor a keyword."
  (let ((theme
          (handler-case
              (with-open-file (in path :external-format :utf-8)
                (with-standard-io-syntax
                  (let ((*read-eval* nil)
                        (*package* (find-package :keyword)))
                    (let ((form (read in))
                          (more (read in nil in)))
                      (unless (eq more in)
                        (error 'theme-file-error
                               :path path
                               :reason "more than one form"))
                      form))))
            (theme-file-error (e) (error e))
            (error (e)
              (error 'theme-file-error :path path
                                       :reason (princ-to-string e))))))
    (flet ((bad (reason)
             (error 'theme-file-error :path path :reason reason)))
      (unless (and (listp theme) (evenp (length theme)))
        (bad "not a plist"))
      (unless (eql (getf theme :format) 1)
        (bad "not format 1"))
      (unless (theme-table-p (getf theme :roles))
        (bad ":roles is not a table of \"#rrggbb\" or keyword values"))
      (unless (theme-table-p (getf theme :elements))
        (bad ":elements is not a table of \"#rrggbb\" or keyword values"))
      (list :format 1
            :name (getf theme :name)
            :roles (copy-list (getf theme :roles))
            :elements (copy-list (getf theme :elements))))))

(defvar *shipped-theme-directory*
  (asdf:system-relative-pathname :rhizome "themes/")
  "The themes/ directory shipped beside rhizome's source.")

(defparameter *default-theme*
  (read-theme-file (merge-pathnames "tokyo-night.theme"
                                    *shipped-theme-directory*))
  "The default theme, read when rhizome loads, so a saved image keeps
it without the file.")

(defparameter *default-theme-name* "tokyo-night"
  "The theme used when the config names none.")

(defvar *theme-search-path* nil
  "Directories searched for NAME.theme, in order. NIL searches the
user's themes/ directory beside the config file, then the shipped
themes/ directory.")

(defun theme-directories ()
  "The directories searched for theme files, in order."
  (or *theme-search-path*
      (list (merge-pathnames "themes/"
                             (uiop:pathname-directory-pathname
                               (config-pathname)))
            *shipped-theme-directory*)))

(defun theme-name-p (name)
  "True when NAME can name a theme file: letters, digits, hyphens, and
underscores only, so a name never leaves the theme directories."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (c) (or (alphanumericp c) (find c "-_"))) name)))

(defun find-theme-file (name)
  "The first NAME.theme in THEME-DIRECTORIES, or NIL."
  (when (theme-name-p name)
    (loop for directory in (theme-directories)
          for path = (probe-file
                       (merge-pathnames (format nil "~A.theme" name)
                                        directory))
          when path return path)))

(defun complete-theme (theme)
  "THEME with every role it omits taken from the default theme."
  (list :format 1
        :name (getf theme :name)
        :roles (merge-plist (getf *default-theme* :roles)
                            (getf theme :roles))
        :elements (getf theme :elements)))

(defvar *theme-cache* (make-hash-table :test #'equal)
  "Loaded themes by name; a name that failed to load maps to the
default theme, so it warns once.")

(defvar *theme-cache-lock* (bt:make-recursive-lock "theme-cache")
  "Serializes reads and writes of *THEME-CACHE*. Recursive: a warning
handler may draw while LOAD-THEME holds it.")

(defun clear-theme-cache ()
  "Forget every loaded theme, so the next draw reads theme files again."
  (bt:with-recursive-lock-held (*theme-cache-lock*)
    (clrhash *theme-cache*)))

(defun load-theme (name)
  "The completed theme called NAME. Searches THEME-DIRECTORIES; the
default theme answers for its own name when no file shadows it. When
no theme loads, warns once and returns the default theme. Holds
*THEME-CACHE-LOCK*: the bar draws from more than one thread."
  (bt:with-recursive-lock-held (*theme-cache-lock*)
    (or (gethash name *theme-cache*)
        (setf (gethash name *theme-cache*)
              (let ((path (find-theme-file name)))
                (handler-case
                    (cond (path (complete-theme (read-theme-file path)))
                          ((equal name *default-theme-name*) *default-theme*)
                          (t (warn "theme ~S not found in ~{~A~^, ~}; ~
                                    using ~A"
                                   name (theme-directories)
                                   *default-theme-name*)
                             *default-theme*))
                  (theme-file-error (e)
                    (warn "~A; using ~A" e *default-theme-name*)
                    *default-theme*)))))))

(defun active-theme (&key (config *config*))
  "The theme CONFIG selects with (:theme :name ...)."
  (let ((name (config-getf '(:theme) :name :config config)))
    (if (stringp name)
        (load-theme name)
        *default-theme*)))

;;; --- Resolution ---

(defun theme-element-entry (name)
  "NAME's (ELEMENT SOURCE OLD-KEY) entry, or NIL for a role."
  (assoc name *theme-elements*))

(defun old-colour-key-value (name config)
  "The colour CONFIG sets under the deprecated key that element NAME replaces."
  (let ((path (third (theme-element-entry name))))
    (when path
      (let ((value (config-get path :config config)))
        (and (stringp value) value)))))

(defun resolve-theme-colour (name config theme depth)
  "NAME's colour under THEME and the overrides in CONFIG (NIL for
none), following keyword values at most DEPTH more steps. NIL when
nothing gives NAME a colour."
  (when (plusp depth)
    (let ((value
            (or (and config
                     (getf (config-getf '(:theme) :elements :config config)
                           name))
                (and config (old-colour-key-value name config))
                (getf (getf theme :elements) name)
                (second (theme-element-entry name))
                (and config
                     (getf (config-getf '(:theme) :roles :config config)
                           name))
                (getf (getf theme :roles) name))))
      (if (keywordp value)
          (resolve-theme-colour value config theme (1- depth))
          (normalize-theme-colour value)))))

(defun theme-color (name &key (config *config*))
  "The \"#rrggbb\" colour of the element or role NAME at this moment.
When the config's overrides leave NAME without a colour (a cycle, an
unknown name, a malformed value), returns NAME's colour under the
default theme, and #ffffff when that fails too."
  (or (resolve-theme-colour name config (active-theme :config config) 16)
      (resolve-theme-colour name nil *default-theme* 16)
      "#ffffff"))

(defun theme-rgb (name &key (config *config*))
  "NAME's colour as (values r g b)."
  (parse-hex-color (theme-color name :config config)))

(defun theme-border-rgb (focused-p)
  "The focused or unfocused window border colour as (values r g b)."
  (theme-rgb (if focused-p :border-focused :border-unfocused)))

(defun theme-border-rgba32 (focused-p)
  "Focused/unfocused theme border colour, opaque, as the four 32-bit
set_borders values (values r g b a). See BORDER-RGBA32."
  (multiple-value-bind (r g b) (theme-border-rgb focused-p)
    (border-rgba32 r g b)))

;;; --- Deprecated colour keys ---

(define-condition deprecated-colour-key (simple-warning) ()
  (:documentation "A config set a colour key that themes replace."))

(defun deprecated-colour-keys-in (plist)
  "Every deprecated or dead colour key PLIST sets, as
(PATH . ELEMENT); ELEMENT is NIL for a key that never had an effect."
  (loop for (path . element)
          in (append (loop for (element nil old) in *theme-elements*
                           when old collect (cons old element))
                     (mapcar #'list *dead-colour-keys*))
        when (nth-value 1 (config-get path :config plist))
          collect (cons path element)))

(defvar *warned-colour-keys* (make-hash-table :test #'equal)
  "Deprecated colour keys already warned about in this process.")

(defun warn-deprecated-colour-keys (plist)
  "Warn once per process for each deprecated colour key PLIST sets,
naming its replacement."
  (loop for (path . element) in (deprecated-colour-keys-in plist)
        unless (gethash path *warned-colour-keys*)
          do (setf (gethash path *warned-colour-keys*) t)
             (if element
                 (warn 'deprecated-colour-key
                       :format-control "~(~S~) is deprecated; use ~
                                        (:theme (:elements (~(~S~) ~S))) ~
                                        instead"
                       :format-arguments (list path element
                                               (config-get path
                                                           :config plist)))
                 (warn 'deprecated-colour-key
                       :format-control "~(~S~) has no effect; remove it"
                       :format-arguments (list path)))))
