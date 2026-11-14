;;; Keys for Rhizome
;;;
;;; A key is (KEYSYM . MODIFIERS): an xkb keysym and a canonical list
;;; of logical modifiers drawn from :control :meta :super :hyper :alt
;;; :shift, in that order. A river modifier keyword with no logical
;;; name in *modifier-map* (for example :mod3 while :hyper is unmapped)
;;; is kept as itself, after the logical ones.
;;;
;;; Key specs use the Emacs/StumpWM syntax: "C-M-s-x", "H-a",
;;; "A-Return", "s-;", and space-separated sequences such as "s-x C-f".
;;; C control, M meta, s super, H hyper, A alt, S shift. A single
;;; character names its own keysym with case kept, so "T" is the
;;; keysym of an uppercase T: the key Shift+t produces.

(in-package #:rhizome)

(defparameter *logical-modifiers* '(:control :meta :super :hyper :alt :shift)
  "The logical modifiers, in canonical order.")

(defparameter *modifier-prefixes*
  '((#\C . :control) (#\M . :meta) (#\s . :super)
    (#\H . :hyper) (#\A . :alt) (#\S . :shift))
  "Key spec prefix character -> logical modifier.")

(defvar *modifier-map*
  (list (cons :control :ctrl)
        (cons :meta :mod1)
        (cons :super :mod4)
        (cons :hyper nil)
        (cons :alt nil)
        (cons :shift :shift))
  "Alist of logical modifier -> river modifier keyword (:ctrl :shift
:mod1 :mod3 :mod4 :mod5), or NIL when the logical modifier is unmapped.
Which river modifier carries Hyper or Alt depends on the xkb options
in force; set them with set-modifier.")

(defparameter *key-names*
  '(("Space" . 32) ("SPC" . 32)
    ("Return" . 65293) ("RET" . 65293) ("Enter" . 65293)
    ("Tab" . 65289) ("TAB" . 65289)
    ("ISO_Left_Tab" . 65056)
    ("Esc" . 65307) ("Escape" . 65307) ("ESC" . 65307)
    ("Backspace" . 65288) ("BackSpace" . 65288) ("DEL" . 65288)
    ("Delete" . 65535) ("Insert" . 65379)
    ("Home" . 65360) ("End" . 65367)
    ("Prior" . 65365) ("PageUp" . 65365)
    ("Next" . 65366) ("PageDown" . 65366)
    ("Left" . 65361) ("Up" . 65362) ("Right" . 65363) ("Down" . 65364)
    ("Print" . 65377) ("Menu" . 65383)
    ("F1" . 65470) ("F2" . 65471) ("F3" . 65472) ("F4" . 65473)
    ("F5" . 65474) ("F6" . 65475) ("F7" . 65476) ("F8" . 65477)
    ("F9" . 65478) ("F10" . 65479) ("F11" . 65480) ("F12" . 65481)
    ;; XF86 keysym values from xkbcommon-keysyms.h
    ("XF86MonBrightnessUp" . #x1008ff02)
    ("XF86MonBrightnessDown" . #x1008ff03)
    ("XF86AudioLowerVolume" . #x1008ff11)
    ("XF86AudioMute" . #x1008ff12)
    ("XF86AudioRaiseVolume" . #x1008ff13)
    ("XF86AudioMicMute" . #x1008ffb2))
  "Key name -> keysym for keys a single character cannot name. The
first name listed for a keysym is the one key-description prints.")

;;; --- Modifiers ---

(defun logical-modifier (keyword)
  "Return the canonical modifier KEYWORD denotes. A logical modifier
returns itself; a river keyword returns the first logical modifier
*modifier-map* maps to it, or itself when none does; :none returns NIL."
  (cond ((eq keyword :none) nil)
        ((member keyword *logical-modifiers*) keyword)
        ((car (find keyword *modifier-map* :key #'cdr)))
        (t keyword)))

(defun modifier< (a b)
  "Canonical order: logical modifiers in *logical-modifiers* order,
then other keywords by name."
  (let ((pa (position a *logical-modifiers*))
        (pb (position b *logical-modifiers*)))
    (cond ((and pa pb) (< pa pb))
          (pa t)
          (pb nil)
          (t (string< a b)))))

(defun canonical-modifiers (modifiers)
  "Return MODIFIERS, logical or river keywords in any order, as a fresh
canonical modifier list."
  (sort (remove-duplicates (remove nil (mapcar #'logical-modifier modifiers)))
        #'modifier<))

(defun river-modifiers (modifiers)
  "Return the river modifier keywords for the canonical MODIFIERS. The
second value lists the logical modifiers *modifier-map* leaves
unmapped; they contribute nothing to the first."
  (let ((river '()) (unmapped '()))
    (dolist (m modifiers)
      (let ((r (if (member m *logical-modifiers*)
                   (cdr (assoc m *modifier-map*))
                   m)))
        (if r
            (pushnew r river)
            (push m unmapped))))
    (values (nreverse river) (nreverse unmapped))))

;;; --- Keys ---

(defun make-key (keysym &optional modifiers)
  "Return the key for KEYSYM with MODIFIERS (logical or river keywords)."
  (cons keysym (canonical-modifiers modifiers)))

(defun key-keysym (key)
  "The keysym of KEY."
  (car key))

(defun key-modifiers (key)
  "The canonical modifier list of KEY."
  (cdr key))

(defun key-p (object)
  "True when OBJECT is a key: a keysym consed onto a list of keywords."
  (and (consp object)
       (integerp (car object))
       (listp (cdr object))
       (every #'keywordp (cdr object))))

(defun key-name-keysym (name)
  "Return the keysym NAME denotes: a single character's code, case kept,
or a *key-names* entry matched without regard to case. NIL otherwise."
  (cond ((= 1 (length name)) (char-code (char name 0)))
        ((cdr (assoc name *key-names* :test #'string-equal)))))

(defun split-key-spec (token)
  "Split one key spec TOKEN into its logical modifiers and key name."
  (let ((mods '()) (rest token))
    (loop while (and (> (length rest) 2)
                     (char= (char rest 1) #\-)
                     (assoc (char rest 0) *modifier-prefixes*))
          do (push (cdr (assoc (char rest 0) *modifier-prefixes*)) mods)
             (setf rest (subseq rest 2)))
    (values (nreverse mods) rest)))

(defun parse-key (token)
  "Parse one key spec TOKEN, such as \"C-M-x\" or \"s-Return\", into a
key. Returns NIL when the key name is unknown."
  (multiple-value-bind (mods name) (split-key-spec token)
    (let ((keysym (key-name-keysym name)))
      (and keysym (make-key keysym mods)))))

(defun kbd (spec)
  "Parse SPEC, one or more space-separated key specs, into a list of
keys. Signals an error naming the first token that is not a key."
  (let ((tokens (loop with s = (string spec)
                      for start = (position #\Space s :test-not #'char=)
                        then (position #\Space s :start end :test-not #'char=)
                      for end = (and start (or (position #\Space s :start start)
                                               (length s)))
                      while start
                      collect (subseq s start end))))
    (unless tokens
      (error "Empty key spec ~S" spec))
    (loop for token in tokens
          collect (or (parse-key token)
                      (error "~S in ~S is not a key spec" token spec)))))

(defun ensure-key (designator)
  "Return the key DESIGNATOR names: a key, a bare keysym (no
modifiers), or a key spec string naming exactly one key."
  (etypecase designator
    (integer (make-key designator))
    (string (let ((keys (kbd designator)))
              (unless (null (rest keys))
                (error "~S names a key sequence, not one key" designator))
              (first keys)))
    (cons (make-key (car designator) (cdr designator)))))

(defun ensure-key-sequence (designator)
  "Return the list of keys DESIGNATOR names: a key spec string, a key,
a bare keysym, or a list of key designators."
  (cond ((stringp designator) (kbd designator))
        ((or (integerp designator) (key-p designator))
         (list (ensure-key designator)))
        (t (mapcar #'ensure-key designator))))

;;; --- Printing ---

(defun keysym-character (keysym)
  "Return the character KEYSYM stands for in the XKB keysym encoding, or
NIL. A keysym from #x20 through #x20ff is the character with that code:
exact for Latin-1 and the euro sign, approximate for the other legacy
keysyms #x100-#x20ff. A Unicode keysym from #x01000100 through
#x0110ffff is the character with code (- keysym #x01000000). Function,
modifier, and vendor keysyms return NIL. Never signals."
  (cond ((<= #x20 keysym #x20ff) (code-char keysym))
        ((<= #x01000100 keysym #x0110ffff) (code-char (- keysym #x01000000)))
        (t nil)))

(defun keysym-name (keysym)
  "A display name for KEYSYM: its *key-names* name, the graphic
character it stands for (see keysym-character), or #N."
  (or (car (find keysym *key-names* :key #'cdr))
      (let ((c (keysym-character keysym)))
        (if (and c (graphic-char-p c))
            (string c)
            (format nil "#~D" keysym)))))

(defun key-description (key)
  "KEY in key spec syntax, e.g. \"C-M-x\"."
  (with-output-to-string (s)
    (dolist (m (key-modifiers key))
      (let ((prefix (car (rassoc m *modifier-prefixes*))))
        (if prefix
            (format s "~C-" prefix)
            (format s "~(~A~)-" m))))
    (write-string (keysym-name (key-keysym key)) s)))

(defun sequence-description (keys)
  "The key list KEYS in key spec syntax, space-separated."
  (format nil "~{~A~^ ~}" (mapcar #'key-description keys)))
