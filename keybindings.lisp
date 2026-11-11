;;; Keyboard bindings for Rhizome
;;;
;;; Uses the river-xkb-bindings-v1 protocol to register key
;;; bindings with the compositor. Bindings are defined with
;;; keysyms and modifiers, and trigger callbacks when pressed.
;;;
;;; Default bindings (StumpWM-style; MOD is the user modifier — Super
;;; by default, see *mod* in config.lisp):
;;;   Ctrl+T       - prefix key (StumpWM-style keychains)
;;;   MOD+Return   - launch terminal (foot)
;;;   MOD+Q        - close focused window
;;;   MOD+J        - focus next window
;;;   MOD+K        - focus previous window
;;;   MOD+Space    - cycle layout
;;;   MOD+1..9     - switch to workspace 1..9
;;;   MOD+Shift+1..9 - move focused window to workspace 1..9
;;;   MOD+E        - launch emacs
;;;   MOD+D        - application launcher (native layer-shell popup)
;;;   MOD+B        - toggle the status bar
;;;   Print        - screenshot of full screen (grim)
;;;   Shift+Print  - screenshot of selected region (grim + slurp)
;;;   MOD+W        - window list (pick, jump to window)
;;;   MOD+G        - workspace picker
;;;   MOD+X        - command palette
;;;   MOD+Shift+Q  - power off the machine
;;;   MOD+Shift+R  - reboot the machine
;;;   MOD+;        - live eval prompt (evaluate Lisp in the running WM)

(in-package #:rhizome)

(defvar *xkb-bindings-proxy* nil
  "The river_xkb_bindings_v1 proxy.")

(defvar *default-top-map*
  (make-keymap :name :default-top
               :doc "Rhizome's direct key bindings, built from *mod* and
*prefix-key-binding*.")
  "The default layer of direct bindings: key -> binding. Only the
default builders write it.")

(defvar *top-map*
  (make-keymap :name :top
               :doc "User direct key bindings."
               :parents (list *default-top-map*))
  "The user layer of direct bindings: key -> binding. River grabs the
first key of every binding in it. define-key writes it; *default-top-map* is its parent, so a user
binding shadows a default on the same key.")

(defvar *active-bindings* nil
  "List of active river_xkb_binding_v1 proxy objects.")

(defvar *grabbed-keys* (make-hash-table :test 'equal)
  "Maps (seat-proxy . key) to the river_xkb_binding_v1 grabbing it.")

(defvar *grabbed-key-modifiers* (make-hash-table :test 'equal)
  "Maps (seat-proxy . key) to the river modifier list its grab was made
with.")

(defvar *key-grabs-dirty* t
  "True while the river key grabs may not match the keymaps.")

(defvar *default-keymaps-built* nil
  "True once setup-default-keybindings has built the default keymaps.")

(defvar *default-top-map-mod* nil
  "The *mod* value *default-top-map* was last built with.")

;;; --- xkb keysym constants ---

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +xkb-key-Return+ 65293)    ; #xff0d
  (defconstant +xkb-key-Tab+     65289)    ; #xff09
  (defconstant +xkb-key-Space+   32)       ; #x0020
  (defconstant +xkb-key-Q+       113)      ; #x0071
  (defconstant +xkb-key-J+       106)      ; #x006a
  (defconstant +xkb-key-K+       107)      ; #x006b
  (defconstant +xkb-key-B+        98)       ; #x0062
  (defconstant +xkb-key-D+       100)      ; #x0064
  (defconstant +xkb-key-F+       102)      ; #x0066
  (defconstant +xkb-key-1+       49)       ; #x0031
  (defconstant +xkb-key-2+       50)       ; #x0032
  (defconstant +xkb-key-3+       51)       ; #x0033
  (defconstant +xkb-key-4+       52)       ; #x0034
  (defconstant +xkb-key-5+       53)       ; #x0035
  (defconstant +xkb-key-6+       54)       ; #x0036
  (defconstant +xkb-key-7+       55)       ; #x0037
  (defconstant +xkb-key-8+       56)       ; #x0038
  (defconstant +xkb-key-9+       57)       ; #x0039
  (defconstant +xkb-key-E+       101)      ; #x0065
  (defconstant +xkb-key-P+       112)      ; #x0070
  (defconstant +xkb-key-Print+   65377)    ; #xff61
  (defconstant +xkb-key-Semicolon+ 59)      ; #x003b
  (defconstant +xkb-key-R+       114)      ; #x0072
  (defconstant +xkb-key-W+       119)      ; #x0077
  (defconstant +xkb-key-T+       116)      ; #x0074
  (defconstant +xkb-key-G+       103)      ; #x0067
  (defconstant +xkb-key-X+       120)      ; #x0078
  (defconstant +xkb-key-Escape+  65307)   ; #xff1b
  (defconstant +xkb-key-Comma+   44)       ; #x002c
  (defconstant +xkb-key-Period+  46))      ; #x002e

;;; --- Modifier bitfield constants ---
;;; Wayflan expects bitfield enums as lists of keywords. The
;;; definitions (*mod*, *mod-alt*, *mod-super*, ...) live in
;;; config.lisp, which loads before both this file and seat.lisp.

;;; --- Keybinding registration ---

(defun setup-xkb-bindings (registry name interface version)
  "Bind to river_xkb_bindings_v1 if advertised."
  (when (string= interface "river_xkb_bindings_v1")
    (let ((iface-class (wl:find-interface-named interface)))
      (unless iface-class
        (format t "~&; Rhizome: xkb-bindings interface not found!~%")
        (return-from setup-xkb-bindings))
      (setf *xkb-bindings-proxy*
            (wl:wl-registry.bind registry name iface-class version))
      (format t "~&; Rhizome: bound river_xkb_bindings_v1 (version ~D)~%" version))))

(defvar *default-layers* (list (cons '*top-map* '*default-top-map*))
  "Alist of user keymap variable -> default keymap variable. While the
defaults are built, a write aimed at the user keymap lands in the
default one.")

(defun default-layer-target (keymap)
  "Return the keymap a write aimed at KEYMAP lands in: its default
layer while the defaults are built (see *default-layers*), KEYMAP
otherwise."
  (or (and *defining-default-bindings*
           (loop for (user . default) in *default-layers*
                 when (and (boundp user) (eq keymap (symbol-value user)))
                   return (symbol-value default)))
      keymap))

(defun define-key (keymap-or-keysym spec &optional binding &rest options)
  "Bind a key in a keymap. Two forms:

  (define-key KEYMAP SPEC BINDING &key pass-event doc)
  (define-key KEYSYM MODIFIERS CALLBACK)

KEYMAP is a keymap or a symbol whose value is one; SPEC is a key spec
string (a sequence binds through nested keymaps, see
bind-key-sequence), a key, or a bare keysym. BINDING is a function or
a symbol naming one, called with no arguments; with PASS-EVENT it is
called with the key event instead. It may also be a keymap, or a
symbol naming one, which a press enters as a key sequence. DOC
describes the binding for which-key and describe-key.

The second form binds KEYSYM with MODIFIERS (river or logical
keywords) in *top-map*. While the defaults are built, writes aimed at
*top-map* or *prefix-map* go to their default layers. A NIL binding
removes the key from the keymap's own table. BINDING may also be
:undefined (see undefine-key). Returns BINDING."
  (if (integerp keymap-or-keysym)
      (keymap-bind (default-layer-target *top-map*)
                   (make-key keymap-or-keysym spec)
                   binding)
      (destructuring-bind (&key pass-event doc) options
        (bind-key-sequence (default-layer-target
                            (or (binding-keymap keymap-or-keysym)
                                (error "~S is not a keymap" keymap-or-keysym)))
                           (ensure-key-sequence spec)
                           (if (and pass-event binding)
                               (event-binding binding)
                               binding)
                           doc))))

(defun undefine-key (keymap spec)
  "Bind SPEC in KEYMAP (a keymap or a symbol naming one) to :undefined
and return :undefined. A key undefined in a user keymap shadows the
default binding. At top level it is not grabbed, so the chord reaches
the focused application; inside a key sequence it is unbound.
(define-key keymap spec nil) undoes it."
  (define-key keymap spec :undefined)
  :undefined)

(defun key-binding (keysym modifiers)
  "Return the effective direct binding of KEYSYM with MODIFIERS (river
or logical keywords), the user layer first, or NIL when the key is
unbound or :undefined."
  (let ((binding (keymap-lookup *top-map* (make-key keysym modifiers))))
    (if (undefined-binding-p binding) nil binding)))

(defvar *current-key-event* nil
  "The key event of the binding being run; NIL outside one.")

(defvar *current-seat* nil
  "The seat whose key press runs the current binding, or NIL.")

(defvar *prefix-argument* nil
  "The numeric prefix argument of the current binding. Reserved: always
NIL until prefix arguments exist.")

(defvar *focused-window* nil
  "The window focused when the current binding's key was pressed, or
NIL.")

(defstruct (key-event (:constructor %make-key-event (key sequence seat))
                      (:copier nil))
  "A key press as a binding sees it: KEY, the keys of the SEQUENCE it
ends, and the SEAT it came from."
  (key nil :read-only t)
  (sequence nil :read-only t)
  (seat nil :read-only t))

(defun make-key-event (key &key sequence seat)
  "Return a key event for KEY, the last key of SEQUENCE (default: KEY
alone), pressed on SEAT."
  (%make-key-event key (or sequence (list key)) seat))

(defun call-binding (binding event)
  "Run BINDING for the key EVENT and return its values. An
event-binding's function is called with EVENT; a function or a symbol
naming one is called with no arguments. *current-key-event*,
*current-seat*, *prefix-argument* and *focused-window* are bound
around the call. An error is logged and shown with message, and the
call returns NIL."
  (let* ((seat (key-event-seat event))
         (*current-key-event* event)
         (*current-seat* seat)
         (*prefix-argument* nil)
         (*focused-window* (ignore-errors
                            (or (and seat (seat-focused-window seat))
                                (find-if #'window-focused-p (all-windows))))))
    (handler-case
        (if (event-binding-p binding)
            (funcall (event-binding-function binding) event)
            (funcall binding))
      (error (e)
        (format t "~&; Rhizome: error in ~A: ~A~%"
                (sequence-description (key-event-sequence event)) e)
        (message "error: ~A" e)
        nil))))

(defun press-key (key &optional seat)
  "Run what KEY (a key designator) is bound to in *top-map*, as a press
on SEAT: a keymap binding begins a key sequence on that keymap, any
other binding is called through call-binding. Returns true when KEY is
bound; an :undefined key runs nothing and returns NIL."
  (let* ((key (ensure-key key))
         (binding (keymap-lookup *top-map* key))
         (map (binding-keymap binding)))
    (cond ((undefined-binding-p binding)
           nil)
          (map
           (enter-prefix-mode map (key-description key) (list key) seat)
           t)
          (binding
           (call-binding binding (make-key-event key :seat seat))
           t))))

(defun mark-key-grabs-dirty ()
  "Mark the river key grabs as out of date with the keymaps. Only the
first mark after a sync calls request-manage-dirty, so a burst of
keymap edits asks for one manage sequence. Returns T."
  (unless *key-grabs-dirty*
    (setf *key-grabs-dirty* t)
    (request-manage-dirty))
  t)

(defun active-top-keymaps ()
  "Return the top-level keymaps whose first keys river grabs, highest
precedence first. The one place a context mechanism adds maps."
  (list *top-map*))

(defun key-grab-set (&optional (maps (active-top-keymaps)))
  "Return a fresh list, without duplicates, of the keys river grabs for
MAPS. A key is a candidate when some map in MAPS enumerates it. The
first map whose lookup of the key is non-NIL decides it, and an
:undefined binding there leaves the key out. Makes no river request."
  (let ((seen (make-hash-table :test 'equal))
        (keys '()))
    (dolist (map maps)
      (dolist (key (keymap-keys map))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (let ((binding (loop for m in maps
                               for b = (keymap-lookup m key)
                               when b return b)))
            (when (and binding (not (undefined-binding-p binding)))
              (push key keys))))))
    (nreverse keys)))

(defun grab-key (seat-proxy key river-modifiers)
  "Create and enable a river grab of KEY on SEAT-PROXY with
RIVER-MODIFIERS. A press runs press-key on KEY, which looks the binding
up at press time, so rebinding a grabbed key needs no new grab."
  (let ((binding (rwm:river-xkb-bindings-v1.get-xkb-binding
                  *xkb-bindings-proxy* seat-proxy (key-keysym key)
                  (or river-modifiers *mod-none*))))
    (push (lambda (event-name &rest args)
            (declare (ignore args))
            (when (eq event-name :pressed)
              (format t "~&; Rhizome: key pressed: ~A~%" (key-description key))
              (press-key key (gethash seat-proxy *seats*))))
          (wl:wl-proxy-hooks binding))
    (rwm:river-xkb-binding-v1.enable binding)
    (setf (gethash (cons seat-proxy key) *grabbed-keys*) binding)
    (push binding *active-bindings*)
    binding))

(defun ungrab-key (seat-proxy key)
  "Release the grab of KEY on SEAT-PROXY and forget its records. A live
grab is destroyed: destroy is legal outside a manage sequence and frees
the river object, so a key bound again gets a fresh grab. Returns NIL."
  (let* ((id (cons seat-proxy key))
         (grab (gethash id *grabbed-keys*)))
    (when (and grab (not (eq grab :unmapped)))
      (ignore-errors
        (rwm:river-xkb-binding-v1.destroy grab))
      (setf *active-bindings* (delete grab *active-bindings*)))
    (remhash id *grabbed-keys*)
    (remhash id *grabbed-key-modifiers*)
    nil))

(defun install-keybindings ()
  "Bring the river key grabs on every seat in line with key-grab-set,
sending only the difference. A grab whose key or seat is no longer
wanted is destroyed. A wanted key without a grab on its current river
modifiers is grabbed. A grab that already matches gets no request. A
key whose modifiers include one *modifier-map* leaves unmapped is not
grabbed; it is logged once and recorded as :unmapped. Must be called
during a manage sequence."
  (when (null *xkb-bindings-proxy*)
    (format t "~&; Rhizome: no xkb-bindings proxy, skipping keybindings~%")
    (return-from install-keybindings))
  (let ((desired (make-hash-table :test 'equal))
        (keys (key-grab-set))
        (stale '())
        (added 0)
        (released 0))
    ;; (seat-proxy . key) -> (river-modifiers unmapped-logical-modifiers)
    (loop for seat-proxy being the hash-keys of *seats*
          do (dolist (key keys)
               (setf (gethash (cons seat-proxy key) desired)
                     (multiple-value-list
                      (river-modifiers (key-modifiers key))))))
    (flet ((release (id)
             (unless (eq (gethash id *grabbed-keys*) :unmapped)
               (incf released))
             (ungrab-key (car id) (cdr id))))
      (maphash (lambda (id grab)
                 (declare (ignore grab))
                 (unless (nth-value 1 (gethash id desired))
                   (push id stale)))
               *grabbed-keys*)
      (mapc #'release stale)
      (maphash
       (lambda (id want)
         (destructuring-bind (river unmapped) want
           (let ((grab (gethash id *grabbed-keys*)))
             (unless (if unmapped
                         (eq grab :unmapped)
                         (and grab
                              (not (eq grab :unmapped))
                              (equal river (gethash id *grabbed-key-modifiers*))))
               (when grab
                 (release id))
               (cond (unmapped
                      (format t "~&; Rhizome: not grabbing ~A: ~{~(~A~)~^, ~} unmapped (see set-modifier)~%"
                              (key-description (cdr id)) unmapped)
                      (setf (gethash id *grabbed-keys*) :unmapped))
                     (t
                      (grab-key (car id) (cdr id) river)
                      (setf (gethash id *grabbed-key-modifiers*) river)
                      (incf added)))))))
       desired))
    (setf *key-grabs-dirty* nil)
    (when (or (plusp added) (plusp released))
      (format t "~&; Rhizome: key grabs: ~D added, ~D released on ~D seats~%"
              added released (hash-table-count *seats*)))))

(defun release-key-grabs ()
  "Destroy every river key grab, forget the records, and mark the grabs
dirty, so the next manage sequence grabs every key the keymaps bind."
  (dolist (binding *active-bindings*)
    (ignore-errors
      (rwm:river-xkb-binding-v1.destroy binding)))
  (setf *active-bindings* nil)
  (clrhash *grabbed-keys*)
  (clrhash *grabbed-key-modifiers*)
  (mark-key-grabs-dirty))

(defun set-modifier (logical river)
  "Map the logical modifier LOGICAL (:control :meta :super :hyper :alt
:shift) to the river modifier keyword RIVER (:ctrl :shift :mod1 :mod3
:mod4 :mod5), or unmap it with NIL, e.g. (set-modifier :hyper :mod3).
Grabs are re-created during the next manage sequence. Returns RIVER."
  (unless (member logical *logical-modifiers*)
    (error "~S is not a logical modifier; expected one of ~S"
           logical *logical-modifiers*))
  (unless (member river '(nil :ctrl :shift :mod1 :mod2 :mod3 :mod4 :mod5))
    (error "~S is not a river modifier" river))
  (setf (cdr (assoc logical *modifier-map*)) river)
  (release-key-grabs)
  (request-manage-dirty)
  river)

;;; --- Default keybindings ---

;;; --- Prefix key (define once, change at runtime) ---

(defvar *prefix-key-binding* (cons +xkb-key-T+ *mod-ctrl*)
  "The keysym and river modifiers of the key *default-top-map* binds to
*prefix-map* (set-prefix-key moves it).")

(defun parse-key-spec (spec)
  "Parse one key spec such as \"C-t\", \"C-M-s\", \"s-Space\" or
\"M-Return\" into (values MODIFIERS KEYSYM), MODIFIERS as river
keywords. A single letter is downcased. KEYSYM is NIL for an unknown
key name. Superseded by kbd, which keeps case and returns keys."
  (multiple-value-bind (mods name) (split-key-spec (string spec))
    (values (river-modifiers (canonical-modifiers mods))
            (key-name-keysym (if (= 1 (length name))
                                 (string-downcase name)
                                 name)))))

(defun set-prefix-key (spec)
  "Move the default binding of *prefix-map* to the key SPEC names, e.g.
(rhizome:set-prefix-key \"C-z\"). Rebuilds *default-top-map*; user
bindings, including any user binding of the old key, are untouched.
Current grabs are destroyed and re-created during the next manage
sequence, like set-user-mod."
  (multiple-value-bind (mods keysym) (parse-key-spec spec)
    (unless keysym
      (message "bad key spec: ~A (e.g. \"C-z\", \"s-t\")" spec)
      (return-from set-prefix-key))
    (setf *prefix-key-binding* (cons keysym mods))
    (build-default-top-map)
    (release-key-grabs)
    (request-manage-dirty)
    (message "prefix: ~A" spec)))

(defun bind-default-direct-keys ()
  "Bind rhizome's direct keys on *mod*, plus the prefix key. Called by
build-default-top-map, which directs the writes to *default-top-map*."

  ;; MOD+Return - launch terminal
  (define-key +xkb-key-Return+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching terminal~%")
      (uiop:launch-program '("foot"))))

  ;; MOD+, / MOD+. - scroll the ribbon viewport (niri-style)
  (define-key +xkb-key-Comma+ *mod*
    (lambda () (scroll-viewport-left)))
  (define-key +xkb-key-Period+ *mod*
    (lambda () (scroll-viewport-right)))

  ;; MOD+Q - close focused window
  (define-key +xkb-key-Q+ *mod*
    (lambda ()
      (let* ((seats (all-seats))
             (focused (and seats (seat-focused-window (first seats)))))
        (when focused
          (format t "~&; Rhizome: closing window ~A~%" (window-title focused))
          (close-window focused)
          (request-manage-dirty)))))

  ;; MOD+J - focus next
  (define-key +xkb-key-J+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-next (first outputs))))))

  ;; MOD+K - focus previous
  (define-key +xkb-key-K+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (focus-previous (first outputs))))))

  ;; MOD+Space - cycle layout
  (define-key +xkb-key-Space+ *mod*
    (lambda ()
      (let ((outputs (all-outputs)))
        (when outputs
          (let* ((out (first outputs))
                 (current (output-current-layout out))
                 (next (case current
                         (:master-stack :even)
                         (:even :monocle)
                         (:monocle :master-stack)
                         (otherwise :master-stack))))
            (switch-layout out next))))))

  ;; MOD+1..9 - switch to workspace N
  ;; MOD+Shift+1..9 - move focused window to workspace N
  ;; (MOD+Space cycles layouts; the old Alt+1/2/3 layout bindings
  ;; were replaced by workspaces)
  (loop for i from 1 to 9
        for keysym = (+ 48 i)
        do (let ((ws i))
             (define-key keysym *mod*
               (lambda ()
                 (switch-workspace ws)))
             (define-key keysym (append *mod* *mod-shift*)
               (lambda ()
                 (move-focused-window-to-workspace ws)))))

  ;; MOD+E - launch emacs
  (define-key +xkb-key-E+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching emacs~%")
      (uiop:launch-program '("emacs"))))

  ;; MOD+D - application launcher (native layer-shell popup)
  (define-key +xkb-key-D+ *mod*
    (lambda ()
      (format t "~&; Rhizome: launching application launcher~%")
      (handler-case
          (show-launcher)
        (error (e)
          (format t "~&; Rhizome: launcher error: ~A~%" e)))))

  ;; MOD+B - toggle the status bar
  (define-key +xkb-key-B+ *mod*
    (lambda ()
      (format t "~&; Rhizome: toggling bar~%")
      (handler-case
          (toggle-bar)
        (error (e)
          (format t "~&; Rhizome: bar error: ~A~%" e)))))

  ;; Ctrl+T - prefix key (StumpWM-style keychains, see prefix.lisp):
  ;;   C-t w windowlist | C-t g workspace picker | C-t x command palette
  ;;   C-t : eval       | C-t ! shell command    | C-t b toggle bar
  ;;   C-t n/p next/prev workspace | C-t q quit rhizome
  ;; The prefix key is an ordinary binding of a keymap (moved at
  ;; runtime by set-prefix-key); the symbol is resolved at press time
  (define-key (car *prefix-key-binding*) (cdr *prefix-key-binding*)
    '*prefix-map*)

  ;; MOD+W - window list (pick a window, jump to it)
  ;; MOD+G - workspace picker
  ;; MOD+X - command palette
  (define-key +xkb-key-W+ *mod*
    (lambda () (windowlist)))
  (define-key +xkb-key-G+ *mod*
    (lambda () (workspace-list)))
  (define-key +xkb-key-X+ *mod*
    (lambda () (command-palette)))

  ;; MOD+Shift+Q - power off the machine
  ;; MOD+Shift+R - reboot the machine
  (define-key +xkb-key-Q+ (append *mod* *mod-shift*)
    (lambda ()
      (shutdown-machine)))
  (define-key +xkb-key-R+ (append *mod* *mod-shift*)
    (lambda ()
      (reboot-machine)))
  ;; MOD+Shift+L - lock the session (see the (:lock ...) config)
  (define-key 76 (append *mod* *mod-shift*)    ; uppercase L
    (lambda ()
      (lock-screen)))
  ;; MOD+Shift+E - log out to the display manager (press twice)
  (define-key 101 (append *mod* *mod-shift*)  ; base keysym e — river matches unshifted
    (lambda ()
      (logout-confirm)))

  ;; MOD+; - live eval prompt (type a Lisp expression, Enter evaluates
  ;; it in the running WM image; Escape cancels)
  (define-key +xkb-key-Semicolon+ *mod*
    (lambda ()
      (format t "~&; Rhizome: opening eval prompt~%")
      (handler-case
          (show-eval-prompt)
        (error (e)
          (format t "~&; Rhizome: eval prompt error: ~A~%" e)))))

  ;; Print - screenshot of the full screen
  ;; Shift+Print - screenshot of a selected region (slurp)
  (define-key +xkb-key-Print+ *mod-none*
    (lambda ()
      (take-screenshot)))
  (define-key +xkb-key-Print+ *mod-shift*
    (lambda ()
      (take-screenshot :region t))))

(defun build-default-top-map ()
  "Rebuild *default-top-map* from *mod* and *prefix-key-binding*. The
user layer is untouched."
  (clear-keymap *default-top-map*)
  (let ((*defining-default-bindings* t))
    (bind-default-direct-keys))
  (setf *default-top-map-mod* *mod*)
  *default-top-map*)

(defun setup-default-keybindings ()
  "Build the default keymaps, *default-top-map* and the default prefix
map. Idempotent; user bindings are untouched."
  (build-default-top-map)
  (setup-default-prefix-bindings)
  (setf *default-keymaps-built* t))

(defun ensure-default-keybindings ()
  "Build the default keymaps when they are unbuilt, and rebuild
*default-top-map* when *mod* differs from the value it was built with.
Returns true when anything was rebuilt."
  (cond ((not *default-keymaps-built*)
         (setup-default-keybindings)
         t)
        ((not (equal *mod* *default-top-map-mod*))
         (build-default-top-map)
         t)))

;;; --- Application launcher ---

(defparameter *app-launcher-entries*
  '(("Foot (terminal)" . "foot")
    ("Emacs" . "emacs")
    ("Vim (in terminal)" . "foot -e vim")
    ("Htop (in terminal)" . "foot -e htop")
    ("Neovim (in terminal)" . "foot -e nvim")
    ("Bash (in terminal)" . "foot -e bash")
    ("MPV (media player)" . "mpv")
    ("Xterm" . "xterm")
    ("Zutty" . "zutty"))
  "Alist of (display-name . command) entries for the application launcher.")

(defun launch-application ()
  "Show an application launcher using fzf in a terminal.
Shows a curated list of Wayland-relevant applications."
  (setf (uiop:getenv "WAYLAND_DISPLAY") "wayland-1")
  (let ((names-file "/tmp/rhizome-launcher-names.txt")
        (cmds-file "/tmp/rhizome-launcher-cmds.txt")
        (result-file "/tmp/rhizome-launcher-result.txt"))
    ;; Write names and commands to temp files
    (with-open-file (f names-file :direction :output :if-exists :supersede)
      (dolist (entry *app-launcher-entries*)
        (format f "~A~%" (car entry))))
    (with-open-file (f cmds-file :direction :output :if-exists :supersede)
      (dolist (entry *app-launcher-entries*)
        (format f "~A|~A~%" (car entry) (cdr entry))))
    ;; Clear result file
    (with-open-file (f result-file :direction :output :if-exists :supersede)
      (format f ""))
    ;; Launch foot with fzf, writing the selection to result-file
    (uiop:launch-program
      (list "foot"
            "-e"
            "bash" "-c"
            (concatenate 'string
              "cat " names-file " | fzf --prompt='Run: ' > " result-file)))
    ;; Poll for the result file to be populated (terminal closed)
    (bordeaux-threads:make-thread
      (lambda ()
        (let ((cmd nil))
          ;; Wait up to 30 seconds for user to select
          (loop for i below 300
                until cmd
                do (sleep 0.1)
                   (setf cmd (handler-case
                                 (with-open-file (f result-file
                                                    :direction :input
                                                    :if-does-not-exist nil)
                                   (when (and f (> (file-length f) 0))
                                     (file-position f 0)
                                     (read-line f nil)))
                               (error () nil))))
          (when cmd
            (let ((entry (find cmd *app-launcher-entries*
                               :key #'car :test #'string=)))
              (when entry
                (format t "~&; Rhizome: launching ~A~%" (cdr entry))
                (uiop:launch-program
                  (list "bash" "-c" (cdr entry)))))))))))

;;; --- Integration with manage sequence ---

(defun install-keybindings-if-needed ()
  "Sync key grabs during a manage sequence: build the default keymaps
if needed, then send river the difference between the grabs held and
the grabs the keymaps call for (see install-keybindings). Never writes
the user layer."
  (ensure-default-keybindings)
  (when *xkb-bindings-proxy*
    (install-keybindings)))

(defun set-user-mod (mod)
  "Change the user modifier — the MOD in MOD+Return, MOD+1..9,
MOD+drag, ... — live. MOD is a friendly name: :alt, :super or :ctrl.
The default bindings move to the new modifier; user bindings keep
the modifiers they were made with. Current grabs are destroyed and
re-created during the next manage sequence (enable requests are
manage-sequence-only in river's protocol).

  (rhizome:set-user-mod :alt)     ; classic StumpWM-on-X11 feel
  (rhizome:set-user-mod :super)   ; the default"
  (let ((resolved (resolve-mod mod)))
    (unless resolved
      (message "unknown modifier: ~A (try :alt, :super, :ctrl)" mod)
      (return-from set-user-mod))
    (setf *mod* resolved)
    ;; Rebuild the default layer on the new modifier; the user layer
    ;; keeps its bindings, literal modifiers included
    (build-default-top-map)
    ;; Destroy the current grabs; install-keybindings-if-needed
    ;; re-creates them during the next manage sequence
    (release-key-grabs)
    ;; MOD+drag pointer grabs likewise
    (dolist (binding *pointer-bindings*)
      (ignore-errors
        (rwm:river-pointer-binding-v1.destroy binding)))
    (setf *pointer-bindings* nil
          *pointer-bindings-enabled* nil)
    (clrhash *seats-with-pointer-bindings*)
    (request-manage-dirty)
    (message "mod key: ~(~A~)" mod)))
