;;; Pointer-driven window menus
;;;
;;; MOD+Middle-click on a window opens a context menu at the
;;; pointer: Minimize, Float/Tile, Maximize, Fullscreen, Close,
;;; and Move-to-workspace entries. Pointer-driven like a real
;;; desktop menu — hover highlights, click to select, dismiss on
;;; Escape or by leaving the menu — with Escape-key support via the
;;; menu's exclusive keyboard grab. All actions route through the
;;; existing (sequence-queued) command paths: focus and close are
;;; flushed from the manage pass, so the menu can never violate
;;; river's sequence rules.
;;;
;;; Surface: the launcher pattern — a zwlr top-layer surface with
;;; an ARGB shm buffer, positioned at the pointer via margins.
;;; Input: a wl_pointer from the WM's wl_seat (the same seat that
;;; feeds the launcher its keyboard events), giving motion (hover)
;;; and button (select) events on the menu surface.

(in-package #:rhizome)

;;; --- State ---

(defvar *menu-active* nil
  "The context menu is open.")
(defvar *menu-window* nil
  "The window the menu acts on.")
(defvar *menu-items* nil
  "((LABEL . ACTION-FN) ...) — ACTION-FN takes the window.")
(defvar *menu-hover* -1
  "Hovered item index, or -1.")
(defvar *menu-pointer* nil
  "The wl_pointer driving the menu (released on close).")
(defvar *menu-pointer-seat* nil
  "The seat whose pointer is in use.")
(defvar *menu-surface* nil
  "The menu's wl_surface.")
(defvar *menu-layer-surface* nil)
(defvar *menu-shm-ptr* nil)
(defvar *menu-shm-fd* nil)
(defvar *menu-shm-size* 0)
(defvar *menu-wl-pool* nil)
(defvar *menu-wl-buffer* nil)
(defvar *menu-width* 240)
(defvar *menu-height* 200)
(defvar *menu-row-height* 26)
(defvar *menu-padding* 8)

(defun menu-color (key)
  "A menu color as (values r g b), from (:menus KEY)."
  (parse-hex-color
    (config-getf '(:menus) key
                 :default (case key
                            (:background "#1f1633")
                            (:item-color "#c9b8e8")
                            (:hover-background "#4b3a78")
                            (:hover-color "#f0e6ff")
                            (:accent "#7aa2f7")
                            (:disabled "#565f89")
                            (otherwise "#ffffff")))))

;;; --- Items ---

(defun menu-item-list (win)
  "The context-menu entries for WIN: (LABEL . ACTION-FN)."
  (let ((cur (and win (window-workspace win))))
    (append
      (list (cons (if (window-minimized-p win) "Restore" "Minimize")
                  (lambda (w)
                    (setf (window-minimized-p w)
                          (not (window-minimized-p w)))
                    (setf (window-hidden-p w)
                          (window-minimized-p w))
                    (request-manage-dirty)))
            (cons (if (window-floating-p win) "Tile" "Float")
                  (lambda (w) (toggle-float w)))
            (cons (if (window-maximized-p win) "Unmaximize" "Maximize")
                  (lambda (w) (toggle-maximize w)))
            (cons (if (window-fullscreen-p win) "Exit fullscreen" "Fullscreen")
                  (lambda (w) (toggle-fullscreen w)))
            (cons "Close" (lambda (w) (close-window w))))
      ;; Move-to-workspace entries, skipping the window's own
      (loop for n from 1 to *workspace-count*
            unless (eql n cur)
              collect (cons (format nil "Move to workspace ~D" n)
                            (lambda (w)
                              (assign-window-workspace w n)
                              (setf (window-minimized-p w) nil)
                              (request-manage-dirty)))))))

;;; --- Geometry helpers ---

(defun menu-px () (or (and *menu-pointer-seat*
                           (seat-pointer-x *menu-pointer-seat*))
                      100))
(defun menu-py () (or (and *menu-pointer-seat*
                           (seat-pointer-y *menu-pointer-seat*))
                      100))

(defun menu-clamp-position ()
  "Where to place the menu so it fits on its output."
  (let* ((out (and *menu-window* (window-output *menu-window*)))
         (ow (or (and out (output-width out)) 1920))
         (oh (or (and out (output-height out)) 1080))
         (x (min (max 0 (- (menu-px) *menu-padding*)) (- ow *menu-width* *menu-padding*)))
         (y (min (max 0 (- (menu-py) *menu-padding*)) (- oh *menu-height* *menu-padding*))))
    (values (max 0 x) (max 0 y))))

(defun menu-row-at (x y)
  "The item index at surface-local (X, Y), or -1."
  (declare (ignore x))
  (let* ((top *menu-padding*)
         (idx (floor (- y top) *menu-row-height*)))
    (if (and (>= idx 0) (< idx (length *menu-items*)))
        idx
        -1)))

;;; --- Rendering ---

(defun menu-ensure-shm (width height)
  "The menu's ARGB shm buffer (premultiplied alpha)."
  (let ((needed (* width height 4)))
    (cond
      ((and *menu-shm-ptr*
            (not (cffi:null-pointer-p *menu-shm-ptr*))
            (= *menu-shm-size* needed))
       t)
      (t
       (menu-destroy-shm)
       (let ((fd (cffi:foreign-funcall "memfd_create" :string "menu" :int 0 :int)))
         (when (minusp fd) (return-from menu-ensure-shm nil))
         (cffi:foreign-funcall "ftruncate" :int fd :long needed :int)
         (let ((ptr (cffi:foreign-funcall "mmap" :pointer (cffi:null-pointer)
                                         :unsigned-long needed :int 3 :int 1
                                         :int fd :long 0 :pointer)))
           (when (cffi:null-pointer-p ptr)
             (cffi:foreign-funcall "close" :int fd :int)
             (return-from menu-ensure-shm nil))
           (setf *menu-shm-fd* fd
                 *menu-shm-ptr* ptr
                 *menu-shm-size* needed
                 *menu-wl-pool* (wl:wl-shm.create-pool *shm* fd needed)
                 *menu-wl-buffer* (wl:wl-shm-pool.create-buffer
                                   *menu-wl-pool* 0 width height
                                   (* width 4) :argb8888))))))))

(defun menu-destroy-shm ()
  (when *menu-wl-buffer*
    (wl:wl-buffer.destroy *menu-wl-buffer*)
    (setf *menu-wl-buffer* nil))
  (when *menu-wl-pool*
    (wl:wl-shm-pool.destroy *menu-wl-pool*)
    (setf *menu-wl-pool* nil))
  (when (and *menu-shm-ptr* (not (cffi:null-pointer-p *menu-shm-ptr*)))
    (cffi:foreign-funcall "munmap" :pointer *menu-shm-ptr*
                          :unsigned-long *menu-shm-size* :int)
    (setf *menu-shm-ptr* nil))
  (when *menu-shm-fd*
    (cffi:foreign-funcall "close" :int *menu-shm-fd* :int)
    (setf *menu-shm-fd* nil))
  (setf *menu-shm-size* 0))

(defun menu-render ()
  "Render the menu into its shm buffer (premultiplied ARGB): the
hovered row fills with the hover color and carries the accent
edge; every row's label draws through the font module."
  (let ((ptr *menu-shm-ptr*))
    (when (or (null ptr) (cffi:null-pointer-p ptr))
      (return-from menu-render))
    (font-ensure-initialized)
    (let* ((w *menu-width*) (h *menu-height*)
           (sa (round (* 255 (min 1.0 (max 0.0
                    (config-getf '(:menus) :opacity :default 0.95))))))
           (bg-pixel (multiple-value-bind (r g b) (menu-color :background)
                       (logior (ash sa 24)
                               (ash (round (* r sa) 255) 16)
                               (ash (round (* g sa) 255) 8)
                               (round (* b sa) 255))))
           (hover-pixel (multiple-value-bind (r g b) (menu-color :hover-background)
                          (logior (ash sa 24)
                                  (ash (round (* r sa) 255) 16)
                                  (ash (round (* g sa) 255) 8)
                                  (round (* b sa) 255)))))
      ;; Background (or hover fill per row) — through the shared
      ;; bounds-checked helper: this exact loop was transposed once
      ;; (y + W·x), a latent segfault for wide menus
      (loop for y below h
            for row = (menu-row-at 0 y)
            for pixel = (if (= row *menu-hover*) hover-pixel bg-pixel)
            do (loop for x below w
                     do (put-pixel-argb ptr w h x y pixel)))
      ;; Accent edge on the hovered row
      (when (>= *menu-hover* 0)
        (multiple-value-bind (r g b) (menu-color :accent)
          (let* ((y0 (+ *menu-padding* (* *menu-hover* *menu-row-height*)))
                 (edge-pixel (logior (ash sa 24)
                                     (ash (round (* r sa) 255) 16)
                                     (ash (round (* g sa) 255) 8)
                                     (round (* b sa) 255))))
            (loop for dy below *menu-row-height*
                  for py = (+ y0 dy)
                  while (< py h) do
              (loop for dx below 3
                    for px = (+ *menu-padding* dx)
                    while (< px w) do
                (put-pixel-argb ptr w h px py edge-pixel))))))
      ;; Labels
      (multiple-value-bind (ir ig ib) (menu-color :item-color)
        (multiple-value-bind (hvr hvg hvb) (menu-color :hover-color)
          (loop for i from 0 below (length *menu-items*)
                for y = (+ *menu-padding* (* i *menu-row-height*))
                for label = (first (nth i *menu-items*))
                do (multiple-value-bind (r g b)
                       (if (= i *menu-hover*)
                           (values hvr hvg hvb)
                           (values ir ig ib))
                     (font-draw-text ptr w h label
                                     (+ *menu-padding* 8)
                                     (+ y 4)
                                     r g b :alpha sa))))))))

(defun menu-redraw ()
  "Render and commit."
  (unless *menu-active*
    (return-from menu-redraw))
  (unless (menu-ensure-shm *menu-width* *menu-height*)
    (format t "~&; Rhizome: menu shm failed~%")
    (return-from menu-redraw))
  (menu-render)
  (wl:wl-surface.attach *menu-surface* *menu-wl-buffer* 0 0)
  (wl:wl-surface.damage *menu-surface* 0 0 *menu-width* *menu-height*)
  (wl:wl-surface.commit *menu-surface*))

;;; --- Pointer events ---

(defun menu-setup-pointer (seat)
  "Get a wl_pointer from the WM's wl_seat and drive the menu with
motion (hover) and button (select) events."
  (let ((wl-seat (and seat (seat-wl-seat-proxy seat))))
    (unless wl-seat
      (format t "~&; Rhizome: menu: no wl_seat to get a pointer from~%")
      (return-from menu-setup-pointer nil))
    (let ((pointer (wl:wl-seat.get-pointer wl-seat)))
      (setf *menu-pointer* pointer
            *menu-pointer-seat* seat)
      (push (lambda (event-name &rest args)
              (case event-name
                (:enter nil)
                (:leave
                 ;; Leaving the menu dismisses it (click-outside)
                 (menu-close))
                (:motion
                 (destructuring-bind (time sx sy) args
                   (declare (ignore time))
                   (let ((hover (menu-row-at sx sy)))
                     (unless (eql hover *menu-hover*)
                       (setf *menu-hover* hover)
                       (menu-redraw)))))
                (:button
                 (destructuring-bind (serial time button state) args
                   (declare (ignore serial time))
                   (when (and (eq state :pressed) (= button 272))
                     ;; Left click: run the hovered item
                     (let ((idx *menu-hover*))
                       (if (and (>= idx 0) (< idx (length *menu-items*)))
                           (let ((action (rest (nth idx *menu-items*))))
                             (menu-close)
                             (when action
                               (funcall action *menu-window*)))
                           (menu-close))))))
                (t nil)))
            (wl:wl-proxy-hooks pointer))
      t)))

;;; --- Open / close ---

(defun menu-close ()
  "Close the context menu."
  (unless *menu-active*
    (return-from menu-close))
  (setf *menu-active* nil)
  (handler-case
      (progn
        (when *menu-pointer*
          (wl:wl-pointer.release *menu-pointer*)
          (setf *menu-pointer* nil))
        (when *menu-layer-surface*
          (rwm:zwlr-layer-surface-v1.destroy *menu-layer-surface*)
          (setf *menu-layer-surface* nil))
        (when *menu-surface*
          (wl:wl-surface.destroy *menu-surface*)
          (setf *menu-surface* nil))
        (menu-destroy-shm))
    (error (e)
      (format t "~&; Rhizome: menu close error: ~A~%" e)))
  (setf *menu-window* nil
        *menu-items* nil
        *menu-hover* -1
        *menu-pointer-seat* nil))

(defun menu-open (win seat)
  "Open the context menu for WIN at the pointer."
  (when *menu-active*
    (menu-close))
  (unless (and win (gethash (window-proxy win) *windows*)
              *compositor* *shm* *layer-shell*)
    (return-from menu-open))
  (setf *menu-window* win
        *menu-items* (menu-item-list win)
        *menu-hover* -1
        *menu-width* 240
        *menu-height* (+ (* 2 *menu-padding*)
                         (* *menu-row-height* (length *menu-items*))))
  (setf *menu-surface* (wl:wl-compositor.create-surface *compositor*)
        *menu-layer-surface*
        (rwm:zwlr-layer-shell-v1.get-layer-surface
          *layer-shell* *menu-surface* nil :top "rhizome-menu"))
  (push (lambda (event-name &rest args)
          (case event-name
            (:configure
             (destructuring-bind (serial width height) args
               (declare (ignore width height))
               (rwm:zwlr-layer-surface-v1.ack-configure
                 *menu-layer-surface* serial)
               (setf *menu-active* t)
               (menu-redraw)))
            (:closed (menu-close))))
        (wl:wl-proxy-hooks *menu-layer-surface*))
  (rwm:zwlr-layer-surface-v1.set-size *menu-layer-surface*
                                       *menu-width* *menu-height*)
  ;; Anchor top-left, positioned at the pointer via margins
  (rwm:zwlr-layer-surface-v1.set-anchor *menu-layer-surface*
                                        '(:top :left))
  (multiple-value-bind (x y) (menu-clamp-position)
    (rwm:zwlr-layer-surface-v1.set-margin *menu-layer-surface*
                                          y 0 0 x))
  (rwm:zwlr-layer-surface-v1.set-keyboard-interactivity
    *menu-layer-surface* :exclusive)
  ;; The pointer for hover/select (before the surface is committed
  ;; so no events are missed)
  (menu-setup-pointer seat)
  (wl:wl-surface.commit *menu-surface*))

(defun menu-handle-key (keysym)
  "Keyboard support for the open menu: Escape closes, arrows move
the hover, Enter selects."
  (case keysym
    (65307 (menu-close) t)
    (65362 ;; up
     (setf *menu-hover*
           (max 0 (1- (max 0 *menu-hover*))))
     (menu-redraw) t)
    (65364 ;; down
     (setf *menu-hover*
           (if (< *menu-hover* 0)
               0
               (min (1- (length *menu-items*)) (1+ *menu-hover*))))
     (menu-redraw) t)
    (65293 ;; enter
     (let ((idx *menu-hover*))
       (when (and (>= idx 0) (< idx (length *menu-items*)))
         (let ((action (rest (nth idx *menu-items*))))
           (menu-close)
           (when action (funcall action *menu-window*))))
       t))
    (t nil)))

(defun menu-open-pointer-trigger (seat)
  "MOD+Middle-click: open the menu for the window under the
pointer."
  (let ((win (window-at-pointer seat)))
    (if win
        (menu-open win seat)
        (menu-close))))

(defun menus-available-p ()
  "Whether the menu can open right now (surfaces + a seat with a
pointer-capable wl_seat). Used by the show_window_menu_requested
handler — CSD apps asking for their menu get it when possible."
  (and *compositor* *shm* *layer-shell*
       (some #'seat-wl-seat-proxy (all-seats))
       t))
