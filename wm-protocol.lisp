;;; Core river-window-management-v1 protocol handling for Stumpway
;;;
;;; This module implements the manage/render sequence loop that is the
;;; heart of river's window management protocol. The flow is:
;;;
;;;   1. Server sends state change events (new window, output, seat, etc.)
;;;   2. Server sends manage_start
;;;   3. Client sends window management requests (propose dimensions, focus, etc.)
;;;   4. Client sends manage_finish
;;;   5. Server sends window dimensions events
;;;   6. Server sends render_start
;;;   7. Client sends rendering requests (set position, borders, z-order)
;;;   8. Client sends render_finish
;;;   9. Loop back to 1 or 5 as needed

(in-package #:stumpway)

;;; --- State --

(defvar *display* nil)
(defvar *wm-proxy* nil)
(defvar *running* nil)
(defvar *registry* nil
  "The wl_registry proxy, kept for binding new globals.")
(defvar *compositor* nil
  "wl_compositor proxy for creating surfaces.")
(defvar *shm* nil
  "wl_shm proxy for shared memory buffers.")
(defvar *layer-shell* nil
  "zwlr_layer_shell_v1 proxy for layer surfaces.")
(defvar *river-layer-shell* nil
  "river_layer_shell_v1 proxy - river's WM-side layer shell support.")

(defvar *windows* (make-hash-table)
  "Maps river_window_v1 proxy objects to stumpway-window instances.")

(defvar *outputs* (make-hash-table)
  "Maps river_output_v1 proxy objects to stumpway-output instances.")

(defvar *seats* (make-hash-table)
  "Maps river_seat_v1 proxy objects to stumpway-seat instances.")

(defvar *wm-snapshot* '(:layout nil :windows 0 :focused nil)
  "Cached WM state for the bar thread, refreshed by update-wm-snapshot.")

(defun snapshot-pathname ()
  "Pathname of the WM snapshot file read by the bar process."
  (merge-pathnames #P".cache/stumpway/snapshot.sexp"
                   (user-homedir-pathname)))

(defun bar-write-snapshot (snapshot)
  "Persist the WM snapshot for the bar process (atomic write)."
  (handler-case
      (let ((tmp (merge-pathnames #P".cache/stumpway/snapshot.sexp.tmp"
                                  (user-homedir-pathname))))
        (ensure-directories-exist tmp)
        (with-open-file (s tmp :direction :output
                           :if-exists :supersede)
          (write snapshot :stream s))
        (let ((target (snapshot-pathname)))
          (when (probe-file target)
            (delete-file target))
          (rename-file tmp target)))
    (error (e)
      (declare (ignore e))
      nil)))

(defvar *pending-autofocus* nil
  "Newly registered window awaiting auto-focus in the next manage
sequence. Focus is applied once per new window, not on every manage
cycle (which would fight the user for focus).")

(defun update-wm-snapshot ()
  "Refresh the snapshot read by the bar thread. Called from the WM
thread during manage and render sequences."
  (setf *wm-snapshot*
        (list :layout (let ((outs (all-outputs)))
                        (when outs (output-current-layout (first outs))))
              :windows (hash-table-count *windows*)
              :workspace (let ((outs (all-outputs)))
                          (when outs (current-workspace (first outs))))
              :workspace-count *workspace-count*
              :focused (let ((focused (find-if #'window-focused-p (all-windows))))
                         (when focused (window-title focused)))))
  (bar-write-snapshot *wm-snapshot*))

;;; --- Sequence state machine --
;;;
;;; The protocol requires strict ordering of manage and render sequences.
;;; We track the current state to enforce this.

(defvar *sequence-state* :idle
  "Current state in the manage/render sequence loop.
One of :idle, :managing, :waiting-render, :rendering.")

(defvar *pending-manage* nil
  "If non-nil, a manage sequence is pending after the current render sequence.")

(defun sequence-state-enter (new-state)
  (setf *sequence-state* new-state))

;;; --- Registry handling --

(defun handle-registry-global (registry name interface version)
  "Handle wl_registry.global events - bind to river protocols."
  (cond
    ((string= interface "river_window_manager_v1")
     (let ((iface-class (wl:find-interface-named interface)))
       (unless iface-class
         (format t "~&; Stumpway: interface ~A not found!~%" interface)
         (return-from handle-registry-global))
       (setf *wm-proxy*
             (wl:wl-registry.bind registry name iface-class version))
       (format t "~&; Stumpway: bound river_window_manager_v1 (version ~D)~%" version)
       (setup-wm-hooks *wm-proxy*)))
    ((string= interface "river_xkb_bindings_v1")
     (setup-xkb-bindings registry name interface version))
    ((string= interface "river_input_manager_v1")
     (setup-input-management registry name interface version))
    ((string= interface "river_libinput_config_v1")
     (setup-libinput-config registry name interface version))
    ((string= interface "wl_compositor")
     (let ((iface-class (wl:find-interface-named interface)))
       (when iface-class
         (setf *compositor* (wl:wl-registry.bind registry name iface-class version))
         (format t "~&; Stumpway: bound wl_compositor (version ~D)~%" version))))
    ((string= interface "wl_shm")
     (let ((iface-class (wl:find-interface-named interface)))
       (when iface-class
         (setf *shm* (wl:wl-registry.bind registry name iface-class version))
         (format t "~&; Stumpway: bound wl_shm (version ~D)~%" version))))
    ((string= interface "zwlr_layer_shell_v1")
     (let ((iface-class (wl:find-interface-named interface)))
       (when iface-class
         (setf *layer-shell* (wl:wl-registry.bind registry name iface-class version))
         (format t "~&; Stumpway: bound zwlr_layer_shell_v1 (version ~D)~%" version))))
    ((string= interface "river_layer_shell_v1")
     (let ((iface-class (wl:find-interface-named interface)))
       (when iface-class
         (setf *river-layer-shell* (wl:wl-registry.bind registry name iface-class version))
         (format t "~&; Stumpway: bound river_layer_shell_v1 (version ~D)~%" version))))
    (t
     ;; We don't bind wl_output/wl_seat directly - river_output_v1 and
     ;; river_seat_v1 provide the corresponding wl_output/wl_seat names
     ;; via their events.
     nil)))

;;; --- Window manager event handling --

(defun setup-wm-hooks (wm)
  "Set up event handlers on the river_window_manager_v1 proxy."
  (push (lambda (event-name &rest args)
          (apply #'handle-wm-event event-name args))
        (wl:wl-proxy-hooks wm)))

(defun handle-wm-event (event-name &rest args)
  "Dispatch river_window_manager_v1 events."
  (format t "~&; Stumpway [debug]: WM event ~A ~S~%" event-name args)
  (case event-name
    (:unavailable
     (format t "~&; Stumpway: window management unavailable (another WM is running?)~%")
     (stop))

    (:finished
     (format t "~&; Stumpway: server finished~%")
     (setf *running* nil))

    (:manage-start
     (handle-manage-start))

    (:render-start
     (handle-render-start))

    (:window
     (destructuring-bind (window-proxy) args
       (register-window window-proxy)))

    (:output
     (destructuring-bind (output-proxy) args
       (register-output output-proxy)))

    (:seat
     (destructuring-bind (seat-proxy) args
       (register-seat seat-proxy)))

    (:session-locked
     (format t "~&; Stumpway: session locked~%"))

    (:session-unlocked
     (format t "~&; Stumpway: session unlocked~%"))

    (t
     (format t "~&; Stumpway: unhandled WM event ~A~%" event-name))))

;;; --- Manage sequence --

(defun handle-manage-start ()
  "Begin a manage sequence. Process all pending state changes and send
window management decisions to the compositor."
  (sequence-state-enter :managing)

  ;; Let the tiling engine apply window management decisions
  ;; (propose dimensions, set focus, set tiled state, etc.)
  (on-manage-sequence)

  ;; Refresh the WM snapshot for the bar
  (update-wm-snapshot)

  ;; Finish the manage sequence
  (rwm:river-window-manager-v1.manage-finish *wm-proxy*)

  ;; If we requested a manage_dirty during this sequence, another
  ;; manage sequence will follow. Otherwise we wait for render_start.
  (if *pending-manage*
      (progn
        (setf *pending-manage* nil)
        (sequence-state-enter :idle))
      (sequence-state-enter :waiting-render)))

(defun on-manage-sequence ()
  "Hook called during a manage sequence. The tiling engine and command
handlers should make window management requests here."
  (format t "~&; Stumpway [debug]: on-manage-sequence - ~D outputs, ~D windows~%"
          (hash-table-count *outputs*) (hash-table-count *windows*))

  ;; Set up default keybindings on first manage sequence
  (setup-default-keybindings)

  ;; Install keybindings if not yet installed
  (install-keybindings-if-needed)

  (maphash (lambda (output-proxy output)
             (declare (ignore output-proxy))
             (format t "~&; Stumpway [debug]: output ~A dims ~Ax~A~%"
                     output (output-width output) (output-height output))
             (apply-layout-for-output output))
           *outputs*)
  ;; Auto-focus: focus a newly registered window once (not every
  ;; manage cycle), and only if it lives on the current workspace
  (let ((win *pending-autofocus*))
    (when (and win
               (gethash (window-proxy win) *windows*)
               (let ((out (window-output win)))
                 (or (null out)
                     (eql (window-workspace win) (current-workspace out)))))
      (setf *pending-autofocus* nil)
      (dolist (seat (all-seats))
        (format t "~&; Stumpway [debug]: auto-focusing window ~A on seat ~A~%" win seat)
        (focus-window win seat)))))

;;; --- Render sequence --

(defun handle-render-start ()
  "Begin a render sequence. Send rendering decisions (positions, borders,
z-ordering) to the compositor."
  (sequence-state-enter :rendering)

  ;; Let the tiling engine apply rendering decisions
  (on-render-sequence)

  ;; Refresh the WM snapshot for the bar
  (update-wm-snapshot)

  ;; Finish the render sequence
  (rwm:river-window-manager-v1.render-finish *wm-proxy*)

  (if *pending-manage*
      (progn
        (setf *pending-manage* nil)
        (sequence-state-enter :idle))
      (sequence-state-enter :idle)))

(defun on-render-sequence ()
  "Hook called during a render sequence. The tiling engine should set
window positions, borders, and z-ordering here."
  (format t "~&; Stumpway [debug]: on-render-sequence - ~D outputs, ~D windows~%"
          (hash-table-count *outputs*) (hash-table-count *windows*))
  (maphash (lambda (output-proxy output)
             (declare (ignore output-proxy))
             (render-layout-for-output output))
           *outputs*))

;;; --- Manage dirty --

(defun request-manage-dirty ()
  "Request that the compositor start a new manage sequence.
Use this when internal state changes (e.g. user command) require
window management updates."
  (when *wm-proxy*
    (rwm:river-window-manager-v1.manage-dirty *wm-proxy*)
    (setf *pending-manage* t)))

;;; --- Main event loop --

(defun run ()
  "Start the stumpway window manager. Connects to the running river compositor."
  (setf *running* t
        *windows* (make-hash-table)
        *outputs* (make-hash-table)
        *seats* (make-hash-table)
        *output-workspace* (make-hash-table)
        *window-workspace* (make-hash-table)
        *minimized-windows* (make-hash-table)
        *pending-autofocus* nil
        *ipc-queue* nil
        *input-manager* nil
        *libinput-config* nil
        *river-input-devices* (make-hash-table)
        *libinput-devices* (make-hash-table)
        *sequence-state* :idle
        *launcher-active* nil
        *launcher-surface* nil
        *launcher-layer-surface* nil
        *launcher-scroll-offset* 0)

  (format t "~&; Stumpway: starting window manager~%")
  (format t "~&; Stumpway: connecting to river compositor...~%")

  ;; Load user configuration (~/.config/stumpway/config.lisp)
  (load-user-config)

  (wl:with-open-display (display)
    (setf *display* display)

    (let ((registry (wl:wl-display.get-registry display)))
      (setf *registry* registry)
      (push (lambda (event-name &rest args)
              (when (eq event-name :global)
                (destructuring-bind (name interface version) args
                  (handle-registry-global registry name interface version))))
            (wl:wl-proxy-hooks registry))

      ;; Initial roundtrip to discover and bind globals
      (wl:wl-display-roundtrip display)

      (unless *wm-proxy*
        (format t "~&; Stumpway: river_window_manager_v1 not found! Is river running?~%")
        (return-from run))

      ;; Retroactively register outputs/seats with river_layer_shell_v1
      ;; (in case they were created before the layer shell global was bound)
      (when *river-layer-shell*
        (loop for output being the hash-values of *outputs* do
          (handler-case
              (let ((ls-output (rwm:river-layer-shell-v1.get-output
                                 *river-layer-shell* (output-proxy output))))
                (push (lambda (event-name &rest args)
                        (declare (ignore event-name args)))
                      (wl:wl-proxy-hooks ls-output)))
            (error (e)
              (format t "~&; Stumpway: retroactive output layer shell registration failed: ~A~%" e))))
        (loop for seat being the hash-values of *seats* do
          (handler-case
              (let ((ls-seat (rwm:river-layer-shell-v1.get-seat
                              *river-layer-shell* (seat-proxy seat))))
                (push (lambda (event-name &rest args)
                        (declare (ignore event-name args)))
                      (wl:wl-proxy-hooks ls-seat)))
            (error (e)
              (format t "~&; Stumpway: retroactive seat layer shell registration failed: ~A~%" e))))
        (format t "~&; Stumpway: registered ~D output(s) and ~D seat(s) with river_layer_shell_v1~%"
                (hash-table-count *outputs*) (hash-table-count *seats*)))

      (format t "~&; Stumpway: connected, managing ~D window(s), ~D output(s), ~D seat(s)~%"
              (hash-table-count *windows*)
              (hash-table-count *outputs*)
              (hash-table-count *seats*))

      ;; Start the status bar if configured
      (bar-apply-config)

      ;; IPC command socket (stumpish equivalent) and SLIME server
      (when (config-getf '(:ipc) :enabled :default t)
        (start-ipc-server))
      (start-swank)

      ;; Main event loop: poll the compositor socket and drain the IPC
      ;; queue so IPC commands execute on this (main) thread
      (unwind-protect
           (loop while *running* do
             (handler-case
                 (when (wl:wl-display-listen display)
                   (wl:wl-display-dispatch-event display))
               (error (e)
                 (format t "~&; Stumpway: error in event dispatch: ~A~%" e)))
             (ipc-drain)
             (message-drain)
             (prompt-drain)
             (unless (wl:wl-display-listen display)
               (sleep 0.05)))
        (format t "~&; Stumpway: shutting down~%")
        (stop-bar)
        (stop-ipc-server))))

  (setf *display* nil
        *wm-proxy* nil))

(defun start-swank ()
  "Start a swank (SLIME) server so the running WM can be driven from
Emacs. Gated by (:swank :enabled t) in the config; runs in a thread so
loading swank never delays WM startup."
  (when (config-getf '(:swank) :enabled :default t)
    (bt:make-thread
      (lambda ()
        (handler-case
            (let ((port (config-getf '(:swank) :port :default 4005)))
              (ql:quickload :swank :silent t)
              (funcall (find-symbol "CREATE-SERVER" "SWANK")
                       :port port :dont-close t)
              (format t "~&; Stumpway: swank (SLIME) server on port ~A~%" port))
          (error (e)
            (format t "~&; Stumpway: swank failed to start: ~A~%" e))))
      :name "stumpway-swank")))

(defun stop ()
  "Stop the stumpway event loop."
  (setf *running* nil))
