;;; Input device configuration via river-libinput-config-v1
;;;
;;; Configures libinput devices through river's libinput config protocol.
;;; Default policy (matching the Omarchy/Hyprland feel):
;;;   - Tap-to-click enabled on touchpads: 1 finger = left, 2 = right,
;;;     3 = middle
;;;   - Tap-and-drag enabled
;;; Natural scroll is left at the device default (disabled on most
;;; touchpads, matching Omarchy's natural_scroll = false).
;;;
;;; Devices are discovered via river_input_manager_v1 (device names/types)
;;; and river_libinput_config_v1 (libinput capabilities and configuration).

(in-package #:rhizome)

;;; --- Configuration knobs ---

(defparameter *enable-tap* t
  "Enable tap-to-click on devices that support it.
1/2/3 finger taps map to left/right/middle click.")

(defparameter *enable-tap-drag* t
  "Enable tap-and-drag (double tap and hold to drag) on tap devices.")

;;; --- State ---

(defvar *input-manager* nil
  "river_input_manager_v1 proxy.")

(defvar *libinput-config* nil
  "river_libinput_config_v1 proxy.")

(defvar *river-input-devices* (make-hash-table)
  "Maps river_input_device_v1 proxies to property plists (:name, :type).")

(defvar *libinput-devices* (make-hash-table)
  "Maps river_libinput_device_v1 proxies to property plists
(:input-device, :tap-fingers, :tap-state, :drag-state, :tap-configured).")

(defun libinput-device-name (device)
  "Best-effort human-readable name for a libinput device."
  (let ((river-device (getf (gethash device *libinput-devices*) :input-device)))
    (or (and river-device (getf (gethash river-device *river-input-devices*) :name))
        "unnamed device")))

;;; --- Result objects ---

(defun handle-libinput-result (result what)
  "Log the outcome of a libinput config request."
  (push (lambda (event-name &rest args)
          (declare (ignore args))
          (case event-name
            (:success
             (format t "~&; Rhizome: input config ok: ~A~%" what))
            (:unsupported
             (format t "~&; Rhizome: input config unsupported: ~A~%" what))
            (:invalid
             (format t "~&; Rhizome: input config invalid: ~A~%" what))))
        (wl:wl-proxy-hooks result)))

;;; --- Applying configuration ---

(defun apply-libinput-config (device)
  "Apply rhizome's input policy to a libinput device.
Called once all initial device information has been received."
  (let ((props (gethash device *libinput-devices*)))
    (let ((tap-fingers (getf props :tap-fingers 0))
          (tap-state (getf props :tap-state))
          (drag-state (getf props :drag-state))
          (name (libinput-device-name device)))
      (when (and *enable-tap* (plusp tap-fingers) (not (eq tap-state :enabled)))
        (format t "~&; Rhizome: enabling tap-to-click (~D finger~:P) on ~S~%"
                tap-fingers name)
        (handle-libinput-result
          (rwm:river-libinput-device-v1.set-tap device :enabled)
          (format nil "tap-to-click on ~A" name)))
      (when (and *enable-tap-drag* (plusp tap-fingers)
                 (not (eq drag-state :enabled)))
        (handle-libinput-result
          (rwm:river-libinput-device-v1.set-drag device :enabled)
          (format nil "tap-and-drag on ~A" name))))))

;;; --- river_input_device_v1 (from river_input_manager_v1) ---

(defun register-river-input-device (device)
  (setf (gethash device *river-input-devices*) '())
  (push (lambda (event-name &rest args)
          (case event-name
            (:type
             (setf (getf (gethash device *river-input-devices*) :type)
                   (first args)))
            (:name
             (setf (getf (gethash device *river-input-devices*) :name)
                   (first args))
             (format t "~&; Rhizome: input device ~S (type ~A)~%"
                     (first args)
                     (getf (gethash device *river-input-devices*) :type)))
            (:removed
             (remhash device *river-input-devices*))))
        (wl:wl-proxy-hooks device)))

;;; --- river_libinput_device_v1 (from river_libinput_config_v1) ---

(defun register-libinput-device (device)
  (setf (gethash device *libinput-devices*) '())
  (push (lambda (event-name &rest args)
          (case event-name
            (:input-device
             (setf (getf (gethash device *libinput-devices*) :input-device)
                   (first args)))
            (:tap-support
             (setf (getf (gethash device *libinput-devices*) :tap-fingers)
                   (first args)))
            ((:tap-default :tap-current)
             (setf (getf (gethash device *libinput-devices*) :tap-state)
                   (first args)))
            ((:drag-default :drag-current)
             (setf (getf (gethash device *libinput-devices*) :drag-state)
                   (first args)))
            (:done
             ;; All initial device information has been sent.
             (apply-libinput-config device))
            (:removed
             (remhash device *libinput-devices*))))
        (wl:wl-proxy-hooks device)))

;;; --- Global setup ---

(defun setup-input-management (registry name interface version)
  "Bind to river_input_manager_v1 if advertised."
  (declare (ignore interface))
  (let ((iface-class (wl:find-interface-named "river_input_manager_v1")))
    (when iface-class
      (setf *input-manager*
            (wl:wl-registry.bind registry name iface-class version))
      (push (lambda (event-name &rest args)
              (case event-name
                (:input-device
                 (register-river-input-device (first args)))
                (:finished
                 (format t "~&; Rhizome: input manager finished~%"))))
            (wl:wl-proxy-hooks *input-manager*))
      (format t "~&; Rhizome: bound river_input_manager_v1 (version ~D)~%" version))))

(defun setup-libinput-config (registry name interface version)
  "Bind to river_libinput_config_v1 if advertised."
  (declare (ignore interface))
  (let ((iface-class (wl:find-interface-named "river_libinput_config_v1")))
    (when iface-class
      (setf *libinput-config*
            (wl:wl-registry.bind registry name iface-class version))
      (push (lambda (event-name &rest args)
              (case event-name
                (:libinput-device
                 (register-libinput-device (first args)))
                (:finished
                 (format t "~&; Rhizome: libinput config finished~%"))))
            (wl:wl-proxy-hooks *libinput-config*))
      (format t "~&; Rhizome: bound river_libinput_config_v1 (version ~D)~%" version))))
