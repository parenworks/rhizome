;;; network-menu.lisp — the network configurator: nmcli terse-line
;;; parsing (colon escaping), scan dedupe/sort, signal bars, connect
;;; and toggle argv. All nmcli calls stubbed.
(ql:quickload :rhizome)

(in-package #:rhizome)

;; The network menu is a MODULE now: load it from the repo's
;; modules/ directory (the same way a user loads it from
;; ~/.config/rhizome/modules/)
(let ((*module-directory*
        (make-pathname
          :directory (append (pathname-directory
                               (asdf:system-source-directory :rhizome))
                              '("modules")))))
  (load-module "rhizome-net"))

(configure '(:bar (:enabled nil)))

(defparameter *stub-out* "")
(defparameter *stub-argvs* nil)

(defun net-stub-run (command &rest keys)
  (declare (ignore keys))
  (push command *stub-argvs*)
  (cond ((equal command '("nmcli" "radio" "wifi"))
         (format nil "enabled~%"))    ; a real newline: the trim must strip it
        (t *stub-out*)))

(let ((pass 0) (fail 0)
      (real-run (symbol-function 'uiop:run-program)))
  (flet ((ok (name actual expected)
           (if (equal actual expected)
               (progn (incf pass)
                      (format t "~&PASS: ~A~%" name))
               (progn (incf fail)
                      (format t "~&FAIL: ~A (got ~S, wanted ~S)~%"
                          name actual expected)))))
    (unwind-protect
         (progn
           (setf (symbol-function 'uiop:run-program) #'net-stub-run)

           ;; --- nmcli terse-line parsing ---
           (ok "plain fields split on colons"
               (network-nmcli-fields "yes:DWAYNE-5G:82:WPA2")
               '("yes" "DWAYNE-5G" "82" "WPA2"))
           (ok "escaped colon stays in the SSID"
               (network-nmcli-fields "no:cafe\\:babe:74:WPA2")
               '("no" "cafe:babe" "74" "WPA2"))
           (ok "escaped backslash stays"
               (network-nmcli-fields "no:home\\\\net:74:WPA2")
               '("no" "home\\net" "74" "WPA2"))
           (ok "trailing field without newline"
               (network-nmcli-fields "x:y:1:")
               '("x" "y" "1" ""))

           ;; --- scan: dedupe by SSID (strongest wins), sort desc,
           ;;     hidden SSIDs skipped ---
           (setf *stub-out* "
 :VM3128818:85:WPA2
 ::74:WPA2
 :BTWholeHome-RSG:72:WPA2 WPA3
 :BTWholeHome-RSG:48:WPA2 WPA3
*:DWAYNE-5G:90:WPA2
")
           (let ((nets (network-wifi-scan :rescan nil)))
             (ok "hidden SSIDs skipped, visible kept"
                 (mapcar (lambda (n) (getf n :ssid)) nets)
                 '("DWAYNE-5G" "VM3128818" "BTWholeHome-RSG"))
             (ok "duplicate SSID collapses to strongest signal"
                 (getf (third nets) :signal) 72)
             (ok "in-use flag parses"
                 (getf (first nets) :in-use) t))

           ;; --- signal bars ---
           (ok "100% is five cells"
               (network-signal-bars 100) "[#####]")
           (ok "42% is two cells"
               (network-signal-bars 42) "[##...]")
           (ok "over and under clamp"
               (list (network-signal-bars 130) (network-signal-bars -5))
               '("[#####]" "[.....]"))

           ;; --- connect argv: no password, with password ---
           (ok "connect argv (open network)"
                 (network-connect-argv "cafe:babe")
                 '("nmcli" "dev" "wifi" "connect" "cafe:babe"))
           (ok "connect argv carries the password"
                 (network-connect-argv "DWAYNE-5G" "s3cret")
                 '("nmcli" "dev" "wifi" "connect" "DWAYNE-5G"
                   "password" "s3cret"))

           ;; --- sync core result parsing ---
           (setf *stub-argvs* nil *stub-out* "successfully activated\n")
           (ok "sync connect success"
               (network-connect-sync "cafe:babe") t)
           (setf *stub-out*
                 (format nil "Error: No network with SSID 'nope' was found.~%"))
           (ok "sync connect failure returns nmcli's text"
               (network-connect-sync "nope")
               "Error: No network with SSID 'nope' was found.")

           ;; --- toggle argv reads the radio state ---
           (ok "wifi radio reads enabled"
               (network-wifi-enabled-p) t)
           (setf *stub-argvs* nil)
           (network-toggle-wifi)
           (ok "enabled radio turns off"
                 (first *stub-argvs*)
                 '("nmcli" "radio" "wifi" "off"))

           ;; --- active connection name ---
           (setf *stub-out* "Glenn's Z Fold7:802-11-wireless\ntailscale0:tun\n")
           (ok "active connection name"
               (network-active-name) "Glenn's Z Fold7"))

      (setf (symbol-function 'uiop:run-program) real-run)))

  (format t "~&=== ~A passed, ~A failed ===~%" pass fail)
  (when (plusp fail)
    (uiop:quit 1)))
