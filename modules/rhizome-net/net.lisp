;;; rhizome-net — the network module.
;;;
;;; The network configurator: scan (nmcli), pick (select-from-menu),
;;; connect (password prompt for secured networks), Wi-Fi radio
;;; toggle. Loaded with (rhizome:load-module "rhizome-net") from
;;; config.lisp or the REPL; the bar's :network module's
;;; :on-click "rhizome-cmd network-menu" drives it.
;;;
;;; Runs inside the WM process, in the rhizome package (like the
;;; example module): all of rhizome is available, and the command
;;; registers into the same command table.

;;; --- Network management (the network configurator, like display-menu) ---

(in-package #:rhizome)

(defun network-nmcli-fields (line)
  "Split one nmcli -t (terse) line on unescaped colons, unescaping
nmcli's escapes (\\: a colon, \\\\ a backslash — SSIDs may contain
either). Returns the unescaped fields."
  (loop with cur = (make-string-output-stream)
        with esc = nil
        with fields = nil
        for ch across line
        do (cond (esc (write-char ch cur) (setf esc nil))
                 ((char= ch #\\) (setf esc t))
                 ((char= ch #\:) (push (get-output-stream-string cur) fields))
                 (t (write-char ch cur)))
        finally (progn
                  (push (get-output-stream-string cur) fields)
                  (return (nreverse fields)))))

(defun network-wifi-scan (&key (rescan t))
  "Visible networks as ((:in-use bool :ssid string :signal 0-100
:security string-or-empty) ...), strongest first, one entry per SSID
(multiple bands of one network collapse to the strongest). NIL when
nmcli fails (radio off, no device)."
  (handler-case
      (let* ((out (uiop:run-program
                    `("nmcli" "-t" "-f" "IN-USE,SSID,SIGNAL,SECURITY"
                      "dev" "wifi" "list"
                      "--rescan" ,(if rescan "yes" "no"))
                    :output :string :ignore-error-status t))
             (nets (loop for line in (uiop:split-string out
                                                     :separator '(#\newline))
                         for f = (network-nmcli-fields line)
                         when (and (>= (length f) 4)
                                   (plusp (length (second f))))
                           collect (list :in-use (string= "*" (first f))
                                         :ssid (second f)
                                         :signal (or (parse-integer (third f)
                                                                    :junk-allowed t)
                                                     0)
                                         :security (fourth f)))))
        (let ((best (make-hash-table :test #'equal)))
          (dolist (n nets)
            (let ((old (gethash (getf n :ssid) best)))
              (when (or (null old)
                        (> (getf n :signal) (getf old :signal))
                        (and (getf n :in-use) (not (getf old :in-use))))
                (setf (gethash (getf n :ssid) best) n))))
          (sort (loop for n being the hash-values of best collect n)
                #'> :key (lambda (n) (getf n :signal)))))
    (error () nil)))

(defun network-signal-bars (signal)
  "A [###..] meter: five cells, one filled per 20% of signal."
  (let ((filled (floor (min (max signal 0) 100) 20)))
    (format nil "[~A~A]"
            (make-string filled :initial-element #\#)
            (make-string (- 5 filled) :initial-element #\.))))

(defun network-wifi-enabled-p ()
  "Whether the Wi-Fi radio is on."
  (string= "enabled"
           (string-trim '(#\space #\newline #\return)
             (handler-case
                 (uiop:run-program '("nmcli" "radio" "wifi")
                                   :output :string
                                   :ignore-error-status t)
               (error () "disabled")))))

(defun network-toggle-wifi ()
  "Toggle the Wi-Fi radio."
  (let ((on (network-wifi-enabled-p)))
    (uiop:run-program `("nmcli" "radio" "wifi" ,(if on "off" "on"))
                      :ignore-error-status t)
    (message "wifi ~A" (if on "off" "on"))))

(defun network-active-name ()
  "The active connection's name, or NIL (for the menu title)."
  (handler-case
      (let* ((out (uiop:run-program
                    '("nmcli" "-t" "-f" "NAME,TYPE"
                      "connection" "show" "--active")
                    :output :string :ignore-error-status t))
             (line (with-input-from-string (s out)
                     (read-line s nil nil))))
        (when (and line (plusp (length line)))
          (let ((colon (position #\: line)))
            (if colon (subseq line 0 colon) line))))
    (error () nil)))

(defun network-connect-argv (ssid &optional password)
  "The nmcli argv that connects to SSID, with an optional PASSWORD."
  (append '("nmcli" "dev" "wifi" "connect")
          (list ssid)
          (when password (list "password" password))))

(defun network-connect-sync (ssid &optional password)
  "Run the connect; T when nmcli reports activation, or the error
text (a string) when it fails."
  (let ((out (handler-case
                 (uiop:run-program (network-connect-argv ssid password)
                                   :output :string :ignore-error-status t)
               (error (e) (princ-to-string e)))))
    (if (and out (search "successfully activated" out))
        t
        (or (and out (string-trim '(#\space #\newline #\return) out))
            "nmcli failed"))))

(defun network-connect (ssid &optional password)
  "Connect to SSID in a BACKGROUND THREAD (nmcli takes seconds —
authentication, DHCP — and running it on the main thread froze the
whole WM for the duration). 'connecting…' feedback at once; the
result message arrives via the IPC queue when the connect lands."
  (message "connecting to ~A…" ssid)
  (bt:make-thread
    (lambda ()
      (let ((result (network-connect-sync ssid password)))
        (ipc-enqueue
          (lambda ()
            (if (eq result t)
                (message "connected to ~A" ssid)
                (message "connect to ~A failed — ~A"
                         ssid (line-of result))))))))
  (values))

(defun line-of (text)
  "TEXT's last non-empty line, trimmed (nmcli errors land there)."
  (let ((lines (remove "" (mapcar (lambda (l) (string-trim " " l))
                                  (uiop:split-string text :separator '(#\newline)))
                      :test #'string=)))
    (first (last lines))))

(defun network-connect-flow (net)
  "Connect to NET: secured networks get a password prompt first
(the prompt echoes what you type — a masked prompt is future work)."
  (let ((ssid (getf net :ssid))
        (sec (getf net :security)))
    (if (and sec (plusp (length sec)))
        (prompt-open (format nil "Password for ~A:" ssid)
                     :secret t
                     :on-complete (lambda (pw) (network-connect ssid pw)))
        (network-connect ssid))))

(defun network-show-menu (nets enabled active-name)
  "Show the network menu from pre-scanned data (main thread only:
the prompt machinery talks Wayland)."
  (select-from-menu
    (format nil "Network — ~A" (or active-name "offline"))
    (append
      (list (cons "Rescan" (lambda () (open-network-menu)))
            (cons (if enabled
                      "Wi-Fi radio off"
                      "Wi-Fi radio on")
                  (lambda () (network-toggle-wifi))))
      (mapcar (lambda (net)
                (cons (format nil "~A ~2D% ~A~:[~; ✓~]"
                              (network-signal-bars (getf net :signal))
                              (getf net :signal)
                              (getf net :ssid)
                              (getf net :in-use))
                      (lambda () (network-connect-flow net))))
              (or nets '()))
      (when (null nets)
        (list (cons "no networks found"
                    (lambda ()
                      (message "no networks — rescan or check the radio"))))))))

(defun open-network-menu ()
  "Scan for networks and open the menu when the scan lands. The
scan runs in a BACKGROUND THREAD — a real radio rescan blocks
nmcli for 1-3 seconds, and running it on the main thread froze the
whole WM (every event, every render) for the duration, which read
on screen as a blurred, half-updated menu that snapped sharp when
the loop unblocked. The menu itself opens via ipc-enqueue, on the
WM's main thread."
  (message "scanning for networks…")
  (bt:make-thread
    (lambda ()
      (let ((nets (network-wifi-scan))
            (enabled (network-wifi-enabled-p))
            (active (network-active-name)))
        (ipc-enqueue
          (lambda () (network-show-menu nets enabled active))))))
  (values))

(define-command :network-menu (output args)
  "The network configurator: scan, pick, connect (password prompt
for secured networks)."
  (declare (ignore output args))
  (open-network-menu))

