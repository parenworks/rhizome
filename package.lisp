(defpackage #:stumpway
  (:use #:cl)
  (:local-nicknames
   (#:wl #:xyz.shunter.wayflan.client)
   (#:rwm #:stumpway.river-wm))
  (:export
   ;; Main entry point
   #:run
   #:stop
   ;; Layout commands
   #:define-layout
   #:switch-layout
   ;; Window operations
   #:focus-window
   #:close-window
   #:hide-window
   #:show-window
   #:propose-window-dimensions
   #:set-window-position
   #:set-window-borders
   #:all-windows
   #:windows-on-output
   ;; Output operations
   #:all-outputs
   #:output-current-layout
   #:output-width
   #:output-height
   ;; Seat operations
   #:all-seats
   #:clear-focus
   ;; Tiling operations
   #:split-horizontal
   #:split-vertical
   #:remove-split
   #:focus-next
   #:focus-previous
   ;; Sequence control
   #:request-manage-dirty
   ;; Window access
   #:window-title
   #:window-app-id
   #:window-width
   #:window-height
   #:window-x
   #:window-y
   #:window-fullscreen-p
   #:window-hidden-p
   #:window-focused-p
   ;; Keybindings
   #:define-key
   #:setup-default-keybindings))
