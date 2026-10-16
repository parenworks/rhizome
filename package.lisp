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
   ;; Workspaces
   #:current-workspace
   #:switch-workspace
   #:move-window-to-workspace
   #:move-focused-window-to-workspace
   #:window-workspace
   ;; Screenshot
   #:take-screenshot
   ;; Power
   #:shutdown-machine
   #:reboot-machine
   ;; IPC (stumpish equivalent) and SLIME
   #:start-ipc-server
   #:stop-ipc-server
   #:ipc-socket-path
   #:ipc-send-command
   #:start-swank
   ;; Interactive prompt and message window
   #:prompt-open
   #:prompt-close
   #:show-message
   #:message
   #:windowlist
   #:workspace-list
   #:command-palette
   #:run-shell-command-prompt
   ;; Prefix key engine
   #:define-prefix-key
   #:define-keymap-key
   #:enter-prefix-mode
   #:exit-prefix-mode
   ;; Floating windows
   #:float-window
   #:unfloat-window
   #:toggle-float
   #:toggle-float-focused
   #:window-floating-p
   #:window-float-x
   #:window-float-y
   #:window-float-width
   #:window-float-height
   ;; Frame trees
   #:frame-tree-for
   #:frame-leaves
   #:current-frame-for
   #:frame-push-window
   #:frame-remove-window
   #:split-current-frame
   #:remove-current-split
   #:resize-current-split
   #:focus-frame-direction
   #:cycle-frame-window
   #:move-window-to-frame-direction
   #:pull-window-into-current-frame
   #:pull-hidden-next
   #:exchange-window-direction
   #:frame-only
   #:frame-balance
   #:frame-clear
   #:set-workspace-tree
   ;; Window numbers, messages, fullscreen, clipboard
   #:window-number
   #:select-window-by-number
   #:renumber-windows
   #:lastmsg
   #:time-message
   #:toggle-fullscreen
   #:getsel
   #:putsel
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
   #:setup-default-keybindings
   #:*mod*
   #:set-user-mod
   #:*mod-alt*
   #:*mod-super*
   ;; Configuration
   #:*config*
   #:configure
   #:reload-config
   #:config-pathname
   ;; Bar
   #:run-bar
   #:start-bar
   #:stop-bar
   #:toggle-bar
   ;; Hooks
   #:add-hook
   #:remove-hook
   #:clear-hook
   #:list-hooks
   #:run-hook
   ;; Eval prompt
   #:show-eval-prompt
   #:hide-eval-prompt
   #:eval-prompt-eval))
