(asdf:defsystem #:stumpway
  :description "StumpWM-style window manager for the River Wayland compositor"
  :author "Glenn Thompson"
  :license "AGPL-3.0"
  :depends-on (#:wayflan-client
               #:cl-ppcre
               #:bordeaux-threads
               #:cffi
               #:pngload)
  :components
  ((:file "river-protocol")
   (:file "package" :depends-on ("river-protocol"))
   (:file "config" :depends-on ("package"))
   (:file "hooks" :depends-on ("package"))
   (:file "session" :depends-on ("package" "config" "hooks"))
   (:file "wm-protocol" :depends-on ("package" "config"))
   (:file "window" :depends-on ("wm-protocol"))
   (:file "output" :depends-on ("wm-protocol"))
   (:file "seat" :depends-on ("wm-protocol"))
   (:file "input" :depends-on ("wm-protocol"))
   (:file "floating" :depends-on ("window" "output" "seat"))
   (:file "wse" :depends-on ("window"))
   (:file "frames" :depends-on ("window" "output" "seat"))
   (:file "workspaces" :depends-on ("window" "output" "seat" "floating" "frames"))
   (:file "tiling" :depends-on ("window" "output" "seat" "workspaces" "floating" "frames"))
   (:file "scroll" :depends-on ("tiling" "bar"))
   (:file "groups" :depends-on ("scroll" "workspaces" "floating" "frames"))
   (:file "dbus" :depends-on ("wm-protocol" "config"))
   (:file "keybindings" :depends-on ("tiling"))
   (:file "launcher" :depends-on ("keybindings"))
   (:file "prompt" :depends-on ("bar" "workspaces"))
   (:file "message" :depends-on ("bar"))
   (:file "prompt-clients" :depends-on ("prompt" "message" "ipc"))
   (:file "prefix" :depends-on ("prompt-clients" "keybindings" "scroll" "groups"))
   (:file "expose" :depends-on ("prefix" "floating" "frames"))
   (:file "eval-prompt" :depends-on ("keybindings" "bar"))
   (:file "bar" :depends-on ("launcher" "config" "wm-protocol"))
   (:file "commands" :depends-on ("keybindings" "launcher" "scroll" "groups"))
   (:file "ipc" :depends-on ("commands" "message"))
   (:file "main" :depends-on ("commands" "bar" "ipc"))))
