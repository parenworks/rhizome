(asdf:defsystem #:stumpway
  :description "StumpWM-style window manager for the River Wayland compositor"
  :author "Glenn Thompson"
  :license "AGPL-3.0"
  :depends-on (#:wayflan-client
               #:cl-ppcre
               #:bordeaux-threads
               #:cffi)
  :components
  ((:file "river-protocol")
   (:file "package" :depends-on ("river-protocol"))
   (:file "config" :depends-on ("package"))
   (:file "hooks" :depends-on ("package"))
   (:file "wm-protocol" :depends-on ("package" "config"))
   (:file "window" :depends-on ("wm-protocol"))
   (:file "output" :depends-on ("wm-protocol"))
   (:file "seat" :depends-on ("wm-protocol"))
   (:file "input" :depends-on ("wm-protocol"))
   (:file "workspaces" :depends-on ("window" "output" "seat"))
   (:file "tiling" :depends-on ("window" "output" "seat" "workspaces"))
   (:file "keybindings" :depends-on ("tiling"))
   (:file "launcher" :depends-on ("keybindings"))
   (:file "prompt" :depends-on ("bar" "workspaces"))
   (:file "message" :depends-on ("bar"))
   (:file "prompt-clients" :depends-on ("prompt" "message" "ipc"))
   (:file "eval-prompt" :depends-on ("keybindings" "bar"))
   (:file "bar" :depends-on ("launcher" "config" "wm-protocol"))
   (:file "commands" :depends-on ("keybindings" "launcher"))
   (:file "ipc" :depends-on ("commands" "message"))
   (:file "main" :depends-on ("commands" "bar" "ipc"))))
