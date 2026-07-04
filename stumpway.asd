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
   (:file "wm-protocol" :depends-on ("package"))
   (:file "window" :depends-on ("wm-protocol"))
   (:file "output" :depends-on ("wm-protocol"))
   (:file "seat" :depends-on ("wm-protocol"))
   (:file "tiling" :depends-on ("window" "output" "seat"))
   (:file "keybindings" :depends-on ("tiling"))
   (:file "launcher" :depends-on ("keybindings"))
   (:file "commands" :depends-on ("keybindings" "launcher"))
   (:file "main" :depends-on ("commands"))))
