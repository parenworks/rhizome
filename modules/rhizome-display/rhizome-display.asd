(asdf:defsystem #:rhizome-display
  :description "The rhizome display module: brightness, night light,
per-output modes/scale — the display configurator menu"
  :author "parenworks"
  :license "AGPL-3.0"
  :depends-on (#:rhizome)
  :components ((:file "display")))
