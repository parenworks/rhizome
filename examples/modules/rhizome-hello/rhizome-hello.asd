(asdf:defsystem #:rhizome-hello
  :description "An example rhizome module: one command, one message"
  :author "parenworks"
  :license "AGPL-3.0"
  :depends-on (#:rhizome)
  :components ((:file "hello")))
