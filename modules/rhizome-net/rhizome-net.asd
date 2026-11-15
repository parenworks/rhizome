(asdf:defsystem #:rhizome-net
  :description "The rhizome network module: scan, pick, connect — the
network configurator (nmcli + NetworkManager)"
  :author "parenworks"
  :license "AGPL-3.0"
  :depends-on (#:rhizome)
  :components ((:file "net")))
