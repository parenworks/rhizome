#!/bin/bash
# Install the rhizome session wiring into your home. No root needed;
# safe to re-run (idempotent overwrites).
#
#   bash session/install.sh
#
# What lands where:
#   river-init             -> ~/.config/river/init            (river runs it at startup)
#   rhizome-session        -> ~/.local/bin/rhizome-session    (the session entry point)
#   rhizome-cmd            -> ~/.local/bin/rhizome-cmd        (WM IPC helper, needs python3)
#   rhizome-session.target -> ~/.config/systemd/user/         (graphical-session anchor)
#   river-portals.conf     -> ~/.config/xdg-desktop-portal/   (portal backend pins)
#
# NOT done here: display-manager registration. Omarchy+SDDM users run
# session/sddm/install-root.sh with sudo; other setups (greetd, ly,
# lightdm, TTY) point their session at ~/.local/bin/rhizome-session.
set -euo pipefail

SRC="$(cd "$(dirname "$0")" && pwd)"

install -Dm755 "$SRC/rhizome-session" "$HOME/.local/bin/rhizome-session"
install -Dm755 "$SRC/rhizome-cmd"     "$HOME/.local/bin/rhizome-cmd"
install -Dm755 "$SRC/river-init"      "$HOME/.config/river/init"
install -Dm755 "$SRC/rhizome-session.target" \
  "$HOME/.config/systemd/user/rhizome-session.target"
install -Dm644 "$SRC/river-portals.conf" \
  "$HOME/.config/xdg-desktop-portal/river-portals.conf"

if systemctl --user daemon-reload 2>/dev/null; then
  echo "; systemd user session reloaded (graphical-session anchor ready)"
else
  echo "; no systemd user session — the anchor stays dormant (fine on"
  echo ";  non-systemd distros; river init's systemctl lines no-op too)"
fi

echo "; Done. Pick Rhizome at your greeter, or from a TTY: ~/.local/bin/rhizome-session"
