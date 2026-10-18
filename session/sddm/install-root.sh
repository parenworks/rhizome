#!/bin/bash
# Privileged setup (Omarchy only): run as root (sudo/pkexec). Installs
# the session picker so SDDM lets you choose between Hyprland and
# Rhizome at login.
#
# Non-Omarchy users: skip this file. Register your display manager's
# own session entry instead — the Exec line is just
# ~/.local/bin/rhizome-session (see session/install.sh).
set -euo pipefail

STAGING="$(cd "$(dirname "$0")" && pwd)"

# The .desktop template carries __HOME__ because SDDM needs an absolute
# path and expands neither ~ nor $HOME.
if [ -z "${SUDO_USER:-}" ]; then
  echo "install-root.sh: run me with sudo from a user shell (SUDO_USER unset)" >&2
  exit 1
fi
USER_HOME="$(getent passwd "$SUDO_USER" | cut -d: -f6)"

# 1. Install the river compositor (needed by the Rhizome session)
omarchy pkg add river

# 2. Disable autologin so the greeter with session picker is shown.
#    NOTE: SDDM 0.21 parses ALL files in sddm.conf.d (no *.conf filter),
#    so the config must leave the directory entirely. Subdirectories are
#    not read, making sddm.conf.d/disabled/ a safe parking spot.
mkdir -p /etc/sddm.conf.d/disabled
for f in /etc/sddm.conf.d/autologin.conf /etc/sddm.conf.d/autologin.conf.disabled; do
  if [ -f "$f" ]; then
    mv "$f" /etc/sddm.conf.d/disabled/autologin.conf
  fi
done

# 3. Install the omarchy-sessions theme (a copy; the stock omarchy theme stays untouched)
rm -rf /usr/share/sddm/themes/omarchy-sessions
cp -r "$STAGING/theme" /usr/share/sddm/themes/omarchy-sessions

# 4. Register the Rhizome session so SDDM lists it
sed "s|__HOME__|$USER_HOME|g" "$STAGING/rhizome.desktop" \
  > /usr/share/wayland-sessions/rhizome.desktop

# 5. Point SDDM at the new theme (overrides 10-theme.conf / 99-omarchy-login.conf)
cp "$STAGING/99-theme-sessions.conf" /etc/sddm.conf.d/99-theme-sessions.conf

echo
echo "Done. Next boot: kernel selection -> SDDM greeter -> pick Hyprland or Rhizome (river)."
