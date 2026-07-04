#!/bin/bash
# Start river maximized in WSLg, then launch stumpway
# Usage: ./start-stumpway.sh

set -e

# Kill any existing instances
pkill -f "sbcl.*stumpway" 2>/dev/null || true
pkill -f "river" 2>/dev/null || true
sleep 1

# Clear FASL cache to ensure clean reload
rm -rf "$HOME"/.cache/common-lisp/sbcl-*/home/glenn/SourceCode/stumpway 2>/dev/null || true

# Start river nested in WSLg
WAYLAND_DISPLAY=wayland-0 river -log-level info &
RIVER_PID=$!
sleep 2

# Verify river is running
if ! ls /run/user/1000/wayland-1 2>/dev/null; then
    echo "ERROR: river did not start (no wayland-1 socket)"
    exit 1
fi

echo "; River started (PID $RIVER_PID)"

# Maximize the river window using Windows APIs
/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe -Command '
Add-Type @"
using System;
using System.Runtime.InteropServices;
public class Win32 {
    [DllImport("user32.dll")]
    public static extern bool ShowWindow(IntPtr hWnd, int nCmdShow);
    [DllImport("user32.dll")]
    public static extern bool SetForegroundWindow(IntPtr hWnd);
    public const int SW_MAXIMIZE = 3;
}
"@
Start-Sleep -Seconds 1
$proc = Get-Process msrdc | Where-Object { $_.MainWindowTitle -like "*river*" }
if ($proc) {
    [Win32]::ShowWindow($proc.MainWindowHandle, [Win32]::SW_MAXIMIZE)
    [Win32]::SetForegroundWindow($proc.MainWindowHandle)
    Write-Host "; River window maximized"
} else {
    Write-Host "; WARNING: Could not find river window to maximize"
}
' 2>/dev/null

echo "; Starting stumpway..."
WAYLAND_DISPLAY=wayland-1 sbcl --eval '(ql:quickload :stumpway)' --eval '(stumpway:run)'
