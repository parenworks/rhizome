#!/bin/bash
# Stop stumpway and river (WSL2 development environment)

pkill -f "sbcl.*stumpway" 2>/dev/null
pkill -f "river" 2>/dev/null
pkill alacritty 2>/dev/null

echo "Stumpway and river stopped."
