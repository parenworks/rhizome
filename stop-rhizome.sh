#!/bin/bash
# Stop rhizome and river (WSL2 development environment)

pkill -f "sbcl.*rhizome" 2>/dev/null
pkill -f "river" 2>/dev/null
pkill alacritty 2>/dev/null

echo "Rhizome and river stopped."
