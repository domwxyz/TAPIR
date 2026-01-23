#!/bin/bash
set -e

INSTALL_DIR="$HOME/.local/bin"
CONFIG_DIR="$HOME/.config/tapir"

# Uninstall
if [ "$1" = "uninstall" ]; then
    echo "Removing tapir..."
    rm -f "$INSTALL_DIR/tapir"
    echo "Done. Config files in $CONFIG_DIR were preserved."
    exit 0
fi

# Check dependencies
command -v ghc >/dev/null 2>&1 || { echo "Error: ghc not found. Install GHC 9.6+"; exit 1; }
command -v cabal >/dev/null 2>&1 || { echo "Error: cabal not found. Install Cabal 3.10+"; exit 1; }

echo "Building tapir..."
cabal build -O2

# Find and install binary
BINARY=$(cabal list-bin tapir)
mkdir -p "$INSTALL_DIR"
cp "$BINARY" "$INSTALL_DIR/tapir"
strip "$INSTALL_DIR/tapir" 2>/dev/null || true
echo "Installed: $INSTALL_DIR/tapir"

# Copy config templates (won't overwrite existing)
mkdir -p "$CONFIG_DIR/languages"
[ -f "$CONFIG_DIR/config.yaml" ] || cp config/config.yaml "$CONFIG_DIR/"
[ -f "$CONFIG_DIR/languages/spanish.yaml" ] || cp languages/spanish.yaml "$CONFIG_DIR/languages/"
echo "Config: $CONFIG_DIR"

# PATH check
case ":$PATH:" in
    *":$INSTALL_DIR:"*) ;;
    *) echo "Warning: $INSTALL_DIR is not in PATH" ;;
esac

echo "Done! Run 'tapir' to start."
