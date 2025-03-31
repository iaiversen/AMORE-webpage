#!/bin/bash
# This script installs Quarto for the Netlify build process

echo "Installing Quarto..."
wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.40/quarto-1.6.40-linux-amd64.deb
dpkg -x quarto-1.6.40-linux-amd64.deb quarto-temp
mkdir -p ~/.local/bin
cp -r quarto-temp/usr/local/bin/* ~/.local/bin/
cp -r quarto-temp/usr/lib/quarto ~/.local/lib/
export PATH=$PATH:~/.local/bin
echo "Quarto installation complete. Version:"
quarto --version || echo "Failed to install Quarto"
