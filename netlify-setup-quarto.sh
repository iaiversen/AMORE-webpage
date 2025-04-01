#!/bin/bash
# This script installs Quarto for the Netlify build process
echo "Installing Quarto..."
wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.40/quarto-1.6.40-linux-amd64.deb
dpkg -i quarto-1.6.40-linux-amd64.deb
quarto --version || echo "Failed to install Quarto"
