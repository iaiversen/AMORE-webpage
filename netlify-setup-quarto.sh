#!/bin/bash
set -e

echo "Installing Quarto..."
QUARTO_VERSION="1.6.40"
QUARTO_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"

# Download and extract Quarto
mkdir -p quarto-temp
curl -L $QUARTO_URL | tar xzf - -C quarto-temp

# Make Quarto available on PATH
mkdir -p ~/.local/bin
cp -r quarto-temp/quarto-${QUARTO_VERSION}/bin/* ~/.local/bin/
chmod +x ~/.local/bin/quarto
export PATH=~/.local/bin:$PATH

# Verify installation
echo "Quarto installation complete. Version:"
~/.local/bin/quarto --version || { echo "Quarto installation failed"; exit 1; }

# Clean up
rm -rf quarto-temp