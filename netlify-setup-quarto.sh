#!/bin/bash
set -ex

echo "Installing Quarto..."
QUARTO_VERSION="1.3.45"
QUARTO_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"

# Download the Debian package
echo "Downloading Quarto DEB package..."
curl -L -o quarto.deb $QUARTO_URL

# Install the package
echo "Installing Quarto package..."
sudo dpkg -i quarto.deb

# Verify installation
echo "Quarto installation complete. Version:"
quarto --version

# Clean up
rm -f quarto.deb