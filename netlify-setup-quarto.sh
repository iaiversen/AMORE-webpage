#!/bin/bash
set -ex

echo "Installing Quarto..."
QUARTO_VERSION="1.3.450"
QUARTO_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"

# Create directories for installation
mkdir -p ~/.local
mkdir -p ~/bin

# Download and extract the tarball
echo "Downloading Quarto..."
curl -L -o quarto.tar.gz $QUARTO_URL

echo "Extracting Quarto..."
tar -xzf quarto.tar.gz

# Move Quarto to the installation directory
echo "Installing Quarto..."
mv quarto-${QUARTO_VERSION} ~/.local/quarto

# Create symbolic link to the binary
echo "Setting up executable link..."
ln -sf ~/.local/quarto/bin/quarto ~/bin/quarto

# Update PATH
export PATH=~/bin:$PATH

# Verify installation
echo "Verifying Quarto installation:"
which quarto
quarto --version

# Clean up
rm -f quarto.tar.gz