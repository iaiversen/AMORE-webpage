#!/bin/bash
set -e

echo "Installing Quarto..."
QUARTO_VERSION="1.6.40"
QUARTO_URL="https://github.com/quarto-dev/quarto-cli/releases/download/v${QUARTO_VERSION}/quarto-${QUARTO_VERSION}-linux-amd64.tar.gz"

# Download and extract Quarto
mkdir -p ~/.local/bin
mkdir -p quarto-temp

curl -L $QUARTO_URL | tar xzf - -C quarto-temp


echo "Extracting Quarto..."
tar -xzf quarto.tar.gz -C quarto-temp

echo "Installing quarto..."
ls -la quarto-temp

cp -r quarto-temp/quarto-${QUARTO_VERSION}/bin/* ~/.local/bin/

echo "Setting up Quarto binary..."
# Check if quarto binary exists in the source
ls -la ~/.local/quarto/bin

# Remove existing quarto binary if it exists
if [ -L ~/.local/bin/quarto ] || [ -e ~/.local/bin/quarto ]; then
  echo "Removing existing quarto binary..."
  rm -f ~/.local/bin/quarto
fi

# create symbolic link 
echo "Creating symbolic link..."
ln -s ~/.local/quarto/bin/quarto ~/.local/bin/quarto

# Make sure the binary is executable
chmod +x ~/.local/quarto/bin/quarto

echo "testing path..."
echo $PATH 

echo "Verifying installation..."
ls -la ~/.local/bin/quarto

# Print the target of the symbolic link
readlink -f ~/.local/bin/quarto

echo "Quarto installation complete. Attempting to run quarto --version..."
~/.local/bin/quarto --version || { echo "Quarto version check failed"; }

# Clean up
echo "Cleaning up..."
rm -rf quarto-temp
rm -f quarto.tar.gz