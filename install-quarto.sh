#!/bin/bash
echo "Installing Quarto..."
wget -q https://github.com/quarto-dev/quarto-cli/releases/download/v1.6.40/quarto-1.6.40-linux-amd64.tar.gz
tar -xzf quarto-1.6.40-linux-amd64.tar.gz
mkdir -p $HOME/.local/bin
cp -r quarto/bin/* $HOME/.local/bin/
export PATH=$PATH:$HOME/.local/bin
quarto --version
