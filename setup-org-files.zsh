#!/usr/bin/env zsh

echo "\n<<< Starting Org Files Setup >>>\n"

DIR="/Users/eerickson/Documents/org"

if [ -d $DIR ]; then
    echo "Org directory exists, skipping install..."
else
    echo "Org directory does not exist, creating symlink..."
    ln -s ~/Library/Mobile\ Documents/com\~apple\~CloudDocs/Documents/org/ ~/Documents/org
fi
