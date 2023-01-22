#!/usr/bin/env zsh

echo "\n<<< Starting Doom Emacs Setup >>>\n"

if exists ~/.emacs.d/bin/doom; then
    echo "Doom Emacs exists, skipping install..."
else
    echo "Doom Emacs doesn't exist, continuing with install..."
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d ~/.emacs.d/bin/doom install
fi
