#!/usr/bin/env zsh

extend_path "/opt/homebrew/bin"
extend_path "/opt/homebrew/sbin"
extend_path "$HOME/bin"
extend_path "$HOME/.local/bin"
extend_path "$HOME/.emacs.d/bin"
extend_path "$HOME/.dotfiles/bin/python_scripts/builds"

export HOMEBREW_CASK_OPTS="--no-quarantine"
