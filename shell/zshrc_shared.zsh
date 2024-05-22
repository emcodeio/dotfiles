#!/usr/bin/env zsh

# Instant Prompt Initialization (if applicable)
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Load Zsh Plugin Manager (Antidote) early to handle plugins
source /opt/homebrew/opt/antidote/share/antidote/antidote.zsh

# Theme  Configuration
[[ ! -f $HOME/.p10k.zsh ]] || source $HOME/.p10k.zsh

# Command Line Completion Initialization
autoload -Uz compinit && compinit

# Antidote Plugin Loading
antidote load

# Prompt Initialization
autoload -Uz promptinit && promptinit && prompt pure

# Powerlevel10k Theme Initialization
source "$(antidote home)/https-COLON--SLASH--SLASH-github.com-SLASH-romkatv-SLASH-powerlevel10k/powerlevel10k.zsh-theme"

# Source environment variables, functions, aliases, etc.
source "$HOME/.dotfiles/shell/functions_shared.zsh"
source "$HOME/.dotfiles/shell/env_vars_shared.zsh"
source "$HOME/.dotfiles/shell/aliases_shared.zsh"
source "$HOME/.dotfiles/private_info/private_info.zsh"

# Machine type specific sources
if [ "$MACHINE_TYPE" = "work" ]; then
  source "$HOME/.dotfiles/shell/zshrc_work.zsh"
  source "$HOME/.dotfiles/shell/env_vars_work.zsh"
  source "$HOME/.dotfiles/shell/aliases_work.zsh"
else
  source "$HOME/.dotfiles/shell/zshrc_personal.zsh"
  source "$HOME/.dotfiles/shell/env_vars_personal.zsh"
  source "$HOME/.dotfiles/shell/aliases_personal.zsh"
fi
