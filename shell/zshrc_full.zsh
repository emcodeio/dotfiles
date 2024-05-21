#!/usr/bin/env zsh

# Instant Prompt Initialization (if applicable)
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Theme and Prompt Configuration
[[ ! -f $HOME/.p10k.zsh ]] || source $HOME/.p10k.zsh

# Environment Variables
export PATH=/opt/homebrew/bin:$PATH
export PATH=/opt/homebrew/sbin:$PATH
export PATH=$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.emacs.d/bin/:$PATH
export DEFAULT_MVN_VERSION=3.6.3
export HOMEBREW_CASK_OPTS="--no-quarantine"

# Antidote Plugin Manager Initialization
source $(brew --prefix)/opt/antidote/share/antidote/antidote.zsh

# Command Line Completion Initialization
autoload -Uz compinit && compinit

# Antidote Plugin Loading
antidote load

# Prompt Initialization
autoload -Uz promptinit && promptinit && prompt pure

# Alias and Function Sourcing
source "$HOME/.dotfiles/shell/aliases_shared.zsh"
source "$HOME/.dotfiles/shell/functions_shared.zsh"

# Conditional Machine-Specific Configuration
if [ "$MACHINE_TYPE" = "work" ]; then
    source "$HOME/.dotfiles/shell/aliases_work.zsh"
else
    source "$HOME/.dotfiles/shell/aliases_personal.zsh"
fi

# Private Information Sourcing
source "$HOME/.dotfiles/private_info/private_info.zsh"

# Powerlevel10k Theme Initialization
source "$(antidote home)/https-COLON--SLASH--SLASH-github.com-SLASH-romkatv-SLASH-powerlevel10k/powerlevel10k.zsh-theme"
