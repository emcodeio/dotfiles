#!/usr/bin/env zsh

alias ls='eza -alF --color=always --group-directories-first' # my preferred listing
alias la='eza -aF --color=always --group-directories-first'  # all files and dirs
alias ll='eza -lF --color=always --group-directories-first'  # long format
alias lt='eza -aT --level=2 --color=always --group-directories-first' # tree listing
alias l.='eza -a | egrep "^\."'

alias doomsync="$HOME/.emacs.d/bin/doom sync"
alias doomdoctor="$HOME/.emacs.d/bin/doom doctor"
alias doomupgrade="$HOME/.emacs.d/bin/doom upgrade"
alias doompurge="$HOME/.emacs.d/bin/doom purge"
alias doomclean="$HOME/.emacs.d/bin/doom clean"
alias doomreset="rm -rf $HOME/.emacs.d/.local/straight && doomsync"

alias yta-best="yt-dlp --extract-audio --audio-format best "
alias ytv-best="yt-dlp -f bestvideo+bestaudio --merge-output-format mkv "
alias ytd="yt-dlp -f "
alias ytl="yt-dlp -F "

# alias yta-aac="youtube-dl --extract-audio --audio-format aac "
# alias yta-best="youtube-dl --extract-audio --audio-format best "
# alias yta-flac="youtube-dl --extract-audio --audio-format flac "
# alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
# alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
# alias yta-opus="youtube-dl --extract-audio --audio-format opus "
# alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
# alias yta-wav="youtube-dl --extract-audio --audio-format wav "
# alias ytv-best="youtube-dl -f bestvideo+bestaudio "

alias doom-config="open -a emacs $HOME/.doom.d/config.org"
alias doom-pack="open -a emacs $HOME/.doom.d/packages.el"
alias doom-init="open -a emacs $HOME/.doom.d/init.el"
alias doom-open="open -a emacs "
alias zsh-config="open -a emacs $HOME/.zshrc"
alias emacs="open -a emacs"

alias reformatvtt="python $HOME/.dotfiles/python-scripts/reformat-vtt.py"

alias dev="cd $HOME/dev"
alias dotfiles="cd $HOME/.dotfiles"
alias newvlc="open -n /Applications/VLC.app"

alias python="python3"
alias pip="pip3"
alias trail='<<<${(F)path}'
alias du="ncdu"
alias src="source $HOME/.zshrc"
alias pei="pipenv install"
alias per="pipenv run python3"

# alias brew="/opt/homebrew/bin/brew"
alias brewupdate="echo 'Updating Homebrew and Casks' && brew cu -af && echo 'Upgrading Packages' && brew upgrade"

alias updateapps="echo 'Updating Homebrew and Casks...' && brew cu -af && echo 'Upgrading Packages...' && brew upgrade && echo 'Updating ZSH Plugins...' && antidote update && echo 'Updating Apps from App Store...' && mas upgrade && echo 'Updating Doom Emacs...' && doomupgrade"
alias deletezsh="echo 'Delete $HOME/.oh-m-zsh/custom' && rm $HOME/.oh-my-zsh/custom && echo 'Updating ZSH' && exec zsh -l"
alias linkzsh="echo 'Relink .oh-my-zsh/custom' && $HOME/.dotfiles/install && sleep 1s && exec zsh"

alias mkw="$HOME/.dotfiles/bin/shell_scripts/make_wallpaper/make_wallpaper.zsh"
alias mkwproc="$HOME/.dotfiles/bin/shell_scripts/make_wallpaper/make_wallpaper.zsh $HOME/Pictures/wallpaper/processing/to_process"
alias mkwdesk="$HOME/.dotfiles/bin/shell_scripts/make_wallpaper/make_wallpaper.zsh -d"
alias mkwphone="$HOME/.dotfiles/bin/shell_scripts/make_wallpaper/make_wallpaper.zsh -p"
alias gptrn="cd $HOME/.dotfiles/bin/python_scripts/gpt_rename_image/ && pipenv run python3 $HOME/.dotfiles/bin/python_scripts/gpt_rename_image/rename_images_unique.py $HOME/.dotfiles/bin/python_scripts/gpt_rename_image/images_to_rename"
alias enhance="$HOME/.dotfiles/bin/shell_scripts/enhance.zsh"
alias linkwall="$HOME/.dotfiles/scripts/symlink_wallpaper"
