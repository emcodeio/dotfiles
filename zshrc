if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# export PATH=$HOME/bin:/opt/homebrew/bin:/usr/local/bin:$PATH

#export ZSH="/Users/eerickson/.oh-my-zsh"
export ZSH="$HOME/.oh-my-zsh"

# eval "$(/opt/homebrew/bin/brew shenv)"

ZSH_THEME="powerlevel10k/powerlevel10k"

# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# CASE_SENSITIVE="true"

# HYPHEN_INSENSITIVE="true"

# DISABLE_AUTO_UPDATE="true"

# DISABLE_UPDATE_PROMPT="true"

# export UPDATE_ZSH_DAYS=13

# DISABLE_MAGIC_FUNCTIONS="true"

# DISABLE_LS_COLORS="true"

# DISABLE_AUTO_TITLE="true"

# ENABLE_CORRECTION="true"

# COMPLETION_WAITING_DOTS="true"

# DISABLE_UNTRACKED_FILES_DIRTY="true"

# HIST_STAMPS="mm/dd/yyyy"

# ZSH_CUSTOM=/path/to/new-custom-folder

plugins=(git)

source $ZSH/oh-my-zsh.sh

export PATH=/usr/local/opt/llvm/bin:$PATH
export PATH=/opt/homebrew/bin:/opt/homebrew/sbin:$HOME/bin:$PATH
export PATH=$HOME/.local/bin:$PATH
export PATH=$HOME/.emacs.d/bin/:$PATH

# export PATH="/Library/Java/JavaVirtualMachines/graalvm-ce-java11-22.2.0/Contents/Home/bin:$PATH"
# export JAVA_HOME="/Library/Java/JavaVirtualMachines/graalvm-ce-java11-22.2.0/Contents/Home"

export DEFAULT_MVN_VERSION=3.6.3

# export PATH="/usr/local/opt/openjdk@11/bin:$PATH"
# export JAVA_HOME="/Library/Java/JavaVirtualMachines/openjdk-11.jdk/Contents/Home"

export PATH="/Library/Java/JavaVirtualMachines/jdk-11.0.8.jdk/Contents/Home/bin:$PATH"
export JAVA_HOME="/Library/Java/JavaVirtualMachines/jdk-11.0.8.jdk/Contents/Home"

# export MANPATH="/usr/local/man:$MANPATH"

# export LANG=en_US.UTF-8

# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# export ARCHFLAGS="-arch x86_64"

# alias zshconfig="mate $HOME/.zshrc"
# alias ohmyzsh="mate $HOME/.oh-my-zsh"

alias doomsync="$HOME/.emacs.d/bin/doom sync"
alias doomdoctor="$HOME/.emacs.d/bin/doom doctor"
alias doomupgrade="$HOME/.emacs.d/bin/doom upgrade"
alias doompurge="$HOME/.emacs.d/bin/doom purge"
alias doomclean="$HOME/.emacs.d/bin/doom clean"
alias doomreset="rm -rf $HOME/.emacs.d/.local/straight && doomsync"

# alias yta-aac="youtube-dl --extract-audio --audio-format aac "
# alias yta-best="youtube-dl --extract-audio --audio-format best "
# alias yta-flac="youtube-dl --extract-audio --audio-format flac "
# alias yta-m4a="youtube-dl --extract-audio --audio-format m4a "
# alias yta-mp3="youtube-dl --extract-audio --audio-format mp3 "
# alias yta-opus="youtube-dl --extract-audio --audio-format opus "
# alias yta-vorbis="youtube-dl --extract-audio --audio-format vorbis "
# alias yta-wav="youtube-dl --extract-audio --audio-format wav "
# alias ytv-best="youtube-dl -f bestvideo+bestaudio "

[[ ! -f $HOME/.p10k.zsh ]] || source $HOME/.p10k.zsh

export MANPAGER="sh -c 'col -bx | bat -l man -p'"

alias ls='exa -alF --color=always --group-directories-first' # my preferred listing
alias la='exa -aF --color=always --group-directories-first'  # all files and dirs
alias ll='exa -lF --color=always --group-directories-first'  # long format
alias lt='exa -aT --level=2 --color=always --group-directories-first' # tree listing
alias l.='exa -a | egrep "^\."'

alias yta-best="yt-dlp --extract-audio --audio-format best "
alias ytv-best="yt-dlp -f bestvideo+bestaudio --merge-output-format mkv "
alias ytd="yt-dlp -f "
alias ytl="yt-dlp -F "

alias doom-config="open -a emacs $HOME/.doom.d/config.org"
alias doom-pack="open -a emacs $HOME/.doom.d/packages.el"
alias doom-init="open -a emacs $HOME/.doom.d/init.el"
alias doom-open="open -a emacs "
alias zsh-config="open -a emacs $HOME/.zshrc"
alias emacs="open -a emacs"

alias dotpdf="$HOME/.dotfiles/scripts/dot2pdf.sh"
alias c320="$HOME/.dotfiles/scripts/convert_to_320_aac.sh . $HOME/Music/encoded"
alias c96="$HOME/.dotfiles/scripts/convert_to_96_aac.sh . $HOME/Music/encoded"

alias dev="cd $HOME/dev"
alias dotfiles="cd $HOME/.dotfiles"
alias rep="cd $HOME/dev/colleague/Representation"
alias repwork="cd $HOME/dev/colleague/Representation/workspace"
alias icgen="cd $HOME/dev/colleague/icGenerator"
alias icgenviz="cd $HOME/dev/colleague/icGenerator/src/main/resources/tmp"
alias gotosync="cp -a $HOME/.dotfiles/obsidian/GoTo\ Elimination/. $HOME/dev/fom/GraphNormaliztion/Goto\ Elimination\ Docs"
alias pullrep="rep && $HOME/.dotfiles/scripts/mvOutVizCode.sh && git restore src/* && git pull && $HOME/.dotfiles/scripts/mvInVizCode.sh"
alias mvoutrep="$HOME/.dotfiles/scripts/mvOutVizCode.sh"
alias mvinrep="$HOME/.dotfiles/scripts/mvInVizCode.sh"
alias dotclean="rm *.dot *.pdf"
alias mkcobol="$HOME/.dotfiles/scripts/make-cobol.sh"
alias newvlc="open -n /Applications/VLC.app"

alias python="python3"
alias pip="pip3"

# alias brew="/opt/homebrew/bin/brew"
alias brewupdate="echo 'Updating Homebrew and Casks' && brew cu -af && echo 'Upgrading Packages' && brew upgrade"

alias updateapps="echo 'Updating Homebrew and Casks...' && brew cu -af && echo 'Upgrading Packages...' && brew upgrade && echo 'Updating Apps from App Store...' && mas upgrade && echo 'Updating Doom Emacs...' && doomupgrade"
alias deletezsh="echo 'Delete $HOME/.oh-my-zsh/custom' && rm $HOME/.oh-my-zsh/custom && echo 'Updating ZSH' && exec zsh -l"
alias linkzsh="echo 'Relink .oh-my-zsh/custom' && $HOME/.dotfiles/install && sleep 1s && exec zsh"

alias mv4k="echo 'Moving 4K movies to server...' && mv $HOME/inbox/00_movies_4k_to_transfer/* /Volumes/files/plex/movies_4k/"
alias mvmovies="echo 'Moving movies to server...' && mv $HOME/inbox/00_movies_to_transfer/* /Volumes/files/plex/movies/"
alias mvporn="echo 'Moving porn to server...' && mv $HOME/inbox/00_porn_to_transfer/* /Volumes/files/plex/videos/"
alias mvfiles="mv4k && mvmovies && mvporn"

function mkcd() {
  mkdir -p "$@" && cd "$_";
}

[[ ! -f $HOME/.dotfiles/p10k.zsh ]] || source $HOME/.dotfiles/p10k.zsh
