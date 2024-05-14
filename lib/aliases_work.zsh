#!/usr/bin/env zsh

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
alias dotpdf="$HOME/.dotfiles/scripts/dot2pdf.sh"
