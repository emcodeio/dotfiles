#!/usr/bin/env zsh

function mkcd() {
  mkdir -p "$@" && cd "$_";
}

function extend_path() {
  [[ -d "$1" ]] || return

  if ! $( echo "$PATH" | tr ":" "\n" | grep -qx "$1" ) ; then
    export PATH="$1:$PATH"
  fi
}
