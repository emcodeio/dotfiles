#!/usr/bin/env zsh

function mkcd() {
  mkdir -p "$@" && cd "$_";
}
