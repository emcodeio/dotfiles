#!/usr/bin/env zsh

alias mv4k="echo 'Moving 4K movies to server...' && rsync $HOME/inbox/00_movies_4k_to_transfer/* /Volumes/files/plex/movies_4k/"
alias mvmovies="echo 'Moving movies to server...' && rsync $HOME/inbox/00_movies_to_transfer/* /Volumes/files/plex/movies/"
alias mvporn="echo 'Moving porn to server...' && rsyn $HOME/inbox/00_porn_to_transfer/* /Volumes/files/plex/videos/"
alias mvfiles="mv4k && mvmovies && mvporn"
