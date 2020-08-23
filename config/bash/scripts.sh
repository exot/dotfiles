#!/bin/bash

function do-startx() {
  startx "$@" >| ~/.xsession-log 2>&1 &
  sleep 1
  vlock
}

function encode-with-ffmpeg() {
  movie=$1
  output=$2

  ffmpeg -y -i "$movie" -c:v libx264 -preset veryslow -b:v 630k -pass 1 -c:a aac -b:a 128k -f mp4 /dev/null && \
    ffmpeg -i "$movie" -c:v libx264 -preset veryslow -b:v 630k -pass 2 -c:a aac -b:a 128k "$output"
}

function run-remote-apt-upgrade() {
  host=$1

  ssh "$host" -t "tmux new-session -d -A -s apt-updates \; send-keys 'sudo apt clean && sudo apt update && sudo apt upgrade && sudo apt autoremove && tripwire --check --interactive' ENTER \; attach"
}
