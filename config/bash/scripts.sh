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

function run-apt-upgrade() {
  local upgrade_seq='sudo systemd-inhibit --what=shutdown:idle:sleep /bin/bash -c "mail && apt clean && apt update && apt upgrade && apt autoremove && tripwire --check --interactive"'
  local host=$1

  if [[ -z $host ]]; then
    echo "No host name provided, exiting."
    exit 1;
  elif [[ $host = $(hostname) || $host = $(hostname --fqdn) ]]; then
    echo "Running upgrade command locally."
    eval "tmux new-session -d -A -s apt-updates \; send-keys '${upgrade_seq}' ENTER\; attach"
  else
    echo "Running upgrade command remotely."
    ssh "$host" -t "tmux new-session -d -A -s apt-updates \; send-keys '${upgrade_seq}' ENTER \; attach"
  fi
}

function tmux-main() {
  tmux attach -t main || \
  tmux new-session -d -A -s main \; \
       send-keys 'journalctl --follow' ENTER \; \
       split-window \; \
       send-keys 'htop' ENTER \; \
       split-window \; \
       select-layout even-vertical \; \
       attach-session -t main
}

# https://unix.stackexchange.com/questions/22834/how-to-uncompress-zlib-data-in-unix
function zlibd() {
  printf "\x1f\x8b\x08\x00\x00\x00\x00\x00" | cat - "$@" | gzip -dc
}
