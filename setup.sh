#!/bin/bash

set -euo pipefail

SOURCE_DIR="$(dirname "$0")"

function setup-config-file () {
  local SOURCE_NAME=$1
  local TARGET_NAME=${2:-.$SOURCE_NAME}

  if [[ -e "$HOME/$TARGET_NAME" ]]; then
    echo "File $TARGET_NAME already exists, not creating symbolic link"
  else
    mkdir -p "$(dirname "$HOME"/"$TARGET_NAME")"
    ln -s "$SOURCE_DIR/$SOURCE_NAME" "$HOME/$TARGET_NAME"
    echo "Created symbolic link $HOME/$TARGET_NAME"
  fi
}

setup-config-file Xresources
setup-config-file ansible.cfg
setup-config-file bashrc
setup-config-file bash_profile
setup-config-file config/bash
setup-config-file config/systemd
setup-config-file inputrc
setup-config-file profile
setup-config-file screenrc
setup-config-file tmux.conf
setup-config-file vimrc
setup-config-file xmonad/xmonad.hs
setup-config-file xsessionrc
setup-config-file zprofile
setup-config-file zshrc

exit 0
