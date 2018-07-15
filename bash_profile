# -*- sh -*-

export LANG=en_US.UTF-8

[[ -r /etc/bashrc ]] && . /etc/bashrc
[[ -r ~/.bashrc ]] && . ~/.bashrc

PATH=$HOME/.local/bin/:$PATH
export PATH

