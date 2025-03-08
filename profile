#!/bin/sh

# Environment variables
export LANG=en_US.UTF-8
export EDITOR=vim
export PATH=${HOME}/.local/share/perl5/bin:${HOME}/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/usr/local/bin:/usr/sbin:/sbin:/usr/local/sbin
export PS1="$ "

# Disable terminal flow control
stty -ixon
