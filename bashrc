# -*- sh -*-

# Based on example .bashrc as provided by Debian

# This file is only meant for interactive shells
[[ $- != *i* ]] && return

# Environment variables go here
export LANG=en_US.UTF-8
export PATH=${HOME}/.local/share/perl5/bin:${HOME}/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/usr/local/bin:/usr/sbin:/sbin:/usr/local/sbin

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# History control
HISTCONTROL=ignoreboth
HISTZIE=20000

# Append to the history file, don't overwrite it
shopt -s histappend

# Correctly save multiline commands in the history
shopt -s cmdhist
shopt -s lithist

# No clobbering
shopt -o -s noclobber

# Extended globbing
shopt -s extglob

# Check the window size after each command and, if necessary,
# Update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# Simple prompt
if [[ "$TERM" = "dumb" ]]; then
  PS1="\u@\h:\w$ "
else
  if [[ -f ~/.config/bash/git-prompt.sh ]]; then
    GIT_PROMPT='$(__git_ps1 "(%s)")'
  else
    GIT_PROMPT=''
  fi
  PS1="┌[\e[0;32m\u@\h\e[m] [\e[0;31m\w\e[m] [\t] $GIT_PROMPT\n└── "
  unset GIT_PROMPT
fi

# Enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
  test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
  alias ls='ls --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# Some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'
alias ..='cd ..'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

