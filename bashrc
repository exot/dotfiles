#!/bin/bash

# Based on example .bashrc as provided by Debian

# If not running interactively, don't do anything
[[ -z "$PS1" ]] && return

# Common Environment variables are defined in .profile
source $HOME/.profile

# History control
HISTCONTROL=ignoreboth
HISTSIZE=20000

# Append to the history file, don't overwrite it
shopt -s histappend

# Correctly save multiline commands in the history
shopt -s cmdhist
shopt -s lithist

# No clobbering
shopt -o -s noclobber

# Extended globbing
shopt -s extglob
shopt -s globstar

# cd to directory if an invalid command is the name of a directory
shopt -s autocd

# Fix typing errors
shopt -s cdspell
shopt -s dirspell

# Check the window size after each command and, if necessary,
# Update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[[ -x /usr/bin/lesspipe ]] && eval "$(SHELL=/bin/sh lesspipe)"

# Set prompt
if [[ "$TERM" = "dumb" ]] && ! [[ "$INSIDE_EMACS" =~ ,comint ]]; then
  # Use simple prompt for dumb terminals, as used by Emacs' TRAMP, but not when we are in Emacs'
  # comint mode (like shell)
  PS1="\u@\h:\w$ "
else
  PURPLE="\e[0;35m"
  YELLOW="\e[0;32m"
  NOCOLOR="\e[m"
  if [[ -f ~/.config/bash/git-prompt.sh ]]; then
    GIT_PROMPT="\$(__git_ps1 \"${PURPLE}─(${YELLOW}%s${PURPLE})${NOCOLOR}\")"
  else
    GIT_PROMPT=''
  fi
  PS1="┌\[${PURPLE}[${NOCOLOR}\u@\h${PURPLE}]─[${NOCOLOR}\w${PURPLE}]─[${NOCOLOR}\t${PURPLE}]${GIT_PROMPT}${NOCOLOR}\]\n└── "
  unset GIT_PROMPT PURPLE YELLOW RED NOCOLOR
fi

# Enable color support of ls and also add handy aliases
if [[ -x /usr/bin/dircolors ]]; then
  if [[ -r ~/.dircolors ]]; then
    eval "$(dircolors -b ~/.dircolors)"
  else
    eval "$(dircolors -b)"
  fi

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
if [[ -f /etc/bash_completion ]] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Load custom functions
if [[ -e ~/.config/bash/scripts.sh ]]; then
  source ~/.config/bash/scripts.sh;
fi
