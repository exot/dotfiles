# -*- sh -*-

# General options

enable -r repeat
setopt no_beep
setopt no_clobber
bindkey -e
setopt hash_list_all
setopt extended_glob
setopt ksh_glob

# Prompt

autoload colors ; colors

hostfg=$color[fg-white]
hostbg=$color[bg-black]

setopt prompt_subst
autoload -Uz vcs_info
zstyle ':vcs_info:*' actionformats \
  '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{3}|%F{1}%a%F{5}]%f '
zstyle ':vcs_info:*' formats       \
  '%F{5}(%f%s%F{5})%F{3}-%F{5}[%F{2}%b%F{5}]%f '
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{3}%r'
zstyle ':vcs_info:*' enable git svn hg

autoload -Uz add-zsh-hook
add-zsh-hook precmd vcs_info

VCS='${vcs_info_msg_0_}%f'
RETURN_CODE="%0(?..[%{$fg[red]%}%B%?%b] )"
WHOAMI="%{$fg[$hostfg]$bg[$hostbg]%}[%n@%m]"
DATE="%{$fg[blue]$bg[$hostbg]%}[%D{%Y-%m-%d %H:%M:%S}]"
CWD="%{$fg[red]%}[%3~]"
JOBS="%1(j. %{$fg[green]%}[%j].)"

export PS1="┌$RETURN_CODE$WHOAMI $CWD $VCS$JOBS%b
└── "

unset hostfg hostbg

# History

HISTSIZE=30000
SAVEHIST=30000
HIST_IGNORE_ALL_DUPS=1
HISTFILE=~/.zsh_history

# Terminal

case "$TERM" in
  "dumb")
    unsetopt zle
    unsetopt prompt_cr
    export PS1="> "
    ;;
  "rxvt-256color")
    export TERM=rxvt-unicode-256color
    ;;
  "screen")
    export TERM=screen-256color
    ;;
esac

# Completion

fpath=(~/.config/zsh/completion $fpath)

autoload -Uz compinit
compinit
zmodload -i zsh/complist

compctl -g '*(-/)' + -g '.*(/)' cd chdir dirs pushd rmdir dircmp cl

zstyle ':completion:*:correct:*' original true
zstyle ':completion:*:correct:*' insert-unambiguous false

zstyle ':completion:*' completer _complete _correct _approximate
zstyle ':completion:*' glob true

zstyle ':completion:*' verbose yes
zstyle ':completion:*' menu select
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s%p

zstyle ':completion:*:corrections' format $'%{\e[0;31m%}%d (errors: %e)%{\e[0m%}'
zstyle ':completion:*:descriptions' format $'%{\e[0;31m%}%d%{\e[0m%}'
zstyle ':completion:*:messages' format $'%{\e[0;31m%}%d%{\e[0m%}'
zstyle ':completion:*:warnings' format $'%{\e[0;31m%}No matches for: %d%{\e[0m%}'
zstyle ':completion:*' group-name ''
zstyle -e ':completion:*:approximate:*' max-errors \
    'reply=( $(( ($#PREFIX+$#SUFFIX)/3 )) numeric )'

zstyle ':completion:*' list-colors ''
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

# Dirstack handling

setopt autopushd
DIRSTACKSIZE=30
alias d='dirs -v'

# Aliases

if ls --version | grep -q coreutils ; then
  LS_COLORS=`dircolors -b | cut -s -d= -f2- | sed -e "s/'\(.*\)';/\1/"`
  export LS_COLORS
  alias ls='ls --color=auto'
else
  alias ls='ls --file-type'
fi

alias ll='ls -l'
alias la='ls -al'
alias l='ls -a'
alias rm="rm -i"
alias mv="mv -i"
alias cp='cp -i'
alias ..='cd ..'
alias vi='vim -X -u ~/.vimrc'
alias vim='vim -X -u ~/.vimrc'
alias ec='emacsclient -c'
alias emacs="emacs --no-site-file"
alias nop="nopaste-it -u http://icore.bomuknu.de/paste/"
alias dquilt="quilt --quiltrc=${HOME}/.quiltrc-dpkg"
alias sbcl="sbcl --noinform"
alias grep="grep --color=auto --binary-files=without-match --exclude=.svn"

# Load some functions

if [[ -e ~/.config/bash/scripts.sh ]]; then
  source ~/.config/bash/scripts.sh;
fi
