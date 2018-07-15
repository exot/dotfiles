# Prompt

enable -r repeat
setopt no_beep
setopt no_clobber
bindkey -e
setopt hash_list_all
setopt extended_glob

setopt autopushd
export DIRSTACKSIZE=30
alias d='dirs -v'

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

# Terminal

case "$TERM" in
  "dumb")
    unsetopt zle
    unsetopt prompt_cr
    export PS1="%n@%m:%c\$ "
    ;;
  "rxvt-256color")
    export TERM=rxvt-unicode-256color
    ;;
  "screen")
    export TERM=screen-256color
    ;;
esac

# Screen Title

termtitle() {
  local title

  title="$*"

  case "${TERM}" in
    screen*)
      echo -en "\033k${title}\033\\"
      ;;
    xterm|rxvt*)
      echo -en "\033]2;${title}\a"
      ;;
  esac
}

preexec() {
  last_command="$1"

  if [[ "${TERM}" =~ "screen*" ]] ; then
    termtitle "${last_command}"
  fi
}

if [[ ! "${TERM}" =~ "screen*" ]] ; then
    termtitle "${USER}@${HOST}"
fi

precmd() {
  last_ret=$?

  if [[ "${TERM}" =~ "screen*" ]] ; then
    if [[ ${last_ret} -eq 0 ]] ; then
      termtitle "[${last_command}]"
    else
      termtitle "[${last_ret} ${last_command}]"
    fi

  fi
}

# if no command is running set screen title to ($PWD)
chpwd() {
  if [[ "${TERM}" =~ "screen*" ]] ; then
    last_command="${PWD}"
  fi
}

last_command=$0

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

# Convenience

do-startx() {
    startx $@ >| ~/.xsession-log 2>&1 &
    sleep 1
    vlock
}

function new-ssh-key() {
  rm -f ~/.ssh/$1 ~/.ssh/$1.pub
  ssh-keygen -b 4096 -f ~/.ssh/$1
  cat ~/.ssh/$1.pub | ssh $1 'cat > ~/.ssh/authorized_keys'
}

# Aliases

if ls --version | grep -q coreutils ; then
    LS_COLORS=`dircolors -b | cut -s -d= -f2- | sed -e "s/'\(.*\)';/\1/"`
    LS_OPTIONS+="--color=auto"
    LS_OPTIONS+="--file-type"
    alias ls='ls --color=auto'
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

# Utility functions

function encode-with-ffmpeg () {
    $movie = $1
    $output = $2
    ffmpeg -y -i  $movie -c:v libx264 -preset veryslow -b:v 630k -pass 1 -c:a aac -b:a 128k -f mp4 /dev/null && \
        ffmpeg -i $movie -c:v libx264 -preset veryslow -b:v 630k -pass 2 -c:a aac -b:a 128k $output
}
