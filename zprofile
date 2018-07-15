# General

if [[ -x "/usr/bin/zsh" ]]; then
  SHELL=/usr/bin/zsh
elif [[ -x "/bin/zsh" ]]; then
  SHELL=/bin/zsh
else
  SHELL=/bin/bash
fi
export SHELL

umask 077

export LANG="en_US.utf8"
unset LC_ALL
export TZ=Europe/Berlin

# Environment Variables

export EDITOR="emacsclient -c -a \"\""
export BROWSER="emacsclient -c -a \"\""
export COLORTERM="yes"
export EXTENDED_GLOB=1

# less

LESS=""
# --raw-control-chars
LESS+="r"
# --search-skip-screen
# LESS+="a"
# --CLEAR-SCREEN
LESS+="C"
# --hilite-search
LESS+="g"
# --ignore-case
LESS+="i"
# --status-column
LESS+="J"
# --LONG-PROMPT
LESS+="M"
# --RAW-CONTROL-CHARS
LESS+="R"
# --hilite-search
LESS+="g"
# --shift
LESS+="#5$"

export LESS

eval "$(lesspipe)"

export HISTSIZE=1000
export SAVEHIST=1000
export HIST_IGNORE_ALL_DUPS=1
export HISTFILE=~/.zsh_history

export PYTHONSTARTUP="${HOME}/.pythonrc"
export WORKON_HOME=${HOME}/.local/share/virtualenvs/

export PKG_CONFIG_PATH=$HOME/.local/lib/pkgconfig/
export LD_LIBRARY_PATH=$HOME/.local/lib/

export TEXMFHOME=$HOME/Documents/texmf
export TEXMFOUTPUT=/tmp

export _JAVA_AWT_WM_NONREPARENTING=1

export PERL5LIB="/home/exot/.local/share/perl5/lib/perl5"
export PERL_LOCAL_LIB_ROOT="/home/exot/.local/share/perl5/"
export PERL_MB_OPT="--install_base \"/home/exot/.local/share/perl5/\""
export PERL_MM_OPT="INSTALL_BASE=/home/exot/.local/share/perl5/"

export PATH="/home/exot/.local/share/perl5/bin:${HOME}/.local/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/games:/usr/games:/usr/local/bin:/usr/sbin:/sbin:/usr/local/sbin"
export MANPATH="${PERL_LOCAL_LIB_ROOT}/man:${HOME}/.local/share/man:/usr/share/man"
export INFOPATH="/usr/share/info:${HOME}/.local/share/info/"

export SSH_AGENT_PID=`systemctl show --user ssh-agent.service -p MainPID --value`
export SSH_AUTH_SOCK=/run/user/${UID}/ssh-agent.socket
