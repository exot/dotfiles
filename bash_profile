if [ -n "$PS1" -a "x$TERM" != "xdumb" -a "`zsh -c '[[ "a" =~ "a" ]] && echo 1' 2> /dev/null`" ]
  then
    exec zsh --login
fi

[[ -r ~/.bashrc ]] && . ~/.bashrc
[[ -r /etc/bashrc ]] && . /etc/bashrc
