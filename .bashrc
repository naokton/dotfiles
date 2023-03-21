PS1="[\[\033[0;32m\]\t \[\033[1;36m\]\u\[\033[0;37m\]@\h \[\033[0;32m\]\w\[\033[0m\]]\$ "

export LANG=en_US.UTF-8

[ -d /usr/local/etc/bash_completion.d ] && . /usr/local/etc/bash_completion.d/*

shopt -s histappend
export HISTTIMEFORMAT="%F %T "
export HISTSIZE=10000
export HISTFILESIZE=10000
export HISTCONTROL=ignorespace
export HISTIGNORE="ll:history"

alias ll='ls -Gl'
alias la='ls -GAl'

if [[ $TERM = screen ]] || [[ $TERM = screen-256color ]] ; then
    LOGDIR=$HOME/Documents/TerminalLog
    LOGFILE=$(date +%Y%m%d_%H%M%S)_#D_#P.log
    [ ! -d $LOGDIR ] && mkdir -p $LOGDIR
    tmux set-option default-terminal "screen" \; \
      pipe-pane       "cat >> $LOGDIR/$(date +%Y%m%d_%H%M%S)_#I-#P.log" \; \
      display-message "Started logging to $LOGDIR/$LOGFILE"
fi

source $HOME/.docker/init-bash.sh || true # Added by Docker Desktop
