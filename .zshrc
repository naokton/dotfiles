export LANG=en_US.UTF-8

# path
[ -d $HOME/.local/bin ] && export PATH=$HOME/.local/bin:$PATH

# aliases
alias vi="vim"

case ${OSTYPE} in
  darwin*)
    alias ls="ls -G"
    alias ll="ls -Gl"
    alias la="ls -Gla"
    ;;
  linux*)
    alias ls="ls --color=auto"
    alias ll="ls --color=auto -l"
    alias la="ls --color=auto -la"
    ;;
esac

# completion
autoload -U compinit && compinit
command -v pipenv 1>/dev/null 2>&1 && eval "$(pipenv --completion)"

# history
HISTFILE=~/.zhistory
SAVEHIST=9999
HISTSIZE=$(($SAVEHIST * 2))
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY

# color
autoload -Uz colors && colors

# PS1
PROMPT="%B[%F{green}%D{%H:%M:%S}%f %F{cyan}%n%f@%m] %F{green}%2~%f %#%b "
