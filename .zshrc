export LANG=en_US.UTF-8

# path
add_path(){
    ADD_PATH=$1
    if [ -d $ADD_PATH ]; then
        PATH=$ADD_PATH:$PATH
    fi
}
ADDITIONAL_PATH=(
    /usr/local/sbin
    $HOME/go/bin
    /usr/local/opt/gnu-sed/libexec/gnubin
    /usr/local/opt/texinfo/bin
    /usr/local/opt/qt/bin
    /usr/local/opt/libressl/bin
    /usr/local/opt/python/libexec/bin
    /usr/local/opt/python@3.8/bin
    /usr/local/opt/python@3.9/bin
    $HOME/Library/Python/3.8/bin
    $HOME/Library/Python/3.9/bin
    $HOME/.poetry/bin
    $HOME/.local/bin
)
for P in $ADDITIONAL_PATH; do
    add_path $P
done
export PATH

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
[ -d ~/.zsh/completion ] && fpath=(~/.zsh/completion $fpath)
autoload -U compinit && compinit -i
#command -v pipenv 1>/dev/null 2>&1 && eval "$(pipenv --completion)"

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

# Misc.

# npm install error workaround
# ref: https://stackoverflow.com/questions/51314888/#answer-55344565
case ${OSTYPE} in
  darwin*)
      export SDKROOT="$(xcrun --show-sdk-path)"
      ;;
esac

export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

#if command -v pyenv 1>/dev/null 2>&1; then
#  eval "$(pyenv init -)"
#fi

# Emacs vterm settings
vterm_printf(){
    if [ -n "$TMUX" ]; then
        # Tell tmux to pass the escape sequences through
        # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}

# fzf history
fh(){
    print -z $(
    fc -nl 1 |
        awk '!a[$0]++' |
        fzf --tac --cycle --no-sort --layout=reverse --height=40%
    )
}
