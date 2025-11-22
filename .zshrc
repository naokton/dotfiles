# path
add_path(){
    ADD_PATH=$1
    if [ -d $ADD_PATH ]; then
        PATH=$ADD_PATH:$PATH
    fi
}

# latter item has precedence over formers
ADDITIONAL_PATH=(
    /usr/local/bin
    /usr/local/sbin
    $HOME/go/bin
    /usr/local/opt/gnu-sed/libexec/gnubin
    /usr/local/opt/texinfo/bin
    /usr/local/opt/qt/bin
    /usr/local/opt/libressl/bin
    /usr/local/opt/python/libexec/bin
    /usr/local/opt/python@3.8/bin
    /usr/local/opt/python@3.9/bin
    /usr/local/opt/python@3.11/libexec/bin
    /usr/local/opt/python@3.12/libexec/bin
    /opt/homebrew/opt/openjdk/bin
    /opt/homebrew/opt/coreutils/libexec/gnubin
    /opt/homebrew/opt/gnu-sed/libexec/gnubin
    /opt/homebrew/opt/gawk/libexec/gnubin
    /opt/homebrew/opt/make/libexec/gnubin
    $HOME/Library/Python/3.8/bin
    $HOME/Library/Python/3.9/bin
    $HOME/Library/Python/3.11/bin
    $HOME/Library/Python/3.12/bin
    $HOME/.poetry/bin
    $HOME/.local/bin
)

for P in $ADDITIONAL_PATH; do
    add_path $P
done
export PATH

# aliases
alias vi="vim"
alias ls="ls --color=auto"
alias ll="ls --color=auto -l"
alias la="ls --color=auto -la"
[ -f "/Applications/Dev/Emacs.app/Contents/MacOS/Emacs" ] && alias emacs="/Applications/Dev/Emacs.app/Contents/MacOS/Emacs"

# completion
[ -d ~/.zsh/completion ] && fpath=(~/.zsh/completion $fpath)
if type brew &>/dev/null
then
  FPATH="$(brew --prefix)/share/zsh/site-functions:${FPATH}"
fi
autoload bashcompinit && bashcompinit
autoload -Uz compinit && compinit -i
[ -f $HOME/.local/bin/aws_completer ] && complete -C "$HOME/.local/bin/aws_completer" aws

zstyle ':completion:*' menu select

# history
HISTFILE=~/.zhistory
SAVEHIST=9999
HISTSIZE=$(($SAVEHIST * 2))
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
setopt SHARE_HISTORY
setopt APPEND_HISTORY
setopt INC_APPEND_HISTORY
setopt HIST_IGNORE_SPACE

# color
autoload -Uz colors && colors

# PS1
# [11:49:31 user@host] pdir/dir %
PROMPT="%B[%F{green}%D{%H:%M:%S}%f %F{cyan}%n%f@%m] %F{green}%2~%f %#%b "

# Misc.
export MANWIDTH=100

if command -v bat 1>/dev/null 2>&1; then
    export MANPAGER="sh -c 'sed -u -e \"s/\\x1B\[[0-9;]*m//g; s/.\\x08//g\" | bat -p -lman'"
fi

# npm install error workaround
# ref: https://stackoverflow.com/questions/51314888/#answer-55344565
case ${OSTYPE} in
  darwin*)
      export SDKROOT="$(xcrun --show-sdk-path)"
      ;;
esac

export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

# Emacs vterm settings
if [[ "$INSIDE_EMACS" = 'vterm' ]] \
    && [[ -n ${EMACS_VTERM_PATH} ]] \
    && [[ -f ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh ]]; then
	source ${EMACS_VTERM_PATH}/etc/emacs-vterm-zsh.sh
    # Initialize TITLE
    print -Pn "\e]2;%m:%2~\a"
fi

# fzf
source <(fzf --zsh)
bindkey -r "^T"  # disable fzf-file-widget
export FZF_DEFAULT_OPTS='--height 40% --tmux bottom,60% --layout reverse'

dev(){
    # Select from all git repositories under ~/Documents
    local repo
    repo=$(fd -td -H -E 'node_modules' -E '.venv' -E '.git/**' '^.git$' ~/git ~/Documents -d10 --prune --exec dirname |
            fzf --cycle
        )
    [[ -n "$repo" ]] && cd "$repo"
}

sf(){
    # Select ssh host from known hosts
    local host
    host=$(awk '{print $1}' ~/.ssh/known_hosts | \
           sed 's/,.*//' | uniq | fzf --cycle --prompt="SSH Host> ")
    if [[ -n $host ]]; then
        print -z "ssh $host"
    fi
}

fcd(){
    local dir
    dir=$(fc -nl 1 |
            awk '/^cd /&&!a[$0]++' |
            cut -d' ' -f2- |
            egrep -v '^([./]*|-)$' |
            fzf --tac --cycle --no-sort
        )
    [[ -n $dir ]] && cd "$dir"
}

makee(){
    if [[ ! -f Makefile ]]; then
        >&2 echo "No Makefile found" >2
        return 1
    fi

    local target
    target=$(grep -E '^[a-zA-Z0-9_-]+:([^=]|$)' Makefile |
                 awk -F: '{print $1}' |
                 fzf
          )
    [[ -n $target ]] && make "$target"
}


if [[ -f ${HOME}/.zshrc.local ]]; then
    source ${HOME}/.zshrc.local
fi

# uv
if command -v uv >/dev/null 2>&1; then
    eval "$(uv generate-shell-completion zsh)"
    eval "$(uvx --generate-shell-completion zsh)"
fi

# Fix completions for uv run.
_uv_run_mod() {
    if [[ "$words[2]" == "run" && "$words[CURRENT]" != -* ]]; then
        _arguments '*:filename:_files'
    else
        _uv "$@"
    fi
}
compdef _uv_run_mod uv

# nvm node version manager
export NVM_DIR="$HOME/.nvm"
#[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" --no-use  # This loads nvm when you use nvm first
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env
