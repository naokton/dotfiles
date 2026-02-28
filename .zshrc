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
    /opt/homebrew/opt/openjdk/bin
    /opt/homebrew/opt/coreutils/libexec/gnubin
    /opt/homebrew/opt/gnu-sed/libexec/gnubin
    /opt/homebrew/opt/gawk/libexec/gnubin
    /opt/homebrew/opt/make/libexec/gnubin
    $HOME/go/bin
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
if [ -e "/Applications/Dev/Emacs.app" ]; then
    alias emacsclient="/Applications/Dev/Emacs.app/Contents/MacOS/bin/emacsclient"
    alias em-t="/Applications/Dev/Emacs.app/Contents/MacOS/bin/emacsclient -t"
    alias em-w="/Applications/Dev/Emacs.app/Contents/MacOS/bin/emacsclient -r"
fi

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

# Emacs vterm integration
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    vterm_printf() {
       if [ -n "$TMUX" ] \
              && { [ "${TERM%%-*}" = "tmux" ] \
                       || [ "${TERM%%-*}" = "screen" ]; }; then
           # Tell tmux to pass the escape sequences through
           printf "\ePtmux;\e\e]%s\007\e\\" "$1"
       elif [ "${TERM%%-*}" = "screen" ]; then
           # GNU screen (screen, screen-256color, screen-256color-bce)
           printf "\eP\e]%s\007\e\\" "$1"
       else
           printf "\e]%s\e\\" "$1"
       fi
   }

   # Directory tracking and prompt tracking
   vterm_prompt_end() {
       vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
   }
   setopt PROMPT_SUBST
   PROMPT=$PROMPT'%{$(vterm_prompt_end)%}'

   # Clear scrollback
   alias clear='vterm_printf "51;Evterm-clear-scrollback";tput clear'

   # Named function prevent corruption with other programs using chpwd
   vterm_update_title() {
       print -Pn "\e]2;%m:%2~\a"
   }
   autoload -U add-zsh-hook
   add-zsh-hook chpwd vterm_update_title
   # Initialize title
   vterm_update_title
fi

# fzf
source <(fzf --zsh)
bindkey -r "^T"  # disable fzf-file-widget
export FZF_DEFAULT_OPTS='--height 40% --tmux bottom,60% --layout reverse'

dev(){
    # Select from all git repositories under ~/Documents
    local repo
    repo=$(fd -H -E 'node_modules' -E '.venv' -E '.git/**' '^.git$' ~/src ~/Documents -d10 --prune --exec dirname |
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

# local configurations
if [[ -f ${HOME}/.zshrc.local ]]; then
    source ${HOME}/.zshrc.local
fi

# uv
if command -v uv >/dev/null 2>&1; then
    eval "$(uv generate-shell-completion zsh)"
    eval "$(uvx --generate-shell-completion zsh)"
fi

if command -v fnm >/dev/null 2>&1; then
    eval "$(fnm env --use-on-cd --version-file-strategy=recursive --shell zsh)"
fi

[ -f "$HOME/.ghcup/env" ] && . "$HOME/.ghcup/env" # ghcup-env

# WSL specifics
if [ -n "$WSL_DISTRO_NAME" ]; then
    export GDK_DPI_SCALE=1.2
fi
