#!/bin/zsh
if [ $# -ne 1 ]; then
    echo "collect.sh path/to/copy"
    exit 1
fi

[ ! -d $1 ] && mkdir $1

cp ~/.screenrc $1
cp ~/.bashrc $1
cp ~/.zshrc $1
cp ~/.tmux.conf $1
[ ! -d $1/.emacs.d ] && mkdir $1/.emacs.d
cp ~/.emacs.d/init.el $1/.emacs.d/init.el
cp ~/.vimrc $1
cp ~/.ripgreprc $1

exit 0
