#!/bin/bash
CUR_DIR=$(dirname $0)

copy_file(){
    FILE=$1
    DIR=$2
    [ -n "$DIR" ] && mkdir -p $CUR_DIR/$DIR
    cp $FILE $CUR_DIR/$DIR
}

# copy only file
copy_file ~/.screenrc
copy_file ~/.bashrc
copy_file ~/.zshrc
copy_file ~/.tmux.conf
copy_file ~/.vimrc
copy_file ~/.ripgreprc

# mkdir and copy file into it
copy_file ~/.emacs.d/init.el    .emacs.d
copy_file ~/.config/pycodestyle .config

exit 0
