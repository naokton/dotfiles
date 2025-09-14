#!/bin/bash

# install dotfiles by symlinking them to the home directory
# existing files will be backed up with a .backup extension

set -euo pipefail

SRC="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

link_file() {
    local src="$1"
    local dest="$2"

    if [[ ! -e "$src" ]]; then
        echo "Source file $src does not exist, skipping"
        return
    fi

    local dest_dir
    dest_dir="$(dirname "$dest")"

    if [[ ! -d "$dest_dir" ]]; then
        mkdir -p "$dest_dir"
        echo "Created directory $dest_dir"
    fi

    if [[ -L "$dest" ]]; then
        echo "Skipping $dest, symbolic link already exists"
        return
    elif [[ -e "$dest" ]]; then
        echo "Back up $dest to $dest.backup"
        mv "$dest" "$dest.backup"
    fi

    echo "Link $src to $dest"
    ln -s "$src" "$dest"
}

main() {
    local files=(
        ".bashrc"
        ".config/pycodestyle"
        ".config/bat/config"
        ".emacs.d/init.el"
        ".ripgreprc"
        ".screenrc"
        ".tmux.conf"
        ".vimrc"
        ".zshrc"
    )

    for file in "${files[@]}"; do
        local src="$SRC/$file"
        local dest="$HOME/$file"
        link_file "$src" "$dest"
    done
}

main
