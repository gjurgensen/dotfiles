#!/usr/bin/env bash

set -o errexit

mkdir -p ${HOME}/.config

# Create symbolic links

links=".bashrc .config/kitty .config/nvim .emacs.d .git .gitconfig .subversion .tmux.conf .zlogout .zshrc .vimrc"

for link in $links; do
  source=${HOME}/dotfiles/$link
  destination=${HOME}/$link

  if [ -e $destination ]; then
    echo "Creating backup of $destination"
    mv $destination $destination.bak.$(date +'%y-%m-%d-%T')
  fi

  if [ -L $destination ]; then
    echo "Removing symbolic link $destination"
    rm $destination
  fi

  echo "Creating symbolic link $destination"
  ln -sn $source $destination
done


# Install vundle
mkdir -p ~/.vim/bundle
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
