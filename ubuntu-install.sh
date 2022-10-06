#!/usr/bin/bash

sudo apt update
sudo apt install zsh stow vim neovim tmux emacs git curl -y

# install zsh
export CHSH=yes
export RUNZSH=no
sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# install vundle
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/vim/bundle/Vundle.vim

stow --adopt .

exec zsh
