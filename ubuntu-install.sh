#!/usr/bin/bash

sudo apt update
sudo apt install zsh stow vim neovim tmux emacs git curl -y

# install zsh
export CHSH=yes
export RUNZSH=no
[ -d "$ZSH" ] || sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# install vundle
mkdir -p ~/.vim/bundle
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# install spacemacs
[ -d ~/.emacs.d ] || git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

stow --adopt .

exec zsh
