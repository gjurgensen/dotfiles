#!/usr/bin/bash

sudo apt update
sudo apt install zsh stow vim neovim tmux emacs git curl unzip -y

# install zsh
export CHSH=yes
export RUNZSH=no
[ -d "$ZSH" ] || sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"

# install vundle
mkdir -p ~/.vim/bundle
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim

# install spacemacs
[ -d ~/.emacs.d ] || git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d

# install fira code font
if [ ! -f ~/.local/share/fonts/FiraCode-Regular.ttf ]; then
  mkdir fira
  cd fira
  curl https://fonts.google.com/download?family=Fira%20Code > fira_code.zip
  unzip fira_code.zip
  mkdir -p ~/.local/share/fonts
  mv static/FiraCode-Regular.ttf ~/.local/share/fonts/FiraCode-Regular.ttf
  cd ..
  rm -rf fira
fi

# load terminal profiles
dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal-profiles.dconf

stow --adopt .

echo "Install finished; restart shell to complete install."
