#!/usr/bin/env bash

sudo apt update
sudo apt install zsh stow vim tmux emacs git curl unzip -y
sudo apt autoremove -y


# Install latest nvim
curl https://github.com/neovim/neovim/releases/download/stable/nvim-linux64.deb > ~/Downloads/nvim-linux64.deb
sudo apt install ~/Downloads/nvim-linux64.deb


# TODO: add (new) Kitty install


# install zsh
export CHSH=yes
export RUNZSH=no
[ -d "$ZSH" ] || sh -c "$(curl -fsSL https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh)"


# install vundle
mkdir -p ~/.vim/bundle
[ -d ~/.vim/bundle/Vundle.vim ] || git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim


# Install Fira Code font
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

# Install Fira Mono italic-hacked font
if [ ! -f ~/.local/share/fonts/Fira\ Mono-Regular\ Italic.otf ]; then
  mkdir fira
  cd fira

  curl -L https://github.com/Avi-D-coder/fira-mono-italic/archive/refs/heads/master.zip > fira_code.zip
  mkdir -p ~/.local/share/fonts
  mv fira-mono-italic-master/distr/otf/*.otf ~/.local/share/fonts

  cd ..
  rm -rf fira
fi


# load terminal profiles
# To write new profiles to gnome-terminal-profiles.dconf, do:
#   dconf dump /org/gnome/terminal/legacy/profiles:/ > gnome-terminal-profiles.dconf
dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal-profiles.dconf


# install terminfo
tic -o ~/.terminfo xterm-256color.terminfo
tic -x -o ~/.terminfo xterm-emacs.terminfo


stow --adopt .
git reset --hard HEAD

echo "Install finished; restart shell to complete install."


