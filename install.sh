#!/bin/bash

git submodule update --init --recursive

xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
brew doctor
brew update

brew install git
brew install heroku/brew/heroku
brew install neovim
brew install openssl
brew install pgcli
brew install rbenv
brew install ruby-build
brew install ssh-copy-id
brew install the_silver_searcher
brew install zsh
brew install httpie

brew cleanup

brew cask install 1password
brew cask install alfred
brew cask install flux
brew cask install slack
brew cask install spectacle
brew cask install spotify
brew cask install tunnelblick

brew cask alfred link
brew cask cleanup

./symlink-dotfiles.sh
