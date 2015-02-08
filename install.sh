#!/bin/bash

xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
curl https://raw.githubusercontent.com/creationix/nvm/v0.23.3/install.sh | bash
brew doctor
brew update

brew install direnv
brew install exiftool
brew install git
brew install heroku-toolbelt
brew install macvim --override-system-vim
brew install openssl
brew install rbenv
brew install ruby-build
brew install ssh-copy-id
brew install the_silver_searcher
brew install zsh

brew cleanup

brew install caskroom/cask/brew-cask
brew tap caskroom/versions

brew cask install 1password
brew cask install alfred
brew cask install apptrap
brew cask install arq
brew cask install dropbox
brew cask install firefox
brew cask install fluid
brew cask install flux
brew cask install google-chrome
brew cask install hipchat
brew cask install iterm2
brew cask install mailbox
brew cask install seil
brew cask install skype
brew cask install spotify
brew cask install steam
brew cask install subsmarine
brew cask install transmission
brew cask install tvshows
brew cask install vlc
brew cask install wkhtmltopdf

brew cask alfred link
brew cask cleanup
rake install
