#!/bin/bash

xcode-select --install
ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
curl -L http://install.ohmyz.sh | sh
brew bundle Brewfile
rake install
