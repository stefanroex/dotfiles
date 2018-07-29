#!/usr/bin/env bash

set -eu -o pipefail

REPO="https://github.com/stefanroex/dotfiles.git"
TARGET_DIR="$HOME/.dotfiles"

if ! xcode-select --print-path &> /dev/null; then
  xcode-select --install &> /dev/null

  until xcode-select --print-path &> /dev/null; do
    sleep 5
  done
fi

if test ! $(which brew); then
  ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi

if test ! $(which ansible); then
  brew install ansible
fi

if [ ! -d "$TARGET_DIR" ]; then
  mkdir -p "$TARGET_DIR" && git clone "$REPO" "$TARGET_DIR"
fi

cd "$TARGET_DIR"

ansible-playbook main.yml
