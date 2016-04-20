#!/bin/bash

link() {
  from="$1"
  to="$2"
  echo "Linking '$from' to '$to'"
  rm -f "$to"
  ln -s "$from" "$to"
}

for location in $(find home -name '.*' -maxdepth 1); do
  file="${location##*/}"
  file="${file%.sh}"
  link "$HOME/.dotfiles/$location" "$HOME/$file"
done
