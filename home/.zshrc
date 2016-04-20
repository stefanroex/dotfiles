if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

unsetopt correct_all
unsetopt correct

source $HOME/.zsh/aliases
source $HOME/.zsh/functions

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color
export EDITOR='mvim -v'
# export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="./bin:$PATH"
