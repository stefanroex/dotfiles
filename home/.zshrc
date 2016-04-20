if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

# Initialize completion
autoload -U compinit
compinit

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

# Disable autocorrection
unsetopt correct_all
unsetopt correct

source $HOME/.zsh/aliases
source $HOME/.zsh/functions

export TERM=xterm-256color
export EDITOR='mvim -v'
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="./bin:$PATH"
