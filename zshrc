if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color

autoload -U compinit && compinit

fpath=(~/.dotfiles/zsh/completions $fpath)

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

unsetopt correct_all
unsetopt correct

source $HOME/.dotfiles/zsh/aliases
source $HOME/.dotfiles/zsh/functions

export EDITOR='nvim'

# export TERM=xterm-256color
export TERMINFO="$HOME/.terminfo"

export RUBY_FREE_MIN=200000
export RUBY_GC_HEAP_FREE_SLOTS=200000
export RUBY_GC_HEAP_INIT_SLOTS=1000000
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export POLTERGEIST_TIMEOUT=10

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/.bin:$PATH"
export PATH="/usr/local/heroku/bin:$PATH"
export PATH="./bin:$PATH"
export PATH="/Library/Haskell/bin:$PATH"
