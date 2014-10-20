ZSH=$HOME/.oh-my-zsh
ZSH_THEME="fwalch"

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

plugins=(git brew gem osx tmux heroku)

unsetopt correct_all
unsetopt correct
DISABLE_CORRECTION="true"

source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/zsh/aliases
source $HOME/.dotfiles/zsh/functions

export EDITOR='mvim -v'

export RUBY_FREE_MIN=200000
export RUBY_GC_HEAP_FREE_SLOTS=200000
export RUBY_GC_HEAP_INIT_SLOTS=1000000
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_HEAP_SLOTS_INCREMENT=1000000

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./bin:$PATH"
export PATH="/usr/local/heroku/bin:$PATH"
