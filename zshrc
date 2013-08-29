ZSH=$HOME/.oh-my-zsh
ZSH_THEME="fwalch"

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

plugins=(git brew gem osx tmux heroku)

unsetopt correct_all

source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/zsh/aliases
source $HOME/.dotfiles/zsh/functions
source $HOME/.rbenv/completions/rbenv.zsh

# Shaves about 0.5s off Rails boot time (when using perf patch). Taken from https://gist.github.com/1688857
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_FREE_MIN=500000

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./bin:$PATH"
