ZSH=$HOME/.oh-my-zsh
ZSH_THEME="fwalch"

# Save a ton of history
HISTSIZE=10000
HISTFILE=~/.zsh_history
SAVEHIST=10000

plugins=(git rake bundler brew gem osx vi-mode tmux heroku)

source $ZSH/oh-my-zsh.sh
source $HOME/.dotfiles/zsh/aliases
source $HOME/.dotfiles/zsh/functions

# Shaves about 0.5s off Rails boot time (when using perf patch). Taken from https://gist.github.com/1688857
export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=1000000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_FREE_MIN=500000

# Customize to your needs...
#export PATH=/usr/local/bin:/Users/stefanroex/.rbenv/shims:/Users/stefanroex/.rbenv/bin:~/bin:/usr/local/bin:/usr/local/sbin:/usr/local/mysql/bin:/usr/local/git/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/local/oracle/instantclient_10_2
export PATH="/usr/local/bin:$PATH"

# No rbenv init for faster login (needs manual rehash)
export PATH="$HOME/.rbenv/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
