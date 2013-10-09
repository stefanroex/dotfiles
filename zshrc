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

export RUBY_HEAP_MIN_SLOTS=1000000
export RUBY_HEAP_SLOTS_INCREMENT=1000000
export RUBY_HEAP_SLOTS_GROWTH_FACTOR=1
export RUBY_GC_MALLOC_LIMIT=90000000
export RUBY_HEAP_FREE_MIN=500000
export RUBY_FREE_MIN=200000

export PATH="/usr/local/bin:$PATH"
export PATH="$HOME/.rbenv/shims:$PATH"
export PATH="$HOME/bin:$PATH"
export PATH="./bin:$PATH"

bindkey -v

function zle-line-init zle-keymap-select {
  VIM_PROMPT="%{$fg_bold[yellow]%} [% NORMAL]%  %{$reset_color%}"
  RPS1="${${KEYMAP/vicmd/$VIM_PROMPT}/(main|viins)/} $EPS1"
  zle reset-prompt
}

# Use vim cli mode
bindkey '^P' up-history
bindkey '^N' down-history

# backspace and ^h working even after
# returning from command mode
bindkey '^?' backward-delete-char
bindkey '^h' backward-delete-char

# ctrl-w removed word backwards
bindkey '^w' backward-kill-word

# ctrl-r starts searching history backward
bindkey '^r' history-incremental-search-backward

zle -N zle-line-init
zle -N zle-keymap-select
export KEYTIMEOUT=1
