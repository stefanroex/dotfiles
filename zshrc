if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
  source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi

if [[ -n ${EMACS} ]]; then
  zstyle ':prezto:module:terminal' auto-title 'no'
  zstyle ':prezto:module:prompt' theme 'off'
fi

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export TERM=xterm-256color

# Save a ton of history
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

unsetopt correct_all
unsetopt correct

source $HOME/.dotfiles/zsh/aliases
source $HOME/.dotfiles/zsh/functions
