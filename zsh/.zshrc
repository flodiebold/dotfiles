# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
#ZSH_THEME="meins"
ZSH_THEME="meins2"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
COMPLETION_WAITING_DOTS="true"

setopt nolistbeep

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git lein mercurial screen dnf zsh-syntax-highlighting pass cargo docker)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
alias ppass='PASSWORD_STORE_DIR=~/.password-store-privat pass'

if [ -f ~/.local/google-cloud-sdk/completion.zsh.inc ]; then
    source ~/.local/google-cloud-sdk/completion.zsh.inc
    source ~/.local/google-cloud-sdk/path.zsh.inc
fi

if [ -f $HOME/.env-local ]; then
    source $HOME/.env-local
fi

GPG_TTY=$(tty) ; export GPG_TTY

if which fd > /dev/null 2> /dev/null; then
    alias find=fd
fi

if which exa > /dev/null 2> /dev/null; then
    alias ls=exa
fi
