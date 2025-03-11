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
plugins=(git dnf pass rust emacs zoxide direnv mise)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...

if [ -f ~/.local/google-cloud-sdk/completion.zsh.inc ]; then
    source ~/.local/google-cloud-sdk/completion.zsh.inc
    source ~/.local/google-cloud-sdk/path.zsh.inc
fi

if [ -f $HOME/.env-local ]; then
    source $HOME/.env-local
fi

GPG_TTY=$(tty) ; export GPG_TTY
export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket):$(pidof gpg-agent):1
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
gpgconf --launch gpg-agent

# exa (https://github.com/ogham/exa)
if which exa > /dev/null 2> /dev/null; then
    alias ls=exa
    alias tree="exa --tree"
fi

unalias gg # don't shadow the jj gui `gg` by the alias

# nice `time` output
TIMEFMT='%J   %U  user %S system %P cpu %*E total'$'\n'\
'avg shared (code):         %X KB'$'\n'\
'avg unshared (data/stack): %D KB'$'\n'\
'total (sum):               %K KB'$'\n'\
'max memory:                %M MB'$'\n'\
'page faults from disk:     %F'$'\n'\
'other page faults:         %R'
