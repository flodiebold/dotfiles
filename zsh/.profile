export PATH="$HOME/.cargo/bin:$HOME/go/bin:$HOME/.go/bin:$HOME/.local/bin:$HOME/.pulumi/bin:$HOME/Software/flutter/bin:$PATH"

eval "$(gpg-agent --daemon --enable-ssh-support)"
export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket):$(pidof gpg-agent):1
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export BROWSER=/usr/bin/google-chrome

export EDITOR=emacsclient

export GO111MODULE=on

. "$HOME/.cargo/env"
