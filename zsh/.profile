export VOLTA_HOME="$HOME/.volta"
export PATH="$HOME/.cargo/bin:$HOME/go/bin::$HOME/.go/bin:$HOME/.local/bin:$VOLTA_HOME/bin:$HOME/.pulumi/bin:$PATH"

eval "$(gpg-agent --daemon --enable-ssh-support)"
export GPG_AGENT_INFO=$(gpgconf --list-dirs agent-socket):$(pidof gpg-agent):1
export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)

export BROWSER=/usr/bin/google-chrome

export GO111MODULE=on
