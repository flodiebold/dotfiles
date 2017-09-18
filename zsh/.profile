export PATH="$HOME/.cargo/bin:$HOME/.go/bin:$HOME/.local/bin:$PATH"

eval $(gpg-agent --daemon --enable-ssh-support)

export RUST_SRC_PATH=$HOME/Software/rust/src

BROWSER=/usr/bin/google-chrome
export BROWSER
