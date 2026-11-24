# ------------------------------
# General
# ------------------------------
typeset -U path PATH
export EDITOR=vim
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export TERM=xterm-256color
export DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer/

bindkey -e

has() {
  type "$1" > /dev/null 2>&1
}

setopt auto_cd
setopt no_beep
setopt nonomatch
setopt interactivecomments

autoload -Uz promptinit
promptinit

# ------------------------------
# Completion
# ------------------------------
setopt auto_list # 補完候補を一覧で表示する(d)
setopt auto_menu # 補完キー連打で補完候補を順に表示する(d)
setopt list_packed
setopt list_types
bindkey "^[[Z" reverse-menu-complete # Shift-Tabで補完候補を逆順する
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# ------------------------------
# Aliases
# ------------------------------
alias ls='ls -GF'
alias la='ls -a'
alias ll='ls -al'
alias grep='grep --color'
alias symbolicatecrash='/Applications/Xcode.app/Contents/SharedFrameworks/DVTFoundation.framework/Resources/symbolicatecrash'
alias dc='docker compose'
alias ccusage='npx ccusage@latest'
if ! command -v python &> /dev/null && command -v python3 &> /dev/null; then
  alias python=python3
fi

# ------------------------------
# Look & Feel
# ------------------------------
autoload colors
colors

# Set terminal title including current directory
case "${TERM}" in
kterm*|xterm*)
  precmd() {
    echo -ne "\e]2;${PWD:t}\a"
    echo -ne "\e]1;${PWD:t}\a"
  }
  export LSCOLORS=exfxcxdxbxegedabagacad
  export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
  zstyle ':completion:*' list-colors 'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
  ;;
esac

# ------------------------------
# Other
# ------------------------------

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

if has "rbenv"; then
  eval "$(rbenv init -)"
fi
export PATH=${HOME}/.gem/ruby/2.7.0/bin:$PATH

# Android
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
# export PATH=$PATH:$ANDROID_HOME/tools
# export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
if [ -d /Library/Java/JavaVirtualMachines/zulu-17.jdk ]; then
  export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home
else
  export JAVA_HOME=$(/usr/libexec/java_home 2>/dev/null)
fi

# ngrok
export PATH=$PATH:$HOME/.ngrok/bin

# AWS
export AWS_DEFAULT_REGION=ap-northeast-1

# MySQL Client
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

# ------------------------------
# Prezto
# ------------------------------

# 自作テーマ（zsh/prompts/prompt_*_setup）を prompt モジュールから見えるようにする
fpath=("${DOTFILES_DIR:-$HOME/dotfiles}/zsh/prompts" $fpath)

source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

# prezto の utility モジュールが CORRECT を有効にするので、読み込み後に無効化する
unsetopt correct

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/completion.zsh.inc'; fi

# deno
[ -f "$HOME/.deno/env" ] && . "$HOME/.deno/env"
if [[ ":$FPATH:" != *":$HOME/completions:"* ]]; then export FPATH="$HOME/completions:$FPATH"; fi

[ -f "$HOME/.local/bin/env" ] && source "$HOME/.local/bin/env"

# bun completions
[ -s "$HOME/.bun/_bun" ] && source "$HOME/.bun/_bun"

# cloudflared tunnel
cftunnel() { cloudflared tunnel --url http://localhost:${1:-8000}; }

# opencode
alias oc='opencode'

[ -f "$HOME/github/saitoxu/my-whisper/scripts/whisper.sh" ] && source "$HOME/github/saitoxu/my-whisper/scripts/whisper.sh"

diff() {
  command diff --color=auto "$@"
}

alias dnsreset='networksetup -setdnsservers Wi-Fi empty'

# vim: set ft=sh :
