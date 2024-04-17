# ------------------------------
# General
# ------------------------------
export EDITOR=vim
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export TERM=xterm-256color
export PATH=$PATH:/usr/local/sbin
export HOMEBREW_CASK_OPTS="--appdir=/Applications"
export DEVELOPER_DIR=/Applications/Xcode.app/Contents/Developer/

bindkey -e

has() {
  type "$1" > /dev/null 2>&1
}

setopt correct
setopt auto_cd
setopt no_beep
setopt nonomatch
setopt interactivecomments
setopt nocorrect

autoload -Uz promptinit
promptinit

# ------------------------------
# Completion
# ------------------------------
autoload -U compinit; compinit
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
alias start-dev='aws ec2 start-instances --profile default --instance-ids i-0a840df9ecfac7bd9 && aws ec2 wait instance-running --profile default --instance-ids i-0a840df9ecfac7bd9'
alias stop-dev='aws ec2 stop-instances --profile default --instance-ids i-0a840df9ecfac7bd9 && aws ec2 wait instance-stopped --profile default --instance-ids i-0a840df9ecfac7bd9'

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

if has "pyenv"; then
  export PYENV_ROOT="${HOME}/.pyenv"
  export PATH="$PATH:${PYENV_ROOT}/bin"
  eval "$(pyenv init -)"
fi

if has "rbenv"; then
  export PATH=~/.rbenv/shims:$PATH
  eval "$(rbenv init -)"
fi
export PATH=${HOME}/.gem/ruby/2.7.0/bin:$PATH

# PHP
export PATH="/usr/local/opt/php@7.4/bin:$PATH"
export PATH="/usr/local/opt/php@7.4/sbin:$PATH"

# Android
export ANDROID_HOME=$HOME/Library/Android/sdk
export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
# export ANDROID_SDK=/Users/yosuke.saito/Library/Android/sdk
# export ANDROID_NDK=/Users/yosuke.saito/android-ndk-r10e

# Read .zsh files
ZSHHOME="${HOME}/.zsh.d"
if [ -d $ZSHHOME -a -r $ZSHHOME -a -x $ZSHHOME ]; then
  for i in $ZSHHOME/**/*; do
    [[ ${i##*/} = *.zsh ]] && [ \( -f $i -o -h $i \) -a -r $i ] && . $i
  done
fi

# AWS
export AWS_ACCESS_KEY_ID=`grep aws_access_key_id ~/.aws/credentials | head -n 1 | awk '{ print $3 }'`
export AWS_SECRET_ACCESS_KEY=`grep aws_secret_access_key ~/.aws/credentials | head -n 1 | awk '{ print $3 }'`
export AWS_DEFAULT_REGION=ap-northeast-1

# binutils
export PATH="/usr/local/opt/binutils/bin:$PATH"

# MySQL Client
# export PATH="/usr/local/opt/mysql-client/bin:$PATH"
export PATH="/opt/homebrew/opt/mysql-client/bin:$PATH"

# ------------------------------
# Prezto
# ------------------------------

source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"

# tabtab source for serverless package
# uninstall by removing these lines or running `tabtab uninstall serverless`
# [[ -f /Users/yosuke.saito/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh ]] && . /Users/yosuke.saito/.config/yarn/global/node_modules/tabtab/.completions/serverless.zsh
# tabtab source for sls package
# uninstall by removing these lines or running `tabtab uninstall sls`
# [[ -f /Users/yosuke.saito/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh ]] && . /Users/yosuke.saito/.config/yarn/global/node_modules/tabtab/.completions/sls.zsh
