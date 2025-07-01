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
# export PATH=$PATH:$ANDROID_HOME/tools
# export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools
export JAVA_HOME=/Library/Java/JavaVirtualMachines/zulu-17.jdk/Contents/Home

# ngrok
export PATH=$PATH:$HOME/.ngrok/bin

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
# personal
# export PATH="/usr/local/opt/mysql-client/bin:$PATH"
# carat
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

# Invoke tab-completion script to be sourced with the Z shell.
# Known to work on zsh 5.0.x, probably works on later 4.x releases as well (as
# it uses the older compctl completion system).

_complete_invoke() {
    # `words` contains the entire command string up til now (including
    # program name).
    #
    # We hand it to Invoke so it can figure out the current context: spit back
    # core options, task names, the current task's options, or some combo.
    #
    # Before doing so, we attempt to tease out any collection flag+arg so we
    # can ensure it is applied correctly.
    collection_arg=''
    if [[ "${words}" =~ "(-c|--collection) [^ ]+" ]]; then
        collection_arg=$MATCH
    fi
    # `reply` is the array of valid completions handed back to `compctl`.
    # Use ${=...} to force whitespace splitting in expansion of
    # $collection_arg
    reply=( $(invoke ${=collection_arg} --complete -- ${words}) )
}


# Tell shell builtin to use the above for completing our given binary name(s).
# * -K: use given function name to generate completions.
# * +: specifies 'alternative' completion, where options after the '+' are only
#   used if the completion from the options before the '+' result in no matches.
# * -f: when function generates no results, use filenames.
# * positional args: program names to complete for.
compctl -K _complete_invoke + -f invoke inv

# vim: set ft=sh :

# bun
export BUN_INSTALL="$HOME/Library/Application Support/reflex/bun"
export PATH="$BUN_INSTALL/bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/Users/yosuke.saito/Documents/dev/google-cloud-sdk/path.zsh.inc' ]; then . '/Users/yosuke.saito/Documents/dev/google-cloud-sdk/path.zsh.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/Users/yosuke.saito/Documents/dev/google-cloud-sdk/completion.zsh.inc' ]; then . '/Users/yosuke.saito/Documents/dev/google-cloud-sdk/completion.zsh.inc'; fi
if [ -f "/Users/yosuke.saito/.deno/env" ]; then
  . "/Users/yosuke.saito/.deno/env"
fi

# Add deno completions to search path
if [[ ":$FPATH:" != *":/Users/yosuke.saito/completions:"* ]]; then export FPATH="/Users/yosuke.saito/completions:$FPATH"; fi

source "$HOME/.local/bin/env"

alias claude="/Users/yosuke.saito/.claude/local/claude"
