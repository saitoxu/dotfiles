# ------------------------------
# General
# ------------------------------
export EDITOR=vim
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8
export TERM=xterm-256color
export PATH=$PATH:/usr/local/sbin

bindkey -e

has() {
  type "$1" > /dev/null 2>&1
}

setopt correct
setopt auto_cd
setopt no_beep

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

# ------------------------------
# Look & Feel
# ------------------------------
autoload colors
colors
case ${UID} in
0)
  PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
  PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
  SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
  ;;
*)
  PROMPT="%{${fg[green]}%}%/%%%{${reset_color}%} "
  PROMPT2="%{${fg[green]}%}%_%%%{${reset_color}%} "
  SPROMPT="%{${fg[green]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
    PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
  ;;
esac

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
if has "nvm"; then
  export NVM_DIR="${HOME}/.nvm"
  [ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"
fi

if has "pyenv"; then
  export PYENV_ROOT="${HOME}/.pyenv"
  export PATH="$PATH:${PYENV_ROOT}/bin"
  eval "$(pyenv init -)"
fi

if has "rbenv"; then
  export PATH=~/.rbenv/shims:$PATH
  eval "$(rbenv init -)"
fi

# Read .zsh files
ZSHHOME="${HOME}/.zsh.d"
if [ -d $ZSHHOME -a -r $ZSHHOME -a -x $ZSHHOME ]; then
  for i in $ZSHHOME/**/*; do
    [[ ${i##*/} = *.zsh ]] && [ \( -f $i -o -h $i \) -a -r $i ] && . $i
  done
fi
