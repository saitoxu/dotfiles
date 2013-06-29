## 補完で大文字小文字を区別しない
#
compctl -M 'm:{a-z}={A-Z}'
# zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' '+m:{A-Z}={a-z}'

## alias
#
alias ls='ls -GF'
alias la='ls -a'
alias ll='ls -al'
alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'
alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'
alias ec='emacsclient -n'
alias valgrind='/usr/local/Cellar/valgrind/3.8.1/bin/valgrind'
## 要らなくなった勢
# alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs -nw'
# alias ruby='/opt/local/bin/ruby1.9'

## Default shell configuration
#
# set prompt
#
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
    PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
    PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
    SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
    [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
        PROMPT="%{${fg[cyan]}%}$(echo ${HOST%%.*} | tr '[a-z]' '[A-Z]') ${PROMPT}"
    ;;
esac

# set terminal title including current directory
#
case "${TERM}" in
kterm*|xterm*)
    precmd() {
        # echo -ne "\033]0;${USER}@${HOST%%.*}:${PWD}\007"
        echo -ne "\e]2;${PWD:t}\a"
        echo -ne "\e]1;${PWD:t}\a"
    }
    export LSCOLORS=exfxcxdxbxegedabagacad
    # export LSCOLORS=gxfxcxdxbxegedabagacad
    export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
    zstyle ':completion:*' list-colors \
        'di=34' 'ln=35' 'so=32' 'ex=31' 'bd=46;34' 'cd=43;34'
    ;;
esac

## load user .zshrc configuration file
#
[ -f ~/.zshrc.mine ] && source ~/.zshrc.mine

export PATH=~/.rbenv/shims:$PATH
eval "$(rbenv init -)"
export PATH=$PATH:/Applications/android-sdk-macosx/platform-tools
export MANPATH=/opt/local/man:$MANPATH
export TERM=xterm-256color

# MacPorts使ってた時代の遺物
# export PATH=/opt/local/bin:/opt/local/sbin:$PATH

# export PATH=$PATH:/Applications/XAMPP/xamppfiles/bin
# export _JAVA_OPTIONS='-Dfile.encoding=UTF-8'
# keychain ~/.ssh/github_id_rsa
# set JAVA_HOME
# JAVA_HOME=/System/Library/Frameworks/JavaVM.framework/Versions/CurrentJDK/Home
# export JAVA_HOME
# PATH=$PATH:$JAVA_HOME/bin
# export PATH
