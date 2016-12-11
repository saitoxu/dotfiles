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
# alias firefox='/Applications/Firefox.app/Contents/MacOS/firefox'
# alias ec='emacsclient -n'
# alias valgrind='/usr/local/Cellar/valgrind/3.8.1/bin/valgrind'
# alias atom='atom -a'

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
    PROMPT="%{${fg[green]}%}%/%%%{${reset_color}%} "
    PROMPT2="%{${fg[green]}%}%_%%%{${reset_color}%} "
    SPROMPT="%{${fg[green]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
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

export MANPATH=/opt/local/man:$MANPATH
export TERM=xterm-256color
export JAVA_HOME=`/System/Library/Frameworks/JavaVM.framework/Versions/A/Commands/java_home -v "1.8"`

### for postgresql
export PGDATA=/usr/local/var/postgres

export NVM_DIR="/Users/Yosuke/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && . "$NVM_DIR/nvm.sh"  # This loads nvm

# pyenv
export PYENV_ROOT="${HOME}/.pyenv"
export PATH="$PATH:${PYENV_ROOT}/bin"
eval "$(pyenv init -)"

export PATH=~/.rbenv/shims:$PATH
eval "$(rbenv init -)"
export PATH=$PATH:/Applications/android-sdk-macosx/platform-tools
# export PATH=$PATH:/opt/AWS-ElasticBeanstalk-CLI-2.5.1/eb/linux/python2.7
export PATH=$PATH:/usr/local/sbin
# export PATH="/Applications/Sublime Text 2.app/Contents/SharedSupport/bin:$PATH"

PATH=${JAVA_HOME}/bin:${PATH}

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

# read .zsh files
ZSHHOME="${HOME}/.zsh.d"

if [ -d $ZSHHOME -a -r $ZSHHOME -a \
     -x $ZSHHOME ]; then
    for i in $ZSHHOME/**/*; do
        [[ ${i##*/} = *.zsh ]] &&
            [ \( -f $i -o -h $i \) -a -r $i ] && . $i
    done
fi
