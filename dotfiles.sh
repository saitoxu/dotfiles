#!/bin/bash

set -e
DOT_DIRECTORY="${HOME}/dotfiles"
PREZTO_URL="https://github.com/sorin-ionescu/prezto.git"
ITERM_PROFILE_DIR="${HOME}/Library/Application Support/iTerm2/DynamicProfiles"
ITERM_PROFILE_GUID="5D0F11E5-D07F-4E5A-9C1E-000000000001"
DOT_TARBALL="https://github.com/saitoxu/dotfiles/tarball/main"
REMOTE_URL="git@github.com:saitoxu/dotfiles.git"

has() {
  type "$1" > /dev/null 2>&1
}

usage() {
  name=`basename $0`
  cat <<EOF
Usage:
  $name [arguments] [command]

Commands:
  deploy [--profile <work|personal>]
  initialize

Arguments:
  -f $(tput setaf 1)** Warning **$(tput sgr0) Overwrite dotfiles.
  -h Print help (this message)
EOF
  exit 1
}

while getopts "fh" opt; do
  case ${opt} in
    f)
      OVERWRITE=true
      ;;
    h)
      usage
      ;;
  esac
done
shift $((OPTIND - 1))

# Working only OS X.
case ${OSTYPE} in
  darwin*)
    ;;
  *)
    echo "$(tput setaf 1)Working only OS X!!$(tput sgr0)"
    exit 1
    ;;
esac

# If missing, download and extract the dotfiles repository
if [ ! -d ${DOT_DIRECTORY} ]; then
  echo "Downloading dotfiles..."
  mkdir ${DOT_DIRECTORY}

  if has "git"; then
    git clone --recursive "${REMOTE_URL}" "${DOT_DIRECTORY}"
  else
    curl -fsSLo ${HOME}/dotfiles.tar.gz ${DOT_TARBALL}
    tar -zxf ${HOME}/dotfiles.tar.gz --strip-components 1 -C ${DOT_DIRECTORY}
    rm -f ${HOME}/dotfiles.tar.gz
  fi

  echo "$(tput setaf 2)Download dotfiles complete! ✔︎$(tput sgr0)"
fi

cd ${DOT_DIRECTORY}
source ./lib/git.sh

link_files() {
  for f in .??*
  do
    # Force remove the vim directory if it's already there
    [ -n "${OVERWRITE}" -a -e ${HOME}/${f} ] && rm -f ${HOME}/${f}
    if [ ! -e ${HOME}/${f} ]; then
      # If you have ignore files, add file/directory name here
      [[ ${f} = ".git" ]] && continue
      [[ ${f} = ".gitignore" ]] && continue
      [[ ${f} = ".claude" ]] && continue
      ln -snfv ${DOT_DIRECTORY}/${f} ${HOME}/${f}
    fi
  done

  echo "$(tput setaf 2)Deploy dotfiles complete! ✔︎$(tput sgr0)"
}

link_iterm_profile() {
  mkdir -p "${ITERM_PROFILE_DIR}"
  ln -snfv "${DOT_DIRECTORY}/iterm/dotfiles.json" "${ITERM_PROFILE_DIR}/dotfiles.json"
  defaults write com.googlecode.iterm2 "Default Bookmark Guid" -string "${ITERM_PROFILE_GUID}"
  echo "$(tput setaf 2)iTerm profile linked ✔︎$(tput sgr0)"
}

set_profile() {
  local profile=$1
  if [ ! -f "${DOT_DIRECTORY}/zsh/profiles/${profile}.zsh" ]; then
    echo "$(tput setaf 1)Unknown profile: ${profile}$(tput sgr0)"
    exit 1
  fi
  echo "${profile}" > ${HOME}/.dotfiles_profile
  echo "$(tput setaf 2)Profile set to ${profile} ✔︎$(tput sgr0)"
}

install_prezto() {
  if [ ! -d ${HOME}/.zprezto ]; then
    git clone --recursive "${PREZTO_URL}" ${HOME}/.zprezto
  fi
}

install_fonts() {
  if has "brew"; then
    brew list --cask font-ubuntu-mono-derivative-powerline > /dev/null 2>&1 \
      || brew install --cask font-ubuntu-mono-derivative-powerline
  else
    echo "$(tput setaf 3)Homebrew not found. Skip installing fonts.$(tput sgr0)"
  fi
}

initialize() {
  run_git
  install_prezto
  install_fonts

  [ ${SHELL} != "/bin/zsh" ] && chsh -s /bin/zsh
  echo "$(tput setaf 2)Initialize complete! ✔︎$(tput sgr0)"
}

command=$1
[ $# -gt 0 ] && shift

case $command in
  deploy)
    if [ "$1" = "--profile" ]; then
      [ -z "$2" ] && usage
      set_profile "$2"
    fi
    link_files
    link_iterm_profile
    ;;
  init*)
    initialize
    ;;
  *)
    usage
    ;;
esac

exit 0
