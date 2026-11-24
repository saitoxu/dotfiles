# ------------------------------
# Loader
# ------------------------------
# 読み込み順: zsh/common.zsh → zsh/profiles/${DOTFILES_PROFILE}.zsh → ~/.zshrc.local
# profile は ~/.dotfiles_profile に work / personal のいずれかを書いて指定する

DOTFILES_DIR="${HOME}/dotfiles"

source "${DOTFILES_DIR}/zsh/common.zsh"

if [ -r "${HOME}/.dotfiles_profile" ]; then
  DOTFILES_PROFILE="$(tr -d '[:space:]' < "${HOME}/.dotfiles_profile")"
fi

if [ -n "${DOTFILES_PROFILE}" ] && [ -r "${DOTFILES_DIR}/zsh/profiles/${DOTFILES_PROFILE}.zsh" ]; then
  source "${DOTFILES_DIR}/zsh/profiles/${DOTFILES_PROFILE}.zsh"
else
  echo "[dotfiles] profile が未設定です: ~/dotfiles/dotfiles.sh deploy --profile <work|personal>" >&2
fi

# マシン固有の設定（git 管理外）
[ -r "${HOME}/.zshrc.local" ] && source "${HOME}/.zshrc.local"

# vim: set ft=sh :
