# dotfiles

macOS 用の dotfiles。
仕事用（work）と個人用（personal）の Mac で共用しており、環境ごとの差分は profile で切り替える。

## セットアップ

```sh
cd ~
git clone git@github.com:saitoxu/dotfiles.git
~/dotfiles/dotfiles.sh initialize
~/dotfiles/dotfiles.sh deploy --profile work   # 個人用 Mac なら personal
```

| コマンド | 内容 |
|---|---|
| `initialize` | git のグローバル設定、ログインシェルを zsh に変更 |
| `deploy` | リポジトリ直下のドットファイルを `~` にシンボリックリンク |
| `deploy --profile <name>` | 上記に加え、`~/.dotfiles_profile` に profile 名を書き込む |
| `-f` | 既存ファイルを削除して上書きリンク |

## 構成

```
dotfiles/
├── .zshrc                  # ローダー（下記の順に読み込むだけ）
├── .vimrc
├── zsh/
│   ├── common.zsh          # 全環境共通の設定
│   └── profiles/
│       ├── work.zsh        # 仕事用 Mac のみの設定
│       └── personal.zsh    # 個人用 Mac のみの設定
├── dotfiles.sh             # セットアップスクリプト
└── lib/git.sh
```

### zsh の読み込み順

1. `zsh/common.zsh`
2. `zsh/profiles/${DOTFILES_PROFILE}.zsh`（`~/.dotfiles_profile` の中身で決まる）
3. `~/.zshrc.local`（存在すれば）

profile が未設定の場合は警告を出し、common のみ読み込む。
profile を切り替えるときは `~/.dotfiles_profile` を書き換えるか、`deploy --profile` を再実行する。

## 設定の置き場所

| 設定の種類 | 置き場所 |
|---|---|
| どの環境でも使う | `zsh/common.zsh` |
| 特定の環境でのみ使う | `zsh/profiles/<name>.zsh` |
| マシン固有・秘密情報（git に入れたくない） | `~/.zshrc.local` |

ツールのパスなど、存在しない環境があるだけのものは `[ -f ... ] && source ...` のように存在チェックを付けて common に置く。
profile に分けるのは、環境によって値や挙動を変えたいものだけにする。

## インストール後の手作業

- 旧 Mac から `.zpreztorc` と `.zprezto/` を移す
- [powerline/fonts](https://github.com/powerline/fonts) をインストールし、iTerm のフォントに設定する
- iTerm のカラーテーマを変更する
