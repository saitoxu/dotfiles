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
| `initialize` | git のグローバル設定、prezto と Powerline フォントのインストール、ログインシェルを zsh に変更 |
| `deploy` | リポジトリ直下のドットファイルを `~` にシンボリックリンクし、iTerm のプロファイルを配置 |
| `deploy --profile <name>` | 上記に加え、`~/.dotfiles_profile` に profile 名を書き込む |
| `-f` | 既存ファイルを削除して上書きリンク |

## 構成

```
dotfiles/
├── .zshrc                  # ローダー（下記の順に読み込むだけ）
├── .vimrc
├── .zpreztorc              # prezto の設定（読み込むモジュールとテーマ）
├── zsh/
│   ├── common.zsh          # 全環境共通の設定
│   ├── profiles/
│   │   ├── work.zsh        # 仕事用 Mac のみの設定
│   │   └── personal.zsh    # 個人用 Mac のみの設定
│   └── prompts/
│       └── prompt_powerline2_setup  # 自作の prezto テーマ
├── iterm/
│   └── dotfiles.json       # iTerm の Dynamic Profile（フォント・配色など）
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

## prezto

`initialize` で `~/.zprezto` に prezto を clone する。
自作テーマ `powerline2` は `zsh/prompts/` に置いてあり、`common.zsh` で fpath に追加して prezto から読み込ませている。
テーマを追加するときは `zsh/prompts/prompt_<name>_setup` を置き、`.zpreztorc` の `theme` を書き換える。

## iTerm

`deploy` で `iterm/dotfiles.json` を `~/Library/Application Support/iTerm2/DynamicProfiles/` にリンクし、デフォルトプロファイルに設定する。
フォントや配色を変えるときは `iterm/dotfiles.json` を編集する（iTerm の設定画面での変更は Dynamic Profile には保存されない）。

iTerm を起動したまま `deploy` すると、デフォルトプロファイルの設定が反映されないことがある。
その場合は Settings → Profiles で `dotfiles` を選び、Other Actions → Set as Default を押す。
