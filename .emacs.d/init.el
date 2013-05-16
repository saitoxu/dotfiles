; -*- Mode: Emacs-Lisp ; Coding: utf-8 -*-
;; ------------------------------------------------------------------------
;; @ load-path

;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
(add-to-load-path "elisp" "conf" "public_repos")

;; スタートアップ非表示
(setq inhibit-startup-screen t)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

(when (require 'color-theme nil t)
  ;; テーマを読み込むための設定
  (color-theme-initialize)
  ;; テーマClarityに変更する
  (color-theme-dark-laptop))

;; メニューバーを非表示
(menu-bar-mode -1)

;; スクロールバー非表示
(set-scroll-bar-mode nil)

;; タイトルバーにファイルのフルパス表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

; (set-language-environment "Japanese")
; (set-default-coding-systems 'utf-8)
; (set-terminal-coding-system 'utf-8)
; (set-keyboard-coding-system 'utf-8)
; (set-buffer-file-coding-system 'utf-8)

;; 改行コードを押してインデントも行う
(global-set-key (kbd "C-m") 'newline-and-indent)

;; 折り返しトグルコマンド
(define-key global-map (kbd "C-c l") 'toggle-truncate-lines)

;; "C-t"でウィンドウを切り替え．初期値はtranspose-chars
(define-key global-map (kbd "C-t") 'other-window)

;; 文字コード指定
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)

;; Mac OS Xの場合のファイル名の設定
(when (eq system-type 'darwin)
  (require 'ucs-normalize)
  (set-file-name-coding-system 'utf-8-hfs)
  (setq locale-coding-system 'utf-8-hfs))

;; 行番号表示
(global-linum-mode t)
(set-face-attribute 'linum nil
                    :foreground "#888"
                    :height 0.9)

;; 行番号フォーマット
(setq linum-format "%4d")

;; 括弧の範囲内を強調表示
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match-face nil)
(set-face-underline-p 'show-paren-match-face "yellow")

;; 括弧の範囲色
(set-face-background 'show-paren-match-face "#000")

;; 選択領域の色
(set-face-background 'region "#555")

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブをスペースで扱う
(setq-default indent-tabs-mode nil)

;; TABの表示幅．初期値は8
(setq-default tab-width 4)

;; タブ幅
; (custom-set-variables '(tab-width 4))

;; yes or noをy or n
(fset 'yes-or-no-p 'y-or-n-p)

;; 最近使ったファイルをメニューに表示
(recentf-mode t)

;; 最近使ったファイルの表示数
(setq recentf-max-menu-items 10)

;; 最近開いたファイルの保存数を増やす
(setq recentf-max-saved-items 3000)

;; ミニバッファの履歴を保存する
(savehist-mode 1)

;; ミニバッファの履歴の保存数を増やす
(setq history-length 3000)

;; バックアップを残さない
; (setq make-backup-files nil)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; フレームの透明度
(set-frame-parameter (selected-frame) 'alpha '(0.85))

;; モードラインに行番号表示
(line-number-mode t)

;; モードラインに列番号表示
(column-number-mode t)

;; C-Ret で矩形選択
;; 詳しいキーバインド操作：http://dev.ariel-networks.com/articles/emacs/part5/
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; バックアップとオートセーブファイルを~/.emacs.d/backups/へ集める
(add-to-list 'backup-directory-alist
             (cons "." "~/projects/dotfiles/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/projects/dotfiles/.emacs.d/backups/") t)))

;; ------------------------------------------------------------------------
;; @ modeline

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

(set-face-background 'modeline "#555")
(set-face-foreground 'modeline "#fff")

;; php-mode
(require 'php-mode)
(setq php-mode-force-pear t) ; PEAR規約のインデント設定にする
(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
; *.phpのファイルのときにphp-modeを自動起動する

;; emacs-lisp-mode-hook用の関数を定義
(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (when (require 'eldoc nil t)
    (setq eldoc-idle-delay 0.2)
    (setq eldoc-echo-area-use-multiline-p t)
    (turn-on-eldoc-mode)))

;; emacs-lisp-modeのフックをセット
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

;; auto-installの設定
; (when (require 'auto-install nil t)
  ;; インストールディレクトリを設定する
  ; (setq auto-install-directory "~/.emacs.d/elisp/")
  ;; EmacsWikiに登録されているelispの名前を取得する
  ; (auto-install-update-emacswiki-package-name t)
  ;; 必要であればプロキシの設定を行う
  ;; プロキシの設定は~/.wgetrcに書いてある
  ; (setq url-proxy-services '(("http" . "proxy.kuins.net:8080")))
  ;; install-elispの関数を利用可能にする
  ; (auto-install-compatibility-setup))

;; 10秒で終了しなかったらauto-installをあきらめる
;; プロキシの影響でemacs起動しないのを防ぐため
(with-timeout (10 nil)
  (when (require 'auto-install nil t)
    (setq auto-install-directory "~/.emacs.d/elisp/")
    (auto-install-update-emacswiki-package-name t)
    (auto-install-compatibility-setup)))

;; redo+の設定
 (when (require 'redo+ nil t)
  ;; C-t にリドゥを割り当てる
   (global-set-key (kbd "C-t") 'redo))

;;; anything
;; (auto-install-batch "anything")
(when (require 'anything nil t)
  (setq
   ;; 候補を表示するまでの時間．デフォルトは0.5
   anything-idle-delay 0.3
   ;; タイプして再描画するまでの時間．デフォルトは0.1
   anything-input-idle-delay 0.2
   ;; 候補の最大表示件数．デフォルトは50
   anything-candidate-number-limit 100
   ;; 候補が多い時に体感速度を早くする
   anything-quick-update t
   ;; 候補選択ショートカットをアルファベットに
   anything-enable-shortcuts 'alphabet)

  (when (require 'anything-config nil t)
    ;; root権限でアクションを実行するときのコマンド
    ;; デフォルトは"su"
    (setq anything-su-ro-sudo "sudo"))

  (require 'anything-match-plugin nil t)

  (when (and (executable-find "cmigemo")
             (require 'migemo nil t))
    (require 'anything-migemo nil t))

  (when (require 'anything-complete nil t)
    ;; lispシンボルの補完候補の再検索時間
    (anything-lisp-complete-symbol-set-timer 150))

  (require 'anything-show-completion nil t)

  (when (require 'auto-install nil t)
    (require 'anything-auto-install nil t))

  (when (require 'descbinds-anything nil t)
    ;; describe-bindingsをAnythingに置き換える
    (descbinds-anything-install)))

;; シンボリックリンクの読み込みを許可
(setq vc-follow-symlinks t)
;; シンボリックリンク先のVCS内で更新が入った場合にバッファを自動更新
(setq auto-revert-check-vc-info t)
