" ---------------------------
" General
" ---------------------------
syntax on      " シンタックスハイライト
set fenc=utf-8 " 文字コードをUFT-8に設定
set nobackup   " バックアップファイルを作らない
set noswapfile " スワップファイルを作らない
set autoread   " 編集中のファイルが変更されたら自動で読み直す
set hidden     " バッファが編集中でもその他のファイルを開けるように
set showcmd    " 入力中のコマンドをステータスに表示する

" ---------------------------
" Visual
" ---------------------------
set number              " 行番号を表示
set cursorline          " 現在の行を強調表示
set cursorcolumn        " 現在の行を強調表示（縦）
set virtualedit=onemore " 行末の1文字先までカーソルを移動できるように
set showmatch           " 括弧入力時の対応する括弧を表示
set laststatus=2        " ステータスラインを常に表示

" ---------------------------
" Tab
" ---------------------------
set list listchars=tab:\▸\- " 不可視文字を可視化(タブが「▸-」と表示される)
set expandtab               " Tab文字を半角スペースにする

" ---------------------------
" Search
" ---------------------------
set ignorecase " 検索文字列が小文字の場合は大文字小文字を区別なく検索する
set smartcase  " 検索文字列に大文字が含まれている場合は区別して検索する
