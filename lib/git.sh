#!/bin/bash

run_git() {
  if has "git"; then
    git config --global user.name "Yosuke Saito"
    git config --global user.email "yosuke.saito1202@gmail.com"
    git config --global core.editor 'vim -c "set fenc=utf-8"'
    git config --global color.diff auto
    git config --global color.status auto
    git config --global color.branch auto
    git config --global core.precomposeunicode true
    git config --global core.quotepath false
  fi
}
