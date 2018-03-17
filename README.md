# dotfiles

OS X dotfiles.

## Installation
The installation step requires the [XCode Command Line Tools](https://developer.apple.com/downloads)

```sh
$ java -v
$ xcode-select --install
```

Run the following commnad.

```sh
$ bash -c "$(curl -fsSL raw.github.com/saitoxu/dotfiles/master/dotfiles)"
# Overwrite dotfiles and deploy
$ bash -c "$(curl -fsSL raw.github.com/saitoxu/dotfiles/master/dotfiles)" -- -f -s deploy
```

## Initialize OS X

```sh
$ ~/dotfiles/dotfiles.sh initialize
```

## Deploy dotfiles

```sh
$ ~/dotfiles/dotfiles.sh deploy
```
