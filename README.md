# dotfiles

OS X dotfiles.

## Installation
The installation step requires the [XCode Command Line Tools](https://developer.apple.com/downloads)

```bash
$ java -v
$ xcode-select --install
```

Run the following commnad.

```bash
$ bash -c "$(curl -fsSL raw.github.com/saitoxu/dotfiles/master/dotfiles)"
# Overwrite dotfiles and deploy
$ bash -c "$(curl -fsSL raw.github.com/saitoxu/dotfiles/master/dotfiles)" -- -f -s deploy
```

## Initialize OS X

```bash
$ ~/dotfiles/dotfiles.sh initialize
```

## Deploy dotfiles

```bash
$ ~/dotfiles/dotfiles.sh deploy
```
