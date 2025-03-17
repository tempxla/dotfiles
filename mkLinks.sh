#!/bin/sh
THIS_DIR=$(cd $(dirname $0); pwd)
HOSTNAME=$(hostname)

## WM & DE
ln -sf ${THIS_DIR}/.xmonad/xmonad.hs         ~/.xmonad/xmonad.hs
ln -sf ${THIS_DIR}/.xmobarrc                 ~/.xmobarrc
ln -sf ${THIS_DIR}/bin/run-urxvtd.sh         ~/bin/run-urxvtd.sh
ln -sf ${THIS_DIR}/bin/get-volume.sh         ~/bin/get-volume.sh
ln -sf ${THIS_DIR}/bin/set-volume-default.sh ~/bin/set-volume-default.sh
ln -sf ${THIS_DIR}/bin/get-os-release.sh     ~/bin/get-os-release.sh
ln -sf ${THIS_DIR}/.Xresources               ~/.Xresources
ln -sf ${THIS_DIR}/.Xmodmap                  ~/.Xmodmap
ln -sf ${THIS_DIR}/.config/fontconfig/fonts.conf  ~/.config/fontconfig/fonts.conf
ln -sf ${THIS_DIR}/bin/run-xrandr.sh         ~/bin/run-xrandr.sh

## shell
ln -sf ${THIS_DIR}/.inputrc             ~/.inputrc
ln -sf ${THIS_DIR}/.dir_colors          ~/.dir_colors

## zsh
ln -sf ${THIS_DIR}/.zshrc               ~/.zshrc
ln -sf ${THIS_DIR}/.zshenv              ~/.zshenv

## pwsh
ln -sf ${THIS_DIR}/.config/powershell/Microsoft.PowerShell_profile.ps1 ~/.config/powershell/Microsoft.PowerShell_profile.ps1

## emacs
ln -sf ${THIS_DIR}/.emacs.d/init.el     ~/.emacs.d/init.el
ln -sf ${THIS_DIR}/.emacs.d/custom.el   ~/.emacs.d/custom.el

## vim
ln -sf ${THIS_DIR}/.vimrc               ~/.vimrc

## git
ln -sf ${THIS_DIR}/.gitconfig           ~/.gitconfig

## stack (haskell)
ln -sf ${THIS_DIR}/.stack/config.yaml                 ~/.stack/config.yaml
ln -sf ${THIS_DIR}/.stack/global-project/stack.yaml   ~/.stack/global-project/stack.yaml

## mpd
ln -sf ${THIS_DIR}/bin/run-mpd.sh        ~/bin/run-mpd.sh
ln -sf ${THIS_DIR}/.config/mpd/mpd.conf  ~/.config/mpd/mpd.conf
ln -sf ${THIS_DIR}/.ncmpcpp/config       ~/.ncmpcpp/config

## 2chproxy
ln -sf ${THIS_DIR}/.config/2chproxy.yml  ~/.config/2chproxy.yml
ln -sf ${THIS_DIR}/bin/2chproxy.sh       ~/bin/2chproxy.sh

## navi2ch
ln -sf ${THIS_DIR}/.navi2ch/init.el     ~/.navi2ch/init.el

## bash
#ln -sf ${THIS_DIR}/.bash_aliases        ~/.bash_aliases
ln -sf ${THIS_DIR}/.bashrc              ~/.bashrc

## VSCode
ln -sf ${THIS_DIR}/.config/Code/User/settings.json ~/.config/Code/User/settings.json

## fish
#ln -sf ${THIS_DIR}/.config/fish/config.fish                  ~/.config/fish/config.fish
#ln -sf ${THIS_DIR}/.config/fish/fishd.hostname               ~/.config/fish/fishd.${HOSTNAME}
#ln -sf ${THIS_DIR}/.config/fish/functions/fish_prompt.fish   ~/.config/fish/functions/fish_prompt.fish

## tmux
#ln -sf ${THIS_DIR}/.tmux.conf           ~/.tmux.conf

## mlterm
#ln -sf ${THIS_DIR}/.mlterm/main         ~/.mlterm/main
#ln -sf ${THIS_DIR}/.mlterm/color        ~/.mlterm/color
#ln -sf ${THIS_DIR}/.mlterm/key          ~/.mlterm/key
