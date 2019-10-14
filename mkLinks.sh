#!/bin/sh
THIS_DIR=$(cd $(dirname $0); pwd)
ln -sf ${THIS_DIR}/.xmonad/xmonad.hs    ~/.xmonad/xmonad.hs
ln -sf ${THIS_DIR}/.xmobarrc            ~/.xmobarrc
ln -sf ${THIS_DIR}/.Xresources          ~/.Xresources
ln -sf ${THIS_DIR}/.config/fontconfig/fonts.conf    ~/.config/fontconfig/fonts.conf
ln -sf ${THIS_DIR}/.tmux.conf           ~/.tmux.conf
ln -sf ${THIS_DIR}/.emacs.d/init.el     ~/.emacs.d/init.el
ln -sf ${THIS_DIR}/.ncmpcpp/config      ~/.ncmpcpp/config
ln -sf ${THIS_DIR}/.config/mpd/mpd.conf ~/.config/mpd/mpd.conf
ln -sf ${THIS_DIR}/bin/get-volume.sh    ~/bin/get-volume.sh
ln -sf ${THIS_DIR}/.inputrc             ~/.inputrc
ln -sf ${THIS_DIR}/.stack/config.yaml   ~/.stack/config.yaml
ln -sf ${THIS_DIR}/.stack/global-project/stack.yaml   ~/.stack/global-project/stack.yaml
ln -sf ${THIS_DIR}/.gitconfig           ~/.gitconfig
ln -sf ${THIS_DIR}/.bash_aliases        ~/.bash_aliases
ln -sf ${THIS_DIR}/.bashrc              ~/.bashrc
