#!/bin/sh
THIS_DIR=$(cd $(dirname $0); pwd)
ln -sf ${THIS_DIR}/.xmonad/xmonad.hs    ~/.xmonad/xmonad.hs
ln -sf ${THIS_DIR}/.xmobarrc            ~/.xmobarrc
ln -sf ${THIS_DIR}/.Xresources          ~/.Xresources
ln -sf ${THIS_DIR}/.config/fontconfig/fonts.conf    ~/.config/fontconfig/fonts.conf
ln -sf ${THIS_DIR}/.tmux.conf           ~/.tmux.conf
ln -sf ${THIS_DIR}/.emacs.d/init.el     ~/.emacs.d/init.el
ln -sf ${THIS_DIR}/.ncmpcpp/config      ~/.ncmpcpp/config
