#!/bin/sh
THIS_DIR=$(cd $(dirname $0); pwd)
ln -sf ${THIS_DIR}/.xmonad/xmonad.hs    ~/.xmonad/xmonad.hs
ln -sf ${THIS_DIR}/.xmobarrc            ~/.xmobarrc
