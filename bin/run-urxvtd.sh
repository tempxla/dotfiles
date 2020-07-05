#!/bin/sh

URXVTD_PS=$(ps axh | grep "urxvtd -q -f -o" | wc -l)
#grep urxvtd -q -f -o

if [ $URXVTD_PS -eq 1 ];
then
    urxvtd -q -f -o
fi
