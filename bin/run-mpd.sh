#!/bin/sh

if [ ! -s ~/.config/mpd/pid ] ; then
    # for io_uring, changed ulimit -l
    # https://github.com/MusicPlayerDaemon/MPD/issues/972
    # /etc/security/limits.conf
    # @wheel           hard    memlock         67108864
    ulimit -l 67108864
    mpd ~/.config/mpd/mpd.conf
fi
