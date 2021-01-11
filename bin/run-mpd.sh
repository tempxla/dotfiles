#!/bin/sh

pid_file=~/.config/mpd/pid

if [ ! -s $pid_file ] ; then
    # for io_uring, changed ulimit -l
    # https://github.com/MusicPlayerDaemon/MPD/issues/972
    # /etc/security/limits.conf
    # @wheel           hard    memlock         67108864
    ulimit -l 67108864
    mpd ~/.config/mpd/mpd.conf
else
    echo "[WARN] mpd didn't start: $pid_file exists."
fi
