#!/bin/bash

#amixer -q set Master 983
#amixer -q set Master 13107
wpctl set-volume @DEFAULT_AUDIO_SINK@ 0.20

get-volume.sh
