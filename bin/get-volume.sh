#!/bin/bash
# Get the maximum volume of any pulseaudio sink channel
# amixer get Master | egrep -o "[0-9]+%"
# vol=$(amixer get Master | awk -F'[][]' '/%/  {print $2 " [" $4 "]"}' | head -n 1)
vol=$(amixer get Master | amixer get Master | awk -F' ' '/%/  {printf "%.2f%% %s\n", 100*$4/65536, $6}' | head -n 1)

echo $vol

exit 0
