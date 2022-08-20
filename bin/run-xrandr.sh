#!/bin/sh

xrandr --output HDMI-1 --auto
xrandr --output DP-1 --auto --primary --left-of HDMI-1
