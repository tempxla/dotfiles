#!/bin/bash

echo $(grep PRETTY_NAME= /etc/os-release | sed s/PRETTY_NAME=\"//g | sed 's/ (.*//g')

exit 0
