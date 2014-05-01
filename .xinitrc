#!/bin/bash

export DE=generic
export GTK2_RC_FILES="$HOME/.gtkrc-2.0"

# Dbus fix
if [ -z "$DBUS_SESSION_BUS_ADDRESS" ]; then
    eval "$(dbus-launch --sh-syntax --exit-with-session)"
fi

/usr/bin/xdg-user-dirs-update

xset r rate 275 50
xset b off
setxkbmap -option ctrl:nocaps
xmodmap .Xmodmap
xsetroot -cursor_name left_ptr
pcmanfm -d &
sh ~/.fehbg &

exec xmonad
