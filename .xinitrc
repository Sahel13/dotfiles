#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# Compositor
picom -b &

# Notification server
/usr/bin/dunst &

# Hide the cursor after a period of inactivity.
unclutter -noevents &

# Set the default X cursor to the usual pointer.
xsetroot -cursor_name left_ptr

# Set the wallpaper.
feh --no-fehbg --bg-fill ~/Downloads/astronaut.jpg

# Trayer
trayer --edge bottom --align right --SetDockType true --SetPartialStrut true --expand true --width 3 --transparent true --alpha 4 --tint 0x000000 --height 30 &

# Network manager applet
nm-applet &

# Start xmonad
exec xmonad
