#!/bin/sh

userresources=$HOME/.Xresources
# usermodmap=$HOME/.Xmodmap
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

# if [ -f "$usermodmap" ]; then
#     xmodmap "$usermodmap"
# fi

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# For gnome-keyring-daemon
# eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
# export SSH_AUTH_SOCK

# If external display is connected, turn off laptop display
# /usr/bin/xrandr --current | grep "DP1 connected 1"
# if [ $? -eq 0 ]; then
# 	/usr/bin/xrandr --output "eDP1" --off
# 	/usr/bin/xrandr --output "DP1" --auto --primary
# fi

# Load Xkeymap
test -f ~/.Xkeymap && xkbcomp ~/.Xkeymap $DISPLAY

# Start xmonad
exec xmonad
