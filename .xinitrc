#!/bin/sh
#
#	.xinitrc
#
#	This file is executed as a Bourne shell script (only!) by
#	xinit after it starts up the X server.  This script is your
#	X session: when it exits, your session ends.  For that reason
#	ONE process below should run in the foreground.  When that
#	process exits, so does this script and your session.
#

echo "*******************************************************"
echo
echo "*****      Logged in on `hostname` at `date`      *****"
echo
echo "*******************************************************"

#
# Load in geometry
#
. $HOME/.geometry

if [ -n "$X11HOME" ]; then
        xhost + `hostname`
	xhost + localhost
fi

#
# Load the X defaults after substituting geometry bindings
#
if [ -f $HOME/.Xdefaults.in ]; then
	rm -f $HOME/.Xdefaults
	subst-geometry < $HOME/.Xdefaults.in > $HOME/.Xdefaults
	xrdb $HOME/.Xdefaults
fi

# Set up ssh-agent
. $HOME/.ssh-attach
if [ "$SSH_AUTH_SOCK" = "" ]; then
	xterm -bg yellow -geom +350+450 -fg black -e $HOME/bin/ssh-init
	. $HOME/.ssh-attach
fi

# Set the background to a nice graphic
xv -quit -rmode 4 $HOME/images/gray_rock.gif

# enable screen saver
xset s 300

# Bring up the window manager and save its process id
fvwm2 &
WMPID=$!

# Utility programs
mozilla &
nice dclock &
nice xpostit+ &
inigo-mailbox &
xoopi-tunnels &
xoopi-mailbox &
haight-mailbox &
everybuddy > .everybuddy/everybuddy.log 2>&1 &
[ -r $HOME/.fetchmailrc ] && xterm -e /bin/sh -c 'fetchmail; sleep 2'  &
KEEPALIVE_DELAY=5 keepalive x2vnc_buchanan > $HOME/.err/x2vnc.log 2>&1 &

# Bring up a console window, email shells and a work shell
xterm -title "Console" -n "Console" -geometry =$CONSOLE_GEOM -e /bin/sh -c \
    '[ -f /var/log/messages ] && tail -f -n 0 /var/log/messages & \
     [ -f /var/log/secure ] && tail -f -n 0 /var/log/secure & \
     clear; zsh' &
xterm -n "Fezzik, it's you . . . true" -geometry =$XTERM_1_GEOM &

xoopi-xmutt &
haight-xmutt &

# Bring up emacs and two work shells in 2nd screen 
emacs -geometry =$EMACS_XINIT_GEOM -title "Inconceivable" &
(sleep 5; gnudoit "(make-frame '((width . 80) (height . 65) (left . 2401) (top . 135)))") &

xterm -n "Fezzik, are there rocks up ahead?" -geometry =$XTERM_2_GEOM &
xterm -n "If there are, we'll all be dead!!" -geometry =$XTERM_3_GEOM &
xterm -n "Anybody want a peanut?" -geometry =$XTERM_4_GEOM &

# Wait for the window manager to quit
wait $WMPID

