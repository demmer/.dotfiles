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

if [ -n "$X11HOME" ]; then
        xhost + `hostname`
	xhost + localhost
fi

# Load the X defaults
if [ -f $HOME/.Xdefaults ]; then
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

# Bring up the window manager and save its process id
fvwm2 &
WMPID=$!

# Utility programs
nice dclock &
$HOME/bin/inigo-mailbox &
$HOME/bin/xoopi-tunnel &
$HOME/bin/xoopi-mailbox &

# Bring up a console window, an email shell and a work shell
xterm -C -n "Console" -geometry '=80x12+0+115' &
$HOME/bin/xoopi-xmutt &
colorxterm -n "Fezzik, it's you . . . true" -geometry '=80x67+590+115'&

# Bring up emacs and two work shells in 2nd screen 
emacs -geometry '=82x65+1280+115' -title "Inconceivable" &
colorxterm -n "Fezzik, are there rocks up ahead?" -geometry '=80x32+1870+115' &
colorxterm -n "If there are, we'll all be dead!!" -geometry '=80x32+1870+568' &

# Wait for the window manager to quit
wait $WMPID

