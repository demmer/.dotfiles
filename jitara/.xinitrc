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
echo "*****      Logged in on `hostname`"
echo "*****      `date`"
echo "*******************************************************"

#
# Bounce back to my home directory for convenience
#
cd $HOME

#
# Get some output
#
if [ -z "$XINITLOG" ]; then
       XINITLOG=1
       export XINITLOG
       exec /bin/sh $HOME/.xinitrc | tee $HOME/.err/startx.log 2>&1
fi

#
# Load in geometry
#
. $HOME/.geometry

if [ -n "$X11HOME" ]; then
        xhost + `hostname`
	xhost + localhost
fi

#
# Set up path to include fink installed apps and Darwin specific overrides
#
PATH=$PATH:/sw/bin
PATH=$PATH:$HOME/bin/Darwin
PATH=$PATH:$HOME/bin

#
# Do any requested xmodmap operations
#
if [ -f $HOME/.xmodmaprc ]; then
	xmodmap $HOME/.xmodmaprc
fi

#
# Load the X defaults after substituting geometry bindings
#
if [ -f $HOME/.Xdefaults.in ]; then
	rm -f $HOME/.Xdefaults
	subst-geometry < $HOME/.Xdefaults.in > $HOME/.Xdefaults
	xrdb -load $HOME/.Xdefaults
fi


# Set up ssh-agent
. $HOME/.ssh-attach
if [ "$SSH_AUTH_SOCK" = "" ]; then
	xterm -bg yellow -geom $SSH_INIT_GEOM -fg black -e $HOME/bin/ssh-init
	. $HOME/.ssh-attach
fi


# Mail shells and yabiff
/usr/bin/wish $HOME/bin/yabiff -geometry $YABIFF_GEOM pisco:inbox,@F,tier,dtn,tinyos xoopi:@,pair-notices &
xmutts

# and one more
xterm &

quartz-wm
# fvwm2 

# kill yabiff
[ "$RUNGREP" = "1" ] || ps auxww | grep yabiff | $HOME/bin/awkill
