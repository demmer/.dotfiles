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
# Load in geometry
#
. $HOME/.geometry

if [ -n "$X11HOME" ]; then
        xhost + `hostname`
	xhost + localhost
fi

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
	xrdb $HOME/.Xdefaults
fi

# Set up ssh-agent
. $HOME/.ssh-attach
if [ "$SSH_AUTH_SOCK" = "" ]; then
	xterm -bg yellow -geom $SSH_INIT_GEOM -fg black -e $HOME/bin/ssh-init
	. $HOME/.ssh-attach
fi

# Set the background to a nice graphic
xv -quit -rmode 4 $HOME/.dotfiles/gray_rock.gif

# enable screen saver
xset s 300

# Bring up the window manager and save its process id
fvwm2 &
WMPID=$!

# Utility programs
mozilla & 
dclock &
yabiff -geometry $YABIFF_GEOM pisco:inbox,@F,tier,dtn,tinyos xoopi:@,pair-notices &
gaim &

# Bring up a console window and a work shell
xterm -title "Console" -n "Console" -geometry =$CONSOLE_GEOM -e /bin/sh -c \
    '[ -f /var/log/messages ] && tail -f -n 0 /var/log/messages & \
     [ -f /var/log/secure ] && tail -f -n 0 /var/log/secure & \
     clear; zsh' &
xterm -n "Fezzik, it's you . . . true" -geometry =$XTERM_1_GEOM &

# Mail shells
if [ x$HOST = xpisco ]; then
	gnome-terminal --geometry $PISCO_XMUTT_GEOM \
		--tab -t "EECS Mail" -e mutt \
		--tab -t "Xoopi Mail" -e 'ssh -t xoopi bin/mutt' &
else
	pisco-xmutt &
	xoopi-xmutt &
fi

# Tunnels for urlview
$HOME/bin/urlview-tunnel-shell xoopi

# Bring up emacs and work shells
emacs -geometry =$EMACS_XINIT_GEOM -title "Inconceivable" &
xterm -n "Fezzik, are there rocks up ahead?" -geometry =$XTERM_2_GEOM &
xterm -n "If there are, we'll all be dead!!" -geometry =$XTERM_3_GEOM &
xterm -n "No more rhymes now, I mean it!!" -geometry =$XTERM_4_GEOM &
xterm -n "Anybody want a peanut?" -geometry =$XTERM_5_GEOM &

# Run any host-specific commands (i.e. xapm on laptops)
if [ -f "$HOME/.xinitrc-$HOST" ]; then
	source "$HOME/.xinitrc-$HOST"
fi

# Wait for the window manager to quit
wait $WMPID

