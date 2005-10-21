#
# .xinitrc for cygwin hosts
#

cd $HOME
export SHELL=/bin/zsh

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

. $HOME/.ssh-attach
if [ "$SSH_AUTH_SOCK" = "" ]; then
	xterm -bg yellow -fg black -e $HOME/bin/ssh-init
	. $HOME/.ssh-attach
fi

xrdb $HOME/.Xdefaults

xmutts &
emacs &
xterm &
