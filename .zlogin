##
## FILE:  .zlogin
## DESC:  zsh init file; sourced by login shells
##

###################################################
# Programs to run at login                        #
###################################################

if [ -f /etc/motd ] ; then 
    cat -s /etc/motd
fi

uname -snmr
# frm -s new 2> /dev/null

###################################################
# Environment variables describing my preferences #
###################################################

export EDITOR='vi'
export VISUAL='vi'

if [ "$TERM" = "" ]; then
	TERM=network
fi

### For those unknown consoles; vt100 default
if [ "$TERM" = "unknown" ]; then
	print -n "(vt100)? "
	read TERM
	if [ "$TERM" = "" ]; then
		TERM=vt100
	fi
fi

if [ "$TERM" = "dec-vt220" ]; then
	export TERM=vt220
fi

#
# Figure out the canonical hostname
#
HOST=`/bin/hostname | sed -e 's/\..*//'`
export HOST

#####################################################
# Prompt the user for initial information gathering #
#####################################################

## Make sure the error directory exists
mkdir -p ~/.err

## load ssh environment if it doesn't exist
if [ "$SSH_AUTH_SOCK" = "" ]; then
	echo -n "Looking for ssh-agent... "
	. .ssh-attach
	if [ "$SSH_AUTH_SOCK" = "" ]; then
		echo "not found."
	else
		echo "found $SSH_AUTH_SOCK."
	fi
fi

# If a dialup terminal
if [ "$TERM" = "dialup" -o "$TERM" = "network" -o "$TERM" = "ansi" ]; then
	export TERM=vt100
	unset TERMCAP
	export XONXOFF
fi

#
# Check for host specific login file
#
if [ -f ~/.zlogin-$HOST ]; then
	source ~/.zlogin-$HOST
fi

#
# Prompt the user about starting X windows
#
STARTX=no
if [ `tty` = "/dev/tty1" -a "$TERM" = "linux" ]; then
    STARTX=yes
elif [ `tty` = "/dev/ttyv0" -a `uname -s` = FreeBSD ]; then
    STARTX=yes
fi

if [ "$STARTX" = "yes" ] ; then
    if [ -r ~/.fetchmailrc ]; then
	echo "starting fetchmail..."
	fetchmail
    fi

    echo -n "Start X Windows? "
    read -q CHOICE
    if [ $CHOICE = 'y' ]; then
	   echo "Starting X Windows..."
	   startx 2>>! ~/.err/startx
	   sleep 1
    fi
fi
