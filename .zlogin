##
## FILE:  .zlogin
## DESC:  zsh init file; sourced by login shells
##
## USAGE: zsh -l
## 
## DATE:  10/4/94
## $Id: .zlogin,v 1.2 2002-04-03 16:23:22 miked Exp $
## 
## Modification history:
##    jal   Sat May 27 17:29:40 1995   made header
##    mjd   Sept. 29 1995 modified

###################################################
# Aliases only for the login shell                #
###################################################

scl() { screen -R; logout }
xst() { startup $*; logout }

###################################################
# Programs to run at login                        #
###################################################

/bin/uname -snmr
/usr/bin/frm -s new -s old

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

# cheap solaris hacks
if [ "$ARCH" = "solaris" ]; then
	if [ "$TERM" = "sun" -a `tty` != "/dev/console" ]; then
		export TERM="network"
	fi
fi


#####################################################
# Prompt the user for initial information gathering #
#####################################################

## create a netscape disk cache directory
mkdir -p /tmp/netscape-$LOGNAME

## check about starting X
# . .prompt-startx

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

# run my overrided version of newmail that supports ringing the bell
# newmail -i 15

# always set the display when logging in from these hosts
AUTO_DISPLAY_HOSTS="vissini fezzik"

