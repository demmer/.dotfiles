##
## FILE:  .zlogin
## DESC:  zsh init file; sourced by login shells
##
## USAGE: zsh -l
## 
## DATE:  10/4/94
## $Id: .zlogin,v 1.1 2001-08-14 16:17:04 miked Exp $
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

## if we are on a sun console
if [ `tty` = "/dev/console" -a "$TERM" = "sun" ]; then

     # set the openwin path
	if [ -d /usr/openwin ]; then
		export OPENWINHOME='/usr/openwin'
	fi

	#  figure out what kind of graphics we've got
	if [ -x /cs/bin/fbname ]; then
		export FBNAME=`fbname`
	else
		export FBNAME='unk'
	fi
	case "$FBNAME" in
	leo)
		export FBBITS=24
		;;
	ffb)
		export FBBITS=24
		;;
	gx)
		export FBBITS=8
		;;
	*)
		export FBBITS=8
		;;
	esac

    	#
	# Ask if the X environment should be activated
	#

	print -n "Start XWindows? [y] "
	CHOICE=`grabchars -dy -t5`
	print "$CHOICE"
	if [ "$CHOICE" != "n" -a "$CHOICE" != "N" ]; then
		if [ "$ARCH" = "solaris" ]; then
			print "Starting OpenWindows under Solaris..."
			$OPENWINHOME/bin/openwin -dev /dev/fb defdepth $FBBITS >&! ~/.err/openwin
		elif [ "$ARCH" = "sunos" ]; then
			print "Starting XWindows under SunOS..."
			xinit 2>>! ~/.err/xinit
		else
			print "Starting XWindows on $ARCH..."
			xinit 2>>! ~/.err/xinit
		fi
		sleep 5
		logout
	else
		stty erase \^H
	fi
elif [ `tty` = "/dev/tty1" -a "$TERM" = "linux" ]; then
    echo -n "Start X Windows? "
    read -q CHOICE
    if [ $CHOICE = 'y' ]; then
	   echo "Starting X Windows..."
	   startx 2>>! ~/.err/startx
	   sleep 1
	   logout
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




