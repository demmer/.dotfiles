##
## FILE:  .zshenv
## DESC:  Environment for the z-shell; read by both interactive and
##        non-interactive shells at startup
##

# the umask specifies what permissions NOT to give when first
#   creating a file.
umask 0002

# fixes the problem with mailbox(1)
if [ "$USER" = "" ]; then
	export USER=$LOGNAME
fi

# Defines the platform that we are running on
UNAME=`which uname`
if [ -x $UNAME ]; then
	export OSARCH=`$UNAME -s`-`$UNAME -r`
  	case "$OSARCH" in
  	SunOS-5*)
  		export ARCH=solaris
  		;;
  	SunOS-4*)
  		export ARCH=sunos
  		;;
	FreeBSD-4*)
		export ARCH=FreeBSD4
		;;
  	FreeBSD-3*)
  		export ARCH=FreeBSD3
  		;;
  	FreeBSD-2*)
  		export ARCH=FreeBSD2
  		;;
  	*)
  		export ARCH=`$UNAME -s`
  		;;
  	esac
else
	export ARCH=unk
	export OSARCH=unk
fi

# I set my path before all the others to override programs

path=(						\
	~/bin/$ARCH                             \
	~/bin                                   \
	/usr/ucb				\
	/usr/sww/bin				\
	/usr/local/bin				\
	/usr/local/sbin				\
	/bin					\
	/sbin					\
	/usr/bin				\
	/usr/sbin				\
	/usr/X11R6/bin				\
	/usr/games				\
	/usr/java/jdk/bin			\
	.					\
      )

# Make sure that these are exported to the environment
# I know that zsh doesn't do this by default for MANPATH
export PATH SHELL

# less is a much better pager than more
export MORE=less
export PAGER=less
export LESS="-s"

# get functions and aliases
source ~/.zalias
source ~/.zfunc

# set umask
chpwd

export EDITOR='vi'
export VISUAL='vi'

export ENSCRIPT_2UP='-p - -2r'

export HOSTNAME=`echo $HOST | sed 's/\..*//g'`
export EMACSLOCKDIR=$HOME/.emacslockdir

# needed for su to still grab my .emacs
export EMACSHOME=$HOME

# Handy to have these readily available
export CVSROOT_INIGO=:ext:inigo:/repository
export CVSROOT_TIER=:ext:tier.cs.berkeley.edu:/usr/projects/ICT/repository
export CVSROOT_TINYOS=:ext:cvs-sourceforge:/cvsroot/tinyos/
export CVSROOT_DTN=:ext:playground:/usr/src/bundles/orig

if [ -d /repository/CVSROOT ]; then
    export CVSROOT=/repository
else 
    export CVSROOT=$CVSROOT_INIGO
fi

export CVS_RSH=ssh

unset XAUTHORITY
export XAUTHORITY

# override Linux's annoying UTF-8 crap
unset LANG
export LANG

# TinyOS / Java / Jython
export JAVA_HOME="/opt/IBMJava2-141"
export TOSROOT="$HOME/work/tinyos-1.x"
export TOSDIR="$TOSROOT/tos"
if [ "$DBG" = "" ] ; then
	export DBG="all"
fi
export CLASSPATH=".:$TOSROOT/tools/java"

export JYTHON_HOME="$HOME/work/jython"
export PATH=$PATH:$JYTHON_HOME


