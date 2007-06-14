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
UNAME=/usr/bin/uname
if [ ! -x $UNAME ]; then
	UNAME=`which uname`
fi
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
	Darwin-*)
		export ARCH=Darwin
		;;
        CYGWIN*)
		export ARCH=Cygwin
		;;
  	*)
  		export ARCH=`$UNAME -s`
  		;;
  	esac
else
	export ARCH=unk
	export OSARCH=unk
fi

#
# Figure out machine local path based on symlinks in ~/bin/bindirs
#
localpath=
for d in ~/bin/bindirs/*; do
if [ -h $d ] ; then
	d=`(cd $d && pwd -r)`
	localpath=($localpath $d)
fi
done

#
# I set my bin path (but not bindirs) before all the others to 
# override programs
#
path=(						\
	~/bin/$ARCH                             \
	~/bin                                   \
	/usr/ucb				\
	/usr/local/bin				\
	/usr/local/sbin				\
	/bin					\
	/sbin					\
	/usr/bin				\
	/usr/sbin				\
	/usr/X11R6/bin				\
	/usr/games				\
	/usr/java/jdk/bin			\
	$localpath				\
	.					\
      )
#
# Similar trick for LD_LIBRARY_PATH and MANPATH
#
ld_library_path=
if [ -d ~/lib ]; then
for d in ~/lib/* ; do
	if [ -d $d ] ; then
		d=`(cd $d && pwd -r)`
		ld_library_path=($ld_library_path $d)
	fi
done
fi

manpath=`manpath 2> /dev/null`
if [ -d ~/man ]; then
for d in ~/man/* ; do
	if [ -d $d ] ; then
		d=`(cd $d && pwd -r)`
		manpath=($manpath $d)
	fi
done
fi

#
# But they're not autoset like path is
#
LD_LIBRARY_PATH=${(j{:})ld_library_path}
MANPATH=${(j{:})manpath}

# Make sure that these are exported to the environment
# I know that zsh doesn't do this by default for MANPATH
export PATH SHELL LD_LIBRARY_PATH MANPATH

#
# On OS X, fink wants us to source an init script to set up path,
# environment, etc
#
if [ -f /sw/bin/init.sh ]; then
	. /sw/bin/init.sh
fi

if [ $ARCH = "Darwin" ] ; then
# pkg-config and autotools
export PKG_CONFIG_PATH="/usr/lib/pkgconfig:/usr/X11R6/lib/pkgconfig"
export ACLOCAL_FLAGS="-I /usr/share/aclocal"
fi

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
export CVSROOT_PISCO=:ext:pisco:/repository
export CVSROOT_TIER=:ext:pisco:/project/cs/brewer/tier/ICT/repository
export CVSROOT_TINYOS=:ext:cvs-sourceforge:/cvsroot/tinyos/
export CVSROOT_DTN=:ext:sandbox:/repository
export CVSROOT_NINJA=:ext:ninja.cs.berkeley.edu:/disks/ninja/.CVS-ninja
export CVSROOT_LLADD=:ext:cvs-sourceforge:/cvsroot/lladd

# Host specific overrides
if [ $HOST = pisco ]; then
    export CVSROOT_PISCO=/repository
    export CVSROOT_TIER=/usr/projects/tier/ICT/repository
    export CVSROOT=$CVSROOT_PISCO

elif [ -d /project/cs/brewer/tier/ICT/repository/CVSROOT ]; then
    export CVSROOT_PISCO=/project/cs/brewer/tier/demmer/repository/
    export CVSROOT_TIER=/project/cs/brewer/tier/ICT/repository/
    export CVSROOT=$CVSROOT_TIER

elif [ -d /repository/CVSROOT ]; then
    export CVSROOT=/repository
    if [ -d /repository/DTN2 ]; then
    	export CVSROOT_DTN=/repository
    fi
else 
    export CVSROOT=$CVSROOT_INIGO
fi

# Ditto for SVNROOT
if [ $HOST = wangari ]; then
    export SVNROOT=file://svndepot/
else
    export SVNROOT=svn+ssh://wangari.cs.berkeley.edu:/svndepot/
fi

export CVS_RSH=ssh
export RSYNC_RSH=ssh
 
if [ $ARCH = Darwin -a "$DISPLAY" = "" ] ; then
    export DISPLAY=:0.0
fi


unset XAUTHORITY
export XAUTHORITY

# override Linux's annoying UTF-8 crap
unset LANG
export LANG

# tex search path
export TEXINPUTS=".:/usr/share/texmf//:~/.dotfiles/latex//:"

# TinyOS / Java / Jython
export JAVA_HOME="/opt/IBMJava2-141"
export TOSROOT="$HOME/work/tinyos-1.x"
export TOSDIR="$TOSROOT/tos"
if [ "$DBG" = "" ] ; then
	export DBG="all"
fi

if [ $ARCH = Darwin ] ; then
    export JUNIT_HOME=$HOME/Library/Java/junit4.1
    export CLASSPATH=".:$JUNIT_HOME:$JUNIT_HOME/junit-4.1.jar"
fi

export JYTHON_HOME="$HOME/work/jython"

