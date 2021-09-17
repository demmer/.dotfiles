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
	export SYSVER=`$UNAME -s`-`$UNAME -r`
  	case "$SYSVER" in
  	SunOS-5*)
  		export SYS=solaris
  		;;
  	SunOS-4*)
  		export SYS=sunos
  		;;
	FreeBSD-4*)
		export SYS=FreeBSD4
		;;
  	FreeBSD-3*)
  		export SYS=FreeBSD3
  		;;
  	FreeBSD-2*)
  		export SYS=FreeBSD2
  		;;
	Darwin-*)
		export SYS=Darwin
		;;
        CYGWIN*)
		export SYS=Cygwin
		;;
  	*)
  		export SYS=`$UNAME -s`
  		;;
  	esac
        export SYSARCH=`$UNAME -m | sed 's/i[3456]86/x86/'`
else
	export SYS=unk
	export SYSVER=unk
	export SYSARCH=unk
fi

#
# Figure out machine local path based on symlinks in ~/bin/bindirs
#
localpath=
if [ -d ~/bin/bindirs -a ! -z "`ls ~/bin/bindirs/ 2> /dev/null`" ] ; then
for d in ~/bin/bindirs/*; do
if [ -h $d ] ; then
	d=`(cd $d && pwd -r)`
	localpath=($localpath $d)
fi
done
fi

#
# I set my bin path (but not bindirs) before all the others to 
# override programs
#
path=(						\
	~/bin/$SYS/$SYSARCH                     \
	~/bin/$SYS                              \
	~/bin                                   \
	/usr/local/opt/openjdk/bin		\
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
# Similar trick for other paths
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

# XXX It's better to just have pylib in the path as opposed to
# expanding out all the links
# pythonpath=
# if [ -d ~/pylib ]; then
# for d in ~/pylib/* ; do
#         if [ -d $d ] ; then
#                 d=`(cd $d && pwd -r)`
#                 pythonpath=($pythonpath $d)
#         fi
# done
# fi

pythonpath=
if [ -d ~/pylib ]; then
  pythonpath=~/pylib
fi

node_path=(						\
	/usr/local/lib/node_modules                     \
)

MANPATH=${(j{:})manpath}
PYTHONPATH=${(j{:})pythonpath}
NODE_PATH=${(j{:})node_path}

# Make sure that these are exported to the environment
# I know that zsh doesn't do this by default for MANPATH
export PATH SHELL MANPATH PYTHONPATH NODE_PATH

#
# On OS X, fink wants us to source an init script to set up path,
# environment, etc
#
if [ -f /sw/bin/init.sh ]; then
	. /sw/bin/init.sh
fi

if [ $SYS = "Darwin" ] ; then
# pkg-config and autotools
export PKG_CONFIG_PATH="/usr/local/lib/pkgconfig:/usr/lib/pkgconfig:/usr/X11R6/lib/pkgconfig:/usr/local/ssl/lib/pkgconfig"
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

if [ $SYS = "Darwin" ] ; then
   export EDITOR='/usr/bin/vim'
   export VISUAL='/usr/bin/vim'
fi

export ENSCRIPT_2UP='-p - -2r'

export HOSTNAME=`echo $HOST | sed 's/\..*//g'`
export EMACSLOCKDIR=$HOME/.emacslockdir

# needed for su to still grab my .emacs
export EMACSHOME=$HOME

export CVS_RSH=ssh
export RSYNC_RSH=ssh

unset XAUTHORITY
export XAUTHORITY

# override Linux's annoying UTF-8 crap
unset LANG
export LANG

export EC2_HOME="$HOME/src/ec2-api-tools"

export GOPATH=$HOME/work/go
#export SLACK_SYNC_DEV_AUTO_ATTACH=1
export SLACK_SYNC_DEV_ASSETS_SOURCE=dev-mains

export VTROOT=$HOME/work/go/src/vitess.io/vitess
export USE_CLI_SERVER=1

export ANT_HOME=/usr/local/ant

export SSH_USER=demmer

