##
## FILE:  .zshenv
## DESC:  Environment for the z-shell; read by both interactive and
##        non-interactive shells at startup
##
## USAGE: zsh
## 
## DATE:  Sat May 27 17:22:34 1995
## 
## Modification history:
##    jal   Sat May 27 17:22:37 1995   made header
##

## NOTE
#  The motif paths are going to change in the near future.

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

# no need for manpath

# Make sure that these are exported to the environment
# I know that zsh doesn't do this by default for MANPATH
export PATH SHELL

# LD_LIBRARY_PATH settings - should be architecture dependent
#  ld_library_path=(				\
#  		 /usr/lib			\
#  		 /usr/lib/java/lib/		\
#  		 /usr/lib/java/lib/i586		\
#   		 )
			 
#  export LD_LIBRARY_PATH=${(j{:})ld_library_path}

# less is a much better pager than more
export MORE=less
export PAGER=less
export LESS="-s"

# hosts for the tip program
export REMOTE=~/.remote

# use the dept printer
# export PRINTER=printer

### Setup gwm
export GWMPATH=~/.gwm:/cs/data/gwm

### Set up INDEX databases
##  usage: index <database> <key-regexp>
export INDEXPATH=/u/system/admin/users:/u/system/admin/inventory

# get functions and aliases
source ~/.zalias
source ~/.zfunc

# set umask
chpwd

# added by AMD
# export nobeep

export EDITOR='vi'
export VISUAL='vi'

export ENSCRIPT_2SIDED='-p - -2r'

export HOSTNAME=`echo $HOST | sed 's/\..*//g'`
export EMACSLOCKDIR=$HOME/.emacslockdir

# needed for su to still grab my .emacs
export EMACSHOME=$HOME

export CVS_RSH=ssh
if [ -d /repository/CVSROOT ]; then
    export CVSROOT=/repository
else 
    export CVSROOT=:ext:inigo:/repository  
fi

unset XAUTHORITY
export XAUTHORITY

# override Linux's annoying UTF-8 crap
unset LANG
export LANG

