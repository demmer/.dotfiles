##
## FILE:  .zshenv
## DESC:  Environment for the z-shell; read by both interactive and
##        non-interactive shells at startup
##
## USAGE: zsh
## 
## DATE:  Sat May 27 17:22:34 1995
## $Id: .zshenv,v 1.1 2002-07-21 16:16:58 miked Exp $
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
if [ -x /bin/uname ]; then
	export OSARCH=`/bin/uname -s`-`/bin/uname -r`
	case "$OSARCH" in
	SunOS-5*)
		export ARCH=solaris
		;;
	SunOS-4*)
		export ARCH=sunos
		;;
	*)
		export ARCH=`uname -s`
		;;
	esac
else
	export ARCH=unk
	export OSARCH=unk
fi

# I set my path before all the others to override programs

path=(						\
        ~/bin					\
	/bin					\
	/sbin					\
	/usr/bin				\
	/usr/sbin				\
	/usr/local/bin				\
	/usr/X11R6/bin				\
	/usr/games				\
	.					\
      )

# no need for manpath

# Make sure that these are exported to the environment
# I know that zsh doesn't do this by default for MANPATH
export PATH SHELL

# LD_LIBRARY_PATH settings - should be architecture dependent
ld_library_path=(				\
		 /usr/lib			\
		 /usr/lib/java/lib/		\
		 /usr/lib/java/lib/i586		\
 		 )
			 
export LD_LIBRARY_PATH=${(j{:})ld_library_path}

export PERL5LIB=/usr/local/lib/perl5

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

### Setup info
infopath=(   \
             /usr/info                               \
             .                                       \
             )
export INFOPATH=${(j{:})infopath}

### Set up INDEX databases
##  usage: index <database> <key-regexp>
export INDEXPATH=/u/system/admin/users:/u/system/admin/inventory

# get functions and aliases
source ~/.zalias
source ~/.zfunc

# TEX stuff 
export TEXINPUTS=.:~/latex:

# added by AMD
# export nobeep

export EDITOR='vi'
export VISUAL='vi'
export AUTOUNSUBSCRIBE="*"
