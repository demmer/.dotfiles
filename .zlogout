##
## FILE:  .zlogout
## DESC:  zsh init file; sourced at logout by login shells
##
## USAGE: zsh -l
## 
## DATE:  Sat May 27 17:41:09 1995
## $Id: .zlogout,v 1.1 2002-08-06 16:12:53 miked Exp $
## 
## Modification history:
##    jal   Sat May 27 17:41:12 1995   made header
##

# make sure these commands run from home directory
cd

# run the logout queue (mjd - I don't want a queue since I don't know
# what it's for
# bin/all/logout_queue -r

# set the "print" command
if [ "$TERM" = "xterm" ]; then
	# no output
	print=true
else
	print=print
fi

# set the "" command
case "$ARCH" in
solaris)
	killall=nkill
	killargs=-all
	;;
linux)
	killall=killall
	killargs=
	;;
*)
	killall=true
	killargs=
	;;
esac

# get rid of the "no matches found" error
setopt nonomatch

# remove any backup .newsrc's, if they exist
$print -n "Removing backup .newsrcs..."
/bin/rm -f .newsrc*\~ >& /dev/null
$print "done."

# remove netscape's cache files
#$print -n "Removing caches..."
#/bin/rm -rf /tmp/netscape-mjd
#$print "done."

# remove emacs' .saves files
$print -n "Removing .saves files..."
/bin/rm -f .saves* >& /dev/null
$print "done."

# remove netscape lock file
$print -n "Removing netscape lock file..."
/bin/rm -f ~/.netscape/lock >! /dev/null
$print "done."

# remove backup todo lists
$print -n "Removing backup todo lists..."
/bin/rm -f todo.txt\~ >& /dev/null
$print "done."

# try to kill cool-mail
$killall $killargs cool-mail >/dev/null

# kill the audio server
$killall $killargs au >/dev/null
$killall $killargs ausun >/dev/null

# kill keep-recent processes
$killall $killargs keep-recent >/dev/null

# fix the utmp file;  invalid logins go bye-bye
if [ -x /usr/local/bin/fix ]; then
	$print -n "Fixing utmp..."
	/usr/local/bin/fix utmp >& /dev/null
	$print "done."
fi

# clean up the .zhist file; who cares about 'exit' statements?
if [ -f .zhist ]; then
if [ -f .zhist_clean ]; then
	/bin/mv .zhist .zhist.$$
	$print -n "Cleaning up history..."
	/bin/sed -f .zhist_clean .zhist.$$ > .zhist 2>/dev/null
	if [ $? -eq 0 ]; then
		/bin/rm -f .zhist.$$
	else
		/bin/mv .zhist.$$ .zhist
	fi
	$print "done."
fi
fi

if [ "$TERM" != "xterm" ]; then
	clear	# all neat and clean
fi
