##
## FILE:  .zlogout
## DESC:  zsh init file; sourced at logout by login shells
##
## USAGE: zsh -l
## 
## DATE:  Sat May 27 17:41:09 1995
## 
## Modification history:
##    jal   Sat May 27 17:41:12 1995   made header
##

# set the "print" command
if [ "$TERM" = "xterm" ]; then
	# no output
	print=true
else
	print=print
fi

# remove emacs' .saves files
$print -n "Removing .saves files..."
/bin/rm -f .saves* >& /dev/null
$print "done."

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
