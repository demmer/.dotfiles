##
## FILE:  .zshrc
## DESC:  zsh init file; sourced by interactive shells
##
## USAGE: zsh
## 
## DATE:  2/16/94
## $Id: .zshrc,v 1.1 2002-08-06 16:12:53 miked Exp $
## 
## Modification history:
##    jal   Sat May 27 17:28:10 1995   made header
##


# set the xterm window to my hostname
#if [ "$TERM" = "xterm" ]; then
#	print -n "]0;"$HOST""
#	print -n "]2;$USER@$HOST   "`pwd`""
#	eval `resize`
#fi

# Fix wierd TERM stuff
if [ "$TERM" = "dumb" ]; then
	echo "Resetting TERM from dumb to xterm"
	TERM=xterm
	export TERM
fi

# search path for the cd command
cdpath=(.)

# ignore these extensions when completing filenames
fignore=(\~)

# use hard limits, except for a smaller stack and no core dumps
unlimit
limit stack 8192
limit core 0
limit -s

# left prompt
PROMPT='%S[%m]%s -> '
# right prompt
RPROMPT=' %~'

# what is a word, really?
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

SAVEHIST=200
HISTFILE=~/.zhist
HISTSIZE=200

DIRSTACKSIZE=50

# This next heinosity is so that zsh does not check the mail file for
# modifications ever.  This might hang the shell if the mail server
# goes down.  I hate that.  zsh has no good way of disabling this
# feature.
# MAILCHECK=90000000
MAILCHECK=5

# lots of options
setopt \
  appendhistory \
  autocd \
  autolist \
  automenu \
  autopushd \
  autoresume \
  braceccl \
  correct \
  extendedglob \
  histignoredups \
  listtypes \
  longlistjobs \
  nobeep \
  noclobber \
  nohistbeep \
  nohup \
  nolistbeep \
  notify \
  pushdminus \
  pushdsilent \
  pushdtohome \
  recexact \
  rcquotes \
  sunkeyboardhack

# don't want these, though
unsetopt \
  bgnice \
  nomatch 

watch=(notme)
WATCHFMT='%n %a %l from %m at %t.'
LOGCHECK=10

# get the completions and bindings from seperate files
source ~/.zcomp
source ~/.zbind

## ARCHITECTURE SPECIFIC
if [ "$ARCH" = "IRIX" -o "$ARCH" = "HP-UX" ]; then
	stty intr '^C' erase '^?' kill '^U' susp '^Z' echoe
fi

eval `dircolors ~/.dir_colors`
## FUNCTIONS FOR INTERACTIVE SHELLS ONLY

## xterm specific functions
if [ "$TERM" = "xterm" ]; then


# change the title of an xterm
#   usage: xtitle <title>
#function xtitle {
#	print -n "]0;$*"
#}

## before each prompt, make the xterm's title display some info
#function precmd {
#	print -n "]2;$USER@$HOST   $PWD"
#}

fi
#  end of xterm specific functions
