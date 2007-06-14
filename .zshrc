##
## FILE:  .zshrc
## DESC:  zsh init file; sourced by interactive shells
##

# search path for the cd command
cdpath=(.)

# on OS X, we don't run the .zlogin, so check for ssh-agent here
if [ $ARCH = "Darwin" -a "$SSH_AUTH_SOCK" = "" ]; then
	source $HOME/.ssh-attach
fi

# ignore these extensions when completing filenames
fignore=(\~)

# use hard limits, except for a smaller stack
unlimit
limit -s

# no cores on OS X
if [ $ARCH = "Darwin" ] ; then
    ulimit -c 0
fi

if [ "$EMACSPARENT" = "1" ]; then
	PROMPT='[%m] %~ -> '
	RPROMPT=''
	unsetopt zle
elif [ "$SCRIPTPARENT" = "1" ] ; then
	PROMPT='%S[%m]%s (script) -> '
else
	PROMPT='%S[%m]%s -> '
	RPROMPT=' %~'
fi

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
  clobber \
  correct \
  extendedglob \
  histignoredups \
  listtypes \
  longlistjobs \
  nobeep \
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

# get key bindings and completions
source ~/.zbind
source ~/.zcomp

## ARCHITECTURE SPECIFIC
if [ "$ARCH" = "IRIX" -o "$ARCH" = "HP-UX" ]; then
	stty intr '^C' erase '^?' kill '^U' susp '^Z' echoe
fi

## FUNCTIONS FOR INTERACTIVE SHELLS ONLY

## xterm specific functions
if [ "$TERM" = "xterm" ]; then

# change the title of an xterm
#   usage: xtitle <title>
function xtitle {
	print -n "]0;$*"
}

fi
#  end of xterm specific functions
