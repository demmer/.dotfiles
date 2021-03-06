##
## FILE:  .zshrc
## DESC:  zsh init file; sourced by interactive shells
##

# search path for the cd command
cdpath=(.)

# on OS X, we don't run the .zlogin, so check for ssh-agent here
if [ $SYS = "Darwin" -a "$SSH_AUTH_SOCK" = "" ]; then
	source $HOME/.ssh-attach
fi

# ignore these extensions when completing filenames
fignore=(\~)

# use hard limits, except for a smaller stack
unlimit
limit -s

# no cores on OS X
if [ $SYS = "Darwin" ] ; then
    ulimit -c 0
fi

# Enable Git info in prompt
autoload -Uz vcs_info
autoload -U colors && colors
precmd_vcs_info() { vcs_info }
precmd_functions+=( precmd_vcs_info )
setopt prompt_subst
zstyle ':vcs_info:git:*' formats ' [%b]'

if [ "$EMACSPARENT" = "1" ]; then
	PROMPT='[%m] %~ -> '
	RPROMPT=''
	unsetopt zle
elif [ "$SCRIPTPARENT" = "1" ] ; then
	PROMPT='%S[%m]%s (script) -> '
else
	PROMPT='%S[%m]%s -> '
	RPROMPT=' %~%{$fg[yellow]%}$vcs_info_msg_0_%{$reset_color%}'
fi

# what is a word, really?
WORDCHARS='*?_-.[]~=&;!#$%^(){}<>'

HISTFILE=~/.zhist
HISTSIZE=500
SAVEHIST=500

DIRSTACKSIZE=50

MAILCHECK=0

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
  extendedhistory \
  histexpiredupsfirst\
  histignoredups \
  incappendhistory \
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

# get key bindings and completions
source ~/.zbind
source ~/.zcomp

## SYSTEM SPECIFIC
if [ "$SYS" = "IRIX" -o "$SYS" = "HP-UX" ]; then
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


export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

