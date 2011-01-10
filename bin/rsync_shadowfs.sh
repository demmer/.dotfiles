#!/bin/sh

SUBDIR=$1
shift
if test -z $SUBDIR ; then
    echo "usage: $0 <SUBDIR>"
    exit 1
fi

if test -d /Users/$USER/shadowfs_data/$SUBDIR ; then
    SUBDIR=$SUBDIR/
fi

ROOTDIR=`echo $SUBDIR | sed 's#/.*##'`

if test ! -e /Users/$USER/shadowfs_data/.config/$ROOTDIR ; then
    echo "No shadowfs target configured for dir $ROOTDIR"
    exit 1
fi

SRCDIR=/Users/$USER/shadowfs_data/$SUBDIR
DSTDIR=`readlink /Users/$USER/shadowfs_data/.config/$ROOTDIR | \
        sed "s#$ROOTDIR#$SUBDIR#"`

# XXX/demmer rsync over ssh seems to work better than using the Mac's
# NFS stack
DSTDIR=`echo $DSTDIR | sed 's#/Volumes/mint-nfs#onyx.lab:/u#'`

if test -z $RSYNC ; then
    RSYNC=rsync
fi

if test ! -z $PULL ; then
   TMP=$SRCDIR
   SRCDIR=$DSTDIR
   DSTDIR=$TMP
fi

set -x
$RSYNC -rlpgoDvt --cvs-exclude --exclude=".git" --exclude="*.pyc" --modify-window=2 --delete $* $SRCDIR $DSTDIR
