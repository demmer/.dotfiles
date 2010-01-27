#!/bin/sh

SUBDIR=$1/
shift
if test -z $SUBDIR ; then
    echo "usage: $0 <SUBDIR>"
    exit 1
fi

SRCDIR=/Users/demmer/shadowfs_data/$SUBDIR
#DSTDIR=/Volumes/mint-nfs/demmer/shadowfs_data/$SUBDIR
DSTDIR=lime.lab.nbttech.com:/u/demmer/shadowfs_data/$SUBDIR

set -x
rsync -rlpgoDvt --modify-window=1 --delete $* $SRCDIR $DSTDIR
