#!/bin/sh

from=$1
to=$2

if [ "x$to" = "x" ]; then
	echo "usage: $0 <from> <to>"
	exit 1
fi

for f in `ls`; do
	echo $f;
done;
