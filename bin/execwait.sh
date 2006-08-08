#!/bin/sh

PROGNAME=$0

echo "`date +%s` $PROGNAME: running '$*'"
$*
EXITCODE=$?

if test $EXITCODE = 0 ; then
    echo "`date +%s` $PROGNAME: $1 exited cleanly"
else
    echo "`date +%s` $PROGNAME: $1 exited with status $EXITCODE"
fi
