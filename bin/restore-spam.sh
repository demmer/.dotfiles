#!/bin/sh

mkdir new

for f in `ls -1`; do
	echo $f
	cat $f | spamassassin -d | rehtml.pl > new/$f
done
