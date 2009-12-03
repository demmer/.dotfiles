#!/bin/sh

# Create a regular pattern of data in a file

size=$1
if test -z $size ; then
    echo "usage: $0 <size in KB>"
    exit 1
fi

i=0
while test $i -lt $size ; do
    printf "%-11s\n" $i
    echo "-----------"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    echo "1234567890123456789012345678901234567890123456789"
    i=$((i + 1))
done