#!/bin/sh

find ~/Downloads -ctime +7d -print 

read -p "Delete? " prompt

if [ "$prompt" = "y" ] ; then
    echo "Deleting..."
    find ~/Downloads -ctime +7d -delete
fi


