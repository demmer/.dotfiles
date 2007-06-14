#!/usr/bin/perl

print "first\tlast\temail\n";

while (<>) {
    if (/#/) { next; }
    if (! /alias (\S+)([^<]*) <(.+)>/) {
        # print "error: $_";
        next;
    }
    $nick = $1;
    $name = $2;
    $email = $3;

    if ($name =~ /\s*(.+) (\S+)\s*$/) {
        $first = $1;
        $last  = $2;
    } else { 
        $first = $name;
        $last  = "";
    }

    print "$first\t$last\t$email\n"
}
