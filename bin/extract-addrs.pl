#!/usr/local/bin/perl

while (<STDIN>) {
    s/.*<//;
    s/>.*//;
    print $_;
}
