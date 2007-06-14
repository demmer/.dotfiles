#!/usr/bin/perl

if ($#ARGV != 2) {
    print "usage: genstats.pl mode limit count\n";
    exit(1);
}
	
$mode = $ARGV[0];
$limit = $ARGV[1];
$count = $ARGV[2];

$mean = $limit / 2;

for ($i = 0; $i < $count; ++$i) {
    if ($mode eq "uniform") {
        $x = $limit * rand();
    } elsif ($mode eq "normal") {
	$x = $limit * (sqrt(-2.0 * log(rand())) * cos(2.0 * PI * rand()));
    } else {
	die "unknown mode $mode";
    }
    
    print "$x\n";
}
