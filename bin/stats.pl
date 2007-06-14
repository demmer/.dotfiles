#!/usr/bin/perl
#
# Statistics code adapted from EECS glookup
#

use POSIX qw(floor ceil);


# sum \@A
# The sum of the elements of @A.
sub sum 
{
    my $A = shift;
    my ($total, $i);
    $total = 0;
    for ($i = 0; $i <= $#$A; $i += 1) {
        $total += $$A[$i];
    }
    return $total;
}

# statistic \@grades $p 
#   is the nominal grade >= a fraction $p of @grades, assuming the
#   the latter is sorted in increasing order.
sub statistic
{
    my ($allGrades, $fraction) = @_;
    my $t = ($#$allGrades + 1) * $fraction;
    $l = floor($t);
    $u = ceil($t);
    
    return $$allGrades[$l] * ($l - $t + 1) + $$allGrades[$u] * ($t - $l);
}

# displayStats $maxScore $bucketSize, @data
# Prints statistics about @grades, assuming that $maxScore is the maximum
# possible score.  The statistics include a histogram, whose buckets contain a
# range of $bucketSize grades, if $bucketSize is non-zero (otherwise
# chooses a reasonable default).  Prints to standard output.
sub displayStats
{
    my ($maxScore, $bucketSize, @data) = @_;
    my $count = $#data + 1;

    my ($mean, $stddev, @histogram);
    my ($i, $j);

    if ($count < 5) {
        die ("Not enough grades to compute statistics (only have $count)\n");
    }

    @data = sort {$a <=> $b} (@data);

    $mean = sum (\@data) / $count;
    
    my $var = 0;
    for ($i = 0; $i < $count; $i += 1) {
        $var += ($data[$i] - $mean) * ($data[$i] - $mean);
    }
    $var = $var / ($count - 1); # unbiased estimator uses n-1...
    $stddev = sqrt($var) * ($count - 0.75) / ($count - 1);
                                  # and to be REALLY anal, we do this 
                                  # tweak to get approximately unbiased 
                                  # estimate of sigma.

    if ($bucketSize <= 0) {
      #$bucketSize = sqrt($maxScore)/2;
      $bucketSize = $maxScore / 25;
    }
    for ($i = 0; $i < $count; $i += 1) {
        $histogram[$data[$i] / $bucketSize] += 1;
    }

    printf "Number of entries:         %5d\n",   $count;
    printf "Mean:                      %5.1f\n", $mean;
    printf "Standard deviation:        %5.1f\n", $stddev;
    printf "Minimum:                   %5.1f\n", $data[0];
    printf "1st quartile:              %5.1f\n", 
           statistic (\@data, 0.25);
    printf "2nd quartile (median):     %5.1f\n", 
           statistic (\@data, 0.50);
    printf "3rd quartile:              %5.1f\n", 
           statistic (\@data, 0.75);
    printf "Maximum:                   %5.1f\n", $data[$#data];
    printf "Max possible:              %5.1f\n", $maxScore;
    printf "Distribution:\n";

    $maxbucket = $histogram[0];
    for ($i = 0; $i <= $#histogram; $i += 1) {
        $maxbucket = $histogram[$i] if ($maxbucket < $histogram[$i]);
    }
    for ($i = 0; $i <= $#histogram; $i += 1) {
        printf "%5.1f  -%5.1f:%6d  ", 
               $i*$bucketSize,
               ($i+1)*$bucketSize,
               $histogram[$i];
        for ($j = 0; $j < 20 * $histogram[$i] / $maxbucket; $j += 1) {
            print "*";
        }
        print "\n";
    }
}


%data = ();
$max = 0;
$lineno = 0;
while (<STDIN>) {
    ++$lineno;
    @line = split(/\s+/);
    if ($#line != 0) {
	chop;
	print("invalid line $lineno '$_'\n");
	next;
    }

    if ($line[0] > $max) {
	$max = $line[0];
    }

    $data[$#data+1] = $line[0];
}

displayStats($max, 0, @data);


