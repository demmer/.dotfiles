#!/usr/local/bin/tclsh

set tot 0
set L {}

while {![eof stdin]} {
    set l [gets stdin]
    if {$l == ""} {
	continue
    }

    lappend L $l
    set tot [expr $tot + $l]
}

set cnt [llength $L]
set mean [expr $tot / $cnt]

set stddev 0
foreach e $L {
    set diff [expr $e - $mean]
    set stddev [expr $stddev + ($diff * $diff)]
}

set stddev [expr sqrt($stddev) / $cnt]

set L [lsort -integer $L]

puts "Count:   $cnt"
puts "Total:   $tot"
puts "Mean:    $mean"
puts "Median:  [lindex $L [expr $cnt / 2]]"
puts "Std dev: $stddev"
