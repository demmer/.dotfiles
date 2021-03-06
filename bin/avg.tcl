#!/usr/bin/tclsh

set tot 0
set L {}
set min 0
set max 0

while {![eof stdin]} {
    set l [gets stdin]
    if {$l == ""} {
	continue
    }

    if [catch {expr $l}] {
	puts "warning: skipping '$l' (not a number)"
	continue
    }

    lappend L $l
    set tot [expr $tot + $l]
}

set cnt [llength $L]
set mean [expr 1.0 * $tot / $cnt]

set stddev 0
foreach e $L {
    # need to force it to use floating point, otherwise it may overflow
    set diff [expr (1.0 * $e) - (1.0 * $mean)]
    set stddev [expr $stddev + ($diff * $diff)]
    if {$min == 0 && $max == 0} {
        set min $e
        set max $e
    } else {
        if {$e < $min} { set min $e }
        if {$e > $max} { set max $e }
    }
}
set stddev [expr sqrt($stddev) / $cnt]

set L [lsort -real $L]

puts "Count:   $cnt"
puts "Total:   $tot"
puts "Min:     $min"
puts "Max:     $max"
puts "Mean:    $mean"
puts "Median:  [lindex $L [expr $cnt / 2]]"
puts "Std dev: $stddev"
