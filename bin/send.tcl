#!/usr/bin/tclsh

#
# Simple tcp sender
#

proc usage {} {
    puts stderr \
	    {Usage: send.tcl [-ack] [-rate rate] [-pktsz sz] [dstaddr:]dstport}
    exit 1
}

proc bgerror {err} {
    global errorInfo
    puts stderr "bgerror: $err\n$errorInfo"
}

set ack     0
set rate    1024
set pktsz   512
set dstaddr 127.0.0.1
set dstport -1
set srcaddr 0

for {set i 0} {$i < [llength $argv]} {incr i} {
    set arg [lindex $argv $i]

    switch -- $arg {
	"-ack" {
	    set ack 1
	}
	
	"-rate" {
	    incr i
	    set rate [lindex $argv $i]
	}

	"-pktsz" {
	    incr i
	    set pktsz [lindex $argv $i]
	}

	"-srcaddr" {
	    incr i
	    set srcaddr [lindex $argv $i]
	}
	
	default {
	    set l [split $arg ":"]

	    if {[llength $l] == 1} {
		set dstport [lindex $l 0]
	    } elseif {[llength $l] == 2} {
		set dstaddr [lindex $l 0]
		set dstport [lindex $l 1]
	    } else {
		usage
	    }
	}
    }
}

if {$dstport == -1} {
    usage
}

puts stderr "send parameters:"
foreach var {rate pktsz dstaddr dstport} {
    puts stderr "    $var:\t[set $var]"
}

set packet ""
for {set i 0} {$i < $pktsz} {incr i} {
    append packet [expr $i % 10]
}

if {$srcaddr == 0} {
    set sock [socket $dstaddr $dstport]
} else {
    set sock [socket -myaddr $srcaddr $dstaddr $dstport]
}

fconfigure $sock -buffering none -encoding binary -translation binary

set delay [expr int(1000.0 / ((1.0 * $rate) / $pktsz))]

set fmtsz [expr $pktsz - 4] 

while {1} {
    set ts [clock clicks]
    set p [binary format ia$fmtsz $ts $packet]
    set len [string length $p]
    
    puts "sending $len byte packet"
    puts -nonewline $sock $p
    flush $sock

    if {$ack} {
	set x [read $sock 1]
	set now [clock clicks]
	puts "round trip time: [expr $now - $ts]"
    }
    after $delay
}
