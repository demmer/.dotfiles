#!/usr/bin/tclsh

#
# Simple tcp receiver
#

proc usage {} {
    puts stderr {Usage: recv.tcl [addr:]port}
    exit 1
}

proc bgerror {err} {
    global errorInfo
    puts stderr "bgerror: $err\n$errorInfo"
}

proc readable {input} {
    if [eof $input] {
	close $input
	return
    }

    set buf [read $input 10000]
    set len [string length $buf]
    puts "got $len byte packet"
}

proc accept {sock addr port} {
    puts "Got channel $sock from $addr:$port"

    fconfigure $sock -buffering none -encoding binary -translation binary \
	    -blocking 0

    fileevent $sock readable "readable $sock"
}

set addr 0
set port -1

for {set i 0} {$i < [llength $argv]} {incr i} {
    set arg [lindex $argv $i]

    switch -- $arg {
	default {
	    set l [split $arg ":"]
	    if {[llength $l] == 1} {
		set port [lindex $arg 0]
	    } elseif {[llength $l] == 2} {
		set addr [lindex $arg 0]
		set port [lindex $arg 1]
	    } else {
		usage
	    }
	}
    }
}

if {$port == -1} {
    usage
}

puts stderr "recv parameters:"
foreach var {addr port} {
    puts stderr "    $var:\t[set $var]"
}

if {$addr == 0} {
    set sock [socket -server "accept" $port]
} else {
    set sock [socket -server "accept" -myaddr $addr $port]
}

vwait forever

