#
# Simple tcp proxy
#

proc usage {} {
    puts stderr "Usage: proxy sport1:server1:dport1 \[sport2:server2:dport2\] ..."
    exit 1
}

proc bgerror {err} {
    global errorInfo
    puts stderr "bgerror: $err\n$errorInfo"
}

proc config_socket s {
    fconfigure $s -blocking 0 -buffering none -encoding binary -translation binary
}

proc readable {input} {
    global sockmap
    set output $sockmap($input)

    if [eof $input] {
	flush $output
	close $input
	close $output
	return
    }
    
    set buf [read $input]
    puts -nonewline $output $buf
    flush $output
}

proc accept {channel addr port} {
    global portmap sockmap
    
    set server [lindex $portmap($port) 0]
    set dport  [lindex $portmap($port) 1]

    puts "New channel $addr:$fport <-> $server:$dport"
    set clientsock $channel
    set serversock [socket $server $dport]

    set sockmap($clientsock) $serversock
    set sockmap($serversock) $clientsock

    config_socket $clientsock
    config_socket $serversock

    fileevent $clientsock readable "readable $clientsock"
    fileevent $serversock readable "readable $serversock"
}

if { [llength $argv] < 1 } {
    usage
}

set addr [lindex $argv 0]
puts stderr "proxy on $addr:"

foreach map [lrange $argv 1 end] {
    if ![regexp {^(.*):(.*):(.*)$} $map match sport server dport] {
	puts stderr "$map not a valid sport:server:dport specification"
	usage
    }

    puts stderr "\t$sport -> $server:$dport"
    set portmap($sport) "$server $dport"
    socket -server "accept $sport" -myaddr $addr $sport
}

vwait forever
