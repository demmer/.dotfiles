
set socket [socket -server accept 7600]

proc accept {chan host port} {
    fileevent $chan readable "readable $chan"
}

proc readable {chan} {
    set line ""
    if {[eof $chan] || [catch {gets $chan} line]} {
	close $chan
	return
    }
    
    puts "read line $line"
    
    if {$line == ""} { 
	return 
    }
    
    if {[string match {*http*} $line] == 0 && \
	    [string match {*:*} $line] == 0} {
	set line "http://$line"
    }

    set cmd "netscape -remote 'openUrl($line)'"
    puts "execing $cmd"
    if [catch {eval exec $cmd &} err] {
	puts "ERROR opening url: $err"
    }
}

proc bgerror {err} {
    puts "Error: $err"
}

vwait forever
