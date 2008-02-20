#!/usr/bin/python

import os;
import sys;
import re;
from select import select;
import time;
import string;

debug = 0

sys.argv.pop(0);

cmd = "tcpdump -n -ttt -l -v ";

printbytes = 0;
printkilo  = 0;

for a in sys.argv:
    if (a == "-B"):
	printbytes = 1;
    elif (a == "-sudo"):
	cmd = "sudo " + cmd;
    elif (a == "-k"):
	printkilo = 1;
    else:
        cmd += a + " ";

print "Cmd is: "+cmd;

inp, outp, errp = os.popen3(cmd);

exp = re.compile('(\d+) IP \(.*length: (\d+)\)');
rate = 0.0;
totalbits = 0;

t0 = time.time();
tlast = t0;

if (printbytes):
    bps = "Bps";
    convert = 1.0;
else:
    bps = "bps";
    convert = 8.0;

if (printkilo):
    bps = "K" + bps;
    convert /= 1024.0;

while True:
    tnow = time.time();
    timeout = 2.0 - (tnow - tlast);

    if (timeout <= 0.0):
        newrate = totalbits / 2.0;

        rate = (rate * 0.1) + (newrate * 0.9)
       
        print "%f %s" % (rate, bps)

        tlast = tnow;
        timeout = 2.0;
        totalbits = 0;
        
    readyrd, readywd, readyerr = select([outp], [], [outp], timeout)

    if (readyrd == []):
        # timeout kicked, adjust on next loop
        continue

    if (readyerr != []):
        print errp.readline();
        sys.exit(1)

    fd = readyrd[0];
    line = fd.readline();
    
    m = exp.match(line);
    if (m == None):
        continue
    
    tdiff = m.group(1);
    bytes = m.group(2);

    if (debug):
        print "dbg: tdiff %s bytes %s" % (tdiff, bytes)

    totalbits += string.atoi(bytes) * convert;
    
# If we got here, there was a problem with tcpdump
for line in err:
    print line;

