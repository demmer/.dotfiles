#!/usr/bin/python

import os, sys, re
from os.path import join, getsize

        
def rmpath(top):
    for root, dirs, files in os.walk(top, topdown=False):
        for name in files:
            os.remove(join(root, name))
        for name in dirs:
            os.rmdir(join(root, name))

pat1 = re.compile('/\*.*?\*/', re.M | re.S)
pat2 = re.compile('//.*')

def strip_comments(dir):
    for root, dirs, files in os.walk(dir):
        tmpdir = join(tmp, root)
        os.mkdir(tmpdir)

        for name in files:
            print 'Stripping comments from', name
            name = join(root,name)
            f = file(name)
            data = f.read()
            data = pat1.sub("", data)
            data = pat2.sub("", data)
            f = file(join(tmp,name), 'w')
            f.write(data)

src = os.getcwd()
tmp = "/tmp/diffsrc"
rmpath(tmp)
if (os.access(tmp, os.F_OK) != 1):
    os.mkdir(tmp)

if (len(sys.argv) != 3):
    print "usage: diff <old> <new>"
    sys.exit(1)
    
old = sys.argv[1]
new = sys.argv[2]

print src, tmp, new, old
strip_comments(new)
strip_comments(old)

os.system('diff -N -r %s %s | grep ">" | egrep -v "^>[ ]*$"'  % (join(tmp,old), join(tmp,new)))

