
all:
	rm -f *.elc
	emacs -batch -l make-settings.elnc -f batch-byte-compile *.el

%.elc: %.el
	rm -f $@
	emacs -batch -l make-settings.elnc -f batch-byte-compile $<

clean:
	rm -f *.elc
