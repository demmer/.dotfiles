SRCS	:= $(shell ls *.el)
OBJS 	:= $(patsubst %.el,%.elc,$(SRCS))

all:	$(OBJS)

%.elc: %.el
	rm -f $@
	emacs -batch -l make-settings.elnc -f batch-byte-compile $^

clean:
	rm -f *.elc
