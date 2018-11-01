OBJS =  mopt.o
MSRC = mopt.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

clean:
	-rm -f *.o
	-rm -f saveo*/*

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
unsave:
	mv saveo/*.o .
osave:
	mv *.o saveo

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

mopt.o: ../mcode/mcodedef_t.t ../mcode/mutil1.t ../mcode/mprint.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
