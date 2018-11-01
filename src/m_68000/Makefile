OBJS =  machine.o

MSRC =	machine.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY:all

.m.o:
	${LMLC} -c ${LMLFLAGS} $<

all :	${OBJS}

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

clean:
	-rm -f *.o

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

machine.o:  ../mcode/mprint.t ../misc/qstring.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
