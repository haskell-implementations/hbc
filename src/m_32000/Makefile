OBJS =  mtrans.o machine.o

MSRC =	machine.m mtrans.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY:	all

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

clean:
	-rm -f *.o

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

machine.o:  ../misc/flags.t ../mcode/mcodedef_t.t ../mcode/mprint.t mtrans.t
mtrans.o:  ../mcode/mcodedef_t.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
