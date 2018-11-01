OBJS =  misc.o mtrans.o misc2.o machine.o

MSRC =	machine.m misc.m mtrans.m misc2.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY: all

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

enscript:
	CAT $(TSRC) $(MSRC) | /usr/lic/enscript -2r

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

machine.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t mtrans.t misc.t misc2.t
misc.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t
mtrans.o:  ../mcode/mcodedef_t.t ../mcode/mutil1.t misc.t
misc2.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t mtrans.t misc.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
