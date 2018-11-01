OBJS =  mtrans1.o mtrans2.o machine.o

MSRC =	machine.m mtrans1.m mtrans2.m

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
	makedepend ${MSRC}

# DO NOT DELETE THIS LINE

machine.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t mtrans1.t mtrans2.t tmp.h
mtrans1.o:  ../mcode/mcodedef_t.t tmp.h
mtrans2.o:  ../mcode/mcodedef_t.t tmp.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
# DO NOT DELETE THIS LINE -- make depend depends on it.

machine.o: ../mcode/mcodedef_t.t ../mcode/mprint.t ../misc/flags.t
machine.o: ../misc/qstring.t mtrans1.t mtrans2.t regno.h reg.h
mtrans1.o: ../mcode/mcodedef_t.t ../mcode/mutil1.t ../misc/flags.t regno.h
mtrans1.o: reg.h
mtrans2.o: ../mcode/mcodedef_t.t ../mcode/mutil1.t ../misc/flags.t regno.h
mtrans2.o: reg.h machine.t ../runtime/tagtable.h
