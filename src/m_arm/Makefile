OBJS =  mtransj.o mtrans3.o mtrans1.o mtrans2.o machine.o
OBJS_S =   mtrans1.s mtransj.s mtrans2.s mtrans3.s machine.s

MSRC =	machine.m mtrans1.m mtransj.m mtrans2.m mtrans3.m

LMLC = lmlc
LMLC_CROSS = lmlc_arm

.SUFFIXES :
.SUFFIXES : .m .o .t .s
.PHONY: all

.m.o:
	${LMLC} -c -DJOIN ${LMLFLAGS} $<
.m.s:
	${LMLC_CROSS} -S -DJOIN ${LMLFLAGS} $<

all :	${OBJS}

m_armS.zoo: 	${OBJS_S}
	toarm ${OBJS_S}
	zoo a m_armS.zoo s/*

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
unsave:
	mv saveo/*.o .
osave:
	mv *.o saveo

clean:
	-rm -f *.o *.s

enscript:
	CAT $(TSRC) $(MSRC) | /usr/lic/enscript -2r

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

machine.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t ../misc/flags.t mtrans1.t mtransj.t mtrans2.t mtrans3.t tmp.h
mtrans1.o:  ../mcode/mcodedef_t.t ../mcode/mutil1.t tmp.h
mtransj.o:  ../mcode/mcodedef_t.t ../mcode/mprint.t tmp.h
mtrans2.o:  ../mcode/mcodedef_t.t ../mcode/mutil1.t tmp.h
mtrans3.o:  ../mcode/mcodedef_t.t tmp.h
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
