OBJS =  triple.o htable.o oset.o qstring.o util.o sort.o text_t.o ioc.o misc.o flags.o Tflat.o pri.o setofid.o
MSRC = text_t.m setofid.m pri.m Tflat.m flags.m misc.m util.m sort.m qstring.m oset.m htable.m triple.m ioc.m

LMLC = lmlc
.SUFFIXES:
.SUFFIXES: .m .o .t
.PHONY : all

.m.o:
	${LMLC} -c ${LMLFLAGS} $*.m

all:	${OBJS}

clean:
	-rm -f *.o
	-rm -f saveo*/*

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

setofid.o: ../expr/id_t.t
pri.o: text_t.t
Tflat.o: text_t.t
flags.o: ../expr/id.t
misc.o: sort.t
ioc.o: ../lib/dialog.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
