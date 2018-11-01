OBJS =  curry0.o
MSRC = curry0.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .
clean:
	-rm -f *.o
	-rm -f saveo*/*

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend $(MSRC)
# DO NOT DELETE THIS LINE

curry0.o: ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/pragma_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/ttype.t ../transform/misc.t ../syntax/listgen.h ../misc/flags.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
