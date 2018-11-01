OBJS =  remzf.o lmlzf.o
MSRC =  lmlzf.m remzf.m

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

lmlzf.o: ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/pprint.t ../syntax/listgen.h ../transform/misc.t
remzf.o: ../expr/types_t.t ../expr/id_t.t ../expr/id.t ../expr/pprint.t ../expr/tinfo.t ../expr/einfo_t.t ../expr/constrfun.t ../misc/misc.t ../misc/util.t ../transform/hexpr.t ../transform/exprs.t ../transform/misc.t ../transform/cutil.t ../transform/lettrans.t ../misc/flags.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
