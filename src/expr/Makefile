OBJS =  booltree.o pragma_t.o constr_t.o types_t.o ttype_t.o id_t.o einfo_t.o impexp_t.o exprid.o annot.o impexp.o constrfun.o einfo.o ttype.o tinfo.o id.o pprint.o eq.o idtab.o subst.o error.o read.o

MSRC =  impexp_t.m einfo_t.m id_t.m ttype_t.m types_t.m \
	pprint.m id.m read.m ttype.m tinfo.m constrfun.m einfo.m \
	error.m subst.m constr_t.m annot.m impexp.m idtab.m eq.m \
	booltree.m exprid.m pragma_t.m

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
unsave:
	mv saveo/*.o .
osave:
	mv *.o saveo

clean:
	-rm -f *.o
	-rm -f saveo*/*

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}

# DO NOT DELETE THIS LINE

pprint.o: id_t.t constr_t.t ttype_t.t einfo_t.t types_t.t ../transform/misc.t id.t ttype.t einfo.t ../misc/flags.t ../misc/misc.t constrfun.t ../syntax/listgen.h impexp.t impexp_t.t pragma_t.t
id.o: ../misc/triple.t ../misc/flags.t einfo_t.t einfo.t id_t.t ttype.t ttype_t.t ../funnos.h
read.o: id_t.t id.t ttype_t.t ttype.t types_t.t einfo_t.t annot.t impexp_t.t booltree.t einfo.t pragma_t.t ../misc/misc.t
ttype.o: ../funnos.h ../transform/misc.t ../transform/hexpr.t ../expr/types_t.t ../misc/flags.t ../misc/misc.t id.t id_t.t ttype_t.t constr_t.t
tinfo.o: types_t.t constr_t.t id_t.t ttype_t.t ttype.t ../transform/hexpr.t ../misc/flags.t
constrfun.o: ../misc/util.t types_t.t constr_t.t id_t.t id.t ttype_t.t ttype.t ../transform/hexpr.t
einfo.o: ../misc/flags.t id.t einfo_t.t ttype.t ttype_t.t pprint.t booltree.t
error.o: id_t.t id.t constr_t.t ttype_t.t ttype.t einfo_t.t types_t.t pprint.t pragma_t.t impexp_t.t ../transform/misc.t
subst.o: id.t types_t.t einfo_t.t pprint.t ../misc/util.t
annot.o: id_t.t ttype_t.t types_t.t einfo_t.t
impexp.o: ../misc/triple.t impexp_t.t id.t ttype_t.t ttype.t pprint.t types_t.t
idtab.o: id_t.t id.t
eq.o: ../misc/misc.t types_t.t ttype.t id.t constr_t.t
exprid.o: types_t.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
