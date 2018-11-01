OBJS =  cunrec.o substear.o xsubst.o earley.o check.o tchk.o
MSRC =  xsubst.m check.m tchk.m substear.m earley.m cunrec.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t

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

print:
	@pr -l72 Makefile check.m tchk.m unify.m

countlines:
	wc $(MSRC)

depend:
	sh ../bin/makedepend ${MSRC}
# DO NOT DELETE THIS LINE
subst.o:  ../expr/ttype_t.t ../expr/ttype.t ../misc/flags.t subst_t.t unify.t conutil.t
xsubst.o: ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/id.t ../misc/flags.t ../type/subst_t.t ../type/subst.t ../type/unify.t
check.o: ../expr/constr_t.t ../expr/einfo_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/types_t.t ../expr/eq.t ../expr/id.t ../expr/id_t.t ../expr/pprint.t ../expr/constrfun.t ../expr/exprid.t ../transform/misc.t ../type/subst_t.t ../type/prefix.t ../type/subst.t ../misc/misc.t xsubst.t ../type/unify.t earley.t
tchk.o: ../expr/ttype_t.t ../expr/id_t.t ../expr/id.t ../expr/impexp_t.t ../expr/ttype.t ../expr/types_t.t ../expr/error.t ../transform/misc.t ../misc/flags.t ../misc/misc.t ../type/subst_t.t check.t ../type/prefix.t ../type/unify.t ../type/subst.t xsubst.t
substear.o: ../expr/types_t.t
earley.o: ../expr/id_t.t ../expr/constr_t.t ../expr/einfo_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/types_t.t ../expr/eq.t ../expr/id.t ../expr/pprint.t ../expr/constrfun.t ../expr/tinfo.t ../transform/misc.t ../transform/hexpr.t ../type/subst_t.t ../type/prefix.t ../type/subst.t ../misc/misc.t ../transform/cutil.t xsubst.t ../type/unify.t substear.t check.t
cunrec.o: ../expr/types_t.t ../expr/id.t ../expr/exprid.t ../misc/misc.t ../misc/util.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
