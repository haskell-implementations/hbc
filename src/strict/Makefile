OBJS =  strictcon.o mkstrict.o usage.o strict.o calcstrict.o
MSRC = strict.m mkstrict.m strictcon.m calcstrict.m usage.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

clean:
	-rm -f *.o
	-rm -f saveo*/*

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

depend:
	sh ../bin/makedepend $(MSRC)
# DO NOT DELETE THIS LINE

strict.o: ../expr/id_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/booltree.t ../ExprE/Expr_t.t ../expr/einfo_t.t ../expr/einfo.t ../expr/id.t ../expr/impexp_t.t ../misc/setofid.t ../misc/misc.t ../misc/flags.t ../funnos.h ../ExprE/Eprint.t ../transform/hexpr.t ../misc/util.t usage.t
mkstrict.o: ../misc/flags.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../ExprE/Eutil.t ../misc/misc.t ../expr/id.t ../expr/constrfun.t ../transform/hexpr.t
strictcon.o: ../expr/id_t.t ../expr/constr_t.t ../expr/constrfun.t ../ExprE/Expr_t.t ../ExprE/Esubst.t ../expr/einfo_t.t ../expr/id.t ../expr/tinfo.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../ExprE/Eprint.t ../simpl/sutil.t
calcstrict.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/booltree.t ../expr/constrfun.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/Egetid.t ../misc/util.t ../misc/flags.t ../transform/hexpr.t strict.t
usage.o: ../expr/id_t.t ../expr/constr_t.t ../expr/impexp_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../expr/einfo_t.t ../expr/einfo.t ../expr/booltree.t ../expr/id.t ../misc/flags.t ../misc/util.t ../misc/misc.t ../misc/sort.t strict.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
