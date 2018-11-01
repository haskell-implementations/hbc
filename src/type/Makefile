OBJS =  subst_t.o prefix.o conutil.o unify.o subst.o check.o hcheck.o tchk.o
MSRC = subst_t.m subst.m prefix.m check.m unify.m tchk.m hcheck.m conutil.m

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

subst.o: ../expr/id_t.t ../expr/constr_t.t ../expr/id.t ../expr/ttype_t.t ../expr/ttype.t ../ExprE/Expr_t.t ../misc/flags.t ../misc/util.t subst_t.t unify.t conutil.t
prefix.o: ../expr/id.t ../expr/id_t.t ../expr/ttype_t.t ../expr/ttype.t ../misc/util.t subst.t
check.o: ../expr/id_t.t ../expr/constr_t.t ../expr/einfo_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/id.t ../expr/constrfun.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/Egetid.t ../ExprE/unrec.t ../transform/misc.t ../misc/misc.t ../misc/flags.t subst_t.t prefix.t subst.t unify.t
unify.o: ../expr/ttype_t.t ../expr/ttype.t ../expr/id_t.t ../expr/id.t ../misc/flags.t subst_t.t subst.t conutil.t
tchk.o: ../expr/ttype_t.t ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/impexp_t.t ../expr/error.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/addspec.t ../ExprE/addrestr.t ../transform/misc.t ../misc/flags.t ../misc/misc.t subst_t.t check.t hcheck.t prefix.t unify.t subst.t
hcheck.o: ../expr/id_t.t ../expr/constr_t.t ../expr/einfo_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/id.t ../expr/idtab.t ../expr/constrfun.t ../expr/pragma_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/Egetid.t ../ExprE/unrec.t ../transform/misc.t ../transform/hexpr.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../main/topexprs.t subst_t.t prefix.t subst.t unify.t conutil.t check.t
conutil.o: ../expr/id_t.t ../expr/id.t ../expr/idtab.t ../expr/ttype_t.t ../expr/ttype.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/Eutil.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../misc/sort.t ../transform/hexpr.t subst_t.t subst.t prefix.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
