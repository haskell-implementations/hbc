OBJS =  Expr_t.o Egetid.o prexport.o eqtrans.o Eutil.o Eprint.o Ecnv.o globalize.o forcearity.o predef.o hprexport.o addrestr.o unrec.o apconv.o Esubst.o remunused.o addspec.o classtrans.o

MSRC =  Expr_t.m Ecnv.m Eprint.m Esubst.m Eutil.m \
	apconv.m eqtrans.m prexport.m Egetid.m \
	unrec.m addrestr.m hprexport.m predef.m classtrans.m \
	addspec.m remunused.m forcearity.m globalize.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY: all

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

Ecnv.o: ../expr/id.t ../expr/ttype.t ../expr/pprint.t ../expr/types_t.t ../expr/einfo_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/id_t.t ../transform/exprs.t ../transform/hexpr.t Expr_t.t
Eprint.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/einfo.t ../expr/id.t ../expr/ttype.t ../expr/constrfun.t Expr_t.t ../transform/misc.t ../misc/misc.t ../misc/flags.t ../expr/impexp.t
Esubst.o: ../expr/id.t Expr_t.t ../misc/misc.t ../expr/einfo_t.t ../misc/util.t Eprint.t
Eutil.o: ../expr/constr_t.t ../expr/tinfo.t ../expr/ttype.t Expr_t.t
apconv.o: ../misc/misc.t ../expr/einfo_t.t ../expr/id_t.t ../expr/id.t Expr_t.t Eprint.t
eqtrans.o: ../expr/id_t.t ../expr/id.t ../expr/ttype_t.t ../expr/types_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/booltree.t ../expr/constr_t.t ../expr/einfo_t.t ../type/subst_t.t ../type/subst.t ../type/unify.t Expr_t.t ../funnos.h ../misc/misc.t ../misc/util.t ../misc/flags.t ../transform/hexpr.t
prexport.o: Expr_t.t ../expr/types_t.t ../expr/ttype_t.t ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/einfo.t ../expr/tinfo.t ../expr/pprint.t ../misc/flags.t ../expr/impexp_t.t ../expr/impexp.t
Egetid.o: Expr_t.t ../expr/einfo_t.t
unrec.o: ../expr/id.t Expr_t.t Egetid.t ../misc/misc.t ../misc/util.t
addrestr.o: ../expr/id_t.t ../expr/id.t ../expr/ttype_t.t ../expr/ttype.t ../expr/constr_t.t ../expr/einfo_t.t ../transform/hexpr.t Expr_t.t ../misc/misc.t ../misc/flags.t Eprint.t
hprexport.o: ../misc/triple.t Expr_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/ttype_t.t ../expr/constr_t.t ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/constrfun.t Eprint.t ../expr/einfo.t ../expr/pragma_t.t ../misc/flags.t ../misc/util.t ../misc/misc.t ../expr/impexp_t.t ../expr/impexp.t ../expr/tinfo.t ../rename/renameutil.t ../rename/renenv.t ../main/topexprs.t ../type/subst.t
predef.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/constr_t.t ../expr/ttype.t ../expr/booltree.t ../ExprE/Expr_t.t ../transform/hexpr.t ../misc/util.t ../misc/misc.t ../misc/flags.t ../rename/renenv.t ../rename/renameutil.t ../main/topexprs.t ../funnos.h ../expr/constrfun.t ../transform/exprs.t Eutil.t
classtrans.o: ../expr/id.t ../expr/id_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/constrfun.t ../expr/einfo_t.t ../expr/booltree.t ../misc/flags.t ../misc/misc.t ../rename/renenv.t ../rename/renameutil.t ../main/topexprs.t ../transform/hexpr.t ../transform/cutil.t predef.t Expr_t.t Eutil.t Eprint.t ../funnos.h
addspec.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/pragma_t.t ../expr/ttype_t.t ../expr/ttype.t ../misc/misc.t ../misc/util.t ../misc/sort.t ../misc/flags.t ../rename/renameutil.t ../type/subst.t Egetid.t Esubst.t Expr_t.t Eprint.t
remunused.o: ../expr/id.t ../expr/id_t.t ../expr/impexp.t ../expr/pragma_t.t ../misc/misc.t ../misc/util.t ../main/topexprs.t Egetid.t Expr_t.t Eprint.t unrec.t
forcearity.o: ../expr/id.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../expr/einfo_t.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../simpl/simpl.t ../transform/hexpr.t Egetid.t
globalize.o: ../misc/misc.t ../misc/util.t ../misc/flags.t ../expr/einfo_t.t ../expr/id_t.t ../expr/id.t Expr_t.t Eprint.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
