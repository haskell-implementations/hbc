OBJS =  treewalk.o casetr.o sutil.o flow.o asimpl.o simpl.o mlet.o
MSRC = mlet.m simpl.m sutil.m casetr.m asimpl.m treewalk.m flow.m

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

mlet.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../misc/misc.t ../misc/util.t ../misc/flags.t sutil.t simpl.t
simpl.o: ../funnos.h ../expr/id_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/id.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../ExprE/Eutil.t ../ExprE/Eprint.t ../ExprE/Esubst.t ../ExprE/Egetid.t ../ExprE/apconv.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../transform/hexpr.t casetr.t asimpl.t mlet.t sutil.t
sutil.o: ../expr/constr_t.t ../expr/constrfun.t ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../misc/setofid.t ../misc/sort.t ../transform/hexpr.t
casetr.o: ../ExprE/Expr_t.t ../misc/misc.t
asimpl.o: ../funnos.h ../expr/id_t.t ../expr/constr_t.t ../expr/id.t ../expr/ttype.t ../ExprE/Expr_t.t ../ExprE/Eutil.t ../misc/misc.t ../misc/flags.t sutil.t ../ExprE/predef.t
treewalk.o: ../ExprE/Expr_t.t ../ExprE/Eprint.t ../misc/misc.t
flow.o: ../expr/id.t ../expr/constrfun.t ../ExprE/Expr_t.t ../ExprE/Esubst.t treewalk.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
