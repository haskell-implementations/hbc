OBJS =  bwm2.o bwm1.o bwm4.o bwm3.o bwm6.o bwm5.o bwm.o

MSRC =  bwm.m bwm1.m bwm2.m bwm3.m bwm4.m bwm5.m bwm6.m

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

bwm.o: bwm1.t bwm2.t bwm3.t bwm4.t bwm5.t bwm6.t
bwm1.o: ../misc/misc.t ../misc/util.t ../misc/flags.t ../expr/constrfun.t ../expr/id_t.t ../expr/constr_t.t ../expr/id.t ../expr/ttype.t ../expr/einfo_t.t ../expr/booltree.t ../ExprE/Expr_t.t ../ExprE/apconv.t ../ExprE/Eprint.t ../transform/hexpr.t ../transform/exprs.t ../funnos.h
bwm2.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/constrfun.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../misc/util.t
bwm3.o: ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/constr_t.t ../misc/util.t ../misc/misc.t ../expr/constrfun.t ../transform/hexpr.t ../funnos.h bwm2.t
bwm4.o: ../expr/id.t ../expr/constr_t.t ../expr/constrfun.t ../funnos.h bwm2.t
bwm5.o: bwm3.t bwm4.t
bwm6.o: ../expr/id_t.t ../expr/id.t ../misc/util.t ../funnos.h bwm3.t bwm4.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
