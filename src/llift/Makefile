OBJS =  chkind.o addarity.o para.o Bconv.o lliftutil.o addclose.o llift.o
MSRC = addclose.m lliftutil.m llift.m Bconv.m para.m addarity.m chkind.m

LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

print:
	@pr -l72 READ_ME Makefile addclose.m llift.m

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

addclose.o: ../expr/einfo_t.t ../expr/id.t ../expr/id_t.t ../ExprE/Expr_t.t ../misc/flags.t lliftutil.t
lliftutil.o: ../expr/id_t.t ../expr/id.t ../ExprE/Expr_t.t ../misc/setofid.t ../misc/util.t
llift.o: ../misc/misc.t ../expr/id_t.t ../expr/id.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../ExprE/unrec.t lliftutil.t ../misc/setofid.t ../misc/util.t
Bconv.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../misc/misc.t ../misc/flags.t ../funnos.h para.t
para.o: ../expr/id_t.t ../expr/einfo_t.t ../ExprE/Expr_t.t
addarity.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/impexp.t ../ExprE/Expr_t.t ../misc/misc.t ../expr/booltree.t
chkind.o: ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../misc/misc.t ../misc/setofid.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
