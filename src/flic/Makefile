OBJS =  pptype.o location.o fesc.o fconvname.o Flic_t.o ppadd.o parse.o fread.o fconv.o fconstr.o ppparse.o pp.o fprint.o ftrans.o flic.o

MSRC =  Flic_t.m fconstr.m fconv.m fconvname.m fesc.m flic.m fprint.m \
	fread.m ftrans.m location.m parse.m pp.m ppadd.m ppparse.m pptype.m

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

fconstr.o: ../ExprE/Expr_t.t ../ExprE/Eutil.t ../expr/constr_t.t ../expr/constrfun.t ../expr/tinfo.t ../expr/id_t.t ../expr/id.t
fconv.o: ../ExprE/Expr_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/id.t ../expr/einfo_t.t Flic_t.t fconvname.t
flic.o: fread.t ftrans.t fconv.t fprint.t
fprint.o: Flic_t.t fesc.t pp.t
fread.o: Flic_t.t
ftrans.o: Flic_t.t fprint.t ../expr/id_t.t ../expr/types_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/id.t ../expr/impexp_t.t ../expr/constr_t.t ../transform/misc.t
parse.o: location.t
pp.o: pptype.t ppparse.t parse.t location.t
ppadd.o: pptype.t
ppparse.o: parse.t pptype.t ppadd.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
