OBJS =  lettrans.o cutil.o hexpr.o misc.o remsign.o constr.o exprs.o failcase.o caseutil.o genderiv.o remclass.o casep.o case.o remlazy.o match.o

MSRC =  misc.m lettrans.m caseutil.m case.m match.m constr.m \
	failcase.m cutil.m casep.m exprs.m remsign.m remclass.m remlazy.m \
	hexpr.m genderiv.m

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

misc.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/einfo_t.t ../expr/types_t.t ../expr/id.t ../misc/util.t hexpr.t
lettrans.o: ../misc/flags.t ../expr/types_t.t ../expr/id.t ../expr/pprint.t ../misc/misc.t ../misc/util.t
caseutil.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/constrfun.t ../expr/pprint.t ../rename/renenv.t ../misc/util.t ../misc/flags.t misc.t ../expr/subst.t cutil.t failcase.t exprs.t hexpr.t
case.o: ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/constr_t.t ../expr/subst.t ../expr/error.t ../expr/pprint.t ../expr/ttype.t ../misc/misc.t ../misc/util.t ../misc/sort.t misc.t failcase.t caseutil.t casep.t exprs.t
match.o: ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/pprint.t ../expr/id.t ../expr/id_t.t ../expr/subst.t ../expr/einfo_t.t ../expr/booltree.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../rename/multi.t ../funnos.h hexpr.t misc.t case.t lettrans.t cutil.t
constr.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/id.t ../expr/ttype.t ../expr/tinfo.t ../expr/constrfun.t ../rename/renameutil.t ../rename/renametype.t ../misc/misc.t cutil.t misc.t
failcase.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/constrfun.t ../expr/ttype.t ../expr/subst.t ../expr/pprint.t hexpr.t misc.t ../expr/tinfo.t
cutil.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/id.t ../expr/ttype.t ../expr/tinfo.t ../expr/error.t ../expr/constrfun.t ../misc/util.t ../misc/flags.t ../rename/renameutil.t hexpr.t misc.t
casep.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/pprint.t failcase.t
exprs.o: ../expr/id.t ../expr/constr_t.t ../expr/types_t.t ../expr/ttype.t ../expr/tinfo.t hexpr.t cutil.t
remsign.o: ../misc/triple.t ../expr/id.t ../expr/id_t.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/einfo_t.t ../expr/pprint.t ../rename/renameutil.t ../misc/util.t ../misc/misc.t ../misc/flags.t ../type/conutil.t lettrans.t hexpr.t misc.t
remclass.o: ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/einfo_t.t ../expr/pprint.t ../expr/constr_t.t ../expr/constrfun.t ../rename/renameutil.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../misc/sort.t ../type/conutil.t ../ExprE/classtrans.t hexpr.t lettrans.t failcase.t cutil.t misc.t ../funnos.h
remlazy.o: ../expr/types_t.t ../expr/pprint.t ../expr/id_t.t ../expr/id.t ../expr/constrfun.t ../expr/einfo_t.t ../expr/ttype_t.t ../expr/ttype.t ../misc/misc.t ../misc/flags.t ../rename/multi.t lettrans.t misc.t case.t hexpr.t exprs.t
hexpr.o: ../misc/flags.t ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../rename/renenv.t ../rename/import.t ../main/topexprs.t cutil.t
genderiv.o: ../misc/triple.t ../expr/types_t.t ../expr/id.t ../expr/id_t.t ../expr/ttype.t ../expr/ttype_t.t ../expr/pprint.t ../expr/einfo_t.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../rename/renameutil.t ../rename/deriv.t ../rename/rename.t lettrans.t hexpr.t exprs.t cutil.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
