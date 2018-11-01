OBJS =  multi.o imptuples.o kind.o fun.o renenv.o importlib.o buildinsts.o classutil.o renameutil.o deriv.o buildclass.o himport.o import.o rename.o renamedef.o renametype.o renameclass.o

MSRC =  renenv.m renameclass.m renamedef.m renameutil.m renametype.m rename.m \
	fun.m import.m importlib.m imptuples.m multi.m himport.m deriv.m \
	buildclass.m buildinsts.m classutil.m kind.m

DQDSTLIB=
#DQDSTLIB=-DQSTDLIB=\"/usr/local/lib/lmlc\"
LMLC = lmlc
.SUFFIXES :
.SUFFIXES : .m .o .t
.PHONY : all

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	${OBJS}

importlib.o:	importlib.m
	$(LMLC) $(LMLFLAGS) $(DQDSTLIB) -c importlib.m

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

renenv.o: ../misc/misc.t ../misc/util.t ../expr/id_t.t ../expr/id.t ../expr/ttype.t
renameclass.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/einfo.t ../expr/types_t.t ../expr/id.t ../expr/ttype.t ../expr/booltree.t ../misc/util.t ../misc/misc.t ../misc/oset.t ../misc/flags.t ../transform/misc.t ../transform/hexpr.t ../type/conutil.t renameutil.t renenv.t renametype.t renamedef.t classutil.t
renamedef.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/ttype.t ../expr/subst.t ../expr/id.t ../expr/pprint.t ../expr/pragma_t.t ../transform/misc.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../misc/sort.t ../transform/lettrans.t ../transform/failcase.t ../transform/hexpr.t renenv.t renametype.t fun.t rename.t renameutil.t renameclass.t
renameutil.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/einfo.t ../expr/types_t.t ../expr/id.t ../expr/ttype.t ../expr/error.t ../expr/impexp_t.t ../expr/impexp.t ../expr/tinfo.t ../expr/pprint.t ../expr/pragma_t.t ../expr/booltree.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../misc/sort.t renenv.t ../transform/misc.t ../main/topexprs.t
renametype.o: ../misc/triple.t ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/ttype.t ../expr/id.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../transform/lettrans.t ../transform/hexpr.t renamedef.t renameutil.t renenv.t classutil.t
rename.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/einfo.t ../expr/tinfo.t ../expr/types_t.t ../expr/impexp_t.t ../expr/idtab.t ../expr/impexp.t ../expr/ttype.t ../expr/pprint.t ../expr/constrfun.t ../expr/pragma_t.t ../misc/flags.t ../misc/misc.t ../misc/util.t ../transform/cutil.t ../transform/misc.t ../transform/lettrans.t ../transform/hexpr.t import.t renenv.t renamedef.t renameutil.t renametype.t buildclass.t buildinsts.t classutil.t deriv.t kind.t ../main/topexprs.t
fun.o: ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/id.t ../expr/pprint.t ../transform/misc.t
import.o: ../misc/triple.t ../expr/impexp_t.t ../expr/impexp.t ../expr/types_t.t ../expr/id_t.t ../expr/constr_t.t ../expr/id.t ../expr/ttype_t.t ../expr/ttype.t ../expr/pprint.t ../misc/misc.t ../misc/util.t ../misc/flags.t ../misc/htable.t ../misc/oset.t ../misc/sort.t imptuples.t renenv.t importlib.t himport.t renameutil.t
importlib.o: ../misc/flags.t ../expr/id_t.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/impexp_t.t ../expr/read.t ../expr/types_t.t imptuples.t
imptuples.o: ../misc/triple.t ../expr/id_t.t ../expr/id.t ../expr/ttype.t ../expr/ttype_t.t ../expr/impexp_t.t ../expr/types_t.t ../misc/flags.t
multi.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/einfo_t.t ../expr/types_t.t ../expr/ttype.t ../expr/pprint.t ../expr/impexp_t.t ../expr/booltree.t ../funnos.h ../transform/misc.t ../misc/flags.t
himport.o: ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/pprint.t ../expr/impexp_t.t ../expr/impexp.t ../expr/einfo_t.t ../expr/constr_t.t ../misc/misc.t ../misc/flags.t ../expr/tinfo.t ../expr/einfo.t renameutil.t renametype.t renenv.t renameclass.t classutil.t
deriv.o: ../misc/triple.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/id_t.t ../expr/id.t ../expr/pprint.t ../expr/booltree.t ../expr/einfo_t.t ../expr/einfo.t ../transform/hexpr.t ../transform/genderiv.t ../transform/lettrans.t ../misc/misc.t ../misc/flags.t ../type/conutil.t renenv.t renameutil.t ../misc/util.t
buildclass.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/types_t.t ../misc/util.t renenv.t classutil.t
buildinsts.o: ../misc/flags.t ../expr/id_t.t ../expr/id.t ../expr/ttype_t.t ../expr/ttype.t ../misc/sort.t ../misc/util.t ../misc/flags.t renenv.t
classutil.o: ../misc/flags.t ../expr/types_t.t ../expr/constr_t.t ../expr/id_t.t ../expr/id.t ../expr/einfo_t.t ../expr/idtab.t ../expr/ttype_t.t ../expr/ttype.t ../expr/impexp_t.t ../expr/tinfo.t ../expr/pprint.t ../misc/util.t ../misc/misc.t ../type/conutil.t renenv.t
kind.o: ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/types_t.t ../expr/einfo_t.t ../expr/pprint.t ../misc/triple.t ../misc/misc.t ../misc/util.t ../misc/flags.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
