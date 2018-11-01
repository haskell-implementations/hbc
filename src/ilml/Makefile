OBJS =  cast.o topprt.o cexpr.o state.o mkprt.o compilek.o compile.o idsubst.o icomp.o Glue.o iabs.o imisc.o mdep.o iwhatis.o iload.o toplev.o econv.o imain.o Main.o

MSRC = state.m cexpr.m mdep.m econv.m cast.m iabs.m icomp.m imisc.m toplev.m idsubst.m compile.m iload.m mkprt.m topprt.m imain.m iwhatis.m Main.m Glue.m compilek.m

MOBJS = cno.o cons.o 
COBJS = predefs.o cvecwrap.o
EOBJS =	../main/version.o ../misc/*.o ../expr/*.o ../transform/*.o ../rename/*.o ../ExprE/*.o ../type/*.o ../zf/*.o ../curry/*.o
IOBJS = $(MOBJS) $(OBJS) $(COBJS) $(EOBJS) $(EXTRAOBJS)

# Destination for the front end, i.e. the visible program
DSTBIN = /usr/local/bin
DST = /usr/local/lib/lmlc

H12	= yes
IFH12	= if [ "$(H12)" = "yes" ]

MAKE = make
CPP = sh ../bin/mycpp
ED = ex
LIB = -ltermcap
LIBINDEX = ../lib
LMLC = lmlc
ASM = sh ../bin/asm
SYMLN = sh ../bin/symln
MTARGET=xxxxx
CC = sh ../bin/mycc
CINTER = ../runtime/cinter - -ZC -
#CFLAGS = -g -DDEBUG ${SYSTEM}
CFLAGS = -O ${SYSTEM}
LINKFLAGS =
.SUFFIXES : .m .o .M .ci .t .tt

.ci.M :
	$(CINTER) < $*.ci > $*.M

.M.o :
	$(ASM) $<
	cp -p $*.tt $*.t

.tt.t :
	cp -p $*.tt $*.t

.m.o :
	${LMLC} -c ${LMLFLAGS} $<

all:	$(IOBJS) hbi

hbi: hbi.in
	sh ../bin/edlib hbi.in hbi

ilmlcomp:	$(IOBJS)
	${LMLC} ${LINKFLAGS} ${LMLFLAGS} ${IOBJS} $(LIB) -o ilmlcomp

callmettab.o:	callmettab.m
	$(LMLC) -Z $(LMLFLAGS) -c callmettab.m

#compilek.o:	compilek.m
#	$(LMLC) -t $(LMLFLAGS) -c compilek.m
#
cast.o:	cast.m
	$(LMLC) ${LMLFLAGS} -c cast.m
	$(ED) - cast.t <cast.ed

topprt.o:	topprt.m topprt.ed
	$(LMLC) ${LMLFLAGS} -c topprt.m
	$(ED) - topprt.t <topprt.ed

compile.o:	compile.m
	$(LMLC) ${LMLFLAGS} -z -c compile.m

#	$(LMLC) ${LMLFLAGS} -z -c -t compile.m
#	echo 'import compile:Cexpr->Univ {# ARITY compile = 1 #};' > compile.t

predefs.o:	predefs.pre pre1.sed pre2.sed pre.ed $(LIBINDEX)/lib.index addlibs.csh
	$(CPP) -P -D$(MTARGET) predefs.pre | sed -e '/;/d' -e '/^$$/d' > predefs.tmp
	./addlibs.csh $(LIBINDEX) >>predefs.tmp
	sed -f pre1.sed predefs.tmp >predefs.c
	sed -f pre2.sed predefs.tmp >>predefs.c
	$(CC) -S predefs.c
	$(ED) - predefs.s < pre.ed
	$(CC) -c predefs.s

#rm predefs.tmp predefs.c predefs.s

cno.o:	cno.M
	$(ASM) cno.M
	cp -p cno.tt cno.t

Bigops.o:	Bigops.m
	$(LMLC) $(LMLFLAGS) -c -Z Bigops.m
	-$(SYMLN) ../ilml/Bigops.o ../lib/Bigops.o
	cd ../lib; sh ../bin/buildshlib $(DST)/lib Bigops.so Bigops.o

Ishow.o:	Ishow.m
	$(LMLC) $(LMLFLAGS) -c Ishow.m
	-$(SYMLN) ../ilml/Ishow.o ../lib/Ishow.o
	cd ../lib; sh ../bin/buildshlib $(DST)/lib Ishow.so Ishow.o

Ishow.p:	Ishow.m makeishowp
	$(LMLC) $(LMLFLAGS) -c Ishow.m
	./makeishowp < Ishow.t > Ishow.p
	-$(SYMLN) ../ilml/Ishow.p ../lib/Ishow.p

clean:
	-rm -f *.o ilmlcomp predefs.tmp predefs.c predefs.s
	-rm -f saveo*/*

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .

countlines:
	wc ${MSRC}

count:
	$(MAKE) MFLAGS=countlines all

hprels:
	$(IFH12); then cd ../hlib; $(MAKE) DST=$(DST) Hprel.o; fi
	cd ../hlib1.3; $(MAKE) DST=$(DST) Hprel.o

INST = Ishow.o Ishow.p Bigops.o OK.o Option.o

install:	install_lml

haskell: hprels $(INST)

install_lml:	$(INST) hbi
	cp $(INST) $(DST)/lib
	sh ../bin/ccp ../lib/Ishow.so $(DST)/lib
	sh ../bin/ccp ../lib/Bigops.so $(DST)/lib
	cp ../lib/ihelp $(DST)/lib
	cp hbi lmli $(DSTBIN)

install_haskell: haskell 
	$(IFH12); then cd ../hlib; $(MAKE) DST=$(DST) install_Hprel; fi
	cd ../hlib1.3; $(MAKE) DST=$(DST) install_Hprel

noinstall:
	@echo ''

depend:
	sh ../bin/makedepend $(MSRC)

# These should not be necessary

loadmodule.o: loadmodule.M
dynlib.o: dynlib.M

# DO NOT DELETE THIS LINE
# DEPENDENCIES MUST END AT END OF FILE
state.o: ../expr/id.t
cexpr.o: ../misc/misc.t ../expr/id_t.t ../expr/types_t.t ../expr/impexp_t.t ../expr/id.t ../ExprE/Expr_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/ttype.t cast.t cons.t cno.t id.h
mdep.o: ../misc/misc.t ../expr/id_t.t ../expr/types_t.t ../expr/id.t ../ExprE/Expr_t.t ../expr/constr_t.t ../expr/constrfun.t ../expr/ttype.t ../misc/misc.t cast.t cons.t cno.t id.h cexpr.t imisc.t ../funnos.h
econv.o: ../misc/misc.t ../expr/id_t.t ../expr/types_t.t ../expr/id.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../expr/constr_t.t ../expr/constrfun.t ../expr/ttype.t ../expr/impexp_t.t ../expr/impexp.t ../transform/hexpr.t cast.t cons.t cno.t id.h cexpr.t mdep.t ../funnos.h
iabs.o: ../expr/id_t.t ../expr/id.t cexpr.t imisc.t cast.t id.h
icomp.o: ../misc/triple.t ../misc/flags.t ../misc/misc.t ../expr/error.t ../expr/constr_t.t ../expr/constrfun.t ../expr/types_t.t ../expr/id_t.t ../expr/id.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/impexp_t.t ../expr/impexp.t ../expr/pprint.t ../expr/einfo_t.t ../rename/renamedef.t ../rename/renameutil.t ../rename/renenv.t ../rename/import.t ../rename/deriv.t ../rename/buildinsts.t ../rename/rename.t ../transform/match.t ../transform/constr.t ../transform/lettrans.t ../transform/misc.t ../transform/exprs.t ../transform/remsign.t ../transform/remclass.t ../transform/remlazy.t ../strict/strictcon.t ../ExprE/Expr_t.t ../ExprE/Ecnv.t ../ExprE/Eprint.t ../ExprE/apconv.t ../ExprE/unrec.t ../ExprE/Eutil.t ../ExprE/classtrans.t ../ExprE/predef.t ../type/tchk.t ../curry/curry0.t ../zf/lmlzf.t ../zf/remzf.t cexpr.t iabs.t id.h imisc.t econv.t ../funnos.h
imisc.o: ../misc/flags.t ../expr/read.t ../expr/pprint.t ../expr/id_t.t ../expr/id.t ../expr/impexp_t.t ../expr/impexp.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/einfo_t.t ../ExprE/Expr_t.t ../ExprE/Eprint.t ../rename/renenv.t ../rename/renameutil.t ../rename/buildinsts.t ../rename/buildclass.t ../transform/remsign.t ../transform/hexpr.t cast.t cexpr.t icomp.t iabs.t idsubst.t compile.t state.t
toplev.o: ../misc/flags.t ../misc/ioc.t ../misc/misc.t ../misc/util.t ../lib/dialog.t ../expr/read.t ../expr/id_t.t ../expr/id.t ../expr/pprint.t ../expr/ttype_t.t ../expr/ttype.t ../expr/types_t.t ../rename/renenv.t ../rename/multi.t ../rename/importlib.t ../ExprE/predef.t ../ExprE/classtrans.t cexpr.t iload.t imisc.t iwhatis.t icomp.t compile.t state.t idsubst.t topprt.t mkprt.t
idsubst.o: cexpr.t ../expr/id_t.t ../expr/id.t ../misc/util.t ../rename/renenv.t imisc.t state.t dynlib.t
compile.o: cexpr.t imisc.t cons.t compilek.t
iload.o: cexpr.t ../expr/ttype.t ../expr/id_t.t ../expr/id.t ../expr/types_t.t ../expr/impexp_t.t ../rename/import.t ../rename/renenv.t ../rename/buildinsts.t ../rename/deriv.t ../rename/renameutil.t ../misc/flags.t imisc.t loadmodule.t dynlib.t state.t
mkprt.o: ../expr/types_t.t ../expr/impexp_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/id_t.t ../expr/id.t ../expr/constr_t.t ../rename/renenv.t ../transform/hexpr.t cexpr.t state.t
topprt.o: ../lib/dialog.t ../misc/ioc.t
imain.o: ../misc/flags.t ../misc/ioc.t ../main/version.t toplev.t ../rename/renenv.t icomp.t imisc.t
iwhatis.o: ../misc/flags.t ../misc/misc.t ../expr/read.t ../expr/pprint.t ../expr/id_t.t ../expr/id.t ../expr/impexp_t.t ../expr/impexp.t ../expr/types_t.t ../expr/ttype_t.t ../expr/ttype.t ../expr/tinfo.t ../expr/constrfun.t ../expr/einfo_t.t ../ExprE/Eprint.t ../rename/renenv.t imisc.t state.t
Main.o: imain.t
Glue.o: icomp.t
compilek.o: cexpr.t cons.t imisc.t
# DEPENDENCIES MUST END AT END OF FILE
# IF YOU PUT STUFF HERE IT WILL GO AWAY
