DST = /usr/local/lib/lmlc

XOBJS=	
OBJS=	QSort.o  Random.o  NameSupply.o  Maybe.o  Option.o  ListUtil.o  Either.o  IO.o  Termcap.o  RunDialogue.o  Pretty.o  Trace.o  UnsafeDirty.o  Miranda.o  Printf.o  Word.o  Time.o  Hash.o  IntegerMisc.o  Parse.o  Native.o  Number.o  NumberTheory.o  LazyArray.o  IOMonad.o  SimpleLex.o  Seq.o  RAList.o  MSort.o  IntSet.o  IntMap.o
HIS=	QSort.hi Random.hi NameSupply.hi Maybe.hi Option.hi ListUtil.hi Either.hi IO.hi Termcap.hi RunDialogue.hi Pretty.hi Trace.hi UnsafeDirty.hi Miranda.hi Printf.hi Word.hi Time.hi Hash.hi IntegerMisc.hi Parse.hi Native.hi Number.hi NumberTheory.hi LazyArray.hi IOMonad.hi SimpleLex.hi Seq.hi RAList.hi LMLbyteops.hi MSort.hi IntSet.hi IntMap.hi


ZAP= -fno-zap-redex -fcaf-call
LMLC = lmlc
FAR =
XLMLFLAGS = -Z $(LMLPROF) $(ZAP) $(LMLFLAGS) $(FAR)
HBC = hbc -1.2
XHBCFLAGS = $(HBCPROF) -O -no-pedantic -cpp $(ZAP) $(HBCFLAGS) $(FAR)
ASM = sh ../bin/asm
XASMFLAGS = $(ASMPROF) $(ASMFLAGS)
XCFLAGS= $(CPROF) $(CFLAGS)
HBC_LIBRARY = hbc_library
MAKE = make
CINTER = ../bin/cinter - -ZC -
MKDIR = sh ../bin/cmkdir
CC = sh ../bin/mycc
RANLIB = sh ../bin/myranlib
RM = rm
BUILDSHLIB = sh ../bin/buildshlib
AR = ar

.SUFFIXES :
.SUFFIXES : .c .M .hs .m .o .has .ci .lhs

.m.o :
	${LMLC} -c ${XLMLFLAGS} $<

.hs.o :
	${HBC} -c ${XHBCFLAGS} $<

.lhs.o :
	${HBC} -c ${XHBCFLAGS} $<

.has.o :
	${HBC} -z -c ${XHBCFLAGS} $<

.ci.o:
	$(CINTER) < $*.ci > $*.M
	$(ASM) $(XASMFLAGS) $*.M
	$(RM) $*.M

.M.o :
	${ASM} ${XASMFLAGS} $<

.c.o :
	${CC} -c ${XCFLAGS} $<

noprof:	lib.a lib.so
timeprof: lib_p.a
heapprof: lib_ph.a

lib.a:	${OBJS} ${XOBJS}
	rm -f lib.a
	ar cr lib.a ${OBJS} ${XOBJS}
	$(RANLIB) lib.a

lib.so:	lib.a
	$(BUILDSHLIB) $(DST)/$(HBC_LIBRARY) lib.so ${OBJS} ${XOBJS}

lib_p.a:	${OBJS} ${XOBJS}
	$(MKDIR) normo profo
	mv -f *.o lib.a normo
	@touch profo/DUMMY.o
	mv -f profo/*.o .
	$(MAKE) HBCPROF=-p LMLPROF=-p ASMPROF=-DPROFILE CPROF=-p lib.a
	mv lib.a lib_p.a
	mv *.o profo
	mv normo/* .

lib_ph.a:	${OBJS} ${XOBJS}
	$(MKDIR) normo hprofo
	mv -f *.o lib.a lib.so normo
	@touch hprofo/DUMMY.o
	mv -f hprofo/*.o .
	$(MAKE) HBCPROF=-ph LMLPROF=-ph ASMPROF=-DHPROFILE CPROF=-DHPROFILE lib.a lib.so
	mv lib.a lib_ph.a
	-mv lib.so lib_ph.so
	mv *.o hprofo
	mv normo/* .

Pforce.o:	Pforce.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pforce.m

Parse.o:	Seq.o

install:
	$(MKDIR) ${DST}/${HBC_LIBRARY}
	cp ${OBJS} ${HIS} lib*.a doc ${DST}/${HBC_LIBRARY}
	sh ../bin/ccp lib.so ${DST}/${HBC_LIBRARY}
	$(RANLIB) ${DST}/${HBC_LIBRARY}/lib*.a

clean:
	-rm -f *.o *.a *.so
	-rm hprofo/*
	-rm profo/*
