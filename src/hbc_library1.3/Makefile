DST = /usr/local/lib/lmlc

XOBJS=	trace.o
OBJS=	Sort.o  PackedString.o  IntMap.o  IntSet.o  Hash.o  RAList.o  Duplicates.o  ListSet.o  ListMap.o  Scans.o  \
	ListOps.o  Subsequences.o  NonStdTrace.o  LazyArray.o  \
	RandomHBC.o  NameSupply.o  ListUtil.o  Pretty.o  UnsafeDirty.o  Word.o  Parse.o  Native.o  SimpleLex.o \
	OrdMap.o OrdSet.o Terminal.o Queue.o Dequeue.o State.o IntegerMisc.o DialogueIO.o ContinuationIO.o \
	UnsafePerformIO.o \
	HO.o Natural.o Printf.o TimeIt.o MutArray.o Graph.o GenericList.o IOUtil.o FileStat.o \
	ByteVector.o  IOMutVar.o SelectIO.o  CCall.o  HandleException.o  \
	Interrupt.o  MonadUtil.o Number.o  Sleep.o  IOMisc.o  Trace.o \
	Identity.o  CatList.o  PriorityQueue.o \
	Nat__8.o Nat__16.o Nat__32.o Nat__64.o Nat.o \
	Signed__8.o Signed__16.o Signed__32.o Signed__64.o Signed.o \
	Bit.o
HIS=	Sort.hi PackedString.hi IntMap.hi IntSet.hi Hash.hi RAList.hi Duplicates.hi ListSet.hi ListMap.hi Scans.hi \
	ListOps.hi Subsequences.hi NonStdTrace.hi LazyArray.hi \
	RandomHBC.hi NameSupply.hi ListUtil.hi Pretty.hi UnsafeDirty.hi Word.hi Parse.hi Native.hi SimpleLex.hi LMLbyteops.hi \
	OrdMap.hi OrdSet.hi Terminal.hi Queue.hi Dequeue.hi State.hi IntegerMisc.hi DialogueIO.hi ContinuationIO.hi \
	UnsafePerformIO.hi \
	HO.hi Natural.hi Printf.hi TimeIt.hi MutArray.hi Graph.hi GenericList.hi IOUtil.hi FileStat.hi \
	ByteVector.hi IOMutVar.hi SelectIO.hi CCall.hi HandleException.hi \
	Interrupt.hi MonadUtil.hi Number.hi Sleep.hi IOMisc.hi Trace.hi \
	Identity.hi CatList.hi PriorityQueue.hi Nat.hi Signed.hi Bit.hi


ZAP= -fno-zap-redex -fcaf-call
LMLC = lmlc
FAR =
XLMLFLAGS = -Z $(LMLPROF) $(ZAP) $(LMLFLAGS) $(FAR)
HBC = hbc -1.3
XHBCFLAGS = $(HBCPROF) -H25M -z -fno-monad-compr -finst-with-c-t -i../hlib1.3 -O -no-pedantic -cpp -fno-syn-expand -fallow-redef $(ZAP) $(HBCFLAGS) $(FAR)
ASM = sh ../bin/asm
XASMFLAGS = $(ASMPROF) $(ASMFLAGS)
XCFLAGS= $(CPROF) $(CFLAGS)
HBC_LIBRARY = hbc_library1.3
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

all:	noprof doc

noprof:	lib.a lib.so
timeprof: lib_p.a
heapprof: lib_ph.a

ByteVector.o:	CCall.o
FileStat.o:	Word.o
Graph.o:	State.o MutArray.o
IOUtil.o:	UnsafePerformIO.o
IntSet.o:	Sort.o
MutArray.o:	State.o
Terminal.o: Terminal.hs CCall.o
	${HBC} -c ${XHBCFLAGS} -fccall Terminal.hs
TimeIt.o:	Printf.o
UnsafeDirty.o	: UnsafeDirty.has
	${HBC} -z -c -feval-inst ${XHBCFLAGS} UnsafeDirty.has

Bit.o:	Bit.hs Nat__8.o Nat__16.o Nat__32.o Nat__64.o bit.h
Nat.o:	Nat.hs Nat__8.o Nat__16.o Nat__32.o Nat__64.o
Nat__8.o:	Nat__8.hs bit.h
Nat__16.o:	Nat__16.hs bit.h
Nat__32.o:	Nat__32.hs bit.h
Nat__64.o:	Nat__64.hs bit.h
Signed.o:	Signed.hs Signed__8.o Signed__16.o Signed__32.o Signed__64.o
Signed__8.o:	Signed__8.hs bit.h
Signed__16.o:	Signed__16.hs bit.h
Signed__32.o:	Signed__32.hs bit.h
Signed__64.o:	Signed__64.hs bit.h

doc:	../doc/hbc_library.html

../doc/hbc_library.html:	makedoc ${HIS}
	sh makedoc ${HIS} > ../doc/hbc_library.html

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

install:
	$(MKDIR) ${DST}/${HBC_LIBRARY}
	cp ${OBJS} ${HIS} lib*.a ${DST}/${HBC_LIBRARY}
	sh ../bin/ccp lib.so ${DST}/${HBC_LIBRARY}
	$(RANLIB) ${DST}/${HBC_LIBRARY}/lib*.a

clean:
	-rm -f *.o *.a *.so
	-rm hprofo/*
	-rm profo/*

links:
	-ln -s ../hbc_library/LMLbitops.hi .
	-ln -s ../hbc_library/LMLbyteops.hi .
	-ln -s ../hbc_library/LMLgensym.hi .
	-ln -s ../hbc_library/LMLfmtf.hi .
	-ln -s ../hbc_library/LMLuid.hi .
	-ln -s ../hbc_library/Trace.M trace.M
	-ln -s ../hlib1.3/LMLunsafe.hi .
