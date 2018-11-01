DST = /usr/local/lib/lmlc

COBJS = gcgen.o gcgen2.o gcslide.o gc.o error.o file.o gcstat.o dump.o print.o main.o node.o gcdb.o sample.o meminit.o traceback.o tenure.o
MOBJS = garb.o gengarb.o gen2garb.o gen2Mgarb.o majorgarb.o gcsample.o jonkergarb.o
HFILES = gcd.h gc.h node.h funs.h vars.h runtime.h
TRCOBJS = traceM.o trace.o
NOTRCOBJS = notraceM.o notrace.o
OPTOBJS = opt.o
EXTRAOBJS = showheap.o noflags.o

ASM = sh ../bin/asm ${ASMPROF} ${ASMFLAGS}
JOINOBJS = sh ../bin/joinobj
CC = sh ../bin/mycc -g
CFLAGS = ${CPROF} ${SYSTEM}
MKDIR = sh ../bin/cmkdir
BUILDSHLIB = sh ../bin/buildshlib

#.SUFFIXES:
#.SUFFIXES: .M .o .c

noprof: all cinter$(EXE)
	cp runtime.*o evalupdunw.o indirunw.o trc.o notrc.o opt.o tagtable.o $(EXTRAOBJS) ../lib

timeprof: runtime_p.o
	cp runtime_p.o evalupdunw_p.o indirunw_p.o trc_p.o notrc_p.o $(EXTRAOBJS) ../lib

heapprof: runtime_ph.o runtime_ph.so mrun_ph.o
	cp runtime_ph.*o evalupdunw_ph.o indirunw_ph.o trc_ph.o notrc_ph.o $(EXTRAOBJS) ../lib


gc.o:		gc.c $(HFILES) ../mcode/limit.h
gcslide.o:	gcslide.c $(HFILES) ../mcode/limit.h
gcgen.o:	gcgen.c $(HFILES) ../mcode/limit.h
gcgen2.o:	gcgen2.c $(HFILES) ../mcode/limit.h
meminit.o:	meminit.c $(HFILES) ../mcode/limit.h
gcd.o:		gcd.c node.h gc.h gcd.h
error.o:	error.c $(HFILES)
file.o:		file.c $(HFILES)
gcstat.o:	gcstat.c $(HFILES)
dump.o:		dump.c $(HFILES)
print.o:	print.c $(HFILES)
main.o:		main.c $(HFILES) sample.h tagtable.h
node.o:		node.c $(HFILES) ../mcode/magic.h
tenure.o:	tenure.c $(HFILES)

#		$(OCC) $(CFLAGS) -c node.c
gcdb.o:		gcdb.c $(HFILES)
sample.o:	sample.c $(HFILES) sample.h ../mcode/magic.h
trace.o:	trace.c $(HFILES)

runtime.so:	runtime.o
	$(BUILDSHLIB) $(DST)/lib runtime.so runtime.o
runtime_ph.so:	runtime_ph.o
	$(BUILDSHLIB) $(DST)/lib runtime_ph.so runtime_ph.o

cinter$(EXE):	cinter.m
	lmlc -s $(STATICFLAG) -o cinter$(EXE) cinter.m
	cp cinter$(EXE) ../bin

all:	mrun.o runtime.o runtime.so tagtable.o evalupdunw.o indirunw.o trc.o notrc.o opt.o $(EXTRAOBJS)

runtime.o:	$(MOBJS) $(COBJS)
	$(JOINOBJS) runtime.o $(MOBJS) $(COBJS)

trc.o:	$(TRCOBJS)
	$(JOINOBJS) trc.o $(TRCOBJS)
notrc.o:	$(NOTRCOBJS)
	$(JOINOBJS) notrc.o $(NOTRCOBJS)

runtime_p.o:	$(MOBJS) $(COBJS) $(TRCOBJS) $(NOTRCOBJS)
	$(MKDIR) normo profo
	mv -f *.o normo
	@touch profo/DUMMY.o
	mv -f profo/*.o .
	make ASMPROF=-DPROFILE CPROF=-p SYSTEM=$(SYSTEM) all
	mv runtime.o runtimeo
	mv evalupdunw.o evalupdunwo
	mv indirunw.o indirunwo
	mv trc.o trco
	mv notrc.o notrco
	mv *.o profo
	mv normo/* .
	mv runtimeo runtime_p.o
	mv evalupdunwo evalupdunw_p.o
	mv indirunwo indirunw_p.o
	mv trco trc_p.o
	mv notrco notrc_p.o

runtime_ph.o:	$(MOBJS) $(COBJS) $(TRCOBJS) $(NOTRCOBJS)
	$(MKDIR) normo hprofo
	mv -f *.o normo
	@touch hprofo/DUMMY.o
	mv -f hprofo/*.o .
	make ASMPROF=-DHPROFILE CPROF=-DHPROFILE SYSTEM=$(SYSTEM) all
	mv runtime.o runtimeo
	mv evalupdunw.o evalupdunwo
	mv indirunw.o indirunwo
	mv trc.o trco
	mv notrc.o notrco
	mv *.o hprofo
	mv normo/* .
	mv runtimeo runtime_ph.o
	mv evalupdunwo evalupdunw_ph.o
	mv indirunwo indirunw_ph.o
	mv trco trc_ph.o
	mv notrco notrc_ph.o

install:
	cp runtime*.*o tagtable.o evalupdunw*.o indirunw*.o trc*.o notrc*.o opt.o $(EXTRAOBJS) $(DST)/lib

tagtable.o: tagtable.M machdep.M gc.h
	$(ASM) tagtable.M

mrun.o:	mrun.M machdep.M gc.h
	$(ASM) mrun.M

mrun_ph.o: mrun.M machdep.M gc.h
	-mv mrun.o mruno
	-make ASMPROF=-DHPROFILE mrun.o
	-mv mrun.o mrun_ph.o
	-mv mruno mrun.o

garb.o:	garb.M machdep.M gc.h
	$(ASM) garb.M

gcsample.o:	gcsample.M machdep.M gc.h
	$(ASM) gcsample.M

gengarb.o:	gengarb.M machdep.M gc.h
	$(ASM) gengarb.M

gen2garb.o:	gen2garb.M machdep.M gc.h
	$(ASM) gen2garb.M

gen2Mgarb.o:	gen2Mgarb.M machdep.M gc.h
	$(ASM) gen2Mgarb.M

majorgarb.o:	majorgarb.M machdep.M gc.h
	$(ASM) majorgarb.M

bakergarb.o:	bakergarb.M machdep.M gc.h
	$(ASM) bakergarb.M

jonkergarb.o:	jonkergarb.M machdep.M gc.h
	$(ASM) jonkergarb.M

evalupdunw.o: evalupdunw.M machdep.M
	$(ASM) evalupdunw.M

indirunw.o: indirunw.M machdep.M
	$(ASM) indirunw.M

traceM.o:	traceM.M machdep.M
	$(ASM) traceM.M

notraceM.o:	notraceM.M machdep.M
	$(ASM) notraceM.M

traceback.o:	traceback.c

clean:
	-rm -f *.o *.so cinter$(EXE)
	-rm -f saveo*/*
	-rm -f normo/*
	-rm -f profo/* 
	-rm -f hprofo/* 

oosave:
	mv *.o saveoo
unosave:
	mv saveoo/*.o .
osave:
	mv *.o saveo
unsave:
	mv saveo/*.o .

countlines:

depend:
