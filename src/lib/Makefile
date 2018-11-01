OBJ=	hd.o tl.o null.o fst.o snd.o \
	maxint.o And.o Or.o Maxl.o Minl.o Sum.o Times.o \
	choplist.o combine.o concmap.o count.o filter.o \
	itlist.o itos.o itosf.o length.o map.o mem.o rev.o \
	reverse.o revitlist.o \
	member.o sublist.o random.o randlist.o \
	leftadj.o rightadj.o space.o split.o splitat.o stoi.o stoil.o take.o \
	assoc.o assocdef.o assocfun.o for.o isalnum.o isalpha.o iscntrl.o \
	isdigit.o islower.o isprint.o ispunct.o isspace.o isupper.o \
	select.o until.o while.o transpose.o \
	abs.o from.o intersect.o difference.o union.o \
	lex.o rotate.o rept.o automat.o odd.o \
	conc.o group.o mapstate.o \
	head.o tail.o index.o mix.o \
	takeword.o translit.o listrec.o reduce.o sort.o tsort.o \
	show.o scan.o last.o all.o exists.o getenv.o not.o \
	anysame.o allsame.o map2.o mapfst.o mapsnd.o partition.o \
	mkset.o subsets.o permutations.o nil.o \
	curry.o uncurry.o \
	force.o argv.o envp.o progname.o \
	trace.o trace_c.o openfile.o opendir.o statfile.o catch.o throw.o getwd_c.o getwd.o \
	uname.o uname_c.o strerror.o strerror_c.o \
	termcap.o termcap_c.o termcap_M.o tofile.o \
	failexit_M.o hiatonic.o delay.o max.o min.o seq.o Pfrom.o \
	ArrayM.o Array_m.o \
	dialog_M.o dialog_c.o dialogfile.o dialog.o bin.o \
	Integer.o integerfunc.o stoiI.o Iitos.o floatfunc.o \
        bignum.o amp.o amp_string.o amp_powm.o amp_gcd.o amp_sqrt.o amp_and.o amp_ior.o \
        mpn_add.o mpn_div.o mpn_mul.o mpn_rshiftci.o mpn_cmp.o mpn_lshift.o mpn_rshift.o mpn_sub.o mp_bases.o mp_clz_tab.o alloca.o \
	nameSupply.o gensym.o cbyteops.o byteops.o byteopsbad.o uid2str.o getpw.o \
	Option.o OK.o \
	unsafe.o
FOBJ =	stof.o stof_c.o \
	ftos.o ftos_c.o \
	ftosf.o ftosf_c.o \
	func.o func_c.o fmtf.o fmtf_c.o \
	pi.o decode_M.o encode_M.o fltcode.o

STUBOBJ = readchan.o xstub2.o

MRUN = mrun.o
AOBJ = $(OBJ) $(FOBJ) ../runtime/$(MRUN)
ALLOBJS = ${AOBJ} ${OBJPRE} Pconc.o
INCLUDES = include/*
INCLUDE = lib/include
LIBS = lib.index prelude.p lib*.a
SHLIBS = lib*.so
LIB = lib
DST = /usr/local/lib/lmlc
LMLC = lmlc
ASM = sh ../bin/asm
RANLIB = sh ../bin/myranlib
BUILDSHLIB = sh ../bin/buildshlib
SYMLN = sh ../bin/symln
ADDINDEX = sh ./addindex
CINTER = ../bin/cinter - -ZC -
RM = rm
CC = sh ../bin/mycc
CFLAGS = -O
XCFLAGS = $(CPROF) $(CFLAGS) $(SYSTEM)
XASMFLAGS = $(ASMPROF) $(ASMFLAGS)
IFLAGS = -fno-zap-redex -fcaf-call
ZAP= -fpbu
XLMLFLAGS = $(LMLPROF) $(LMLFLAGS) $(ZAP) -O -fno-case-tag-optim
XLMLFLAGS = $(LMLPROF) $(LMLFLAGS) $(ZAP) -O
MKDIR = sh ../bin/cmkdir
AR = ar
MAPFILE = mapfile

# the predefined functions:
OBJPRE =	Psel.o Ppre.o fail.o Pselb.o

.SUFFIXES:
.SUFFIXES:	.o .m .c .ci .M

.ci.o:
	$(CINTER) < $*.ci > $*.M || (rm $*.M && exit 1)
	$(ASM) $(XASMFLAGS) $*.M && $(ADDINDEX) < $*.t

.ci.M:
	$(CINTER) < $*.ci > $*.M || (rm $*.M && exit 1)

.M.o:
	$(ASM) $(XASMFLAGS) $*.M && $(ADDINDEX) < $*.t

.m.o:
	$(LMLC) $(XLMLFLAGS) -c $*.m && $(ADDINDEX) < $*.t

.c.o:
	$(CC) $(XCFLAGS) -c $*.c

noprof:	lib.a lib.so lib_i.a
timeprof: lib_p.a
heapprof: lib_ph.a

lib.so:	${ALLOBJS}
	$(BUILDSHLIB) $(DST)/$(LIB) lib.so ${ALLOBJS}

lib.a: 	${OBJPRE} ${AOBJ} FILE.t Pfail.t $(STUBOBJ)
	$(ADDINDEX) <FILE.t
	$(ADDINDEX) -Z <Pfail.t
	$(ADDINDEX) -Z <Pforce.t
	rm -f lib.a
	$(AR) cr lib.a ${ALLOBJS}
	rm -f libstub.a
	$(AR) cr libstub.a ${STUBOBJ}
	$(RANLIB) lib.a libstub.a

Descriptions:	Header_test
	(echo "LML Library Functions:   `date`"; echo) > Descriptions
	Header_test *.m >> Descriptions

lib_p.a:	${OBJPRE} ${AOBJ}
	$(MKDIR) normo profo
	mv -f *.o lib.a libstub.a normo
	@touch profo/DUMMY.o
	mv -f profo/*.o .
	make LMLPROF=-p ASMPROF=-DPROFILE CPROF=-p lib.a
	mv lib.a lib_p.a
	mv libstub.a libstub_p.a
	mv *.o profo
	mv normo/* .

lib_ia: $(ALLOBJS)
	$(AR) cr lib_i.a ${ALLOBJS}
	$(RANLIB) lib_i.a
	$(BUILDSHLIB) $(DST)/$(LIB) lib_i.so $(ALLOBJS)

lib_i.a: $(ALLOBJS)
	$(MKDIR) normo prelo
	mv -f *.o normo
	-mv -f prelo/*.o .
	make LMLPROF='$(IFLAGS)' lib_ia
	mv *.o prelo
	mv normo/* .

lib_ph.a: lib.a ../runtime/mrun_ph.o
	$(MKDIR) normo hprofo
	mv -f *.o lib.a lib.so libstub.a normo
	@touch hprofo/DUMMY.o
	mv -f hprofo/*.o .
	-make LMLPROF=-ph ASMPROF=-DHPROFILE CPROF=-DHPROFILE MRUN=mrun_ph.o lib.a lib.so
	mv lib.a lib_ph.a
	-mv lib.so lib_ph.so
	mv libstub.a libstub_ph.a
	mv *.o hprofo
	mv normo/* .

install:
	$(MKDIR) $(DST)/$(LIB) $(DST)/$(INCLUDE) $(DST)/$(LIB)/parsed
	cp $(LIBS) $(DST)/$(LIB)
	-cp $(SHLIBS) $(DST)/$(LIB)
	-cp $(INCLUDES) $(DST)/$(INCLUDE)
	cp parsed/* $(DST)/$(LIB)/parsed
	$(RANLIB) $(DST)/$(LIB)/lib*.a
	sh ../bin/ccp $(MAPFILE) $(DST)/$(LIB)

countlines:
	wc *.m

#predef
Ppre.o:	Ppre.m Ptype Pconc.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pconc.m && $(LMLC) $(XLMLFLAGS) -c -Z Ppre.m && ( sed -e 's/import .\([^a-z]*\):/import (\1):/' Ppre.t Pconc.t ; cat Ptype ) | ../syntax/lmlp -Z -p > prelude.p

Psel.o:	Psel.m
	$(LMLC) $(XLMLFLAGS) -c -Z Psel.m && $(ADDINDEX) -Z <Psel.t
argv.o:	argv.m
	$(LMLC) $(XLMLFLAGS) -c -Z argv.m && $(ADDINDEX) -Z <argv.t
progname.o:	progname.m
	$(LMLC) $(XLMLFLAGS) -c -Z progname.m && $(ADDINDEX) -Z <progname.t
envp.o:	envp.m
	$(LMLC) $(XLMLFLAGS) -c -Z envp.m && $(ADDINDEX) -Z <envp.t
force.o:	force.m
	$(LMLC) $(XLMLFLAGS) -c -Z force.m && $(ADDINDEX) -Z <force.t
Pforce.o:	Pforce.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pforce.m && $(ADDINDEX) -Z <Pforce.t
concargs.o:	concargs.m
	$(LMLC) $(XLMLFLAGS) -c -Z concargs.m
Pselb.o:	Pselb.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pselb.m
bin.o:	bin.m
	$(LMLC) $(XLMLFLAGS) -c bin.m && $(ADDINDEX) <bint.t
ArrayM.o:	ArrayM.M
	$(ASM) $(XASMFLAGS) ArrayM.M && $(ADDINDEX) -Z <ArrayM.t
Array_m.o:	Array_m.m
	$(LMLC) $(XLMLFLAGS) -c -Z Array_m.m && $(ADDINDEX) -Z <Array_m.t
Integer.o:	Integer.M
	$(ASM) $(XASMFLAGS) Integer.M && $(ADDINDEX) -Z <Integer.t
integerfunc.o:	integerfunc.m
	$(LMLC) $(XLMLFLAGS) -c -Z integerfunc.m && $(ADDINDEX) -Z <integerfunc.t
dialog.o:	dialog.m
	$(LMLC) $(XLMLFLAGS) -c dialog.m && grep ' std' dialog.t | $(ADDINDEX)
stubmain.o:	stubmain.m
	$(LMLC) $(XLMLFLAGS) -c stubmain.m

#Float.o:	Float.M
#	$(ASM) $(XASMFLAGS) Float.M && $(ADDINDEX) -Z <Float.t
fmtf.o:	fmtf.M
	$(ASM) $(XASMFLAGS) fmtf.M && $(ADDINDEX) -Z <fmtf.t
ftoi.o:	ftoi.M
	$(ASM) $(XASMFLAGS) ftoi.M && $(ADDINDEX) -Z <ftoi.t
ftos.o:	ftos.M
	$(ASM) $(XASMFLAGS) ftos.M && $(ADDINDEX) -Z <ftos.t
ftosf.o:	ftosf.M
	$(ASM) $(XASMFLAGS) ftosf.M && $(ADDINDEX) -Z <ftosf.t
func.o:	func.M
	$(ASM) $(XASMFLAGS) func.M && $(ADDINDEX) -Z <func.t
itof.o:	itof.M
	$(ASM) $(XASMFLAGS) itof.M && $(ADDINDEX) -Z <itof.t
stof.o:	stof.M
	$(ASM) $(XASMFLAGS) stof.M && $(ADDINDEX) -Z <stof.t
floatfunc.o:	floatfunc.m
	$(LMLC) $(XLMLFLAGS) -c -Z floatfunc.m && $(ADDINDEX) -Z <floatfunc.t
encode_M.o:	encode_M.M
	$(ASM) $(XASMFLAGS) encode_M.M && $(ADDINDEX) -Z <encode_M.t
decode_M.o:	decode_M.M
	$(ASM) $(XASMFLAGS) decode_M.M && $(ADDINDEX) -Z <decode_M.t

seq.o:	seq.m
	$(LMLC) $(XLMLFLAGS) -c -Z seq.m && $(ADDINDEX) -Z <seq.t
fail.o:	fail.m
	$(LMLC) $(XLMLFLAGS) -c -Z fail.m && $(ADDINDEX) -Z <fail.t
Pfrom.o:	Pfrom.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pfrom.m && $(ADDINDEX) -Z <Pfrom.t
catch.o:	catch.m
	$(LMLC) $(XLMLFLAGS) -c -Z catch.m && $(ADDINDEX) -Z <catch.t
throw.o:	throw.m
	$(LMLC) $(XLMLFLAGS) -c -Z throw.m && $(ADDINDEX) -Z <throw.t
#
bignum.o:	amp.h

#
# library functions interdependencies
difference.o intersect.o:	filter.o
group.o:			choplist.o take.o
index.o:			head.o length.o
isalpha.o:			islower.o isupper.o
isalnum.o:			isalpha.o isdigit.o
ispunct.o:			isprint.o isalnum.o
iscntrl.o:			isprint.o
space.o:			rept.o
leftadj.o rightadj.o:		rept.o length.o
itosf.o:			itos.o rightadj.o
randlist.o:			abs.o
stoi.o:				revitlist.o take.o isdigit.o
stoil.o:			isdigit.o
take.o:				reverse.o
transpose.o:			map.o Or.o
union.o:			difference.o
show.o:				itos.o mix.o map.o concmap.o
takeword.o:			isalpha.o isalnum.o isdigit.o take.o
translit.o:			assocdef.o
Maxl.o Minl.o:			revitlist.o maxint.o
sort.o:				automat.o
scan.o:				takeword.o take.o mem.o isdigit.o stoi.o isspace.o
subset.o:			map.o
permutations.o:			map.o concmap.o

Header_test:	Header_test.l
		lex Header_test.l
		$(CC) -O lex.yy.c -o Header_test
		rm lex.yy.c

clean:
	-rm -f *.o *.a Header_test so_locations *.so
	-rm -f saveo*/*
	-rm -f normo/*
	-rm -f profo/* 
	-rm -f prelo/* 
	-rm -f hprofo/* 

oosave:
	mv -f *.o lib.a saveoo
	mv saveoo/runtime* .
	mv *.o saveoo
	-$(MKDIR) saveoo_prelo
	mv prelo/* saveoo_prelo
unosave:
	mv saveoo/* .
	mv saveoo_prelo/* prelo
osave:
	mv -f *.o lib.a saveo
	mv saveo/runtime* .
	mv *.o saveo
	-$(MKDIR) saveo_prelo
	mv prelo/* saveo_prelo
unsave:
	mv saveo/* .
	mv saveo_prelo/* prelo
depend:
