HOBJ =  never.o error.o enum.o misc.o gencmp.o bin.o P_Bin.o \
	C_Eq.o C_Ord.o C_Num.o C_Real.o C_Integral.o C_Fractional.o \
	C_Floating.o C_RealFrac.o C_RealFloat.o C_Ix.o C_Enum.o C_Binary.o \
	C_Text.o \
	I_Eq_Char.o I_Ord_Char.o I_Ix_Char.o I_Enum_Char.o I_B_Char.o \
	I_Eq_Int.o I_Ord_Int.o I_Num_Int.o I_Real_Int.o I_I_Int.o \
	I_Ix_Int.o I_Enum_Int.o I_B_Int.o \
	I_Eq_Integer.o I_Ord_Integer.o I_Num_Integer.o I_R_Integer.o I_I_Integer.o \
	I_Ix_Integer.o I_E_Integer.o I_B_Integer.o \
	I_B_Float.o I_Fr_Float.o I_RFl_Float.o \
	I_Enum_Float.o I_RFr_Float.o I_Eq_Float.o \
	I_Num_Float.o I_Real_Float.o I_Fl_Float.o I_Ord_Float.o \
	I_B_Double.o I_Fr_Double.o I_RFl_Double.o \
	I_Enum_Double.o I_RFr_Double.o I_Eq_Double.o \
	I_Num_Double.o I_Real_Double.o I_Fl_Double.o I_Ord_Double.o \
	I_B_Bool.o I_Eq_Bool.o I_Ord_Bool.o I_Enum_Bool.o I_Ix_Bool.o \
	I_P2.o I_P3.o I_P4.o I_P5.o \
	I_List.o I_Unit.o \
	P_T_util.o \
	IT_P2.o IT_P3.o IT_P4.o IT_P5.o \
	IT_Bin.o IT_Bool.o IT_Char.o IT_File.o IT_List.o IT_Unit.o \
	IT_Int.o IT_Integer.o \
	IT_function.o IT_numbers.o \
	P_IO_data.o P_IO.o P_IOC.o \
	PL_all.o PL_and.o PL_any.o PL_break.o PL_conc.o PL_concat.o \
	PL_cycle.o PL_diff.o PL_drop.o PL_dropWhile.o PL_elem.o \
	PL_filter.o PL_foldl.o PL_foldl1.o PL_foldr.o PL_foldr1.o \
	PL_gLength.o PL_head.o PL_init.o PL_iterate.o PL_last.o \
	PL_length.o PL_lines.o PL_map.o PL_maximum.o PL_minimum.o \
	PL_notElem.o PL_nub.o PL_null.o PL_or.o PL_partition.o \
	PL_product.o PL_products.o PL_repeat.o PL_reverse.o PL_scanl.o \
	PL_scanl1.o PL_scanr.o PL_scanr1.o PL_span.o PL_splitAt.o \
	PL_sub.o PL_sum.o PL_sums.o PL_tail.o PL_take.o PL_takeWhile.o \
	PL_transpose.o PL_unlines.o PL_unwords.o PL_words.o PL_zip.o \
	PL_zip3.o PL_zip4.o PL_zip5.o PL_zip6.o PL_zip7.o PL_zipWith.o \
	PL_zipWith3.o PL_zipWith4.o PL_zipWith5.o PL_zipWith6.o PL_zipWith7.o \
	PL_unzip.o PL_unzip3.o PL_unzip4.o PL_unzip5.o PL_unzip6.o PL_unzip7.o \
	PT_lex.o PT_read.o PT_readParen.o PT_reads.o \
	PT_show.o PT_shows.o PT_showChar.o PT_showParen.o PT_showString.o PT_showType.o \
	PP_asTypeOf.o PP_atan2.o PP_compose.o \
	PP_fexp.o PP_flip.o PP_fIntegral.o \
	PP_fRealFrac.o PP_fst.o PP_gcd.o PP_isAlpha.o \
	PP_land.o PP_lcm.o PP_lor.o PP_maxInt.o \
	PP_minInt.o PP_nexp.o PP_not.o PP_otherwise.o \
	PP_snd.o PP_subtract.o PP_until.o \
        PP_ord.o PP_chr.o PP_minChar.o PP_maxChar.o PP_id.o PP_const.o PP_dollar.o \
	I_Complex.o I_Eq_Complex.o I_Num_Complex.o I_Fr_Complex.o I_Fl_Complex.o \
	PC_cis.o PC_conjugate.o PC_imagPart.o PC_magnitude.o \
	PC_mkPolar.o PC_phase.o PC_polar.o PC_realPart.o \
	P_extra.o RF_Spec.o \
	P_Array_data.o P_Array.o P_Array_extra.o \
	PA_accum.o PA_assocs.o PA_indices.o PA_accumArray.o PA_bang.o PA_ixmap.o PA_amap.o \
	PA_bounds.o PA_listArray.o PA_array.o PA_elems.o PA_update.o \
	P_Ratio.o PR_appRat.o \
	Xinst.o

EOBJ =	Main_main.o

OBJ =	$(HOBJ) $(EOBJ)

PREL = Prelude.p PreludeCore.p PreludeRT.p PreludeMini.p PreludeType.p

LIBS = ${PREL} PreludeEmpty.hi Prelude.hi PreludeCore.hi lib*.a 
SHLIBS = lib.so
LIB = hlib
DST = /usr/local/lib/lmlc
LMLC = lmlc
# Use -H6Mg if memory is tight.
#HBC = hbc -H6Mg
HBC = hbc -H25M
ASM = sh ../bin/asm
MYAS = sh ../bin/myas
RANLIB = sh ../bin/myranlib
HBCPROF=
LMLPROF=
CPROF=
IFLAGS = -fno-zap-redex -fcaf-call
ZAP= -fpbu -O
#ZAP= -fpbu -O -fno-case-tag-optim
XASMFLAGS = $(ASMFLAGS) $(ASMPROF)
HBCF = -1.2 $(HBCPROF) -z -cpp -fprelude -no-pedantic $(ZAP)
XHBCFLAGS = $(HBCF) $(HBCFLAGS)
XHBCFLAGSI = $(HBCF) -fno-toprec $(HBCFLAGS)
PARSEP = ../bin/curryp -z -p
PHI = PL*.hi P_IO.hi P_IOC.hi PT*.hi PP*.hi P_Array.hi PA_*.hi PC*.hi P_Ratio.hi PR_*.hi
PHIX = P_T_util.hi
PCHI = P_IO_data.hi
IPCHI = IT_Char.hi IT_Int.hi IT_Integer.hi \
	I_Enum_Bool.hi I_Ix_Bool.hi \
	I_Enum_Char.hi I_Ix_Char.hi \
	I_Enum_Int.hi I_Ix_Int.hi I_Num_Int.hi I_Real_Int.hi I_I_Int.hi \
	I_E_Integer.hi I_Ix_Integer.hi I_Num_Integer.hi I_R_Integer.hi I_I_Integer.hi \
	I_Enum_Float.hi I_Num_Float.hi I_Real_Float.hi I_Fr_Float.hi I_Fl_Float.hi I_RFr_Float.hi I_RFl_Float.hi \
	I_Enum_Double.hi I_Num_Double.hi I_Real_Double.hi I_Fr_Double.hi I_Fl_Double.hi I_RFr_Double.hi I_RFl_Double.hi \
	I_Num_Complex.hi I_Eq_Complex.hi I_Fr_Complex.hi I_Fl_Complex.hi

CC = sh ../bin/mycc
XCFLAGS = -O $(CPROF) $(CFLAGS)
XLMLFLAGS = $(ZAP) $(LMLPROF) $(LMLFLAGS)
CPP = sh ../bin/mycpp -C
MKDIR = sh ../bin/cmkdir
BUILDSHLIB = sh ../bin/buildshlib
MKIPREL = sh ../bin/mkiprel
AR = ar

.SUFFIXES:
.SUFFIXES:	.o .M .m .c .hs .has .p .hi

.M.o:
	$(ASM) $(XASMFLAGS) $*.M

.m.o:
	$(LMLC) $(XLMLFLAGS) -c -Z $*.m

.hs.o:
	$(HBC) $(XHBCFLAGS) -c $*.hs

.has.o:
	$(HBC) $(XHBCFLAGSI) -c $*.has

.c.o:
	$(CC) $(XCFLAGS) -c $*.c

.hi.p:
	$(CPP) $*.hi | $(PARSEP) > $*.p

noprof:	lib.a lib.so

timeprof: lib_p.a

heapprof: lib_ph.a


lib.a: 	${OBJ} ${PREL}
	rm -f lib.a
	ar cr lib.a ${OBJ}
	$(RANLIB) lib.a

lib.so:	lib.a
	$(BUILDSHLIB) $(DST)/$(LIB) lib.so ${OBJ}

lib_p.a:	${OBJ}
	$(MKDIR) normo profo
	-mv -f *.o lib.a normo
	@touch profo/DUMMY.o
	mv -f profo/*.o .
	make HBCPROF=-p LMLPROF=-p ASMPROF=-DPROFILE CPROF=-p lib.a
	mv lib.a lib_p.a
	mv *.o profo
	-mv normo/* .

lib_ph.a:	${OBJ}
	$(MKDIR) normo hprofo
	-mv -f *.o lib.a lib.so normo
	@touch hprofo/DUMMY.o
	mv -f hprofo/*.o .
	make HBCPROF=-ph LMLPROF=-ph ASMPROF=-DHPROFILE CPROF=-DHPROFILE lib.a lib.so
	mv lib.a lib_ph.a
	-mv lib.so lib_ph.so
	mv *.o hprofo
	mv normo/* .

Hprel.o:	$(HOBJ)
	$(MKDIR) normo hprelo
	-mv -f *.o lib.a normo
	@touch hprelo/DUMMY.o
	mv -f hprelo/*.o .
	make HBCPROF='$(IFLAGS)' LMLPROF='$(IFLAGS)' Hprelo
	$(BUILDSHLIB) $(DST)/$(LIB) lib_i.so $(HOBJ)
	mv *.o hprelo
	mv normo/* .
	-mv Hprelo Hprel.o

Hprelo:	$(HOBJ)
	$(MKIPREL) Hprelo $(HOBJ)

install:
	$(MKDIR) $(DST)/$(LIB)
	cp $(LIBS) $(DST)/$(LIB)
	sh ../bin/ccp $(SHLIBS) $(DST)/$(LIB)
	$(RANLIB) $(DST)/$(LIB)/lib*.a

install_Hprel:
	$(MKDIR) $(DST)/$(LIB)
	cp Hprel.o ihelp $(DST)/$(LIB)
	sh ../bin/ccp lib_i.so $(DST)/$(LIB)

Prelude.hi:	Prelude.phi ${PHI} ${PHIX} phi.sed util.sym
	cp Prelude.phi Prelude.hi
	sed -f phi.sed ${PHI} >> Prelude.hi
	sed -f phi.sed ${PHIX} | fgrep -f util.sym >> Prelude.hi

#
PreludeCore.hi:	PreludeCore.phi ${PCHI} ${IPCHI} phi.sed iphi.sed
	cp PreludeCore.phi PreludeCore.hi
	cat ${IPCHI} | sed -f iphi.sed >> PreludeCore.hi
	sed -f phi.sed ${PCHI} >> PreludeCore.hi

#
PreludeRT.p:	PreludeRT.hi
	$(CPP) PreludeRT.hi | $(PARSEP) -Z >PreludeRT.p

gencmp.o:	gencmp.m
	$(LMLC) $(XLMLFLAGS) -c -t -fno-zap-redex -fno-eta-expand -fno-const-caf -Z gencmp.m

Pselb.o:	Pselb.m
	$(LMLC) $(XLMLFLAGS) -c -Z Pselb.m

PP_isAlpha.o:	PP_isAlpha.hs
	$(HBC) $(HBCPROF) -1.2 -z -fprelude -no-pedantic $(ZAP) $(HBCFLAGS) -c PP_isAlpha.hs

C_Text.o:	C_Text.hs
	$(HBC) -DText=TeXt $(XHBCFLAGS) -S C_Text.hs
	sed 's/TeXt/Text/g' C_Text.s > tmp.s
	$(MYAS) -o C_Text.o tmp.s
	rm tmp.s C_Text.s

clean:
	-rm -f *.o *.a *.so
	-rm -f saveo*/*
	-rm -f normo/*
	-rm -f profo/* 
	-rm -f hprofo/* 
	-rm -f hprelo/* 

oosave:
	mv *.o lib.a saveoo
	mv saveoo/runtime* .
unosave:
	mv saveoo/* .
osave:
	mv *.o lib.a saveo
	mv saveo/runtime* .
unsave:
	mv saveo/* .
