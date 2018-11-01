#if 0
#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#else
	.word	_USE_none
#endif
#define ATMP2 r1
	.data
	.malign
	.export	CDFloatAdd
CDFloatAdd:
	.word	FUN
	.word	VCDFloatAdd
	.export	VCDFloatAdd
VCDFloatAdd:
	.word	$2
	.word	CDFloatAdd
	.word	unw2
	.word	vunw2
	.word	J2CDFloatAdd
	.word	S2CDFloatAdd
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CDFloatAdd
	.export	J2CDFloatAdd
	.malign
S2CDFloatAdd:
	move	$3(Sp),Vpush
J2CDFloatAdd:
	.funbegin	CDFloatAdd
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	1(Sp),ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FADD3(1(ATMP),1(ATMP2),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatAdd_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend

	.data
	.malign
	.export	CDFloatNeg
CDFloatNeg:
	.word	FUN
	.word	VCDFloatNeg
	.export	VCDFloatNeg
VCDFloatNeg:
	.word	$1
	.word	CDFloatNeg
	.word	unw1
	.word	vunw1
	.word	J1CDFloatNeg
	.word	S1CDFloatNeg
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CDFloatNeg
	.export	J1CDFloatNeg
	.malign
S1CDFloatNeg:
	move	$2(Sp),Vpush
J1CDFloatNeg:
	.funbegin	CDFloatNeg
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,ATMP
	move	$fzero,ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FSUB3(1(ATMP2),1(ATMP),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatNeg_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend
	.data
fzero:	.word	DFLOAT
	.float	0.0e0


	.data
	.malign
	.export	CDFloatSub
CDFloatSub:
	.word	FUN
	.word	VCDFloatSub
	.export	VCDFloatSub
VCDFloatSub:
	.word	$2
	.word	CDFloatSub
	.word	unw2
	.word	vunw2
	.word	J2CDFloatSub
	.word	S2CDFloatSub
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CDFloatSub
	.export	J2CDFloatSub
	.malign
S2CDFloatSub:
	move	$3(Sp),Vpush
J2CDFloatSub:
	.funbegin	CDFloatSub
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	1(Sp),ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FSUB3(1(ATMP),1(ATMP2),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatSub_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend

	.data
	.malign
	.export	CDFloatMul
CDFloatMul:
	.word	FUN
	.word	VCDFloatMul
	.export	VCDFloatMul
VCDFloatMul:
	.word	$2
	.word	CDFloatMul
	.word	unw2
	.word	vunw2
	.word	J2CDFloatMul
	.word	S2CDFloatMul
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CDFloatMul
	.export	J2CDFloatMul
	.malign
S2CDFloatMul:
	move	$3(Sp),Vpush
J2CDFloatMul:
	.funbegin	CDFloatMul
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	1(Sp),ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FMUL3(1(ATMP),1(ATMP2),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatMul_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend
	.data
	.malign
	.export	CDFloatDiv
CDFloatDiv:
	.word	FUN
	.word	VCDFloatDiv
	.export	VCDFloatDiv
VCDFloatDiv:
	.word	$2
	.word	CDFloatDiv
	.word	unw2
	.word	vunw2
	.word	J2CDFloatDiv
	.word	S2CDFloatDiv
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CDFloatDiv
	.export	J2CDFloatDiv
	.malign
S2CDFloatDiv:
	move	$3(Sp),Vpush
J2CDFloatDiv:
	.funbegin	CDFloatDiv
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	1(Sp),ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FDIV3(1(ATMP),1(ATMP2),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatDiv_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend
#if 0
	.data
	.malign
	.export	CDFloatMod
CDFloatMod:
	.word	FUN
	.word	VCDFloatMod
	.export	VCDFloatMod
VCDFloatMod:
	.word	$2
	.word	CDFloatMod
	.word	unw2
	.word	vunw2
	.word	J2CDFloatMod
	.word	S2CDFloatMod
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CDFloatMod
	.export	J2CDFloatMod
	.malign
S2CDFloatMod:
	move	$3(Sp),Vpush
J2CDFloatMod:
	.funbegin	CDFloatMod
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	1(Sp),ATMP2
	move	Vpop,Sp
	move	-1(Sp),r0
	FMOD3(1(ATMP),1(ATMP2),1(r0))
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$FloatMod_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2: 	DW */
#endif
#endif
	return
	.funend
#endif

#ifdef HPROFILE
	.malign
FloatAdd_Float_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
	.word $Float_LABEL
        .word $FloatAdd_LABEL
        .word $Float_LABEL
        .word $Float_LABEL

	.malign
FloatSub_Float_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
	.word $Float_LABEL
        .word $FloatSub_LABEL
        .word $Float_LABEL
        .word $Float_LABEL

	.malign
FloatMul_Float_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
	.word $Float_LABEL
        .word $FloatMul_LABEL
        .word $Float_LABEL
        .word $Float_LABEL

	.malign
FloatDiv_Float_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
	.word $Float_LABEL
        .word $FloatDiv_LABEL
        .word $Float_LABEL
        .word $Float_LABEL

	.malign
FloatNeg_Float_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
	.word $Float_LABEL
        .word $FloatNeg_LABEL
        .word $Float_LABEL
        .word $Float_LABEL

Float_LABEL:		.string "Float"				/* DW */
FloatAdd_LABEL:   	.string "FloatAdd"                	/* DW */
FloatSub_LABEL:   	.string "FloatSub"                	/* DW */
FloatMul_LABEL:   	.string "FloatMul"                	/* DW */
FloatDiv_LABEL:   	.string "FloatDiv"                	/* DW */
FloatNeg_LABEL:   	.string "FloatNeg"                	/* DW */
lib_LABEL:		.string "lib"				/* DW */
#endif
#endif
