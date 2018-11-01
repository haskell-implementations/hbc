#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
	.data
	.malign
	.export	CDftoi
CDftoi:
	.word	FUN
	.word	VCDftoi
	.export	VCDftoi
VCDftoi:
	.word	$1
	.word	CDftoi
	.word	unw1
	.word	vunw1
	.word	J1CDftoi
	.word	S1CDftoi
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CDftoi
	.export	J1CDftoi
	.malign
S1CDftoi:
	move	$2(Sp),Vpush
J1CDftoi:
	.funbegin	CDftoi
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	Vpop,Sp
	move	-1(Sp),r0
	ftoi	1(ATMP),1(r0)
	move	$INT,0(r0)
#ifdef HPROFILE
	move	$ftoi_Int_Int_LABEL, 2(r0)		/* slop 1: 	DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* slop 2:	DW */
#endif
#endif
	return
	.funend

#ifdef HPROFILE
	.malign
ftoi_Int_Int_LABEL:
	.word HEAPMAGIC
	.word $lib_LABEL
        .word $ftoi_LABEL
        .word $ftoi_LABEL
        .word $Int_LABEL
        .word $Int_LABEL

ftoi_LABEL:             .string "ftoi"                  /* DW */

lib_LABEL:		.string "lib"			/* DW */

Int_LABEL:		.string "Int"			/* DW */
#endif
