#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
	.data
	.malign
	.export	CDitof
CDitof:
	.word	FUN
	.word	VCDitof
	.export	VCDitof
VCDitof:
	.word	$1
	.word	CDitof
	.word	unw1
	.word	vunw1
	.word	J1CDitof
	.word	S1CDitof
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CDitof
	.export	J1CDitof
	.malign
S1CDitof:
	move	$2(Sp),Vpush
J1CDitof:
	.funbegin	CDitof
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(Sp),ATMP
	move	Vpop,Sp
	move	-1(Sp),r0
	itof	1(ATMP),1(r0)
	move	$DFLOAT,0(r0)
#ifdef HPROFILE
	move	$itof_Float_Float_LABEL, 3(r0)	/* slop 1:	DW */
#ifdef SLOP2
	move	$0, 4(r0)			/* slop 2:	DW */
#endif
#endif
	return
	.funend

#ifdef HPROFILE
	.malign
itof_Float_Float_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$itof_LABEL
	.word 	$itof_LABEL
	.word 	$Float_LABEL
	.word 	$Float_LABEL

itof_LABEL: 	.string "itof"			/* DW */

lib_LABEL: 	.string "lib"			/* DW */

Float_LABEL: 	.string "Float"			/* DW */
#endif
