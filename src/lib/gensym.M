#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
	.data
	.malign
	
counter:	.word	0		; global counter

	.export	C___gensym
C___gensym:
	.word	FUN
	.word	VC___gensym
	.export	VC___gensym
	.word	S1C___gensym
VC___gensym:
	.word	$1
	.word	C___gensym
	.word	unw1
	.word	vunw1
	.word	J1C___gensym
	.word	S1C___gensym
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C___gensym
	.export	J1C___gensym
	.malign
S1C___gensym:
	move	$2(Sp),Vpush
J1C___gensym:
	.funbegin	C___gensym
	add2	$1,counter		; increment global counter
	move	Vpop,Sp
	move	-1(Sp),r0
	move	counter,1(r0)		; update result node
	move	$INT,0(r0)
#ifdef HPROFILE
	move	$gensym_Int_Int_LABEL, 2(r0)		/* slop 1: 	DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* slop 2:	DW */
#endif
#endif
	return
	.funend

#ifdef HPROFILE
	.malign
gensym_Int_Int_LABEL:
	.word	HEAPMAGIC
	.word $lib_LABEL
        .word $gensym_LABEL
        .word $gensym_LABEL
        .word $Int_LABEL
        .word $Int_LABEL

gensym_LABEL:             .string "gensym"                  /* DW */

lib_LABEL:		.string "lib"			/* DW */

Int_LABEL:		.string "Int"			/* DW */
#endif
