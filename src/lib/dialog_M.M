#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
#ifdef HPROFILE
#ifdef SLOP2
#define HPROFWORDS(w1,w2) 	.word	w1,w2
#else
#define HPROFWORDS(w1,w2) 	.word	w1
#endif
#else
#define HPROFWORDS(w1,w2)
#endif
;

/* Main program, calls C function */
        .data
        .malign
        .export CPmain
CPmain:
        .word   FUN
        .word   VCPmain
        .export VCPmain
VCPmain:
        .word   $1
        .word   CPmain
        .word   unw1
        .word   vunw1
        .word   J1CPmain
        .word   S1CPmain
        .word   $0
        .word   $0
        .word   $0
        .word   $0
        .word   $1
	.word	LT1
        .text
        .export S1CPmain
        .export J1CPmain
	.malign
S1CPmain:
        move    $2(Sp),Vpush
J1CPmain:
        .funbegin       CPmain

	move	1(Sp),r0
	move	$HOLE,0(r0)			; zap application node

        .data
#ifdef LINKWORD
	.word 0
#endif
LT1:
        .word   $APG   ; NR 921026
        .word   C_main
        .word   LT1				; filled later by toploop
	HPROFWORDS($SYSTEMdialog_Cons_List_LABEL,$0)
        .text
	move	$LT1,r0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_toploop)
	move	_ep,Sp
	move	_hp,Hp
/* All is done, return [] to ordinary printer */
        move    Vpop,Sp
        move    -1(Sp),r0
        move    $0,1(r0)
        move    $TAG0,0(r0)
#ifdef HPROFILE
	move 	$SYSTEMdialog_Nil_List_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move 	$0, 3(r0)				/* DW */
#endif
#endif
        return

#ifdef HPROFILE
	.malign
SYSTEMdialog_Nil_List_LABEL:
	.word HEAPMAGIC
	.word $lib_LABEL
	.word $dialog_M_LABEL
        .word $SYSTEMdialog_LABEL
        .word $Nil_LABEL
        .word $List_LABEL

	.malign
SYSTEMdialog_Cons_List_LABEL:
	.word HEAPMAGIC
	.word $lib_LABEL
	.word $dialog_M_LABEL
        .word $SYSTEMdialog_LABEL
        .word $Cons_LABEL
        .word $List_LABEL

	.malign
SYSTEMdialog_String_List_LABEL:
	.word HEAPMAGIC
	.word $lib_LABEL
	.word $dialog_M_LABEL
        .word $SYSTEMdialog_LABEL
        .word $String_LABEL
        .word $List_LABEL

dialog_M_LABEL: 	.string "dialog_M"            	/* DW */

SYSTEMdialog_LABEL:	.string "SYSTEM(dialog)"       	/* DW */

String_LABEL:		.string "String"		/* DW */

lib_LABEL:		.string "lib"			/* DW */

Nil_LABEL:		.string "[]"			/* DW */

Cons_LABEL:		.string ":"			/* DW */

List_LABEL:		.string "List"			/* DW */
#endif
