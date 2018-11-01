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
	.data
#ifdef LINKWORD
	.word 0
#endif
	.export	_respfail
_respfail:	.word	APG,CPfail,msg   ; NR 920126
	HPROFWORDS($SYSTEMdialog_Cons_List_LABEL,$0)
#ifdef LINKWORD
	.word 0
#endif
msg:	.word	STRING,smsg,0
	HPROFWORDS($SYSTEMdialog_String_List_LABEL,$0)
smsg:	.string	"Response evaluated before request finished"


	.data
	.export	_fileStdin
	.export	C__fileStdin
#ifdef LINKWORD
        .word   0
#endif
_fileStdin:
C__fileStdin:
	.word	INPUT,0,-1
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif
	.export	_fileStdout
	.export	C__fileStdout
#ifdef LINKWORD
        .word   0
#endif
_fileStdout:
C__fileStdout:
	.word	INPUT,0,-1
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif
	.export	_fileStderr
	.export	C__fileStderr
#ifdef LINKWORD
        .word   0
#endif
_fileStderr:
C__fileStderr:
	.word	INPUT,0,-1
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif


        .data
        .malign
        .export C___runDialogue
C___runDialogue:
        .word   FUN
        .word   VC___runDialogue
        .export VC___runDialogue
VC___runDialogue:
        .word   $1
        .word   C___runDialogue
        .word   unw1
        .word   vunw1
        .word   J1C___runDialogue
        .word   S1C___runDialogue
        .word   $0
        .word   $0
        .word   $0
        .word   $0
        .word   $0
        .text
        .export S1C___runDialogue
        .export J1C___runDialogue
	.malign
S1C___runDialogue:
        move    $2(Sp),Vpush
J1C___runDialogue:
        .funbegin       ___runDialogue

	move	0(Sp),ATMP

	move	Hp,r0
	move	$APG,toH
	move	ATMP,toH
	move	ATMP,toH			; dummy, filled by rundialogue
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_rundialogue)
	move	_ep,Sp
	move	_hp,Hp
	move	CRETR,r0
	move	$1(Sp),Sp			; pop arg
	jumpf	evalupdunw

;;;; XXXX fix for heap profiling

#if 1
	SDATAD
	.export	_concargs
_concargs:	.word	CPargv
	.text
#endif

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
