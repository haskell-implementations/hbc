#include "../runtime/machdep.M"
	.data
	.malign
	.export	C_main
C_main:
	.word	FUN
	.word	VC_main
	.export	VC_main
	.word	S1C_main
VC_main:
	.word	$1
	.word	C_main
	.word	unw1
	.word	vunw1
	.word	J1C_main
	.word	S1C_main
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	C_Main$__main
	.text
	.export	S1C_main
	.export	J1C_main
	.malign
S1C_main:
	move	$2(Sp),Vpush
J1C_main:
	.funbegin	C_main
        move    $C_Main$__main,r0
        move    $1,ARGCREG
        move    0(r0),INDREG
        jump    ojfun(INDREG)
	.funend
