#include "../runtime/machdep.M"
	.data
	.malign
	.export	C_kno
C_kno:
	.word	FUN
	.word	VC_kno
	.export	VC_kno
VC_kno:
	.word	$1
	.word	C_kno
	.word	unw1
	.word	vunw1
	.word	J1C_kno
	.word	S1C_kno
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C_kno
	.export	J1C_kno
	.malign
S1C_kno:
	move	$2(Sp),Vpush
J1C_kno:
	.funbegin	C_kno
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(r0),INDREG
	call	ogettag(INDREG)
	move	Vpop,Sp
	move	-1(Sp),r0
	move	TAGREG,1(r0)
	move	$INT,0(r0)
	return
	.funend
