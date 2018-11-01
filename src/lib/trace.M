#include "../runtime/machdep.M"
; Trace function
	.data
	.malign
	.export	C_trace
C_trace:
	.word	FUN
	.word	VC_trace
	.export	VC_trace
VC_trace:
	.word	$2
	.word	C_trace
	.word	unw2
	.word	vunw2
	.word	J2C_trace
	.word	S2C_trace
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_trace
	.export	J2C_trace
	.malign
S2C_trace:
	move	$3(Sp),Vpush
J2C_trace:
	.funbegin	C_trace
; Call C trace with first argument (which is on top of the stack already)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(0,_trace)
	move	_hp,Hp
	move	_ep,Sp
	move	Spop,r0
	jumpf	evalupdunw
	.funend

