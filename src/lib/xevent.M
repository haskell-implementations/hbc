#include "../runtime/machdep.M"
	.data
	.malign
	.export	CPxevent
CPxevent:
	.word	FUN
	.word	VCPxevent
	.export	VCPxevent
VCPxevent:
	.word	$1
	.word	CPxevent
	.word	unw1
	.word	vunw1
	.word	J1CPxevent
	.word	S1CPxevent
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPxevent
	.export	J1CPxevent
	.malign
S1CPxevent:
	move	$2(Sp),Vpush
J1CPxevent:
	.funbegin CPxevent
	move	0(Sp),r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_cxevent)
	move	_hp,Hp
	move	_ep,Sp
#if 1
	move    CRETR,r0
	move	$1(Sp),Sp
	jumpf   evalupdunw
#else
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
	return
#endif

	SDATAD
	.export	_xeventp
_xeventp:	.word	CPxevent
	.text
