#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
	.data
	.malign
	.export	CPreadchan
	.export	_readchan
_readchan:
CPreadchan:
	.word	FUN
	.word	VCPreadchan
	.export	VCPreadchan
VCPreadchan:
	.word	$1
	.word	CPreadchan
	.word	unw1
	.word	vunw1
	.word	J1CPreadchan
	.word	S1CPreadchan
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPreadchan
	.export	J1CPreadchan
	.malign
S1CPreadchan:
	move	$2(Sp),Vpush
J1CPreadchan:
	.funbegin CPreadchan
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_creadchan)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
#ifdef HPROFILE
	move	3(r1),3(r0)		/* DW */
#ifdef SLOP2
	move	4(r1),4(r0)		/* DW */
#endif
#endif
	return
