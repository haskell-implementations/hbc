#include "../runtime/machdep.M"
	.data
	.malign
	.export	CPref
CPref:
	.word	FUN
	.word	VCPref
	.export	VCPref
	.word	S1CPref
VCPref:
	.word	$1
	.word	CPref
	.word	unw1
	.word	vunw1
	.word	J1CPref
	.word	S1CPref
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPref
	.export	J1CPref
	.malign
S1CPref:
	move	$2(Sp),Vpush
J1CPref:
	.funbegin	CPref
	move	$TAG,toH	; create the ref cell
	move	$0,toH
	move	0(Sp),toH	; and fill it with the argument.  This word is the real variable!

        move    Vpop,Sp
        move    -1(Sp),r0
        move    $TAG,0(r0)	; return a cell that can be copied (with a pointer to the actual variable)
        move    $0,1(r0)
        move    $-3(Hp),2(r0)
        return
	.funend

	.data
	.malign
	.export	CPderef
CPderef:
	.word	FUN
	.word	VCPderef
	.export	VCPderef
	.word	S1CPderef
VCPderef:
	.word	$1
	.word	CPderef
	.word	unw1
	.word	vunw1
	.word	J1CPderef
	.word	S1CPderef
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPderef
	.export	J1CPderef
	.malign
S1CPderef:
	move	$2(Sp),Vpush
J1CPderef:
	.funbegin	CPderef
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
; r0 must point to a TAG node now
	move	2(r0),r0		; pointer to the ref cell, which is always evaluated
	move	2(r0),0(Sp)		; this is the real value
	move	Spop,r0
	jumpf	evalupdunw		; evaluate and return
	.funend

	.data
	.malign
	.export	CPassign
CPassign:
	.word	FUN
	.word	VCPassign
	.export	VCPassign
	.word	S2CPassign
VCPassign:
	.word	$2
	.word	CPassign
	.word	unw2
	.word	vunw2
	.word	J2CPassign
	.word	S2CPassign
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPassign
	.export	J2CPassign
	.malign
S2CPassign:
	move	$3(Sp),Vpush
J2CPassign:
	.funbegin	CPassign
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
; r0 must point to a TAG node now
	move	2(r0),r0		; pointer to the ref cell, which is always evaluated
	move	1(Sp),ATMP		; new value
	move	ATMP,2(r0)		; The actual assignment!
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$0,1(r0)
	move	$TAG0,0(r0)		; return ()
	return
	.funend

	.data
	.malign
	.export	CPeqref
CPeqref:
	.word	FUN
	.word	VCPeqref
	.export	VCPeqref
	.word	S2CPeqref
VCPeqref:
	.word	$2
	.word	CPeqref
	.word	unw2
	.word	vunw2
	.word	J2CPeqref
	.word	S2CPeqref
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPeqref
	.export	J2CPeqref
	.malign
S2CPeqref:
	move	$3(Sp),Vpush
J2CPeqref:
	.funbegin	CPeqref
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	2(r0),r0		; get first pointer
	move	0(Sp),r1
	move	2(r1),r1		; and the other one
	comp	r0,r1
	boolcc	eq,DTMP
	move	Vpop,Sp
	move	-1(Sp),r0
	move	DTMP,1(r0)
	move	$TAG0,0(r0)
	return
	.funend

	.data
	.malign
	.export	CPeqptr
CPeqptr:
	.word	FUN
	.word	VCPeqptr
	.export	VCPeqptr
	.word	S2CPeqptr
VCPeqptr:
	.word	$2
	.word	CPeqptr
	.word	unw2
	.word	vunw2
	.word	J2CPeqptr
	.word	S2CPeqptr
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPeqptr
	.export	J2CPeqptr
	.malign
S2CPeqptr:
	move	$3(Sp),Vpush
J2CPeqptr:
	.funbegin	CPeqptr
	move	0(Sp),r0
	move	1(Sp),r1
	comp	r0,r1
	boolcc	eq,DTMP
	move	Vpop,Sp
	move	-1(Sp),r0
	move	DTMP,1(r0)
	move	$TAG0,0(r0)
	return
	.funend
