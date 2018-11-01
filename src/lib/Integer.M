#include "../runtime/machdep.M"
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#endif
	.data
	.malign
	.export	CPIntegerAdd
CPIntegerAdd:
	.word	FUN
	.word	VCPIntegerAdd
	.export	VCPIntegerAdd
VCPIntegerAdd:
	.word	$2
	.word	CPIntegerAdd
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerAdd
	.word	S2CPIntegerAdd
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerAdd
	.export	J2CPIntegerAdd
	.malign
S2CPIntegerAdd:
	move	$3(Sp),Vpush
J2CPIntegerAdd:
	.funbegin CPIntegerAdd
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigadd)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerAdd_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerSub
CPIntegerSub:
	.word	FUN
	.word	VCPIntegerSub
	.export	VCPIntegerSub
VCPIntegerSub:
	.word	$2
	.word	CPIntegerSub
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerSub
	.word	S2CPIntegerSub
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerSub
	.export	J2CPIntegerSub
	.malign
S2CPIntegerSub:
	move	$3(Sp),Vpush
J2CPIntegerSub:
	.funbegin CPIntegerSub
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigsub)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerSub_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerMul
CPIntegerMul:
	.word	FUN
	.word	VCPIntegerMul
	.export	VCPIntegerMul
VCPIntegerMul:
	.word	$2
	.word	CPIntegerMul
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerMul
	.word	S2CPIntegerMul
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerMul
	.export	J2CPIntegerMul
	.malign
S2CPIntegerMul:
	move	$3(Sp),Vpush
J2CPIntegerMul:
	.funbegin CPIntegerMul
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigmul)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerMul_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerDiv
CPIntegerDiv:
	.word	FUN
	.word	VCPIntegerDiv
	.export	VCPIntegerDiv
VCPIntegerDiv:
	.word	$2
	.word	CPIntegerDiv
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerDiv
	.word	S2CPIntegerDiv
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	zdivstr
	.text
	.export	S2CPIntegerDiv
	.export	J2CPIntegerDiv
	.malign
S2CPIntegerDiv:
	move	$3(Sp),Vpush
J2CPIntegerDiv:
	.funbegin CPIntegerDiv
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigdiv)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	comp	r1,$0
	jeq	zerodiv
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerDiv_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
zdivstr:
        .word   STRINGN
        .word   zdivs
        .word   $18
#ifdef HPROFILE
	.word	IntegerDiv_Integer_LABEL
#ifdef SLOP2
	.word	0
#endif
#endif
zdivs:
        .string "Bignum divide by 0"
        .text

zerodiv:
        move    $zdivstr,Spush
        jumpf   J1CPfail
	

	.data
	.malign
	.export	CPIntegerMod
CPIntegerMod:
	.word	FUN
	.word	VCPIntegerMod
	.export	VCPIntegerMod
VCPIntegerMod:
	.word	$2
	.word	CPIntegerMod
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerMod
	.word	S2CPIntegerMod
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	zdivstr
	.text
	.export	S2CPIntegerMod
	.export	J2CPIntegerMod
	.malign
S2CPIntegerMod:
	move	$3(Sp),Vpush
J2CPIntegerMod:
	.funbegin CPIntegerMod
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigmod)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	comp	r1,$0
	jeq	zerodiv
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerMod_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerDivMod
CPIntegerDivMod:
	.word	FUN
	.word	VCPIntegerDivMod
	.export	VCPIntegerDivMod
VCPIntegerDivMod:
	.word	$2
	.word	CPIntegerDivMod
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerDivMod
	.word	S2CPIntegerDivMod
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	zdivstr
	.text
	.export	S2CPIntegerDivMod
	.export	J2CPIntegerDivMod
	.malign
S2CPIntegerDivMod:
	move	$3(Sp),Vpush
J2CPIntegerDivMod:
	.funbegin CPIntegerDivMod
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigdivmod)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	comp	r1,$0
	jeq	zerodiv
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
#ifdef HPROFILE
	move	3(r1),3(r0)	/* DW */
#ifdef SLOP2
	move	4(r1),4(r0)				/* DW */
#endif
#endif
	return

	.data
	.malign
	.export	CPInt2Integer
CPInt2Integer:
	.word	FUN
	.word	VCPInt2Integer
	.export	VCPInt2Integer
VCPInt2Integer:
	.word	$1
	.word	CPInt2Integer
	.word	unw1
	.word	vunw1
	.word	J1CPInt2Integer
	.word	S1CPInt2Integer
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPInt2Integer
	.export	J1CPInt2Integer
	.malign
S1CPInt2Integer:
	move	$2(Sp),Vpush
J1CPInt2Integer:
	.funbegin CPInt2Integer
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_int2big)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
	move	r1,1(r0)
#ifdef HPROFILE
	move	$Int2Integer_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	return

	.data
	.malign
	.export	CPInteger2Int
CPInteger2Int:
	.word	FUN
	.word	VCPInteger2Int
	.export	VCPInteger2Int
VCPInteger2Int:
	.word	$1
	.word	CPInteger2Int
	.word	unw1
	.word	vunw1
	.word	J1CPInteger2Int
	.word	S1CPInteger2Int
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPInteger2Int
	.export	J1CPInteger2Int
	.malign
S1CPInteger2Int:
	move	$2(Sp),Vpush
J1CPInteger2Int:
	.funbegin CPInteger2Int
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_big2int)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$INT,0(r0)
	move	r1,1(r0)
#ifdef HPROFILE
	move	$Integer2Int_Int_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	return

	.data
	.malign
	.export	CPIntegerNeg
CPIntegerNeg:
	.word	FUN
	.word	VCPIntegerNeg
	.export	VCPIntegerNeg
VCPIntegerNeg:
	.word	$1
	.word	CPIntegerNeg
	.word	unw1
	.word	vunw1
	.word	J1CPIntegerNeg
	.word	S1CPIntegerNeg
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPIntegerNeg
	.export	J1CPIntegerNeg
	.malign
S1CPIntegerNeg:
	move	$2(Sp),Vpush
J1CPIntegerNeg:
	.funbegin CPIntegerNeg
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_bigneg)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerNeg_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPInteger2IntList
CPInteger2IntList:
	.word	FUN
	.word	VCPInteger2IntList
	.export	VCPInteger2IntList
VCPInteger2IntList:
	.word	$1
	.word	CPInteger2IntList
	.word	unw1
	.word	vunw1
	.word	J1CPInteger2IntList
	.word	S1CPInteger2IntList
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPInteger2IntList
	.export	J1CPInteger2IntList
	.malign
S1CPInteger2IntList:
	move	$2(Sp),Vpush
J1CPInteger2IntList:
	.funbegin CPInteger2IntList
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_big2intlist)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
#ifdef HPROFILE
	move	3(r1),3(r0)	/* DW */
#ifdef SLOP2
	move	4(r1),4(r0)				/* DW */
#endif
#endif
	return

	.data
	.malign
	.export	CPInteger2String
CPInteger2String:
	.word	FUN
	.word	VCPInteger2String
	.export	VCPInteger2String
VCPInteger2String:
	.word	$2
	.word	CPInteger2String
	.word	unw2
	.word	vunw2
	.word	J2CPInteger2String
	.word	S2CPInteger2String
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPInteger2String
	.export	J2CPInteger2String
	.malign
S2CPInteger2String:
	move	$3(Sp),Vpush
J2CPInteger2String:
	.funbegin CPInteger2String
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_big2str)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
#ifdef HPROFILE
	move	3(r1),3(r0)	/* DW */
#ifdef SLOP2
	move	4(r1),4(r0)				/* DW */
#endif
#endif
	return

	.data
	.malign
	.export	CPIntegerPowMod
CPIntegerPowMod:
	.word	FUN
	.word	VCPIntegerPowMod
	.export	VCPIntegerPowMod
VCPIntegerPowMod:
	.word	$3
	.word	CPIntegerPowMod
	.word	unw3
	.word	vunw3
	.word	J3CPIntegerPowMod
	.word	S3CPIntegerPowMod
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S3CPIntegerPowMod
	.export	J3CPIntegerPowMod
	.malign
S3CPIntegerPowMod:
	move	$4(Sp),Vpush
J3CPIntegerPowMod:
	.funbegin CPIntegerPowMod
	move	2(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,2(Sp)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG2(r0)
	move	Spop,r0
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(3,_bigpowm)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerPowMod_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerGcd
CPIntegerGcd:
	.word	FUN
	.word	VCPIntegerGcd
	.export	VCPIntegerGcd
VCPIntegerGcd:
	.word	$2
	.word	CPIntegerGcd
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerGcd
	.word	S2CPIntegerGcd
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerGcd
	.export	J2CPIntegerGcd
	.malign
S2CPIntegerGcd:
	move	$3(Sp),Vpush
J2CPIntegerGcd:
	.funbegin CPIntegerGcd
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_biggcd)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerGcd_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerAnd
CPIntegerAnd:
	.word	FUN
	.word	VCPIntegerAnd
	.export	VCPIntegerAnd
VCPIntegerAnd:
	.word	$2
	.word	CPIntegerAnd
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerAnd
	.word	S2CPIntegerAnd
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerAnd
	.export	J2CPIntegerAnd
	.malign
S2CPIntegerAnd:
	move	$3(Sp),Vpush
J2CPIntegerAnd:
	.funbegin CPIntegerAnd
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigand)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerAnd_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerOr
CPIntegerOr:
	.word	FUN
	.word	VCPIntegerOr
	.export	VCPIntegerOr
VCPIntegerOr:
	.word	$2
	.word	CPIntegerOr
	.word	unw2
	.word	vunw2
	.word	J2CPIntegerOr
	.word	S2CPIntegerOr
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2CPIntegerOr
	.export	J2CPIntegerOr
	.malign
S2CPIntegerOr:
	move	$3(Sp),Vpush
J2CPIntegerOr:
	.funbegin CPIntegerOr
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG1(r0)
	move	Spop,r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(2,_bigor)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$BIGNUM,0(r0)
#ifdef HPROFILE
	move	$IntegerOr_Integer_LABEL, 2(r0)	/* DW */
#ifdef SLOP2
	move	$0, 3(r0)				/* DW */
#endif
#endif
	move	r1,1(r0)
	return

	.data
	.malign
	.export	CPIntegerSqrt
CPIntegerSqrt:
	.word	FUN
	.word	VCPIntegerSqrt
	.export	VCPIntegerSqrt
VCPIntegerSqrt:
	.word	$1
	.word	CPIntegerSqrt
	.word	unw1
	.word	vunw1
	.word	J1CPIntegerSqrt
	.word	S1CPIntegerSqrt
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPIntegerSqrt
	.export	J1CPIntegerSqrt
	.malign
S1CPIntegerSqrt:
	move	$2(Sp),Vpush
J1CPIntegerSqrt:
	.funbegin CPIntegerSqrt
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_bigsqrt)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(r1),0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
#ifdef HPROFILE
	move	3(r1),3(r0)	/* DW */
#ifdef SLOP2
	move	4(r1),4(r0)				/* DW */
#endif
#endif
	return

#ifdef HPROFILE
	.malign
IntegerAdd_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerAdd_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerSub_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerSub_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerMul_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerMul_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerDiv_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerDiv_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerMod_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerMod_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerNeg_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerNeg_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerGcd_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerGcd_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerAnd_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerAnd_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerOr_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerOr_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
IntegerPowMod_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$IntegerPowMod_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
Int2Integer_Integer_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$Int2Integer_LABEL
	.word	$Integer_LABEL
	.word	$Integer_LABEL

	.malign
Integer2Int_Int_LABEL:
	.word	HEAPMAGIC
	.word 	$lib_LABEL
	.word 	$Integer_LABEL
	.word	$Integer2Int_LABEL
	.word	$Int_LABEL
	.word	$Int_LABEL

IntegerAdd_LABEL: 	.string "IntegerAdd"    		/* DW */
IntegerSub_LABEL: 	.string "IntegerSub"    		/* DW */
IntegerMul_LABEL: 	.string "IntegerMul"    		/* DW */
IntegerDiv_LABEL: 	.string "IntegerDiv"    		/* DW */
IntegerMod_LABEL: 	.string "IntegerMod"    		/* DW */
IntegerNeg_LABEL: 	.string "IntegerNeg"    		/* DW */
IntegerGcd_LABEL: 	.string "IntegerGcd"    		/* DW */
IntegerAnd_LABEL: 	.string "IntegerAnd"    		/* DW */
IntegerOr_LABEL: 	.string "IntegerOr"	    		/* DW */
IntegerPowMod_LABEL: 	.string "IntegerPowMod"    		/* DW */
Int2Integer_LABEL: 	.string "Int2Integer"    		/* DW */
Integer2Int_LABEL: 	.string "Integer2Int"    		/* DW */
Integer_LABEL:		.string "Integer"			/* DW */
Int_LABEL:		.string "Int"				/* DW */
lib_LABEL:		.string "lib"				/* DW */
#endif
