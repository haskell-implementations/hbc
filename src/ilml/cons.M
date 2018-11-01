#include "../runtime/machdep.M"
#include "../runtime/gc.h"		/* VFrefs */

	.data
	.malign
	.export	C_ktag0
C_ktag0:
	.word	FUN
	.word	VC_ktag0
	.export	VC_ktag0
VC_ktag0:
	.word	$1
	.word	C_ktag0
	.word	unw1
	.word	vunw1
	.word	J1C_ktag0
	.word	S1C_ktag0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C_ktag0
	.export	J1C_ktag0
	.malign
S1C_ktag0:
	move	$2(Sp),Vpush
J1C_ktag0:
	.funbegin	C_ktag0
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	1(r1),1(r0)
	move	$TAG0,0(r0)
	return
	.funend

	.data
	.malign
	.export	C_ktag
C_ktag:
	.word	FUN
	.word	VC_ktag
	.export	VC_ktag
VC_ktag:
	.word	$2
	.word	C_ktag
	.word	unw2
	.word	vunw2
	.word	J2C_ktag
	.word	S2C_ktag
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_ktag
	.export	J2C_ktag
	.malign
S2C_ktag:
	move	$3(Sp),Vpush
J2C_ktag:
	.funbegin	C_ktag
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	1(r1),1(r0)
	move	INDREG,2(r0)
	move	$TAG,0(r0)
	return
	.funend

	.data
	.malign
	.export	C_kpair0
C_kpair0:
	.word	FUN
	.word	VC_kpair0
	.export	VC_kpair0
VC_kpair0:
	.word	$2
	.word	C_kpair0
	.word	unw2
	.word	vunw2
	.word	J2C_kpair0
	.word	S2C_kpair0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpair0
	.export	J2C_kpair0
	.malign
S2C_kpair0:
	move	$3(Sp),Vpush
J2C_kpair0:
	.funbegin	C_kpair0
	move	0(Sp),r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	r1,1(r0)
	move	INDREG,2(r0)
	move	$PAIR0,0(r0)
	return
	.funend


	.data
	.malign
	.export	C_kpair1
C_kpair1:
	.word	FUN
	.word	VC_kpair1
	.export	VC_kpair1
VC_kpair1:
	.word	$2
	.word	C_kpair1
	.word	unw2
	.word	vunw2
	.word	J2C_kpair1
	.word	S2C_kpair1
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpair1
	.export	J2C_kpair1
	.malign
S2C_kpair1:
	move	$3(Sp),Vpush
J2C_kpair1:
	.funbegin	C_kpair1
	move	0(Sp),r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	r1,1(r0)
	move	INDREG,2(r0)
	move	$PAIR1,0(r0)
	return
	.funend


	.data
	.malign
	.export	C_kpair2
C_kpair2:
	.word	FUN
	.word	VC_kpair2
	.export	VC_kpair2
VC_kpair2:
	.word	$2
	.word	C_kpair2
	.word	unw2
	.word	vunw2
	.word	J2C_kpair2
	.word	S2C_kpair2
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpair2
	.export	J2C_kpair2
	.malign
S2C_kpair2:
	move	$3(Sp),Vpush
J2C_kpair2:
	.funbegin	C_kpair2
	move	0(Sp),r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	r1,1(r0)
	move	INDREG,2(r0)
	move	$PAIR2,0(r0)
	return
	.funend


	.data
	.malign
	.export	C_kpair3
C_kpair3:
	.word	FUN
	.word	VC_kpair3
	.export	VC_kpair3
VC_kpair3:
	.word	$2
	.word	C_kpair3
	.word	unw2
	.word	vunw2
	.word	J2C_kpair3
	.word	S2C_kpair3
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpair3
	.export	J2C_kpair3
	.malign
S2C_kpair3:
	move	$3(Sp),Vpush
J2C_kpair3:
	.funbegin	C_kpair3
	move	0(Sp),r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	r1,1(r0)
	move	INDREG,2(r0)
	move	$PAIR3,0(r0)
	return
	.funend


	.data
	.malign
	.export	C_kpair4
C_kpair4:
	.word	FUN
	.word	VC_kpair4
	.export	VC_kpair4
VC_kpair4:
	.word	$2
	.word	C_kpair4
	.word	unw2
	.word	vunw2
	.word	J2C_kpair4
	.word	S2C_kpair4
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpair4
	.export	J2C_kpair4
	.malign
S2C_kpair4:
	move	$3(Sp),Vpush
J2C_kpair4:
	.funbegin	C_kpair4
	move	0(Sp),r1
	move	1(Sp),INDREG
	move	Vpop,Sp
	move	-1(Sp),r0
	move	r1,1(r0)
	move	INDREG,2(r0)
	move	$PAIR4,0(r0)
	return
	.funend

	.data
	.malign
	.export	C_ktvek
C_ktvek:
	.word	FUN
	.word	VC_ktvek
	.export	VC_ktvek
VC_ktvek:
	.word	$2
	.word	C_ktvek
	.word	unw2
	.word	vunw2
	.word	J2C_ktvek
	.word	S2C_ktvek
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_ktvek
	.export	J2C_ktvek
	.malign
S2C_ktvek:
	move	$3(Sp),Vpush
J2C_ktvek:
	.funbegin	C_ktvek
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	comp	Hp,_ehp
	jlth	tkvekgc
	call	GARB
tkvekgc:
	move	1(Sp),r0
	move	1(r0),DTMP		; argument count
	move	0(Sp),r1		; tag pointer
	move	$3(Sp),Sp		; pop first two args and node to update
	move	Hp,ATMP
	move	$VEK,toH
	move	DTMP,toH
tkvek1:
	move	Spop,r0			; pop an AP node
	move	2(r0),toH		; and use the argument
	DECR(DTMP)
	TSTC(DTMP)
	jne	tkvek1
;
	move	$1(Vp),Vp		; ignore pushed Sp
	move	-1(Sp),r0		; next AP node
	move	$TAG,0(r0)		; update it
	move	1(r1),1(r0)
	move	ATMP,2(r0)
	return
	.funend

	.data
	.malign
	.export	C_kpvek
C_kpvek:
	.word	FUN
	.word	VC_kpvek
	.export	VC_kpvek
VC_kpvek:
	.word	$2
	.word	C_kpvek
	.word	unw2
	.word	vunw2
	.word	J2C_kpvek
	.word	S2C_kpvek
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_kpvek
	.export	J2C_kpvek
	.malign
S2C_kpvek:
	move	$3(Sp),Vpush
J2C_kpvek:
	.funbegin	C_kpvek
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; eval tag
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; eval arg count
	comp	Hp,_ehp
	jlth	kpvekgc
	call	GARB
kpvekgc:
	move	1(Sp),r0		; pointer to argument count arg
	move	1(r0),DTMP		; argument count
	move	0(Sp),r1		; pointer to tag arg
	move	$3(Sp),Sp		; pop first two args and node to update
	move	Hp,ATMP			; save pointer to VEK node
	move	$VEK,toH		; make VEK node
	DECR(DTMP)			; last arg in PAIR node
	move	DTMP,toH		; VEK size
kpvek1:
	move	Spop,r0			; pointer to AP node
	move	2(r0),toH		; argument part into the heap
	DECR(DTMP)
	TSTC(DTMP)
	jne	kpvek1
;
	move	$1(Vp),Vp		; pop old Sp
	move	Spop,r0			; pointer to AP node to update (last arg at 8(r0))
	move	ATMP,1(r0)		; fill pointer to VEK node
	move	1(r1),DTMP		; tag #
	; pointer to last arg already in place
	move	$tagtab,ATMP
	ADDAW(DTMP,ATMP)
	move	0(ATMP),0(r0)
	return
	.data
tagtab:
	.word	PAIR0
	.word	PAIR1
	.word	PAIR2
	.word	PAIR3
	.word	PAIR4
	.text
	.funend

	.data
	.malign
	.export	C_ktpair
C_ktpair:
	.word	FUN
	.word	VC_ktpair
	.export	VC_ktpair
VC_ktpair:
	.word	$3
	.word	C_ktpair
	.word	unw3
	.word	vunw3
	.word	J3C_ktpair
	.word	S3C_ktpair
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S3C_ktpair
	.export	J3C_ktpair
	.malign
S3C_ktpair:
	move	$4(Sp),Vpush
J3C_ktpair:
	.funbegin	C_ktpair
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	comp	Hp,_ehp
	jlth	ktpairgc
	call	GARB
ktpairgc:
	move	$PAIR,toH
	move	1(Sp),toH
	move	2(Sp),toH
;
	move	0(Sp),r1
	move	Vpop,Sp
	move	-1(Sp),r0
	move	1(r1),1(r0)
	move	$-3(Hp),2(r0)
	move	$TAG,0(r0)
	return
	.funend


	.data
	.malign
	.export	C_kppair
C_kppair:
	.word	FUN
	.word	VC_kppair
	.export	VC_kppair
VC_kppair:
	.word	$4
	.word	C_kppair
	.word	unw4
	.word	vunw4
	.word	J4C_kppair
	.word	S4C_kppair
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S4C_kppair
	.export	J4C_kppair
	.malign
S4C_kppair:
	move	$5(Sp),Vpush
J4C_kppair:
	.funbegin	C_kppair
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	comp	Hp,_ehp
	jlth	kppairgc
	call	GARB
kppairgc:
	move	$PAIR,toH
	move	1(Sp),toH
	move	2(Sp),toH
	move	3(Sp),ATMP
;
	move	0(Sp),r1
	move	1(r1),DTMP
;
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$-3(Hp),1(r0)
	move	ATMP,2(r0)
	move	$tagtab,ATMP
	ADDAW(DTMP,ATMP)
	move	0(ATMP),0(r0)
	return
	.funend


	.data
	.malign
	.export	C_ksplvek
C_ksplvek:
	.word	FUN
	.word	VC_ksplvek
	.export	VC_ksplvek
VC_ksplvek:
	.word	$2
	.word	C_ksplvek
	.word	unw2
	.word	vunw2
	.word	J2C_ksplvek
	.word	S2C_ksplvek
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C_ksplvek
	.export	J2C_ksplvek
	.malign
S2C_ksplvek:
	move	$3(Sp),Vpush
J2C_ksplvek:
	.funbegin	C_ksplvek
	comp	Hp,_ehp
	jlth	ksplgc
	call	GARB
ksplgc:
;
	move	0(Sp),r0
	move	1(Sp),ATMP
	move	1(ATMP),DTMP		; vector size
	DECR(DTMP)
	move	$2(ATMP),ATMP
kspl:
	move	$AP,toH
	move	r0,toH
	move	0(ATMP),toH
	move	$1(ATMP),ATMP
	move	$-3(Hp),r0
	DECR(DTMP)
	TSTC(DTMP)
	jne	kspl
	
	move	r0,r1
	move	$2(Sp),Sp
	move	Spop,r0
	move	-1(Sp),r0
	move	r1,1(r0)
	move	0(ATMP),2(r0)
	move	$AP,0(r0)
	jumpf	apunw
	.funend

	.data
	.malign
	.export	C_cvectorwrap
C_cvectorwrap:
	.word	FUN
	.word	VC_cvectorwrap
	.export	VC_cvectorwrap
VC_cvectorwrap:
	.word	$1
	.word	C_cvectorwrap
	.word	unw1
	.word	vunw1
	.word	J1C_cvectorwrap
	.word	S1C_cvectorwrap
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C_cvectorwrap
	.export	J1C_cvectorwrap
	.malign
S1C_cvectorwrap:
	move	$2(Sp),Vpush
J1C_cvectorwrap:
	.funbegin	C_cvectorwrap
	move	0(Sp),r0
	CPUSHARG0(r0)
	move	Sp,_ep
	move	Hp,_hp
	CCALL(1,_cvecwrap)
	move	_hp,Hp
	move	_ep,Sp
	move	CRETR,ATMP
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(ATMP),0(r0)
	move	1(ATMP),1(r0)
	move	2(ATMP),2(r0)
	return
	.funend

; M-code part of the great kludge.
; Wrapper function for interactively compiled vectors.  Converts
; register calling convention to ordinary.
	.text
; INDREG points to a specific FunInfo, get the function pointer from there.
; r0 points to dictionary, ARGCREG is an int on entry.
	.export	_jvecwrap
_jvecwrap:
	move	r0,Spush		; push dictionary
	move	Hp,Spush		; push pointer to an INT
	move	$INT,toH		; tag
	move	ARGCREG,toH		; and the value
; Now we have to figure out how many arguments there really are on the stack.
; Use the update address on the V stack to do this (pointer subtraction would do,
; but there is no macro for this.)
	move	$1,ARGCREG		; there are at least 2
	move	0(Vp),ATMP		; get update addr
	move	$-2(ATMP),ATMP		; compensate for these two
jvl:
	add2	$1,ARGCREG
	move	$-1(ATMP),ATMP
	comp	ATMP,Sp
	jgt	jvl
;
	move	VFrefs(INDREG),r0	; pointer to a TAG node (outside the heap)
	move	2(r0),r0		; pointer to real function (may be unevaled)
	move	0(r0),INDREG
	jump	ojfun(INDREG)		; and jump to it

