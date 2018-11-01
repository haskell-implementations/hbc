#include "../runtime/machdep.M"
#include "../runtime/tagtable.h"

#define WRITEUPDATED(reg,label) && \
	move _tp,ATMP           && \
	comp ATMP,$0 		&& \
	jeq label 		&& \
	comp reg,_starthp	&& \
	jge  label		&& \
	move $-1(ATMP),ATMP 	&& \
	move reg,0(ATMP) 	&& \
	move ATMP,_tp           && \
	comp ATMP,_etp		&& \
	jgt  label		&& \
	move $0,_ehp		&& \
	move $0,_tp		&& \
label:

	.data
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#else
	.word	_USE_none
#endif

	.data
	.malign
	.export	CPsarray
CPsarray:
	.word	FUN
	.word	VCPsarray
	.export	VCPsarray
VCPsarray:
	.word	$4
	.word	CPsarray
	.word	unw4
	.word	vunw4
	.word	J4CPsarray
	.word	S4CPsarray
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$4
	.word	CPfail_undefined_elem
	.word	CPfail_multiple_index
	.word	CPfail_out_of_range
	.word	CPfail_funny_bounds
	.text
	.export	S4CPsarray
	.export	J4CPsarray
	.malign
S4CPsarray:
	move	$5(Sp),Vpush
J4CPsarray:
	.funbegin	CPsarray
#define	lo 0(Sp)
#define Pvek 0(Sp)
#define hi 1(Sp)
#define val 1(Sp)
#define offs 2(Sp)
#define list 3(Sp)
#define Blo 0(Vp)
#define Bsize 1(Vp)
#define Boffs 2(Vp)
#define Bno_of_GCs 3(Vp)
#define BASICVARSIZE 4

	; allocate local vars
	move	$-BASICVARSIZE(Vp),Vp
	move	$-1,Bno_of_GCs      	; Will force the first writeupdated
	; EVAL offs
	move	offs,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),Boffs		; save offset
	; EVAL lo
	move	lo,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),Blo		; save lower bound
	; EVAL hi
	move	hi,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),ARGCREG
	sub2	Blo,ARGCREG		; compute Bsize
	add2	$1,ARGCREG
	move	ARGCREG,Bsize		; save size
	comp	ARGCREG,$0
	jlt	funnybnds
	; MEMCHECK size+VEKhead(2)
	add2	$20,ARGCREG		; some slop
	call	NGARB
	
	; allocate the array
	move	Hp,Pvek			; save pointer to final vector
	move	$TAG,toH
	move	Blo,toH
#ifdef HPROFILE
#ifdef SLOP2
	move	$3(Hp),toH
	move	$array_Array_LABEL,toH
	move 	$0,toH
#else
	move	$2(Hp),toH
	move	$array_Array_LABEL,toH
#endif
#else
	move	$1(Hp),toH
#endif
	move	$VEK,toH
	move	Bsize,DTMP
	move	DTMP,toH

	; loop through the vector and fill it with the undef message
	move	$CPfail_undefined_elem,r0
fills:
	move	r0,toH
	DECR(DTMP)
	TSTC(DTMP)
	jgt	fills
#ifdef HPROFILE
	move	$array_Array_LABEL,toH		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0,toH				/* slop 2:	DW 	*/
#endif
#endif

	; run through the list and put each value in it's place
lfills:
	move	list,r0			; pointer to index-value list
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate list
#if 1
	move	0(r0),ATMP
	comp	oconstr(ATMP),$ O_TAG0
#else
	comp	0(r0),$TAG0	; nil ?
	jeq	LTnils
	comp	0(r0),$TAG0_1	; gc-gen2
#endif
	jeq	LTnils
	move	2(r0),list		; save the tail of the list
	move	1(r0),r0		; head of list
	move	0(r0),INDREG
	call	oeval(INDREG)		; eval the pair
	move	2(r0),val		; save value
	move	1(r0),r0		; index
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate index	
	move	1(r0),DTMP		; the integer index
	sub2	Boffs,DTMP		; subtract offset
	sub2	Blo,DTMP		; subtract lower bound
	comp	DTMP,$0			; and check
	jlt	badix
	comp	DTMP,Bsize		; check upper bound
	jge	badix
	move	Pvek,ATMP		; pointer to vector header
	move	2(ATMP),ATMP		; pointer to VEK node
	ADDAW(DTMP,ATMP)		; next slot to fill
	comp	2(ATMP),$CPfail_undefined_elem
	jne	dupix
	move	val,2(ATMP)		; fill (2 is to compensate for VEK&size)
	move	_no_of_GCs,DTMP
	comp	DTMP,Bno_of_GCs	; Any garbage collection?
	jeq	lfills
	move	DTMP,Bno_of_GCs	; Yes, repeat writeback!
	move	Pvek,DTMP
	WRITEUPDATED(DTMP,sarrayupd)
	jump	lfills

; we are done, just return the vector
LTnils:
	move	Pvek,ATMP
	move	$ BASICVARSIZE(Vp),Vp
	move	Vpop,Sp
	move	-1(Sp),r0
	move	0(ATMP),0(r0)
	move	1(ATMP),1(r0)
	move	2(ATMP),2(r0)
#ifdef HPROFILE
	move	3(ATMP),3(r0)
#ifdef SLOP2
	move 	4(ATMP),4(r0)
#endif
#endif
	return	

badix:
	move	$3(Sp),Sp
	jumpf	J0CPfail_out_of_range

dupix:
	move	$3(Sp),Sp
	jumpf	J0CPfail_multiple_index

funnybnds:
	move	hi,2(Sp)
	move	lo,1(Sp)
	move	$1(Sp),Sp
	move	$ BASICVARSIZE(Vp),Vp
	jumpf	J2CPfail_funny_bounds


#undef lo
#undef hi
#undef list
#undef val
#undef Blo
#undef Bsize
#undef Pv
#undef Pvek
#undef BASICVARSIZE
	.funend

	.data
	.malign
	.export	CParray
CParray:
	.word	FUN
	.word	VCParray
	.export	VCParray
VCParray:
	.word	$4
	.word	CParray
	.word	unw4
	.word	vunw4
	.word	J4CParray
	.word	S4CParray
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	C_elemfilter
	.text
	.export	S4CParray
	.export	J4CParray
	.malign
S4CParray:
	move	$5(Sp),Vpush
J4CParray:
	.funbegin	CParray

; Arguments to Parray: low index, high index, reduce function, index-value list
#define	lo 0(Sp)
#define hi 1(Sp)
#define	reducefun 2(Sp)
#define list 3(Sp)

; Local variables in Parray
#define Blo 0(Vp)
#define Bhi 1(Vp)
#define Bindex 2(Vp)
#define Bsize 3(Vp)
#define Pva 4(Vp)
#define Pv 5(Vp)
#define Pvek 6(Vp)
#define Pauxvek 7(Vp)

; Size of locals
#define BASICVARSIZE 8

; Offsets into the aux vector for lo (the lower bound) and the list
#define offset_list 2
#define offset_lo 3

	; allocate local vars
	move	$-BASICVARSIZE(Vp),Vp

	; EVAL lo
	move	lo,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),Blo		; save lower bound

	; EVAL hi
	move	hi,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),Bhi		; save upper bound

	; compute Bsize
	move	Bhi,Bsize
	sub2	Blo,Bsize
	add2	$1,Bsize		; save size

	comp	Bsize,$1
	jge	Ll0
	move	lo,2(Sp)
	move	hi,3(Sp)
	move	$2(Sp),Sp
	move	$ BASICVARSIZE(Vp),Vp
	jumpf	J2CPfail_funny_bounds

Ll0:
	; MEMCHECK (INT(4)+AP(5)+VAP(6)+2)*size+VEKhead(4)+auxVEKhead(6) HPROFILE SLOP2
	; MEMCHECK (INT(3)+AP(4)+VAP(5)+2)*size+VEKhead(3)+auxVEKhead(5) HPROFILE
	; MEMCHECK (INT(2)+AP(3)+VAP(4)+2)*size+VEKhead(2)+auxVEKhead(4)
	move	Bsize,ARGCREG
#ifdef HPROFILE
#ifdef SLOP2
	mul2	$17,ARGCREG		; size needed for vector and aux vector
	add2	$10,ARGCREG
#else
	mul2	$14,ARGCREG		; size needed for vector and aux vector
	add2	$8,ARGCREG
#endif
#else
	mul2	$11,ARGCREG		; size needed for vector and aux vector
	add2	$6,ARGCREG
#endif
	call	NGARB
	
	; allocate the array
	move	Hp,Pvek			; save pointer to final vector
	move	$VEK,toH
	move	Bsize,toH
	move	Hp,Pv			; init pointer into the vector
	move	Bsize,DTMP
	ADDAW(DTMP,Hp)
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif

	; allocate the auxilliary array
	move	Hp,Pauxvek		; save pointer to aux vector
	move	$VEK,toH
	move	Bsize,toH
	add2	$2,-1(Hp)
	move	list,toH
	move	lo,toH
	move	Hp,Pva			; init pointer into the aux vector
	move	Bsize,DTMP
	ADDAW(DTMP,Hp)
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif

	; initialize the loop
	move	Blo,Bindex
Lloopstart:
	comp 	Bindex,Bhi
	jgt	Lloopend

		; constr INT-node: Bindex
		move	$INT,toH
		move	Bindex,toH
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
		move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif
		; fill in aux-array location to point at
		; beginning of VAP node (below)
		move	Pva,ATMP
		move	Hp,0(ATMP)
		; constr VAP-node: elemfilter index auxvek
		move	$VAP,toH
		move	$VC_elemfilter,toH
#ifdef HPROFILE
#ifdef SLOP2
		move	$-6(Hp),toH
#else /* SLOP2 */
		move	$-5(Hp),toH
#endif /* SLOP2 */
#else /* HPROFILE */
		move	$-4(Hp),toH
#endif
		move	Pauxvek,toH
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
		move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif
		; fill in array-location to point at
		; beginning of AP node (below)
		move	Pv,ATMP
		move	Hp,0(ATMP)
		; constr AP-node: reducefun vap-node
		move	$AP,toH
		move	reducefun,toH
#ifdef HPROFILE
#ifdef SLOP2
		move	$-8(Hp),toH
#else /* SLOP2 */
		move	$-7(Hp),toH
#endif /* SLOP2 */
#else /* HPROFILE */
		move	$-6(Hp),toH
#endif
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
		move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif

		add2	$1,Bindex	; increment index
		move	Pv,ATMP		; increment pointer into vector
		move	$1(ATMP),Pv
		move	Pva,ATMP	; increment pointer into aux vector
		move	$1(ATMP),Pva

		jump	Lloopstart

Lloopend:
	move	Blo,DTMP
	move	Pvek,ATMP
	move	$ BASICVARSIZE(Vp),Vp
	move	Vpop,Sp
	move	-1(Sp),r0
	move	DTMP,1(r0)
	move	ATMP,2(r0)
#ifdef HPROFILE
	move	$array_Array_LABEL, 3(r0)		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, 4(r0)				/* slop 2:	DW 	*/
#endif
#endif
	move	$TAG,0(r0)
	.export	assocarrayreturn
assocarrayreturn:
	return
	.funend


	.data
	.malign
C_elemfilter:
	.word	FUN
	.word	VC_elemfilter
VC_elemfilter:
	.word	$2
	.word	C_elemfilter
	.word	unw2
	.word	vunw2
	.word	J2C_elemfilter
	.word	S2C_elemfilter
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.malign
S2C_elemfilter:
	move	$3(Sp),Vpush
J2C_elemfilter:
	.funbegin	C_elemfilter


	move	1(Sp),r0		; access the array
	move	offset_list(r0),Spush	; push the list
	move	r0,Spush		; location for tail
	move	r0,Spush		; location for v
	move	r0,Spush		; location for i
	move	r0,Spush		; location for TOUPDATE
#undef list
#define array 6(Sp)
#define index 5(Sp)
#define list 4(Sp)
#define tail 3(Sp)
#define v 2(Sp)
#define i 1(Sp)
#define TOUPDATE 0(Sp)
#define Bno_of_GCs2 0(Vp)
#define BASICVARSIZE2 1	

	; allocate local vars
	move	$-BASICVARSIZE2(Vp),Vp
	move	$-1,Bno_of_GCs2      	; Will force the first writeupdated
elemfilter2:
	; eval list
	move	list,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

#if 1
	move	0(r0),ATMP
	comp	oconstr(ATMP),$ O_TAG0
#else
	comp	0(r0),$TAG0	; nil ?
	jeq	LTnil
	comp	0(r0),$TAG0_1	; gc-gen2
#endif
	jeq	LTnil
	move	2(r0),tail
	move	1(r0),r0		; head of list

	; eval the pair
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	1(r0),i			; index
	move	2(r0),v			; value

	; eval i
	move	i,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	; MEMCHECK
	comp	Hp,_ehp
	jlth	LT9
	move	r0,Spush
	call	GARB
	move	Spop,r0
LT9:
	move	_no_of_GCs,DTMP
	comp	DTMP,Bno_of_GCs2
	jeq	LT11
	move	DTMP,Bno_of_GCs2
	move	array,DTMP
	WRITEUPDATED(DTMP,Larray0)
LT11:

#define OFFSETREG DTMP
#define ARRAYREG ATMP

	move	1(r0),OFFSETREG

	move	array,ARRAYREG
	move	offset_lo(ARRAYREG),r1
	sub2	1(r1),OFFSETREG		; OFFSETREG := i - lo
	comp	OFFSETREG,$0		; if offset < 0 (ie i < lo) ...
	jlt	LTnext			; .. ignore this element
	add2	$2,OFFSETREG		; compensate for list and lo
	comp	OFFSETREG,1(ARRAYREG)	; if offset >= vector size (ie, i > hi), ...
	jge	LTnext			; .. ignore this element


	ADDAW(OFFSETREG,ARRAYREG)	; 2(ATMP) holds the pointer to update
	move	2(ARRAYREG),TOUPDATE

	move	Hp,2(ARRAYREG)		; aux-vek entry point to new filter node

	move	$VAP,toH
	move	$VC_elemfilter,toH
	move	i,toH			; ... of this index
	move	array,toH		; the array
#ifdef HPROFILE
	move	$array_Array_LABEL, toH		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, toH				/* slop 2:	DW 	*/
#endif
#endif

	move	TOUPDATE,r0
	move	$PAIR1,0(r0)
	move	v,1(r0)
#ifdef HPROFILE
#ifdef SLOP2
	move	$-6(Hp),2(r0)
#else /* SLOP2 */
	move	$-5(Hp),2(r0)
#endif /* SLOP2 */
#else /* HPROFILE */
	move	$-4(Hp),2(r0)
#endif
#ifdef HPROFILE
	move	$array_Array_LABEL, 3(r0)		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, 4(r0)				/* slop 2:	DW 	*/
#endif
#endif
	WRITEUPDATED(r0,Lelem)

	; now was the index which we just updated
	; the one we asked for?
	.export	filternext1
filternext1:
	move	index,r1
	move	i,ATMP
	comp	1(r1),1(ATMP)
	jeq	LT10
	; no:
LTnext:
	move	tail,list
	jump	elemfilter2
LT10:
	; yes: orderly return:
	; first replace the list in the array by the tail.
	move	array,ATMP
	move	tail,offset_list(ATMP)
	move	array,DTMP
	WRITEUPDATED(DTMP,Larray1)
	move	$ BASICVARSIZE2(Vp),Vp
	move	Vpop,Sp
	; now TOUPDATE must be in r0!
	return

LTnil:
	; the list is in r0
	move	array,ATMP
	move	r0,offset_list(ATMP)
	move	array,DTMP
	WRITEUPDATED(DTMP,Larray2)
	move	$ BASICVARSIZE2(Vp),Vp
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$0,1(r0)
#ifdef HPROFILE
	move	$array_Array_LABEL, 2(r0)		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, 3(r0)				/* slop 2:	DW 	*/
#endif
#endif
	move	$TAG0,0(r0)
	return


;
;	T H E   I N D E X   O P E R A T O R 
;
	.data
	.malign
	.export	CPaindex
CPaindex:
	.word	FUN
	.word	VCPaindex
	.export	VCPaindex
VCPaindex:
	.word	$2
	.word	CPaindex
	.word	unw2
	.word	vunw2
	.word	J2CPaindex
	.word	S2CPaindex
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$2
	.word	CPfail_index_too_small
	.word	CPfail_index_too_big
	.text
	.export	S2CPaindex
	.export	J2CPaindex
	.malign
S2CPaindex:
	move	$3(Sp),Vpush
J2CPaindex:
	.funbegin	CPaindex
#undef i
#define a 0(Sp)
#define i 1(Sp)
	move	a,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	i,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	.export index1
index1:
	move	a,ATMP
	move	1(r0),DTMP
	sub2	1(ATMP),DTMP
	comp	DTMP,$0
	jge	LT30

	; index out of bounds (too small).
	move	2(ATMP),Spush
	jumpf	J3CPfail_index_too_small
LT30:
	move	2(ATMP),ATMP
	comp	DTMP,1(ATMP)
	jlt	LT31
	; index out of bounds (too big)
	move	ATMP,Spush
	jumpf	J3CPfail_index_too_big

LT31:
	ADDAW(DTMP,ATMP)
	move	2(ATMP),r0
	move	$2(Sp),Sp
	jumpf	evalupdunw
	.funend

;
;	T H E   U P D A T E   O P E R A T O R 
;
	.data
	.malign
	.export	CPaupdate
CPaupdate:
	.word	FUN
	.word	VCPaupdate
	.export	VCPaupdate
VCPaupdate:
	.word	$3
	.word	CPaupdate
	.word	unw3
	.word	vunw3
	.word	J3CPaupdate
	.word	S3CPaupdate
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$2
	.word	CPfail_index_too_small
	.word	CPfail_index_too_big
	.text
	.export	S3CPaupdate
	.export	J3CPaupdate
	.malign
S3CPaupdate:
	move	$4(Sp),Vpush
J3CPaupdate:
	.funbegin	CPaupdate
#undef i
#define a 0(Sp)
#define i 1(Sp)
#define x 2(Sp)
	move	a,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	i,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	.export update1
update1:
	move	a,ATMP
	move	1(r0),DTMP
	sub2	1(ATMP),DTMP
	comp	DTMP,$0
	jge	LTU30

	; index out of bounds (too small).
	move	2(ATMP),Spush
	jumpf	J3CPfail_index_too_small
LTU30:
	move	2(ATMP),ATMP
	comp	DTMP,1(ATMP)
	jlt	LTU31
	; index out of bounds (too big)
	move	ATMP,Spush
	jumpf	J3CPfail_index_too_big

LTU31:
	ADDAW(DTMP,ATMP)
	move	x,2(ATMP)
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$TAG0,0(r0)
	move	$0,1(r0)
#ifdef HPROFILE
	move	array_Array_LABEL,2(r0)
#ifdef SLOP2
	move 	$0,3(r0)
#endif
#endif
	return
	.funend

; lower bound
	.data
	.malign
	.export	CPlowerbound
CPlowerbound:
	.word	FUN
	.word	VCPlowerbound
	.export	VCPlowerbound
VCPlowerbound:
	.word	$1
	.word	CPlowerbound
	.word	unw1
	.word	vunw1
	.word	J1CPlowerbound
	.word	S1CPlowerbound
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPlowerbound
	.export	J1CPlowerbound
	.malign
S1CPlowerbound:
	move	$2(Sp),Vpush
J1CPlowerbound:
	.funbegin	CPlowerbound

	move	a,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	a,ATMP
	move	1(ATMP),DTMP

	move	Vpop,Sp
	move	-1(Sp),r0
	move	DTMP,1(r0)
#ifdef HPROFILE
	move	$array_Array_LABEL, 2(r0)		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, 3(r0)				/* slop 2:	DW 	*/
#endif
#endif
	move	$INT,0(r0)

	return
	.funend

; upper bound
	.data
	.malign
	.export	CPupperbound
CPupperbound:
	.word	FUN
	.word	VCPupperbound
	.export	VCPupperbound
VCPupperbound:
	.word	$1
	.word	CPupperbound
	.word	unw1
	.word	vunw1
	.word	J1CPupperbound
	.word	S1CPupperbound
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPupperbound
	.export	J1CPupperbound
	.malign
S1CPupperbound:
	move	$2(Sp),Vpush
J1CPupperbound:
	.funbegin	CPupperbound

	move	a,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	a,ATMP
	move	1(ATMP),DTMP
	move	2(ATMP),ATMP
	add2	1(ATMP),DTMP
	sub2	$1,DTMP

	move	Vpop,Sp
	move	-1(Sp),r0
	move	DTMP,1(r0)
#ifdef HPROFILE
	move	$array_Array_LABEL, 2(r0)		/* slop 1:	DW	*/
#ifdef SLOP2
	move 	$0, 3(r0)				/* slop 2:	DW 	*/
#endif
#endif
	move	$INT,0(r0)
	return
	.funend


	.data
	.malign
	.export	CPmkarray
CPmkarray:
	.word	FUN
	.word	VCPmkarray
	.export	VCPmkarray
	.word	S3CPmkarray
VCPmkarray:
	.word	$3
	.word	CPmkarray
	.word	unw3
	.word	vunw3
	.word	J3CPmkarray
	.word	S3CPmkarray
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S3CPmkarray
	.export	J3CPmkarray
	.malign
S3CPmkarray:
	move	$4(Sp),Vpush
J3CPmkarray:
	.funbegin	CPmkarray
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate lower bound

	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate upper bound

	move	1(r0),ARGCREG		; size = upper
	move	0(Sp),r0
	sub2	1(r0),ARGCREG		;        - lower
	add2	$20,ARGCREG		; some slop
	call	NGARB

	move	1(Sp),r0
	move	1(r0),DTMP
	move	0(Sp),r0
	sub2	1(r0),DTMP
	add2	$1,DTMP			; the size
	move	2(Sp),r0		; the filler value

	move	Hp,ATMP			; save vector address

	move	$VEK,toH
	move	DTMP,toH
fill:
	move	r0,toH
	DECR(DTMP)
	TSTC(DTMP)
	jgt	fill
#ifdef HPROFILE
	move	$array_Array_LABEL,toH
#ifdef SLOP2
	move 	$0,toH
#endif
#endif
	move	0(Sp),r0
	move	1(r0),DTMP
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$TAG,0(r0)
	move	DTMP,1(r0)
	move	ATMP,2(r0)
#ifdef HPROFILE
	move	array_Array_LABEL,3(r0)
#ifdef SLOP2
	move 	$0,4(r0)
#endif
#endif
	return
	.funend

#ifdef HPROFILE

	.malign
array_elemfilter_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word	$Array_LABEL
	.word	$array_LABEL
	.word	$elemfilter_LABEL
	.word	$elemfilter_LABEL

	.malign
array_Array_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word   $array_LABEL
	.word   $Array_LABEL
	.word   $Array_LABEL

	.malign
array_Int_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word	$Array_LABEL
	.word 	$array_LABEL
	.word	$Int_LABEL
	.word	$Int_LABEL

	.malign
array_NIL_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word	$Array_LABEL
	.word 	$array_LABEL
	.word	$NIL_LABEL
	.word	$NIL_LABEL

	.malign
array_arrayf_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$array_LABEL
	.word	$arrayf_LABEL
	.word	$arrayf_LABEL

	.malign
elemfilter_nil_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$elemfilter_LABEL
	.word	$Nil_LABEL
	.word	$List_LABEL

	.malign
elemfilter_elemfilter_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$elemfilter_LABEL
	.word 	$elemfilter_LABEL
	.word 	$elemfilter_LABEL

	.malign
elemfilter_arrayf_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$elemfilter_LABEL
	.word	$arrayf_LABEL
	.word	$arrayf_LABEL

	.malign
lowerbound_INT_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$lowerbound_LABEL
	.word	$Int_LABEL
	.word	$Int_LABEL

	.malign
upperbound_INT_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $Array_LABEL
	.word 	$upperbound_LABEL
	.word	$Int_LABEL
	.word	$Int_LABEL


array_LABEL: 		.string "array"

elemfilter_LABEL:	.string "elemfilter"

lib_LABEL:		.string "lib"

lowerbound_LABEL:	.string "lowerbound"

upperbound_LABEL:	.string "upperbound"

Array_LABEL:		.string "Array"

List_LABEL:		.string "List"

Nil_LABEL:		.string "[]"

NIL_LABEL:		.string "NIL"

Int_LABEL:		.string "Int"

arrayf_LABEL:		.string "array.f"


#endif

