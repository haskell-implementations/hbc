#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"

#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif

#if 1

#define PRINT(m)
#define PRINTD(m)
#define PRINTDUMP(m)

#else

#define PRINT(m)         	\
 	move	ATMP,Vpush &&	\
	move	m,ATMP &&	\
	call	print &&	\
	move	Vpop,ATMP


#define PRINTD(m)        	\
 	move	ATMP,Vpush &&	\
	move	m,ATMP &&	\
	call	printd &&	\
	move	Vpop,ATMP


#define PRINTDUMP(m)     	\
 	move	ATMP,Vpush &&	\
	move	m,ATMP &&	\
	call	printdump &&	\
	move	Vpop,ATMP

print:
	move	Sp,Vpush
	move	DTMP,Vpush
	move	Hp,Vpush
	move	r1,Vpush
	move	r0,Vpush
	CPUSHARG0(ATMP)
	CCALL(1,_pr)
	move	Vpop,r0
	move	Vpop,r1
	move	Vpop,Hp
	move	Vpop,DTMP
	move	Vpop,Sp
	return

printd:
	move	Sp,Vpush
	move	DTMP,Vpush
	move	Hp,Vpush
	move	r1,Vpush
	move	r0,Vpush
	CPUSHARG0(ATMP)
	CCALL(1,_prd)
	move	Vpop,r0
	move	Vpop,r1
	move	Vpop,Hp
	move	Vpop,DTMP
	move	Vpop,Sp
	return

printdump:
	move	Sp,Vpush
	move	DTMP,Vpush
	move	Hp,Vpush
	move	r1,Vpush
	move	r0,Vpush
	CPUSHARG0(ATMP)
	CCALL(1,_prdump)
	move	Vpop,r0
	move	Vpop,r1
	move	Vpop,Hp
	move	Vpop,DTMP
	move	Vpop,Sp
	return

#endif


; The heap:
;                ------------------------------------------------------------------------------
;  OUTSIDE (low) |                      OLD heap   |     | from  |               | to |       | OUTSIDE (high)
;                ------------------------------------------------------------------------------
;                                                  ^                                  ^       ^
;                                               GCSTART                               Hp     GCEND

#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

; Previous(a) falls through if r0 points into the previous part of the heap,
; otherwise it jumps to allmoved.

#define PREVIOUS        \
	comp	r0,GCSTART 	&& \
	jlth    allmoved 	&& \
	comp	r0,GCEND 	&& \
	jgeh    allmoved


	.pragma	GC_ON

; ATMP -> pointer to tag entry of this node
; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving
; Hp     = new heap pointer

	.data
tmpTp:		.word  	0
	.text

; _ g c :   i n t e r f a c e   t o   C

/****************************************/
/*   gen2mall(void)  uses _tp,_oldhp,_hp,_ep */

	.text
	.export _gen2mall
CLABEL(_gen2mall)
	CENTRYS(GC_STACK)
	PRINT($MSG_GEN2MALL)

	move	_ep,Sp

	move	_hp,Hp
	move	_tp,tmpTp

	move	_oldhp,GCSTART
	move	_endheap,GCEND

#if 0
	move	GCSTART,ATMP
	call	printdF
	move	GCEND,ATMP
	call	printdF
#endif

	PRINT($MSG_GEN2OALL)	; fall through to g2lgsret

/****************************************************************/
/*   g2mgsret is the code that are called when all pointers	*/
/*   on the Vp-stack are garbage collected, during garbage 	*/
/*   collection of the tp-table.	          		*/
/*   Note only pointers outside previous in tp here!            */
/****************************************************************/

	.export	g2mgsret
g2mgsret:			; Get next pointer, if it exist
				; Entries in table must point at nodes that existed befor gc started
	move	tmpTp,r1	; that is: outside, old or previous
	comp	r1,_endtable
	jge	gen2o_ep	; No more pointers here, garbage collect the stack
	move	0(r1),r0
	move	$1(r1),tmpTp

	move	$0,Vpush
	move	$NGEN2SRET,Vpush
	PRINTDUMP(r0)

#if 0
	move	r0,ATMP
	call	printdumpF
#endif

	move	0(r0),ATMP
	jump	ogen2l(ATMP)	; look at node


/******** gen2o(ep) ************/

	.data
dummy:		.word	0

	.text
gen2o_ep:
	PRINT($MSG_GEN2O_EP)

	move	$1,Vpush
	move	$NGEN2CRET,Vpush
	move	$dummy,r1
	move	_ep,r0
	jump	g2lvek  /* No need to dispatch */

/***** Code used by ogen2o() and ogen2m() *****************/

	.export	gen2mpop
	.export g2mOut
/**** do nothing g2l__ ****/
	.export	g2l20
g2l20:
	.export	g2l30
g2l30:
	.export	g2lfun
g2lfun:
	.export	g2ldvek
g2ldvek:
/**************************/
allmoved:			; node already moved
g2mOut:                         ; nodes that must be outside, i.e., FUN
	move	r0,0(r1)	; Set pointer to node
gen2mpop:
	move	Vpop,r0
	move	Vpop,r1
	PRINTD(Hp)
	PRINTDUMP(r0)

	move	0(r0),ATMP
	jump	ogen2m(ATMP)



/****************************************************************/
/*   g2mgcret is the code that are called when all pointers	*/
/*   on the Vp-stack are garbage collected, during garbage	*/
/*   collection of ep (, the stack).	          		*/
/****************************************************************/

	.export	g2mgcret
g2mgcret:

	move	Hp,_hp		; Save new hp
	move	Sp,_ep		; Sp is same as when we started, or we are in troubles.
	PRINT($MSG_GEN2END)
	CRET
	.data

/***** Some messages *******/

	.malign
MSG_mark:	 .string "mark:"
MSG_GEN2MALL:	 .string "gen2mall:"
MSG_GEN2OALL:	 .string "gen2oall:"
MSG_GEN2O_EP:	 .string "gen2o(ep):"
MSG_GEN2END:	 .string "gen2mall end:"
	.malign
	.text


/**************************************************************************/
; ATMP -> pointer to tag entry of this node
; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving
; Hp     = new heap pointer

; g2l__     push all pointers
; g2m__     move to hp

/************  Fix size nodes ********************/

; Node with 1 nonpointer and 1 pointer part
	.export	g2m11
	.export	g2l11
g2m11:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	r0,1(r0)
	move	ATMP,0(r1)
	jump gen2mpop

g2l11:
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

; Node with 1 nonpointer and 2 pointer part
	.export	g2m12
	.export	g2l12
g2m12:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH
	move	$1(Hp),Vpush
	move	2(r0),Vpush
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$2(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2mpop

g2l12:
	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

; Node with 2 nonpointer and 0 pointer part
	.export	g2m20

g2m20:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2mpop

; Node with 2 nonpointer and 1 pointer part
	.export	g2m21
	.export	g2l21
g2m21:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2mpop

g2l21:
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Node with 3 nonpointer and 0 pointer part
	.export	g2m30

g2m30:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2mpop

/************  Special fixed size nodes **************/

; Indir node
;               g2mindir should never happen
	.export	g2lindir

g2lindir:                   ; Think more here, can't do much better but it will probably never happend anyway.
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

/************  Function nodes (except VAP) ********************/
; FUN node
;		g2mfun = g2mOut
;		g2lfun = allmoved

	.data
	.malign
MSG_FUN:	.string	"gslfun"
	.malign
	.text

; ZAP node (a zapped application)
;	.export	g2mzap  g2m30
;	.export	g2lzap  g2l30

/************  Varying size nodes ********************/

; Vektor node with non-pointers
	.export	g2mdvek

g2mdvek:
	PREVIOUS
	move	Hp,ATMP		; save new address
	move	0(r0),toH
	move	1(r0),DTMP
	move	DTMP,toH
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	jump	g2ndvekT
g2ndvekL:
	move	0(r0),toH
	move	$1(r0),r0
g2ndvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	g2ndvekL
	jump	gen2mpop

; Vektor node with pointers
	.export	g2mvek
	.export	g2lvek
g2mvek:
	PREVIOUS
	move	Hp,ATMP
	move	0(r0),toH	; Copy tag
	move	1(r0),DTMP	; and size
	move	DTMP,toH
gen2mvekE:
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	jump	gen2mvekT
gen2mvekL:
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
gen2mvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2mvekL
	jump	gen2mpop

g2lvek:
	move	1(r0),DTMP
gen2lvekE:
	move	r0,0(r1)
	move	$2(r0),r0
	jump	gen2lvekT
gen2lvekL:
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
gen2lvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2lvekL
	jump	gen2mpop

; Vector apply node
	.export	g2mvap
	.export	g2lvap
g2mvap:
	PREVIOUS
	move	0(r0),toH	; Copy tag
	move	1(r0),ATMP	; and function
	move	ATMP,toH
	move	VFarity(ATMP),DTMP
	move	$-2(Hp),ATMP
	jump	gen2mvekE  ; r0 = Old address, ATMP = new address, DTMP = size

g2lvap:
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gen2lvekE ; r0 = address, r1 = pointer, DTMP = size

/***************   Special gc nodes  *************/

; Moved node
	.export	g2mmvd
g2mmvd:
;	No need for previous as never outside
	move	1(r0),0(r1)
	jump	gen2mpop

	.export	g2lmvd   ; This should never happen, but it does!
g2lmvd:
	move	1(r0),0(r1)
	jump	gen2mpop

	.data
	.malign
MSG_MOVED:	.string "g2lmvd"
	.malign

/**************************************************************************/

