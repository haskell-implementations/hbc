#include "machdep.M"
#include "../mcode/limit.h"

#include "gc.h"

#define STATISTICS 0
#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif


	.pragma	GC_ON
; _ g c :   i n t e r f a c e   t o   C

	.text
	.export	_gc
CLABEL(_gc)
	CENTRYS(GC_STACK)
	CARG1(r1)
	CARG0(r0)
	move	$0,Vpush
	move	$NGCRET,Vpush
	move	r1,Vpush
	move	r0,Vpush
	move	_ep,Sp
	move	_hp,Hp
	move	_startheap,GCSTART
	move	_endheap,GCEND
	move	_curheap,GCCUR
	move	$-1,_funlink
	move	Sp,gcSp
	jump	gcpop

	.export	gcfun
gcfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
	move	VFpointer(FDISP),0(r1)
; check (& set) the link
gcfunz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	gcfun1
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	jump	gcv2
gcfun1:
	jump	gcpop



	.data
	.export	NGCRET
NGCRET:	.word	GCRET

_funlink:	.word	0
	.export	gcSp
gcSp:	.word	0
	.text

	.export	gcret
; reset all funlinks
gcret:
	move	_funlink,r0
	jump	gcret0
gcret1:
	move	r0,r1
	move	0(r0),r0
	move	$0,0(r1)
        add2	$1,_nfunlink
gcret0:
	comp	r0,$-1
	jne	gcret1
; reset all marked nodes
gcret3:
	comp	Sp,gcSp
	jeq	gcret2
	move	Spop,r0
	move	Spop,0(r0)
	jump	gcret3
gcret2:
	move	Sp,_ep
	move	Hp,_hp
	CRET

	.export	allmoved
	.export	gcpop
	.export	gcd
allmoved:	; node already moved
	move	r0,0(r1)
gcpop:
	move	Vpop,r0
	move	Vpop,r1
gcd:	move	0(r0),INDREG
	jump	ogc(INDREG)


; Special cases for values that have constants in the runtime system.
	.export	gcchr
gcchr:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	gc20
	comp	DTMP,$ MAXCHAR
	jgt	gc20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gcpop

	.export	gcint
gcint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	gc20
	comp	DTMP,$ MAXINTTAB
	jgt	gc20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gcpop

	.export	gctag0
gctag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	gc20
	comp	DTMP,$ MAXTAG0
	jgt	gc20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gcpop


; Node with 2 nonpointer parts
	.export	gc20
gc20:
	SINCR(_ngc20)
	comp	r0,GCSTART
	jlth	allmoved
	comp	r0,GCEND
	jgeh	allmoved

	comp	r0,Hp
	jgeh	gc20i
	comp	r0,GCCUR
	jgeh	allmoved

gc20i:
	SINCR(_ngc20i)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
#ifdef HPROFILE
	move    2(r0),toH               /* slop 1:      DW */
#ifdef SLOP2
        move    3(r0),toH               /* slop 2:      DW */
#endif
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gcpop

; gc a ZAP node (a zapped application)
	.export	gczap
gczap:
	SINCR(_ngczap)
	comp	r0,GCSTART
	jlth	gczap1
	comp	r0,GCEND
	jgeh	gczap1

	comp	r0,Hp
	jgeh	gczapi
	comp	r0,GCCUR
	jgeh	gczap1

gczapi:
	SINCR(_ngczapi)
; r0 points to an overwritten application.
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$-1,toH			; unused
#ifdef HPROFILE
	move	3(r0),toH		; /* slop1:	DW */
#ifdef SLOP2
	move	4(r0),toH		; /* slop2:	DW */
#endif
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	gcfunz
gczap1:
	move	r0,0(r1)
	move	1(r0),FDISP
	jump	gcfunz


; GC of input directory node, call C routine to set ref bit
	.export gcind
gcind:
	SINCR(_ngcind)
	move	r0,Vpush
	move	r1,Vpush
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	Vpop,r1
	move	Vpop,r0
	jump	gc30

; GC of input node, call C routine to set ref bit
	.export gcinp
gcinp:
	SINCR(_ngcinp)
	move	r0,Vpush
	move	r1,Vpush
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	Vpop,r1
	move	Vpop,r0
; and fall into ...

; Node with 3 nonpointer parts
	.export	gc30
gc30:
	SINCR(_ngc30)
	comp	r0,GCSTART
	jlth	allmoved
	comp	r0,GCEND
	jgeh	allmoved

	comp	r0,Hp
	jgeh	gc30i
	comp	r0,GCCUR
	jgeh	allmoved

gc30i:
	SINCR(_ngc30i)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
#ifdef HPROFILE
	move    3(r0),toH               /* slop 1:      DW */
#ifdef SLOP2
        move    4(r0),toH               /* slop 2:      DW */
#endif
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gcpop

; Node with 2 nonpointer and 1 pointer part
	.export	gc21
gc21:
	SINCR(_ngc21)
	comp	r0,GCSTART
	jlth	gc21h
	comp	r0,GCEND
	jgeh	gc21h

	comp	r0,Hp
	jgeh	gc21i
	comp	r0,GCCUR
	jgeh	allmoved

gc21i:
	SINCR(_ngc21)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	Hp,Vpush
#ifdef HPROFILE
#ifdef SLOP2
	move    $3(Hp),Hp                       /* was: 1       DW */
	move	2(r0),Vpush
	move    3(r0),-2(Hp)                    /* slop 1:      DW */
        move    4(r0),-1(Hp)                    /* slop 2:      DW */
#else
	move    $2(Hp),Hp                       /* was: 1       DW */
	move	2(r0),Vpush
	move    3(r0),-1(Hp)                    /* slop 1:      DW */
#endif
#else
	move	$1(Hp),Hp
	move	2(r0),Vpush
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gcpop

	.export	gc21h
gc21h:
	move	0(r0),Spush	; set mark
	move	r0,Spush	; 
	move	$MARKED,0(r0)	;

	move	r0,0(r1)
	move	$2(r0),r1
	move	2(r0),r0
	jump	gcd

; Node with 1 nonpointer and 2 pointer parts
	.export gccap
	.export gcap
#if STATISTICS
gccap:
	SINCR(_ngccap)
	jump	gc12
gcap:
	SINCR(_ngcap)
	; fall into gc12
#else
gcap:
gccap:
#endif
	.export	gc12
gc12:
	SINCR(_ngc12)
	comp	r0,GCSTART
	jlth	gc12h
	comp	r0,GCEND
	jgeh	gc12h

	comp	r0,Hp
	jgeh	gc12i
	comp	r0,GCCUR
	jgeh	allmoved

gc12i:
	SINCR(_ngc12i)
	move	Hp,ATMP
	move	0(r0),toH
#ifdef HPROFILE
#ifdef SLOP2
	move    $4(Hp),Hp                       /* was 2:       DW */
	move    $-3(Hp),Vpush                   /* was -1:      DW */
	move	2(r0),Vpush
	move    $-4(Hp),Vpush                   /* was -2:      DW */
	move	1(r0),Vpush
	move    3(r0),-2(Hp)                    /* slop 1:      DW */
        move    4(r0),-1(Hp)                    /* slop 2:      DW */
#else
	move    $3(Hp),Hp                       /* was 2:       DW */
	move    $-2(Hp),Vpush                   /* was -1:      DW */
	move	2(r0),Vpush
	move    $-3(Hp),Vpush                   /* was -2:      DW */
	move	1(r0),Vpush
	move    3(r0),-1(Hp)                    /* slop 1:      DW */
#endif
#else
	move	$2(Hp),Hp
	move	$-1(Hp),Vpush
	move	2(r0),Vpush
	move	$-2(Hp),Vpush
	move	1(r0),Vpush
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gcpop

	.export	gc12h
gc12h:
	move	0(r0),Spush	; set mark
	move	r0,Spush	; 
	move	$MARKED,0(r0)	;

	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	gcd

; Node with 1 nonpointer and 1 pointer part
	.export	gc11
gc11:
	SINCR(_ngc11)
	comp	r0,GCSTART
	jlth	gc11h
	comp	r0,GCEND
	jgeh	gc11h

	comp	r0,Hp
	jgeh	gc11i
	comp	r0,GCCUR
	jgeh	allmoved

gc11i:
	SINCR(_ngc11i)
	move	Hp,ATMP
	move	0(r0),toH
#ifdef HPROFILE
#ifdef SLOP2
	move	$3(Hp),Hp				/* was 1   	DW */
	move	$-3(Hp),Vpush				/* was -1 	DW */
	move	1(r0),Vpush
	move    2(r0),-2(Hp)                    	/* slop 1:      DW */
	move	3(r0), -1(Hp)				/* slop 2:	DW */
#else
	move	$2(Hp),Hp				/* was 1   	DW */
	move	$-2(Hp),Vpush				/* was -1 	DW */
	move	1(r0),Vpush
	move    2(r0),-1(Hp)                    	/* slop 1:      DW */
#endif
#else
	move	$1(Hp),Hp
	move	$-1(Hp),Vpush
	move	1(r0),Vpush
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gcpop

	.export	gc11h
gc11h:
	move	0(r0),Spush	; set mark
	move	r0,Spush	; 
	move	$MARKED,0(r0)	;

	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	gcd

; Vector apply node
	.export	gcvap
gcvap:
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gcvap1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gcvap2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gcvap1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gcvap2
gcvap1:
#ifdef STINGY
	move	1(r0),ATMP
	comp	VFstingy(ATMP),$0
	jeq	gcvapst1
; Stingy code present
	comp	r0,GCSTART
	jlth	gcvekh
	comp	r0,GCEND
	jgeh	gcvekh

	comp	r0,Hp
	jgeh	gcvapst2
	comp	r0,GCCUR
	jgeh	allmoved

 .export gcstingy
gcstingy:

gcvapst2:
; push all args and jump to the code
	move	r0,Vpush		; save r0
	move	r1,Vpush		; and r1

	move	VFarity(ATMP),DTMP	; number of args
	move	$2(r0),r0
	ADDAW(DTMP,r0)			; pointer to last arg
gcvapst4:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gcvapst3
	move	$-1(r0),r0
	move	0(r0),Spush		; push arg
	jump	gcvapst4
gcvapst3:
	call	jVFstingy(ATMP)
	move	Vpop,r1			; old r1
	move	r0,0(r1)		; redirect pointer
	move	Vpop,r0			; old r0
	move	$MOVED,0(r1)		; retag
	move	r0,1(r1)		; and install redirection
	jump	gcpop

gcvapst1:
	move	VFarity(ATMP),DTMP
	jump	gcvekt
#else /* STINGY */
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gcvekt
#endif /* STINGY */

; Vector node
	.export	gcvek
gcvek:
	SINCR(_ngcvek)
	move	1(r0),DTMP
	; fall into ...
	.export	gcvekt
gcvekt:
	comp	r0,GCSTART
	jlth	gcvekh
	comp	r0,GCEND
	jgeh	gcvekh

	comp	r0,Hp
	jgeh	gcveki
	comp	r0,GCCUR
	jgeh	allmoved
	
gcveki:
	SINCR(_ngcveki)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0
	.export	gcv1
gcv1:
	DECR(DTMP)
	TSTC(DTMP)
#ifdef HPROFILE
	jlt	gcv1end
#else
	jlt	gcpop
#endif
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	gcv1
#ifdef HPROFILE
gcv1end:
#ifdef SLOP2
	move    0(r0),toH		; slop 1:	DW
	move    1(r0),toH		; slop 2:	DW
	move    $2(r0),r0		;
#else
	move    0(r0),toH		; slop 1:	DW
	move    $1(r0),r0		;
#endif
	jump	gcpop
#endif

	.export	gcvekh
gcvekh:	
	move	0(r0),Spush	; set mark
	move	r0,Spush	; 
	move	$MARKED,0(r0)	;

	move	r0,0(r1)
	move	$2(r0),r0
	.export	gcv2
gcv2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gcpop
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	gcv2

; Marked node	
	.export	gcmkd
gcmkd:
	SINCR(_ngcmkd)
	move	r0,0(r1)
	jump	gcpop
	
	.export	gcm
gcm:
	SINCR(_ngcm)
	move	Vpop,0(r1)
	jump	gcpop

; Moved node
	.export	gcmvd
gcmvd:
	SINCR(_ngcmvd)
	move	1(r0),0(r1)
	jump	gcpop

/**************************************************************************/

; Basic vector node
	.export gcbvek
	.export gcdvek
gcdvek:
gcbvek:
	comp	r0,GCSTART
	jlth	allmoved
	comp	r0,GCEND
	jgeh	allmoved

	comp	r0,Hp
	jgeh	gcbvi
	comp	r0,GCCUR
	jgeh	allmoved

gcbvi:
	move	Hp,Vpush	; save new address
	move	1(r0),DTMP
	add2	$2,DTMP		; DTMP is the counter.
	move	r0,ATMP		; ATMP points to the words in the vector.
gcbv1:	move	0(ATMP),toH
	move	$1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jne	gcbv1
	move	$MOVED,0(r0)
	move	Vpop,ATMP
	move	ATMP,1(r0)
	move    ATMP,0(r1)
	jump	gcpop

; INDIR node, just short-circuit it
	.export	gcindi
gcindi:
	SINCR(_ngcindir)
#if 0
;;; old version - can't handle circular indirections
	move	1(r0),r0
	jump	gcd
#else
;;; jsp version
	move	1(r0), r0
	comp	0(r0), $INDIR 			
	jne	gcd
	move	r0, ATMP
	move	1(r0), r0
gcindi1:		
	comp	0(r0), $INDIR
	jne	gcd
	comp	r0, ATMP
	jeq	gcindicircular
	move	1(r0), r0
	comp	0(r0), $INDIR
	jne	gcd
	move	1(r0), r0
	move	1(ATMP), ATMP
	jump	gcindi1	
gcindicircular:
	move	$MOVED, 0(r0)
	move	Hp, 1(r0)
	move	Hp, 0(r1)
	move	Hp, r0
	move	$INDIR, toH
	move	$-1(Hp), toH	
;	move	$HOLE, toH
;	move	Hp, toH	
;	move	Hp, toH	
	jump	gcpop
#endif

	.pragma	GC_OFF
