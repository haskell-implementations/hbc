#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"

#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif


#define MARKNODE(addr)                 \
	move	0(addr),Spush	&& \
	move	addr,Spush	&& \
	move	$MARKED,0(addr)

#define WRITEOUTSIDE(reg)	   \
	move _tp,ATMP 		&& \
	move $-1(ATMP),ATMP 	&& \
	move reg,0(ATMP) 	&& \
	move ATMP,_tp

; The heap:
;                ----------------------------+------------------------------
;  OUTSIDE (low) | PREVIOUS         |  OLD heap    | moved |               |  OUTSIDE (high)
;                ----------------------------+------------------------------
;                ^                  ^              ^       ^
;             GCSTART             GCCUR          GCEND     Hp


; QMPREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to MPOINTERS(a).

#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

#define QMPREVIOUS(kind)                                   \
	comp	r0,GCCUR && jgeh CAT3(majorc,kind,qout) && \
	comp	r0,GCSTART && jlth CAT3(majorc,kind,out)

; MPREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to allmoved.

#define MPREVIOUS(kind)                                 \
	comp	r0,GCCUR && jgeh allmoved            && \
	comp	r0,GCSTART && jlth allmoved

; MPOINTERS(a) falls through if r0 points into the OLD part, or OUTSIDE, the heap,
; otherwise it jumps to allmoved. If ro points OUTSIDE the heap then the node is
; remembered with Writeoutside.

#define MPOINTERS(kind)                                 \
	CAT3(majorc,kind,qout):                         && \
	comp	r0,GCEND && jlth CAT3(majorc,kind,old)  && \
	comp	r0,Hp && jlth allmoved                  && \
	CAT3(majorc,kind,out):                          && \
	WRITEOUTSIDE(r0)                                && \
	CAT3(majorc,kind,old): 

	.pragma	GC_ON

#if 1

#define PRINT(m)
#define PRINTD(m)
#define PRINTDUMP(m)

#else

#define PRINT(m)          \
	move	m,ATMP && \
	call	print

#define PRINTD(m)         \
	move	m,ATMP && \
	call	printd

#define PRINTDUMP(m)      \
	move	m,ATMP && \
	call	printdump

print:
	move	Sp,_ep
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
	move	_ep,Sp
	return

printd:
	move	Sp,_ep
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
	move	_ep,Sp
	return
printdump:
	move	Sp,_ep
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
	move	_ep,Sp
	return

#endif

; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving

	.data
	.export	NMAJORCRET
NMAJORCRET:	.word	GCRET

_funlink:	.word	0
majorcSp:	.word	0
oldGCEND:	.word	0
	.text

; _ g c :   i n t e r f a c e   t o   C
;This is the entry point for a major garbage collection
	
	.export	_majorc
CLABEL(_majorc)
	CENTRYS(GC_STACK)
	CARG1(r1)
	CARG0(r0)
#if defined(sparc) || defined(hppa)
	move	Vp,_garbvp
#endif
	move	$0,Vpush
	move	$NMAJORCRET,Vpush
	move	r1,Vpush
	move	r0,Vpush
	move	_ep,Sp
	move	_hp,Hp
	move	_startheap,GCSTART
;!!!	move	_oldoldhp,GCCUR
	move	GCEND,oldGCEND
	move	Hp,GCEND
	move	$-1,_funlink
	move	$-1,_gclink
	move	Sp,majorcSp
	PRINT($MSG_MAJORC)
	PRINTD(Vp)
	jump	majorcpop

	.export	majorcret
; reset all funlinks

	.data
MSG_MAJORC:	.string "majorc"
MSG_GCRET:	.string "gcret"
	.text
majorcret:
	PRINT($MSG_GCRET)
	move	_funlink,r0
	jump	majorcret0
majorcret1:
	move	r0,r1
	move	0(r0),r0
	move	$0,0(r1)
	add2	$1,_nfunlink
majorcret0:
	comp	r0,$-1
	jne	majorcret1
; reset all marked nodes
majorcret3:
	comp	Sp,majorcSp
	jeq	majorcret2
	move	Spop,r0
	move	Spop,0(r0)
	jump	majorcret3
majorcret2:                  ; Major collection is now finnished
; The heap:
;                ----------------------------+------------------------------
;  OUTSIDE (low) |                  |  OLD heap    | Previous      |       |  OUTSIDE (high)
;                ----------------------------+------------------------------
;                ^                  ^              ^               ^
;             GCSTART             GCCUR          GCEND            Hp

	move	oldGCEND,GCEND
	move	Sp,_ep
	move	Hp,_hp
	CRET

	.export	majorcpop
	.export	majorcd
allmoved:	; node already moved
	move	r0,0(r1)
	PRINT($MSG_ALLMOVED)
majorcpop:
	move	Vpop,r0
	move	Vpop,r1
	PRINTDUMP(r0)
majorcd: move	0(r0),INDREG
	jump	omajorc(INDREG)

.data
thenode: .word 0
thetag: .word 0
therest: .word 0
thehp: .word 0
.text


	.data
MSG_ALLMOVED:	.string "="
MSG_11:	.string "11 "
	.text
; Node with 1 nonpointer and 1 pointer part
	.export	majorc11
majorc11:
	PRINT($MSG_11)
	SINCR(_ngc11)
	QMPREVIOUS(11)
	move	Hp,ATMP
	move	0(r0),toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(11)	
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

	.data
MSG_indi:	.string "indir "
	.text
; Indir node
	.export	majorcindi
majorcindi:
	PRINT($MSG_indi)
	SINCR(_ngcindir)
	QMPREVIOUS(indi)
	move	1(r0),r0
	jump	majorcd
	MPOINTERS(indi)	
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved


; Application node in old format
	.data
MSG_CAP:	.string "cap "
	.text
	.export	majorcapG
majorcapG:
; Constant application node
	.export	majorccap
majorccap:
	PRINT($MSG_CAP)
	SINCR(_ngccap)
	QMPREVIOUS(cap)
	move	Hp,ATMP
	move	0(r0),toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(cap)	
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Node with 1 nonpointer and 2 pointer part
	.data
MSG_12P:	.string "12p"
MSG_12O:	.string "12o"
	.text
	.export	majorc12
majorc12:
	SINCR(_ngc12)
	QMPREVIOUS(12)
	PRINT($MSG_12P)
	move	Hp,ATMP
	move	0(r0),toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(12)	
	PRINT($MSG_12O)
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Application node
	.data
MSG_AP:	.string "@ "
	.text
	.export	majorcap
majorcap:
	PRINT($MSG_AP)
	SINCR(_ngcap)
	QMPREVIOUS(ap)
	move	Hp,ATMP
	move	$APG,toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(ap)
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved


; Special cases for values that have constants in the runtime system.
	.export	majorcchr
majorcchr:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	majorc20
	comp	DTMP,$ MAXCHAR
	jgt	majorc20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	majorcpop

	.export	majorcint
majorcint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	majorc20
	comp	DTMP,$ MAXINTTAB
	jgt	majorc20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	majorcpop

	.export	majorctag0
majorctag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	majorc20
	comp	DTMP,$ MAXTAG0
	jgt	majorc20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	majorcpop


; Node with 2 nonpointer and 0 pointer part
	.data
MSG_20:	.string "20?"
MSG_20P:.string "20p!"
	.text
	.export	majorc20
majorc20:
	PRINT($MSG_20)
	SINCR(_ngc20)
	MPREVIOUS(20)
	PRINT($MSG_20P)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop

; Node with 2 nonpointer and 1 pointer part
	.data
MSG_21:	.string "21 "
	.text
	.export	majorc21
majorc21:
	PRINT($MSG_21)
	SINCR(_ngc21)
	QMPREVIOUS(21)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(21)	
	MARKNODE(r0)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; GC of input directory node, reference bit is already set in the previous minor collection
; must remember if OUTSIDE heap.
	.data
MSG_D:	.string "D "
	.text
	.export majorcind
majorcind:
	PRINT($MSG_D)
	SINCR(_ngcind)
	QMPREVIOUS(ind)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	ATMP,DTMP
	jump majorcpop
	MPOINTERS(ind)
	jump allmoved

; GC of input node, reference bit is already set in the previous minor collection
; must remember if OUTSIDE heap.
	.data
MSG_P:	.string "P "
	.text
	.export majorcinp
majorcinp:
	PRINT($MSG_P)
	SINCR(_ngcinp)
	QMPREVIOUS(inp)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop
	MPOINTERS(inp)
	jump allmoved

; Node with 3 nonpointer and 0 pointer part
	.data
MSG_30:	.string "30 "
	.text
	.export	majorc30
majorc30:
	PRINT($MSG_30)
	SINCR(_ngc30)
	MPREVIOUS(30)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump majorcpop

	.data
MSG_HOLE:	.string "h "
	.text
	.export majorchole
majorchole:
	PRINT($MSG_HOLE)
	SINCR(_ngc30)
	QMPREVIOUS(hole)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	ATMP,DTMP
	jump majorcpop
	MPOINTERS(hole)
	jump allmoved

; Vektor node with pointers
	.data
MSG_VP:	.string "Vp<"
MSG_VO:	.string "V<"
MSG_VO2:.string ">"
	.text
	.export	majorcvek
majorcvek:
	SINCR(_ngcvek)
	move	1(r0),DTMP
majorcvekt:
	QMPREVIOUS(vek)
	PRINT($MSG_VP)
	PRINTD(DTMP)
	PRINT($MSG_VO2)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0
majorcv1:
#if 0
	comp	DTMP,$1000
	jlt	majorcv11
	move	$1,_Gflag
	PRINTD(DTMP)
	move	$-2(r0),r0
	PRINTDUMP(r0)
	CCALL(0,_exit)
#endif
majorcv11:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	majorcpop
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	majorcv11

	MPOINTERS(vek)	
	PRINT($MSG_VO)
	PRINTD(DTMP)
	PRINT($MSG_VO2)
	MARKNODE(r0)
	move	r0,0(r1)
	move	$2(r0),r0
	.export	majorcv2
majorcv2:
#if 0
	comp	DTMP,$1000
	jlt	majorcv21
	move	$1,_Gflag
	PRINTD(DTMP)
	move	$-2(r0),r0
	PRINTDUMP(r0)
	CCALL(0,_exit)
#endif
majorcv21:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	majorcpop
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	majorcv21


; Vektor node with nonpointers
	.data
MSG_DVEK:	.string "v "
	.text
	.export	majorcdvek
majorcdvek:
	PRINT($MSG_DVEK)
	SINCR(_ngcvek)
	MPREVIOUS(dvek)
	move	Hp,Vpush	; save new address
	move	1(r0),DTMP
	add2	$2,DTMP		; DTMP is the counter.
	move	r0,ATMP		; ATMP points to the words in the vector.
majorcbv1:
	move	0(ATMP),toH
	move	$1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jne	majorcbv1
	move	$MOVED,0(r0)
	move	Vpop,ATMP
	move	ATMP,1(r0)
	move    ATMP,0(r1)
	jump	majorcpop

; Marked node
	.data
MSG_MRK:	.string ". "
	.text

	.export	majorcmkd
majorcmkd:
	PRINT($MSG_MRK)
	SINCR(_ngcmkd)
	move	r0,0(r1)
	jump	majorcpop
	
; Moved node
	.data
MSG_MVD:	.string "> "
	.text
	.export	majorcmvd
majorcmvd:
	PRINT($MSG_MVD)
	SINCR(_ngcmvd)
	move	1(r0),0(r1)
	jump	majorcpop

/**************************************************************************/

; Vector apply node
	.data
MSG_VAP:	.string "A "
MSG_VAPG:	.string "AG "
	.text
	.export	majorcvap
majorcvap:
	PRINT($MSG_VAP)
	move    $VAPG,0(r0)
	.export	majorcvapG
majorcvapG:
	PRINT($MSG_VAPG)
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	majorcvap1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
majorcvap2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	majorcvap1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	majorcvap2
majorcvap1:
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	majorcvekt

	.data
MSG_FUN:	.string "f "
	.text
	.export	majorcfun
majorcfun:	
	PRINT($MSG_FUN)
	move	1(r0),FDISP		; p to infovector in FDISP
	move	VFpointer(FDISP),0(r1)
; check (& set) the link
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	majorcfun1
	SINCR(_ngcfun)
; remember this node, it is outside the heap.
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	move	VFpointer(FDISP),r1
	WRITEOUTSIDE(r1)                ; FDISP =?= ATMP
	jump	majorcv2
majorcfun1:
	jump	majorcpop


; majorc a ZAP node (a zapped application)
	.data
MSG_ZAP:	.string "z "
	.text
	.export	majorczap
majorczap:
	PRINT($MSG_ZAP)
	SINCR(_ngczap)
	QMPREVIOUS(zap)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	majorczapz
	MPOINTERS(zap)
	MARKNODE(r0)
	move	1(r0),FDISP		; prepare to do as with a FUN node
majorczapz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	majorcpop
	SINCR(_ngczap)
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	jump	majorcv2

