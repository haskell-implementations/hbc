#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"

#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif

#define DEBUGPRINT 0

#if !DEBUGPRINT

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

#endif

	.text
	.export _gensall
CLABEL(_gensall)
	CENTRYS(GC_STACK)
	CARG1(r1)           ; endtable when called
	CARG0(r0)           ; tp when called
	PRINT($MSG_GENSALL)
	move	r0,IndPtr
	move	r1,EndIndPtr
	move	_ep,Sp
	move	_hp,Hp
	move	_startheap,GCSTART
	move	_endheap,GCEND
	move	$-1,_funlink
	move	$-1,_gclink
	move	Sp,gencSp
	jump	scanning

	.export	_gens
CLABEL(_gens)
	CENTRYS(GC_STACK)
	CARG1(r1)
	CARG0(r0)
	move	$0,Vpush
	move	$NGENCRET,Vpush
	move	r1,Vpush
	move	r0,Vpush
	move	_ep,Sp
	move	_hp,Hp
	move	_startheap,GCSTART
	move	_endheap,GCEND
	move	$-1,_funlink
	move	$-1,_gclink
	move	Sp,gencSp
	move	Vpop,r0
	move	Vpop,r1
	move	0(r0),INDREG
	jump	ogens(INDREG)

; _ g c :   i n t e r f a c e   t o   C

	.export	_genc
CLABEL(_genc)
	CENTRYS(GC_STACK)
	CARG1(r1)
	CARG0(r0)
#if defined(sparc) || defined(hppa)
	move	Vp,_garbvp
#endif
	move	$0,Vpush
	move	$NGENCRET,Vpush
	move	r1,Vpush
	move	r0,Vpush
	move	_ep,Sp
	move	_hp,Hp
	move	_startheap,GCSTART
	move	_endheap,GCEND
	move	Hp,GCCUR
	move	$-1,_funlink
	move	$-1,_gclink
	move	Sp,gencSp
	jump	gencpop

#define MARKNODE(addr)                 \
	PRINT($MSG_mark)    	&& \
	PRINTD(addr)		&& \
	PRINTD(0(addr))		&& \
	move	0(addr),Spush	&& \
	move	addr,Spush	&& \
	move	$MARKED,0(addr)

#define WRITEUPDATED(reg,label)	   \
	move _tp,ATMP		&& \
	move $-1(ATMP),ATMP 	&& \
	move reg,0(ATMP) 	&& \
	move ATMP,_tp

; The heap:
;                -----------------------------------------------------------
;  OUTSIDE (low) | OLD heap    | moved |                 |  PREVIOUS heap  |  OUTSIDE (high)
;                -----------------------------------------------------------
;                ^             ^       ^                                   ^
;             GCSTART        GCCUR    Hp                                GCEND


#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

; QPREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to OUTSIDE(a) or OLDorOUTSIDE(a).

#define QPREVIOUS(kind)                               \
	comp	r0,Hp && jlth CAT3(genc,kind,qout) && \
	comp	r0,GCEND && jgeh CAT3(genc,kind,out)

; PREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to allmoved.

#define PREVIOUS(kind)                                \
	comp	r0,Hp && jlth allmoved &&             \
	comp	r0,GCEND && jgeh allmoved

; OLD_OUTSIDE(a) falls through if r0 points into the OLD part, or OUTSIDE, the heap,
; otherwise it jumps to allmoved.

#define OLDorOUTSIDE(kind)                            \
	CAT3(genc,kind,qout): &&                      \
	comp	r0,GCCUR && jgeh allmoved &&          \
	CAT3(genc,kind,out):
	
; OUTSIDE(a) falls through if r0 points into the OLD heap,
; otherwise it jumps to allmoved.

#define OUTSIDE(kind)                                \
	CAT3(genc,kind,qout): &&                     \
	comp	r0,GCSTART && jgeh allmoved &&       \
	CAT3(genc,kind,out):

; NOT_PREVIOUS(a) cath all that miss Previous(a)

#define NOT_PREVIOUS(kind)                                \
	CAT3(genc,kind,qout): &&                     \
	CAT3(genc,kind,out):
	

	.pragma	GC_ON

; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving

	.data
	.export	NGENCRET
NGENCRET:	.word	GCRET

_funlink:	.word	0
gencSp:	.word	0
	.text

	.export	gencret
	.export	gensret
; reset all funlinks
gencret:
gensret:
	comp	r1,$0		; It is a scan updated that we are doing
	jne	scanning	; Check if there is more pointers to follow
	PRINT($MSG_NOSCANRET)
scanfinish:
	PRINT($MSG_SCANFIN)
	move	_funlink,r0
	jump	gencret0
gencret1:
	move	r0,r1
	move	0(r0),r0
	move	$0,0(r1)
	add2	$1,_nfunlink
gencret0:
	comp	r0,$-1
	jne	gencret1
	PRINT($MSG_SpgencSp)
	PRINTD(Sp)
	PRINTD(gencSp)
; reset all marked nodes
gencret3:
	comp	Sp,gencSp
	jeq	gencret2
	PRINT($MSG_restore)
	move	Spop,r0
	PRINTD(r0);
	move	Spop,r1
	PRINTD(r1);
	move	r1,0(r0)
	jump	gencret3
gencret2:
	PRINT($MSG_RET)
	move	Sp,_ep
	move	Hp,_hp
	CRET
	.data
MSG_RET: .string "CRET"
MSG_NOSCANRET: .string "no-scan"
MSG_SCANFIN: .string "scan-finish"
MSG_SpgencSp: .string "Sp  gencSp"
MSG_restore: .string "A T"
MSG_mark: .string "a t"
	.text

	.export	gencpop
	.export	gencd
allmoved:	; node already moved
	move	r0,0(r1)
gencpop:
	move	Spop,r0
	move	r0,Spush
	PRINTD(r0)
	move	Vpop,r0
	move	Vpop,r1
gencd:
	PRINTDUMP(r0)
	move	0(r0),INDREG
	jump	ogenc(INDREG)


; Node with 1 nonpointer and 1 pointer part
	.export	genc11
	.export	gens11
genc11:
	SINCR(_ngc11)
	QPREVIOUS(11)
	move	Hp,ATMP
	move	0(r0),toH
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop
	OUTSIDE(11)	
gens11:
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved


; Indir node
	.export	gencindi
	.export	gensindi
gencindi:
	SINCR(_ngcindir)
	QPREVIOUS(indi)
	move	1(r0),r0
	jump 	gencd
	OUTSIDE(indi)	                   ; Think more here
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

gensindi:			; This might work
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

; Application node in old format
	.export	gencapG
	.export	gensapG
gencapG:
; Constant application node
	.export	genccap
	.export	genscap
genccap:
	SINCR(_ngccap)
	QPREVIOUS(cap)
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
	jump gencpop
	OUTSIDE(cap)	
gensapG:
genscap:
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Node with 1 nonpointer and 2 pointer part
	.export	genc12
	.export	gens12
genc12:
	SINCR(_ngc12)
	QPREVIOUS(12)
	move	Hp,ATMP
	move	0(r0),toH
#if 1
; This method is much better for long lists
	move	$1(Hp),Vpush
	move	2(r0),Vpush
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$2(Hp),Hp
#else
	move	Hp,Vpush
	move	1(r0),Vpush
	move	$1(Hp),Hp
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop
	OUTSIDE(12)	
gens12:
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Application node
	.export	gencap
	.export	gensap
gencap:
	SINCR(_ngcap)
	QPREVIOUS(ap)
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
	jump gencpop
	OLDorOUTSIDE(ap)
#if 0
	move    $GCAP_OUTSIDE,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
GCAP_OUTSIDE: .string "ap outside previous"
#endif
gensap:
	move	$APG,0(r0)
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; Special cases for values that have constants in the runtime system.
	.export	gencchr
gencchr:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	genc20
	comp	DTMP,$ MAXCHAR
	jgt	genc20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gencpop


	.export	gencint
gencint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	genc20
	comp	DTMP,$ MAXINTTAB
	jgt	genc20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gencpop

	.export	genctag0
genctag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	genc20
	comp	DTMP,$ MAXTAG0
	jgt	genc20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gencpop

; Node with 2 nonpointer and 0 pointer part
	.export	genc20
	.export	gens20
genc20:
	SINCR(_ngc20)
	PREVIOUS(20)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop
gens20:
	jump allmoved

; Node with 2 nonpointer and 1 pointer part
	.export	genc21
	.export	gens21
genc21:
	SINCR(_ngc21)
	QPREVIOUS(21)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	Hp,Vpush
	move	2(r0),Vpush
	move	$1(Hp),Hp
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop
	OUTSIDE(21)	
gens21:
	MARKNODE(r0)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump allmoved

; GC of input directory node, call C routine to set ref bit
	.export gencindf
	.export gensindf
gencindf:
gensindf:
	move	$INPUTD,0(r0)
	.export gencind
	.export gensind
gencind:
gensind:
	move	r0,Vpush
	move	r1,Vpush
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
;; XXX	comp	CRETR,DTMP
	move	_ep,Sp
	move	_hp,Hp
	move	Vpop,r1
	move	Vpop,r0
	QPREVIOUS(ind)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	comp	DTMP,$0
	jne	gencpop
	SINCR(_ngcind)
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,Wind1)
	jump gencpop
	OLDorOUTSIDE(ind)
	comp	DTMP,$0
	jne	allmoved
	SINCR(_ngcind)
	WRITEUPDATED(r0,Wind2)
	jump allmoved


; GC of input node, call C routine to set ref bit
	.export gencinpf
	.export gensinpf
gencinpf:
gensinpf:
	move	$INPUT,0(r0)
	.export gencinp
	.export gensinp
gencinp:
gensinp:
	move	r0,Vpush
	move	r1,Vpush
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	CRETR,DTMP
	move	_ep,Sp
	move	_hp,Hp
	move	Vpop,r1
	move	Vpop,r0
	QPREVIOUS(inp)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	comp	DTMP,$0
	jne	gencpop
	SINCR(_ngcinp)
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,Winp1)
	jump gencpop
	OLDorOUTSIDE(inp)
	comp	DTMP,$0
	jne	allmoved
	SINCR(_ngcinp)
	WRITEUPDATED(r0,Winp2)
	jump allmoved

; STRING_F & STRING_1
	.export	gencstr
	.export	gensstr
gencstr:
	SINCR(_ngc30)
	QPREVIOUS(str)
	move	Hp,ATMP
	move	$STRING,toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop

	NOT_PREVIOUS(str)
gensstr:
	move	$STRING,0(r0)
	jump allmoved

; STRINGN_F & STRINGN_1
	.export	gencstrn
	.export	gensstrn
gencstrn:
	SINCR(_ngc30)
	QPREVIOUS(str)
	move	Hp,ATMP
	move	$STRINGN,toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop

	NOT_PREVIOUS(strn)
gensstrn:
	move	$STRINGN,0(r0)
	jump allmoved

; Node with 3 nonpointer and 0 pointer part
	.export	genc30
	.export	gens30
genc30:
	SINCR(_ngc30)
	PREVIOUS(30)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gencpop
gens30:
	jump allmoved

	.export genchole
	.export genshole
genchole:
	SINCR(_ngc30)
	QPREVIOUS(hole)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,Whole1)
	jump gencpop
	OLDorOUTSIDE(hole)
	MARKNODE(r0)               ; Otherwise it might get registered in Updated more than once
	WRITEUPDATED(r0,Whole2)
genshole:
	jump allmoved

; Vektor node with pointers
	.export	gensvek
gensvek:
	move	1(r0),DTMP
	jump gensvekt

	.export	gencvek
gencvek:
	SINCR(_ngcvek)
	move	1(r0),DTMP
gencvekt:
	QPREVIOUS(vek)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0
gencv1:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gencpop
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	gencv1

	OUTSIDE(vek)	
gensvekt:
	MARKNODE(r0)
	PRINT($MSG_VEK)
	PRINTD(DTMP)
	move	r0,0(r1)
	move	$2(r0),r0
	.export	gencv2
gencv2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gencpop
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	gencv2

	.data
MSG_VEK: .string "Vek "
	.text


; Vektor node with nonpointers
	.export	gencdvek
	.export	gensdvek
gencdvek:
	SINCR(_ngcvek)
	PREVIOUS(dvek)
	move	Hp,Vpush	; save new address
	move	1(r0),DTMP
	add2	$2,DTMP		; DTMP is the counter.
	move	r0,ATMP		; ATMP points to the words in the vector.
gencbv1:
	move	0(ATMP),toH
	move	$1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jne	gencbv1
	move	$MOVED,0(r0)
	move	Vpop,ATMP
	move	ATMP,1(r0)
	move    ATMP,0(r1)
	jump	gencpop
gensdvek:
	jump	allmoved

; Marked node
	.export	gencmkd
	.export	gensmkd
gencmkd:
	SINCR(_ngcmkd)
gensmkd:
	jump	allmoved
	
; Moved node
	.export	gencmvd
	.export	gensmvd
gencmvd:
	SINCR(_ngcmvd)
gensmvd:
	move	1(r0),0(r1)
	jump	gencpop

/**************************************************************************/

; Vector apply node
	.export	gencvap
gencvap:
	move    $VAPG,0(r0)
	.export	gencvapG
gencvapG:
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gencvap1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gencvap2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gencvap1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gencvap2
gencvap1:
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gencvekt

	.data
MSG_VAP: .string "VFlink = "
MSG_VAP2: .string " ->"
	.text
; Vector apply node scan
	.export	gensvap
gensvap:
	move    $VAPG,0(r0)
	.export	gensvapG
gensvapG:
	SINCR(_ngcvap)
	PRINT($MSG_VAP)
	move	1(r0),ATMP
	PRINTD(VFlink(ATMP))
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gensvap1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gensvap2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gensvap1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gensvap2
gensvap1:
	PRINT($MSG_VAP2)
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gensvekt


	.export	gencfun
	.export	gensfun
gensfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
	jump    gencfunz                ; Do not change pointer to node, we are scanning!
gencfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
	move	VFpointer(FDISP),0(r1)
; check (& set) the link
gencfunz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	gencpop
	SINCR(_ngcfun)
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	PRINT($MSG_FUN)
	PRINTD(DTMP)
	jump	gencv2

	.data
MSG_FUN:	.string "Fun "
	.text

; genc a ZAP node (a zapped application)
	.export	genczap
	.export	genszap
genczap:
	SINCR(_ngczap)
	QPREVIOUS(zap)
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	1(ATMP),r1		; prepare to do as with a FUN node
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,Wzap1)
	move	r1,FDISP
	jump	gencfunz
	OUTSIDE(zap)
	WRITEUPDATED(r0,Wzap2)
genszap:
	MARKNODE(r0)
	move	1(r0),FDISP		; prepare to do as with a FUN node
	jump	gencfunz
	; Never executed

	.data
IndPtr:	  .word 0
EndIndPtr: .word 0
	.text

scanning:
	move	IndPtr,r1
nextEntry:
	comp	r1,EndIndPtr
	jge	scanfinish
	move	0(r1),r0
	comp	r0,Hp				; Check if it is outside previous
	jlth    outPrevious
	comp	r0,GCEND
	jgeh    outPrevious

inPrevious:
	PRINT($MSG_4)
	PRINTD(r0)
	move	$0,0(r1)	; Zap the entry
	move	$1(r1),r1
	jump	nextEntry

outPrevious:
	move	$1(r1),IndPtr

	PRINT($MSG_1)
	PRINTD(r1)
	PRINT($MSG_0)
	PRINTD(r0)
	PRINTD(0(r0))
	PRINT($MSG_3)

	move	$1,Vpush
	move	$NGENCRET,Vpush
	PRINTDUMP(r0)
	move	0(r0),INDREG
	jump	ogens(INDREG)


	.data
MSG_GENSALL:	.string "gensall:"
MSG_0:	.string "r0 = "
MSG_1:	.string "r1 = "
MSG_2:	.string "tag = "
MSG_3:	.string ":"
MSG_4:	.string "PREVIOUS"
	.text

#if DEBUGPRINT

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


