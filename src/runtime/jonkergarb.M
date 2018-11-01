#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"

#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

#define STATISTICS 0
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

#define PRINT(m)     	move ATMP,Vpush && move m,ATMP && call print && move Vpop,ATMP

#define PRINTD(m)    	move ATMP,Vpush && move m,ATMP && call printd && move Vpop,ATMP

#define PRINTDUMP(m) 	move ATMP,Vpush && move m,ATMP && call printdump && move Vpop,ATMP

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
;                -----------------------------------------------------------------------------
;  OUTSIDE (low) |                                                                 |         | OUTSIDE (high)
;                -----------------------------------------------------------------------------
;                ^                      ^       ^                                 ^    ^    ^
;             STARTHEAP                 NEW     NODE                              ENDU  WU  ENDHEAP
;             startheap          	                            	     starttable tp  endtable
;									       BitHeap
#define STARTHEAP GCSTART				
#define NEW	  Hp
#define toNEW	  toH
#define NODE      Sp
#define frNODE	  Spop
#define ENDU      GCEND
#define WU        _tp

; Sp = edi   DTMP = ecx  INDREG = esi
; Hp = ebp   ATMP = edx  r0 = ebx
; #define ENDHEAP   GCEND    Doesn't use ENDHEAP

; oJmove  Pack Nodes

#define WRITEUPDATED(reg,tmp,label)	\
	move 	WU,tmp && 		\
	comp	tmp,ENDU &&		\
	jlt	XOverflow &&		\
	move	$-1(tmp),tmp &&		\
	move	reg,0(tmp) &&		\
	move	tmp,WU

; OUTSIDE(a) falls through if r0 points into the OLD heap,
; otherwise it jumps to movenext.

#define OUTSIDE(kind)                                 \
	comp	r0,STARTHEAP && jgeh movenext &&      \
	CAT3(M,kind,out):

; LINK(chain,a) links a node into the gclink-chain
; if it isn't already there.

#define LINKNODE(kind)					\
	move	-1(r0),r1 &&				\
	comp	r1,$0 &&				\
	jne	CAT3(M,kind,link) &&			\
	move	gclink,-1(r0) &&			\
	move	$-1(r0),gclink &&			\
	CAT3(M,kind,link):

	.pragma	GC_ON

; _ g c :   i n t e r f a c e   t o   C

	.data
SpOld:	.word 0
scantp:	.word 0

STRini: .string "Jonker move on indir"
	.text
	.export JMini
JMini:
	move	Sp,_ep
	move    $STRini,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.export JMfun
JMfun:
#if 0
	move	Sp,_ep
	move    $STRfun,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

STRfun: .string "Jonker move on fun"
#else
	move	INDREG,toNEW
	move	frNODE,toNEW
	jump	NextNode
#endif

	.data
STRmvd: .string "Jonker move on moved"
	.text
	.export JMmvd
JMmvd:
	move	Sp,_ep
	move    $STRmvd,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.export	_moveheapGen
CLABEL(_moveheapGen)
	CENTRYS(GC_STACK)
	move	Sp,SpOld		; We use Sp as NODE
	move	_startheap,NODE
	move	_startheap,NEW
	move	_endheap,ENDU
	jump	NextNode
MoveReturn:
	move	SpOld,Sp
	move	NEW,_hp
	CRET

	.export	JMstrF
JMstrF:
	move	$STRING,toNEW
	move	frNODE,toNEW
	move	frNODE,toNEW
	jump	NextNode

	.export	JMstrnF
JMstrnF:
	move	$STRINGN,toNEW
	move	frNODE,toNEW
	move	frNODE,toNEW
	jump	NextNode

	.export	JMindF
JMindF:
	move	$INPUTD,toNEW
	move	frNODE,toNEW
	move	frNODE,toNEW
	jump	NextNode

	.export	JMinpF
JMinpF:
	move	$INPUT,toNEW
	move	frNODE,toNEW
	move	frNODE,toNEW
	jump	NextNode

	.export	JM3u
JM3u:
	WRITEUPDATED(NEW,ATMP,hole)
	.export	JM3
JM3:					; Three cells node are common,
	move	INDREG,toNEW		; we can use fallthrough if we
	move	frNODE,toNEW		; place their code here.
	move	frNODE,toNEW
	.export	JMmrk
JMmrk:
NextNode:
	comp	NODE,ENDU
	jge	MoveReturn
	move	frNODE,INDREG
	jump 	TestTag

NoTag:
	move	0(INDREG),r1
	move	NEW,0(INDREG)
	move	r1,INDREG
TestTag:
	comp	INDREG,$TAGFIRST
	jlt	NoTag
	comp	INDREG,$TAGLAST
	jgt	NoTag

	move	INDREG,-1(NODE)
	PRINTDUMP($-1(NODE))

	jump	oJmove(INDREG)



	.export	JM2
JM2:
	move	INDREG,toNEW
	move	frNODE,toNEW
	jump	NextNode


	.export	JMap
JMap:
	move	$APG,toNEW
	move	frNODE,toNEW
	move	frNODE,toNEW
	jump	NextNode

	.export	JMvap
JMvap:
	.export	JMvapG
JMvapG:
	move	$VAPG,toNEW
	move	frNODE,r0
	move	r0,toNEW
	move	VFarity(r0),DTMP
moveDST:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	NextNode
	move	frNODE,toNEW
	jump	moveDST


	.export	JMvek
JMvek:
	move	INDREG,toNEW
	move	frNODE,DTMP
	move	DTMP,toNEW
	jump	moveDST


	.export	JMret
JMret:
	move	frNODE,NODE
	jump	NextNode
	

	.export XOverflow
XOverflow:
	move	Sp,_ep
	move    $MSG_Overflow,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	
	.data
MSG_Overflow:	.string "Overflow in update table"

	.text
