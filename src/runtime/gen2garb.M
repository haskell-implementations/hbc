#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"


#define MemUsage 1

#define STATISTICS 0
#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif

#if 1
#define SINCRDEBUG(a)
#else
#define SINCRDEBUG(a) add2 $1,a
#endif

#define startT(tot,start) \
	move	Sp,Vpush	&& \
	move	Hp,Vpush	&& \
	move	r1,Vpush	&& \
	move	r0,Vpush	&& \
	CPUSHARG1(start)	&& \
	CPUSHARG0(tot)		&& \
	CCALL(2,_startTime)	&& \
	move	Vpop,r0		&& \
	move	Vpop,r1		&& \
	move	Vpop,Hp		&& \
	move	Vpop,Sp

#define stopT(tot,start) \
	move	Sp,Vpush	&& \
	move	Hp,Vpush	&& \
	move	r1,Vpush	&& \
	move	r0,Vpush	&& \
	CPUSHARG1(start)	&& \
	CPUSHARG0(tot)		&& \
	CCALL(2,_stopTime)	&& \
	move	Vpop,r0		&& \
	move	Vpop,r1		&& \
	move	Vpop,Hp		&& \
	move	Vpop,Sp



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


#define MARKNODE(addr)                 \
	PRINT($MSG_mark)    	&& \
	PRINTD(addr)		&& \
	PRINTD(0(addr))		&& \
	move	0(addr),Spush	&& \
	move	addr,Spush	&& \
	move	$MARKED,0(addr)

; The heap: alt 1
;                ------------------------------------------------------------------------------
;  OUTSIDE (low) | OLD heap    | old moved |           | new moved |        |  PREVIOUS heap  |  OUTSIDE (high)
;                ------------------------------------------------------------------------------
;                ^                         ^                       ^                          ^
;             GCSTART                   INDREG                     Hp                       GCEND

; The heap: alt 2
;                ------------------------------------------------------------------------------
;  OUTSIDE (low) | OLD heap    | old moved |           |  PREVIOUS heap  |  | new moved |     | OUTSIDE (high)
;                ------------------------------------------------------------------------------
;                ^                         ^                                            ^     ^
;             GCSTART                   INDREG                                          Hp   GCEND

; Note that the garbage collector only follows pointers pointing at nodes that existed before GC,
;       i.e., never to pointers pointing into old or new moved.

#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

; Qprevious(a) falls through if r0 points into the previous part of the heap,
; otherwise it jumps to outside(a) or not_previous(a).
; Greater than INDREG and less than GCEND means new moved or previous
; but nothing points into new moved => previous

#define QPREVIOUS(kind)                                   \
	comp	r0,INDREG && jlth CAT3(gen2c,kind,qout) && \
	comp	r0,GCEND && jgeh CAT3(gen2c,kind,out)

#define DQPREVIOUS(kind)                                   \
	comp	r0,INDREG && jlth CAT3(gen2c,kind,qout) && \
	comp	r0,GCEND && jgeh CAT3(gen2c,kind,out)

; Previous(a) falls through if r0 points into the previous part of the heap,
; otherwise it jumps to allmoved.

#define PREVIOUS(kind)                                \
	comp	r0,INDREG && jlth allmoved &&         \
	comp	r0,GCEND && jgeh allmoved

#define DPREVIOUS(kind)                                \
	comp	r0,INDREG && jlth allmovedD &&         \
	comp	r0,GCEND && jgeh allmovedD

; NOT_previous(a) always falls through. It is used as destination for Qprevious(a)

#define NOT_PREVIOUS(kind)                             \
	CAT3(gen2c,kind,qout): &&                      \
	CAT3(gen2c,kind,out):

#define DNOT_PREVIOUS(kind)                             \
	CAT3(gen2c,kind,qout): &&                      \
	CAT3(gen2c,kind,out):

; Outside(a) falls through if r0 points outside the heap,
; otherwise it jumps to allmoved.
; Only used after qprevious!

#define OUTSIDE(kind)                                \
	CAT3(gen2c,kind,qout): &&                     \
	comp	r0,GCSTART && jgeh allmoved &&       \
	CAT3(gen2c,kind,out):

#define DOUTSIDE(kind)                                \
	CAT3(gen2c,kind,qout): &&                     \
	comp	r0,GCSTART && jgeh allmovedD &&       \
	CAT3(gen2c,kind,out):
	
/* Remember adds reg to the tp-table (r1 must be tp) */
#define REMEMBER(reg)	   \
	move $-1(r1),r1 	&& \
	move reg,0(r1)

/* Inside fall through if inside new moved otherwise jump to olabel */
#define	INSIDE(reg,olabel)		\
	comp	reg,GCCUR &&		\
	jlt	olabel &&		\
	comp	reg,Hp &&		\
	jgt	olabel


	.pragma	GC_ON

/**************************************************************************/

/*   fixMovedOld(void)  uses _tp,_endtable,_startheap,_oldhp */

	.text
	.export _fixMovedOld
CLABEL(_fixMovedOld)
	CENTRYS(GC_STACK)
	PRINT($MSG_FIX)

	move	_curheap,GCCUR		; Load start of new moved, Hp contains end of new moved.
	move	_hp,Hp			; Save new hp
	move	_ep,Sp
	move	_oldhp,INDREG
	move	$GCRET,0(INDREG)	; Used to detect end of old moved.
	move	_endtable,r1		; r1 is tp.


	move	$fOM,INDREG	; Get return address.
fOM:				; Reset all marked nodes.
	move	_oldgclink,r0
	comp	r0,$-1
	jeq	gen2cret2
	move	0(r0),_oldgclink
	move	$0,0(r0)
	move	$1(r0),r0
	move	0(r0),ATMP
	PRINTDUMP(r0)
	jump	ogen2p(ATMP)

	.data
MSG_FIX:	 .string "fixMovedHeap:"

/**************************************************************************/

; ATMP -> pointer to tag entry of this node
; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving
; INDREG = old heap pointer
; Hp     = new heap pointer

	.data
	.export	gen2cSp    ; Only exported for debugging
gen2cSp:	.word	0
_startHp:	.word	0
	.text

; _ g c :   i n t e r f a c e   t o   C

/****************************************/
/*   gen2call(void)  uses _tp,_endtable,_oldhp,_hp,_ep (_ep should be pointing at a vektor node representing the stack)  */
/*    The function does:  gen2sall(); gen2c(ep); gen2p(all marked nodes); gen2p(all nodes in old moved); */

	.text
	.export _gen2call
CLABEL(_gen2call)
	CENTRYS(GC_STACK)
	PRINT($MSG_GEN2CALL)

#if defined(sparc) || defined(hppa)
	move	Vp,_garbvp	; Used when checking the stack, ( I think ).
#endif

	move	_ep,Sp

	move	_hp,Hp
	move	Hp,_startHp
	move	_oldhp,INDREG
	move	INDREG,_oldstarthp
#if MemUsage
	move	Hp,_gen2_startHp ; !!!
	move	INDREG,_gen2_startOldHp ; !!!
#endif

	move	_startheap,GCSTART
	move	_endheap,GCEND
	move	$-1,_funlink
	move	$-1,_gclink

	move	Sp,gen2cSp       ; Stack when we start, needed to restore MARKED nodes


/******* gen2sall() **********/
	
	.export	gen2sall
gen2sall:

	PRINT($MSG_GEN2SALL)
	move	$_GCstart_scan,ATMP
	move	$_GC_tot_scan,DTMP
	startT(DTMP,ATMP)
	
	.export	debugStop
debugStop:
	move	INDREG,_oldhp	; Save current oldhp
	move	INDREG,GCCUR
	move	_tp,r1
	move	1(r1),INDREG

	move	$0,Vpush	; push stop node
	move	$NGEN2SRET,Vpush
	move	$2(r1),r1	; Step to pointers

/*********** do nothing g2s__ *********/
	.export	g2s20
g2s20:
	.export	g2s30
g2s30:
	.export	g2sdvek		; scan
g2sdvek:
	.export	g2smkd
g2smkd:
	.export	g2smvd
g2smvd:
/*******************************/
scanNext:
	DECR(INDREG)
	TSTC(INDREG)
	jlt	allScaned
	move	0(r1),r0	; pointer to node
	move	$1(r1),r1

	comp	r0,GCCUR	; Check if it is outside previous.
	jlth    outPrevious	;	Jump if outside low or old  (old moved is not possible)
	comp	r0,GCEND
	jlth    scanNext	;	Jump if outside high.

outPrevious:
	PRINTDUMP(r0)
	move	0(r0),ATMP	; get tag
	jump	ogen2s(ATMP)	; scan node

allScaned:
	PRINT($MSG_GEN2SALL2)
	move	_oldhp,INDREG
	jump	gen2Dcpop	; Garbage collect all pushed nodes and force to old heap

/****************************************************************/
/*   g2cgsret is the code that are called when all pointers	*/
/*   on the Vp-stack are garbage collected, during garbage 	*/
/*   collection of the tp-table.	          		*/
/****************************************************************/

	.export	g2dgcret
g2dgcret:
	.export	g2dgsret
g2dgsret:
	.export	g2cgsret
g2cgsret:
#if MemUsage
	move	Hp,_gen2_scanHp ; !!!
	move	INDREG,_gen2_scanOldHp ; !!!
#endif
	move	$_GCstart_scan,ATMP
	move	$_GC_tot_scan,DTMP
	stopT(DTMP,ATMP)

	move	_startHp,GCCUR		; Load start of new moved, Hp contains end of new moved.


/******** gen2c(ep) ************/

	.data
	.export	NGEN2CRET
NGEN2CRET:	.word	GCRET
	.export	NGEN2SRET
NGEN2SRET:	.word	GSRET
saveINDREG:	.word	0
dummy:		.word	0

	.text
gen2c_ep:
	PRINT($MSG_GEN2C_EP)
	move	$_GCstart_stack,ATMP
	move	$_GC_tot_stack,DTMP
	startT(DTMP,ATMP)

	move	$0,Vpush
	move	$NGEN2CRET,Vpush

	move	$dummy,r1
	move	gen2cSp,r0      ; Can't use _ep as some nodes are marked
	jump	g2sStack	; Jump to g2svek, but after Marknode
				; to prevent the stack from enter the tp-table

/***** Code used by ogen2c() *****************/

	.export	gen2cpop
	.export	gen2cd
/********* do nothing gen2c__ *******/
	.export	g2cmkd
g2cmkd:
/****************************/
allmoved:			; node already moved
	move	r0,0(r1)	; Set pointer to node
gen2cpop:
	move	Vpop,r0
	move	Vpop,r1
gen2cd:				; Label used by gen2cindir
	PRINTD(INDREG)
	PRINTD(Hp)
	PRINTDUMP(r0)
	move	0(r0),ATMP
	jump	ogen2c(ATMP)

/***** Code used by ogen2d() *****************/

	.export	gen2Dcpop
	.export	gen2Dcd
/********* do nothing gen2d__ *******/
	.export	g2dmkd
g2dmkd:
/****************************/
allmovedD:			; node already moved
	move	r0,0(r1)	; Set pointer to node
gen2Dcpop:
	move	Vpop,r0
	move	Vpop,r1
gen2Dcd:
	PRINTD(INDREG)
	PRINTD(Hp)
	PRINTDUMP(r0)
	move	0(r0),ATMP
	jump	ogen2c(ATMP)
/*	jump	ogen2d(ATMP)  */

/****************************************************************/
/*   g2cgcret is the code that are called when all pointers	*/
/*   on the Vp-stack are garbage collected, during garbage	*/
/*   collection of ep (, the stack).	          		*/
/****************************************************************/

	.export	g2cgcret
g2cgcret:
	move	$_GCstart_stack,ATMP
	move	$_GC_tot_stack,DTMP
	stopT(DTMP,ATMP)
#if MemUsage
	move	Hp,_gen2_stackHp ; !!!
	move	INDREG,_gen2_stackOldHp ; !!!
#endif

	/* Fall through */

/******** gen2p(MARKED) ************/

	.text
gen2p_marked:
	PRINT($MSG_GEN2PMARKED)
	move	$_GCstart_marked,ATMP
	move	$_GC_tot_marked,DTMP
	startT(DTMP,ATMP)

	move	_startHp,GCCUR		; Load start of new moved, Hp contains end of new moved.
	move	Hp,_hp			; Save new hp
	move	INDREG,_oldhp		; Save new oldhp
	move	$GCRET,0(INDREG)	; Used to detect end of old moved.
	PRINTD(INDREG)
	PRINTD(0(INDREG))
	PRINTDUMP(INDREG)
	move	_endtable,r1		; r1 is tp.
	move	$nextGen2d,INDREG	; Get return address.
nextGen2d:
	comp	Sp,gen2cSp
	jeq	gen2cret3
	move	Spop,r0
	move	Spop,ATMP
	move	ATMP,0(r0)
	PRINTDUMP(r0)
	jump	ogen2p(ATMP)


/******** gen2p(old moved) ************/

gen2cret3:
	move	$_GCstart_marked,ATMP
	move	$_GC_tot_marked,DTMP
	stopT(DTMP,ATMP)
	move	$_GCstart_moved,ATMP
	move	$_GC_tot_moved,DTMP
	startT(DTMP,ATMP)

gen2cret2:
	PRINT($MSG_GEN2PMOVED)
	move	$gen2cret4,INDREG	; check old moved for pointers into the new heap
	move	_oldstarthp,r0          ; scan old moved
gen2cret4:
	move	0(r0),ATMP
	PRINTDUMP(r0)
	jump	ogen2p(ATMP)

/****************************************************************/
/*   gen2pret is the code that are called when all nodes in old	*/
/*   moved are checked.						*/
/****************************************************************/

	.export	g2pgcret
g2pgcret:
	move	$_GCstart_moved,ATMP
	move	$_GC_tot_moved,DTMP
	stopT(DTMP,ATMP)
	move	Sp,_ep		; Sp is same as when we started, or we are in troubles.
	move	r1,_tp

/************** Clear funlink list ************/
	move	_funlink,r0
	jump	gen2cret0
gen2cret1:
	move	r0,r1
	move	0(r0),r0
	move	$0,0(r1)
	add2	$1,_nfunlink
gen2cret0:
	comp	r0,$-1
	jne	gen2cret1

	PRINT($MSG_GEN2END)
	CRET
	.data

/***** Some messages *******/

	.data
MSG_mark:	 .string "mark:"
MSG_GEN2CALL:	 .string "gen2call:"
MSG_GEN2SALL:	 .string "gen2sall:"
MSG_GEN2SALL2:	 .string "gen2sall2:"
MSG_GEN2C_EP:	 .string "gen2c(ep):"
MSG_GEN2PMARKED: .string "gen2p(MARKED):"
MSG_GEN2PMOVED:	 .string "gen2p(old moved):"
MSG_GEN2END:	 .string "gen2call end:"
	.text

/**************************************************************************/
; ATMP -> pointer to tag entry of this node
; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving
; INDREG = old heap pointer
; Hp     = new heap pointer

; ogen2t(ATMP) is the tag for the new copy of this node


; g2n__     destination new heap
; g2o__     destination old heap
; g2c__     no destination! (indir,gcret)
/**********/

; Node with 1 nonpointer and 1 pointer part
#ifdef NOUNDERSCORE
#define L(l) .export l && l:
#else
#define L(l) .export l && .export _/**/l && l: && _/**/l:
#endif
L(g2n11)
	SINCR(_ngc11)
	QPREVIOUS(11)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	$MOVED,0(r0)
	move	1(r0),DTMP
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	Hp,r1
	move	DTMP,r0
	move	$1(Hp),Hp
	jump	gen2cd
L(g2o11)
	SINCR(_ngc11)
	QPREVIOUS(11)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	$MOVED,0(r0)
	move	1(r0),DTMP
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$1(INDREG),r1
	move	DTMP,r0
	move	$2(INDREG),INDREG
	jump	gen2cd

	OUTSIDE(11)		; both qprevious(11) land here
	MARKNODE(r0)
	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	gen2cd

; Node with 1 nonpointer and 2 pointer part
L(g2n12)
	SINCR(_ngc12)
	QPREVIOUS(12)
	move	ogen2t(ATMP),DTMP	; New tag
	move	Hp,ATMP			; New address
	move	DTMP,toH		;	Store new tag
	move	$1(Hp),Vpush		;	Push address of second pointer (auto increment)
	move	1(r0),DTMP		; Save value of old first pointer
	move	2(r0),Vpush		;	Push value of second pointer
	move	$MOVED,0(r0)		;		Write MOVED tag
	move	ATMP,1(r0)		;		write new address
	move	ATMP,0(r1)		;		fix pointer to node
	move	Hp,r1			; Get address of first pointer
	move	DTMP,r0			; and saved value
	move	$2(Hp),Hp		; Increment heap pointer
	jump	gen2cd

L(g2o12)
	SINCR(_ngc12)
	QPREVIOUS(12)
	move	ogen2t(ATMP),DTMP	; New tag
	move	INDREG,ATMP		; New address
	move	DTMP,0(INDREG)		;	Store new tag
	move	$2(INDREG),Vpush	;	Push address of second pointer
	move	1(r0),DTMP		; Save value of old first pointer
	move	2(r0),Vpush		;	Push value of second pointer
	move	$MOVED,0(r0)		;		Write MOVED ta
	move	ATMP,1(r0)		;		write new address
	move	ATMP,0(r1)		;		fix pointer to node
	move	$1(INDREG),r1		; Get address of first pointer
	move	DTMP,r0			; and saved value
	move	$3(INDREG),INDREG	; Increment heap pointer
	jump	gen2cd

	OUTSIDE(12)	
	MARKNODE(r0)
	move	r0,0(r1)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	$1(r0),r1
	move	1(r0),r0
	jump	gen2cd

; Node with 2 nonpointer and 0 pointer part
L(g2n20)
	SINCR(_ngc20)
	PREVIOUS(20)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2cpop

L(g2o20)
	SINCR(_ngc20)
	PREVIOUS(20)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	$2(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gen2cpop


; Node with 2 nonpointer and 1 pointer part
L(g2n21)
	SINCR(_ngc21)
	QPREVIOUS(21)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH	; Tag
	move	1(r0),toH	; value
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	Hp,r1
	move	2(r0),r0
	move	$1(Hp),Hp
	jump	gen2cd

L(g2o21)
	SINCR(_ngc21)
	QPREVIOUS(21)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)	; Tag
	move	1(r0),1(INDREG)	; Value
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(INDREG),r1
	move	2(r0),r0
	move	$3(INDREG),INDREG
	jump	gen2cd
	OUTSIDE(21)
	MARKNODE(r0)
	move	r0,0(r1)
	move	$2(r0),r1
	move	2(r0),r0
	jump	gen2cd

; Node with 3 nonpointer and 0 pointer part
L(g2n30)
	SINCR(_ngc30)
	PREVIOUS(30)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2cpop

L(g2o30)
	SINCR(_ngc30)
	PREVIOUS(30)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2cpop



/************  Special fixed size nodes **************/

; Indir node
	.export	g2cindir

g2cindir:
	SINCR(_ngcindir)
	QPREVIOUS(indir)
	move	1(r0),r0
	jump 	gen2cd
	OUTSIDE(indir)        ; Think more here, can't do much better but it will probably never happend anyway.
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump allmoved

/************ Special fixed size nodes with runtime tables ******/

; Characters
L(g2nchr)
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	g2n20
	comp	DTMP,$ MAXCHAR
	jgt	g2n20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

	.export	g2ochr

L(g2ochr)
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	g2o20
	comp	DTMP,$ MAXCHAR
	jgt	g2o20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

; Integers
	.export	g2nint
g2nint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	g2n20
	comp	DTMP,$ MAXINTTAB
	jgt	g2n20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

	.export	g2oint
g2oint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	g2o20
	comp	DTMP,$ MAXINTTAB
	jgt	g2o20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

; Constructor without arguments
	.export	g2ntag0
g2ntag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	g2n20
	comp	DTMP,$ MAXTAG0
	jgt	g2n20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

	.export	g2otag0
g2otag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	g2o20
	comp	DTMP,$ MAXTAG0
	jgt	g2o20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2cpop

/************  IO nodes  ********************/

; GC of input directory node, call C routine to set ref bit
L(g2nind)
	QPREVIOUS(ind)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	1(r0),DTMP	; remember dir reference
	move	DTMP,toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2cpop

L(g2oind)
	QPREVIOUS(ind)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),DTMP	; remember dir reference
	move	DTMP,1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2cpop
	OUTSIDE(ind)
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	allmoved


; GC of input file node, call C routine to set ref bit
L(g2ninp)
	QPREVIOUS(inp)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	1(r0),DTMP	; remember file reference
	move	DTMP,toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2cpop

L(g2oinp)
	QPREVIOUS(inp)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),DTMP	; remember file reference
	move	DTMP,1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2cpop
	OUTSIDE(inp)
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	allmoved

/************  Function nodes (except VAP) ********************/

	.export	g2cfun

g2cfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
	move	VFpointer(FDISP),0(r1)
; check (& set) the link
gen2cfunz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	gen2cpop
	SINCR(_ngcfun)
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	PRINT($MSG_FUN)
	PRINTD(DTMP)
	jump	gen2ivekT

	.data
MSG_FUN:	.string "Fun "
	.text

; ZAP node (a zapped application)
L(g2nzap)
	SINCR(_ngczap)
	QPREVIOUS(zap)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH
	move	1(r0),toH
	move	2(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	gen2cfunz

L(g2ozap)
	SINCR(_ngczap)
	QPREVIOUS(zap)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	gen2cfunz
	OUTSIDE(zap)
	MARKNODE(r0)
	move	1(r0),FDISP		; prepare to do as with a FUN node
	jump	gen2cfunz

/************  Varying size nodes ********************/

; Vektor node with non-pointers
L(g2ndvek)
	SINCR(_ngcvek)
	PREVIOUS(dvek)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP		; save new address
	move	DTMP,toH
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
	jump	gen2cpop

L(g2odvek)
	SINCR(_ngcvek)
	PREVIOUS(dvek)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP		; save new address
	move	DTMP,0(INDREG)
	move	1(r0),DTMP
	move	DTMP,1(INDREG)
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	move	$2(INDREG),INDREG
	jump	gen2odvekT
gen2odvekL:
	move	0(r0),0(INDREG)
	move	$1(INDREG),INDREG
	move	$1(r0),r0
gen2odvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	g2ndvekL
	jump	gen2cpop

; Vektor node with pointers
L(g2nvek)
	SINCR(_ngcvek)
	QPREVIOUS(vek)
	move	ogen2t(ATMP),DTMP
	move	Hp,ATMP
	move	DTMP,toH	; Copy tag
	move	1(r0),DTMP	; and size
	move	DTMP,toH
gen2nvekE:
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	jump	gen2nvekT
gen2nvekL:
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
gen2nvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2nvekL
	jump	gen2cpop

L(g2ovek)
	SINCR(_ngcvek)
	QPREVIOUS(vek)
	move	ogen2t(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)	; Copy tag
	move	1(r0),DTMP	; and size
	move	DTMP,1(INDREG)
gen2ovekE:
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	move	$2(INDREG),INDREG
	jump	gen2ovekT
gen2ovekL:
	move	INDREG,Vpush
	move	$1(INDREG),INDREG
	move	0(r0),Vpush
	move	$1(r0),r0
gen2ovekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2ovekL
	jump	gen2cpop
	OUTSIDE(vek)
	
	MARKNODE(r0)
g2sStack:			; Used when scanning the stack to prevent
	move	1(r0),DTMP	; it from enter the tp-table
gen2ivekE:
	move	r0,0(r1)
	move	$2(r0),r0
	jump	gen2ivekT
gen2ivekL:
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
gen2ivekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2ivekL
	jump	gen2cpop


; Vector apply node
L(g2nvap)
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gen2nvapL1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gen2nvapL2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gen2nvapL1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gen2nvapL2
gen2nvapL1:
	QPREVIOUS(vap)
	move	0(r0),ATMP
	move	ogen2t(ATMP),DTMP
	move	DTMP,toH	; Copy tag
	move	1(r0),ATMP	; and function
	move	ATMP,toH
	move	VFarity(ATMP),DTMP
	move	$-2(Hp),ATMP
	jump	gen2nvekE  ; r0 = Old address, ATMP = new address, DTMP = size

L(g2ovap)
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gen2ovapL1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gen2ovapL2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gen2ovapL1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gen2ovapL2
gen2ovapL1:
	QPREVIOUS(vap)
	move	0(r0),ATMP
	move	ogen2t(ATMP),DTMP
	move	DTMP,0(INDREG)	; Copy tag
	move	1(r0),ATMP	; and function
	move	ATMP,1(INDREG)
	move	VFarity(ATMP),DTMP
	move	INDREG,ATMP
	jump	gen2ovekE  ; r0 = Old address, ATMP = new address, DTMP = size
	OUTSIDE(vap)	

	MARKNODE(r0)
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gen2ivekE ; r0 = address, r1 = pointer, DTMP = size

/***************   Special gc nodes  *************/

; Marked node
	
; Moved node
	.export	g2cmvd
g2cmvd:
	SINCR(_ngcmvd)
	move	1(r0),0(r1)
	jump	gen2cpop

/**************************************************************************/

; INDREG = return address
; r1 =  tp
; r0 -> pointer to node that we are checking
; GCCUR = start of new moved
; Hp =  end of new moved
; ATMP is destroyed between nodes

; g2p__     add to tp-table if pointing inside new heap, increment r0 with size
/************  Fix size nodes ********************/

	.export	g2p11

g2p11:
	move	1(r0),DTMP
	INSIDE(DTMP,p11out)
	REMEMBER(r0)
p11out:
	move	$2(r0),r0
	JUMPREG(INDREG)

	.export	g2pap
g2pap:
	move	$APG,0(r0)
	.export	g2p12
g2p12:
	move	1(r0),DTMP
	INSIDE(DTMP,p12out0)
	REMEMBER(r0)
	move	$3(r0),r0
	JUMPREG(INDREG)
p12out0:
	move	2(r0),DTMP
	INSIDE(DTMP,p12out1)
	REMEMBER(r0)
p12out1:
	move	$3(r0),r0
	JUMPREG(INDREG)

	.export	g2p20		; remember
g2p20:
	move	$2(r0),r0
	JUMPREG(INDREG)	

	.export	g2p21		; remember
g2p21:
	move	2(r0),DTMP
	INSIDE(DTMP,p21out)
	REMEMBER(r0)
p21out:
	move	$3(r0),r0
	JUMPREG(INDREG)

	.export	g2p30		; skip
	.export	g2phole		; remember
g2phole:
	REMEMBER(r0)
g2p30:
	move	$3(r0),r0
	JUMPREG(INDREG)	

;		g2pindir = g2p11
;		g2pchr = g2p20
;		g2pint = g2p20
;		g2ptag0 = g2p20
;		g2pind = g2p30
;		g2pinp = g2p30
	.export	g2pdvek		; remember
g2pdvek:
	move	1(r0),DTMP
	ADDAW(DTMP,r0)
	move	$2(r0),r0	
	JUMPREG(INDREG)	

	.export	g2pvek	; remember
g2pvek:
	move	1(r0),DTMP
gen2pvekE:
	move	$2(r0),ATMP
	ADDAW(DTMP,ATMP)
	move	ATMP,DTMP
	move	$2(r0),ATMP	
	jump	gen2pvekT
gen2pvekL:
	INSIDE(0(ATMP),pvekout)
	REMEMBER(r0)
	move	DTMP,r0
	JUMPREG(INDREG)
pvekout:
	move	$1(ATMP),ATMP
gen2pvekT:
	comp	ATMP,DTMP
	jlt	gen2pvekL
	move	DTMP,r0
	JUMPREG(INDREG)

	.export	g2pvap
g2pvap:
	move	$VAPG,0(r0)
	.export	g2pvapG
g2pvapG:
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gen2pvekE	; r0 = address & DTMP = size
	
/**************************************************************************/
; ATMP -> pointer to tag entry of this node
; r1 -> pointer to pointer to node that we are moving
; r0 -> pointer to node that we are moving
; INDREG = old heap pointer
; Hp     = new heap pointer  (Not supposed to use this one)

; ogen2t(ATMP) is the tag for the new copy of this node


; g2d__     force all to old heap
/**********/

; Node with 1 nonpointer and 1 pointer part
	.export	g2d11
g2d11:
	SINCR(_ngc11)
	DQPREVIOUS(d11)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	$MOVED,0(r0)
	move	1(r0),DTMP
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$1(INDREG),r1
	move	DTMP,r0
	move	$2(INDREG),INDREG
	jump	gen2Dcd

	DOUTSIDE(d11)		; both qprevious(11) land here
	MARKNODE(r0)
	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	gen2Dcd

; Node with 1 nonpointer and 2 pointer part
	.export	g2dap
g2dap:
	.export	g2dapG
g2dapG:
	.export	g2d12
g2d12:
	SINCR(_ngc12)
	DQPREVIOUS(d12)
	move	ogen2dt(ATMP),DTMP	; New tag
	move	INDREG,ATMP		; New address
	move	DTMP,0(INDREG)		;	Store new tag
	move	$2(INDREG),Vpush	;	Push address of second pointer
	move	1(r0),DTMP		; Save value of old first pointer
	move	2(r0),Vpush		;	Push value of second pointer
	move	$MOVED,0(r0)		;		Write MOVED ta
	move	ATMP,1(r0)		;		write new address
	move	ATMP,0(r1)		;		fix pointer to node
	move	$1(INDREG),r1		; Get address of first pointer
	move	DTMP,r0			; and saved value
	move	$3(INDREG),INDREG	; Increment heap pointer
	jump	gen2Dcd

	DOUTSIDE(d12)	
	MARKNODE(r0)
	move	r0,0(r1)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	$1(r0),r1
	move	1(r0),r0
	jump	gen2Dcd

; Node with 2 nonpointer and 0 pointer part
	.export	g2d20

g2d20:
	SINCR(_ngc20)
	DPREVIOUS(d20)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	$2(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump	gen2Dcpop


; Node with 2 nonpointer and 1 pointer part
	.export	g2d21
g2d21:
	SINCR(_ngc21)
	DQPREVIOUS(d21)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)	; Tag
	move	1(r0),1(INDREG)	; Value
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(INDREG),r1
	move	2(r0),r0
	move	$3(INDREG),INDREG
	jump	gen2Dcd
	DOUTSIDE(d21)
	MARKNODE(r0)
	move	r0,0(r1)
	move	$2(r0),r1
	move	2(r0),r0
	jump	gen2Dcd

; Node with 3 nonpointer and 0 pointer part
	.export	g2dhole		; mark done whan scaning old moved
g2dhole:
	.export	g2d30
g2d30:
	SINCR(_ngc30)
	DPREVIOUS(d30)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	jump gen2Dcpop

/************  Special fixed size nodes **************/

; Indir node
	.export	g2dindir

g2dindir:
	SINCR(_ngcindir)
	DQPREVIOUS(dindir)
	move	1(r0),r0
	jump 	gen2Dcd
	DOUTSIDE(dindir)        ; Think more here, can't do much better but it will probably never happend anyway.
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump	allmovedD

/************ Special fixed size nodes with runtime tables ******/

; Characters
	.export	g2dchr
g2dchr:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MINCHAR
	jlt	g2d20
	comp	DTMP,$ MAXCHAR
	jgt	g2d20
	TABADDR(chartab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2Dcpop

; Integers
	.export	g2dint
g2dint:
	move	1(r0),DTMP		; get value
	comp	DTMP,$ MININTTAB
	jlt	g2d20
	comp	DTMP,$ MAXINTTAB
	jgt	g2d20
	TABADDR(inttab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2Dcpop

; Constructor without arguments
	.export	g2dtag0
g2dtag0:
	move	1(r0),DTMP		; get value
	comp	DTMP,$0
	jlt	g2d20
	comp	DTMP,$ MAXTAG0
	jgt	g2d20
	TABADDR(tag0tab,DTMP,ATMP)	; within range
	move	ATMP,0(r1)
	jump	gen2Dcpop

/************  IO nodes  ********************/

; GC of input directory node, call C routine to set ref bit
	.export g2dind

g2dind:
	DQPREVIOUS(dind)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),DTMP	; remember dir reference
	move	DTMP,1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2Dcpop
	DOUTSIDE(dind)
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	allmovedD


; GC of input file node, call C routine to set ref bit
	.export g2dinp
g2dinp:
	DQPREVIOUS(dinp)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),DTMP	; remember file reference
	move	DTMP,1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	gen2Dcpop
	DOUTSIDE(dinp)
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,_oldhp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	_oldhp,INDREG
	jump 	allmovedD

/************  Function nodes (except VAP) ********************/

	.export	g2dfun

g2dfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
	move	VFpointer(FDISP),0(r1)
; check (& set) the link
gen2dfunz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	gen2Dcpop
	SINCR(_ngcfun)
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),r0	; first entry
	PRINT($MSG_FUN)
	PRINTD(DTMP)
	jump	gen2divekT

; ZAP node (a zapped application)
	.export	g2dzap
g2dzap:
	SINCR(_ngczap)
	DQPREVIOUS(dzap)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)
	move	1(r0),1(INDREG)
	move	2(r0),2(INDREG)
	move	$3(INDREG),INDREG
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	gen2dfunz
	DOUTSIDE(dzap)
	MARKNODE(r0)
	move	1(r0),FDISP		; prepare to do as with a FUN node
	jump	gen2dfunz

/************  Varying size nodes ********************/

; Vektor node with non-pointers
	.export	g2ddvek
g2ddvek:
	SINCR(_ngcvek)
	DPREVIOUS(ddvek)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP		; save new address
	move	DTMP,0(INDREG)
	move	1(r0),DTMP
	move	DTMP,1(INDREG)
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	move	$2(INDREG),INDREG
	jump	gen2ddvekT
gen2ddvekL:
	move	0(r0),0(INDREG)
	move	$1(INDREG),INDREG
	move	$1(r0),r0
gen2ddvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2ddvekL
	jump	gen2Dcpop

; Vektor node with pointers
	.export	g2dvek
g2dvek:
	SINCR(_ngcvek)
	DQPREVIOUS(Dvek)
	move	ogen2dt(ATMP),DTMP
	move	INDREG,ATMP
	move	DTMP,0(INDREG)	; Copy tag
	move	1(r0),DTMP	; and size
	move	DTMP,1(INDREG)
gen2dvekE:
	move	$MOVED,0(r0)	; overwrite with MOVED
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move	$2(r0),r0	; get address of first pointer
	move	$2(INDREG),INDREG
	jump	gen2dvekT
gen2dvekL:
	move	INDREG,Vpush
	move	$1(INDREG),INDREG
	move	0(r0),Vpush
	move	$1(r0),r0
gen2dvekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2dvekL
	jump	gen2Dcpop
	DOUTSIDE(Dvek)
	
	MARKNODE(r0)
g2dTable:			; Used when scanning the table to prevent
	move	1(r0),DTMP	; it from enter the tp-table
gen2divekE:
	move	r0,0(r1)
	move	$2(r0),r0
	jump	gen2divekT
gen2divekL:
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
gen2divekT:
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2divekL
	jump	gen2Dcpop


; Vector apply node
	.export	g2dvap
g2dvap:
	.export	g2dvapG
g2dvapG:
	SINCR(_ngcvap)
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	gen2dvapL1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
gen2dvapL2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	gen2dvapL1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	gen2dvapL2
gen2dvapL1:
	DQPREVIOUS(dvap)
	move	0(r0),ATMP
	move	ogen2dt(ATMP),DTMP
	move	DTMP,0(INDREG)	; Copy tag
	move	1(r0),ATMP	; and function
	move	ATMP,1(INDREG)
	move	VFarity(ATMP),DTMP
	move	INDREG,ATMP
	jump	gen2dvekE  ; r0 = Old address, ATMP = new address, DTMP = size
	DOUTSIDE(dvap)	
	MARKNODE(r0)
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gen2divekE ; r0 = address, r1 = pointer, DTMP = size

/***************   Special gc nodes  *************/

; Marked node
	
; Moved node
	.export	g2dmvd
g2dmvd:
	SINCR(_ngcmvd)
	move	1(r0),0(r1)
	jump	gen2Dcpop




/**************************************************************************/
; r1 -> tp
; INDREG = counter
; r0 -> pointer to node that we are moving
; GCCUR and GCEND  = limit of porevious heap

; ogen2t(ATMP) is the tag for the new copy of this node


; g2s__     scan
/**********/


	.export	g2s11
g2s11:
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump	scanNext

	.export	g2s12
g2s12:
	MARKNODE(r0)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump	scanNext

	.export	g2s21
g2s21:
	MARKNODE(r0)
	move	$2(r0),Vpush
	move	2(r0),Vpush
	jump	scanNext

	.export	g2shole		; mark
g2shole:
	MARKNODE(r0)
	jump	scanNext

	.export	g2sindir
g2sindir:                   ; Think more here, can't do much better but it will probably never happend anyway.
	MARKNODE(r0)
	move	$1(r0),Vpush
	move	1(r0),Vpush
	jump	scanNext

	.export	g2sind
g2sind:
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,saveINDREG
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)
	move	_ep,Sp
	move	_hp,Hp
	move	saveINDREG,INDREG
	jump 	scanNext

	.export	g2sinp
g2sinp:
	move	1(r0),DTMP
	move	Sp,_ep
	move	Hp,_hp
	move	INDREG,saveINDREG
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)
	move	_ep,Sp
	move	_hp,Hp
	move	saveINDREG,INDREG
	jump 	scanNext

	.export	g2sfun
g2sfun:	
	move	1(r0),FDISP		; p to infovector in FDISP
gen2sfunz:
	move	VFlink(FDISP),DTMP
	comp	DTMP,$0
	jne	scanNext
	SINCR(_ngcfun)
	move	_funlink,VFlink(FDISP)
	move	$ VFlink(FDISP),_funlink
; push the references
	move	VFnref(FDISP),DTMP	; number of refs
	move	$ VFrefs(FDISP),ATMP	; first entry
	PRINT($MSG_FUN)
	PRINTD(DTMP)
	jump	gen2svekT	; ATMP -> pointer table,  DTMP = size

	.export	g2szap
g2szap:
	MARKNODE(r0)
	move	1(r0),FDISP		; prepare to do as with a FUN node
	jump	gen2sfunz


	.export	g2svap	; scan
g2svap:
	MARKNODE(r0)
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP
	jump	gen2svekE ; r0 = node address, DTMP = size

	.export	g2svek	; scan
g2svek:
	MARKNODE(r0)
	move	1(r0),DTMP
gen2svekE:                    ; r0 = node address, DTMP = size
	move	$2(r0),ATMP
	jump	gen2svekT
gen2svekL:
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
gen2svekT:	; ATMP -> pointer table,  DTMP = size
	DECR(DTMP)
	TSTC(DTMP)
	jge	gen2svekL
	jump	scanNext

