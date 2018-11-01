#include "machdep.M"
#include "../mcode/limit.h"
#include "gc.h"

#ifdef __ANSI__
#define CAT3(a,b,c) a##b##c
#else
#define CAT3(a,b,c) a/**/b/**/c
#endif

/* #define STATISTICS 1 */

#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif

	.text
	.export	_doGenBaker0
CLABEL(_doGenBaker0)
	CENTRYS(GC_STACK)
	CARG0(r0)        	       	; stack as a vector node,
	jump	realEntryBaker

#if 1

#define PRINT(m)
#define PRINTD(m)
#define PRINTDUMP(m)

#else

#define PRINT(m)     	move ATMP,Vpush && 	move	m,ATMP &&	call	print &&	move Vpop,ATMP

#define PRINTD(m)    	move ATMP,Vpush &&	move	m,ATMP &&	call	printd &&	move Vpop,ATMP

#define PRINTDUMP(m) 	move ATMP,Vpush &&	move	m,ATMP &&	call	printdump &&	move Vpop,ATMP

	.malign
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

	.malign
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
;  OUTSIDE (low) | OLD heap    | scaned | moved |                 |  PREV heap      |         | OUTSIDE (high)
;                ------------------------------------------------------------------------------
;                ^             ^        ^       ^                                   ^    ^    ^
;             STARTHEAP       OLDHP   SCAN     DST                                 ENDU  WU  ENDHEAP
;             startheap       oldhp                               starthp     starttable tp  endtable
#define STARTHEAP GCSTART
#define OLDHP     _oldhp
#define SCAN	  Sp
#define DST       Hp
#define toDST     toH
#define ENDU      GCEND
#define WU        _tp

; Sp = edi   DTMP = ecx  INDREG = esi
; Hp = ebp   ATMP = edx  r0 = ebx
; #define ENDHEAP   GCEND    Doesn't use ENDHEAP

; oBscanO  Scan node but don't increment scanpointer
; oBscan   Scan node and increment scanpointer
; oBmove   Move a node to DST, (or link into gc/fun-link)

#define WRITEUPDATED(reg,tmp,label)	\
	move 	WU,tmp && 		\
	comp	tmp,ENDU &&		\
	jlt	Overflow &&		\
	move	$-1(tmp),tmp &&		\
	move	reg,0(tmp) &&		\
	move	tmp,WU

; QPREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to OUTSIDE(a) or OLDorOUTSIDE(a).

#define QPREVIOUS(kind)                               \
	comp	r0,DST && jlth CAT3(M,kind,qout) && \
	comp	r0,ENDU && jgeh CAT3(M,kind,out)

; PREVIOUS(a) falls through if r0 points into the PREVIOUS part of the heap,
; otherwise it jumps to movenext.

#define PREVIOUS(kind)                                \
	comp	r0,DST && jlth movenext &&             \
	comp	r0,ENDU && jgeh movenext

; OLD_OUTSIDE(a) falls through if r0 points into the OLD part, or OUTSIDE, the heap,
; otherwise it jumps to movenext.

#define OLDorOUTSIDE(kind)                            \
	CAT3(M,kind,qout): &&                        \
	comp	r0,OLDHP && jgeh movenext &&          \
	CAT3(M,kind,out):
	
; OUTSIDE(a) falls through if r0 points into the OLD heap,
; otherwise it jumps to movenext.

#define OUTSIDE(kind)                                 \
	CAT3(M,kind,qout): &&                        \
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
	.export scantp
scantp:	.word 0
	.text

realEntryBaker:
	move	WU,scantp		; save current tp
	move	Sp,SpOld		; We use Sp as SCAN

	move 	_startheap,STARTHEAP
        move    _oldhp,OLDHP
	move	_oldhp,SCAN
	move    _oldhp,DST
	move	_starttable,ENDU		; Oldpointers may grow down to this
;	move 	_endheap,ENDHEAP   Doesn't use endheap
	move	$-1,funlink
	move	$-1,gclink
	move	$-1,oldfunlink
	move	$-1,oldgclink

	move	1(r0),DTMP	; size of vector node
	move	$2(r0),ATMP	; first pointer

	jump	movefirst
	.export movenext
movenext:
	move	r0,0(ATMP)
	move	$1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jeq	scannext

	.export movefirst
movefirst:
	PRINT($MV)
	PRINTDUMP(0(ATMP))
	move	0(ATMP),r0
	comp	r0,_chkaddr
	jne	skip
	move	$1,_Gflag
	move	$-1,_Gno
	PRINT($FOUND)
	PRINTDUMP(r0)
skip:	
	move	0(r0),INDREG
	jump	oBmove(INDREG)

	.data
FOUND:	.string "\nFound "
MV:	.string "\nMove "
SC1:	.string "\nScan1 "
FUN:	.string "\nFun "
SC2:	.string "\nScan2 "
SI:	.string "\nTp "
TP:	.string "--tp = "
	.malign
	.text

	.export scannext
scannext:
	comp	SCAN,DST
	jeq	testlinks
	PRINT($SC1)
	PRINTDUMP(SCAN)
	move	SCAN,r0
	move	0(r0),INDREG
	jump	oBscan(INDREG)

	.export testlinks
testlinks:
	move	funlink,r0
	comp	r0,$-1
	jeq	testnodes
	PRINT($FUN)
	move	0(r0),funlink
	move	oldfunlink,0(r0)
	move	r0,oldfunlink
	move	1(r0),DTMP	; nrefs
	comp	DTMP,$0
	jeq	testlinks       ; take next if empty
	move	$2(r0),ATMP	; refs
	jump	movefirst

	.export testnodes
testnodes:
	move	gclink,r0
	comp	r0,$-1
	jeq	bakertp
	move	0(r0),gclink
	move	oldgclink,0(r0)
	move	r0,oldgclink
	move	$1(r0),r0
	PRINT($SC2)
	PRINTDUMP(r0)
	move	0(r0),INDREG
	jump	oBscanO(INDREG)

	.export	bakertp
bakertp:
	move	scantp,ATMP
scanloop:
	comp	ATMP,_endtable
	jeq	bakerret0
	move	0(ATMP),r0
	move	$1(ATMP),ATMP
	comp	r0,DST
	jlth 	scanit		/* Below DST so not in previous heap */
	comp	r0,ENDU
	jgeh	scanit		/* Above heapend so not in previous heap */
;	move	$0,-1(ATMP)	/* Forget it because it was in the previous heap */
	jump	scanloop	/* Fetch next */
scanit:
	PRINT($SI)
	PRINTD(ATMP)
	PRINTDUMP(r0)
	move	ATMP,scantp
	move	0(r0),INDREG
	jump	oBscanO(INDREG)
	
bakerret0:
	move	oldgclink,r0
	jump	TSTgclink
cleargclinks:
	move	0(r0),r1
	move	$0,0(r0)
	move	r1,r0
TSTgclink:
	comp	r0,$-1
	jne	cleargclinks

bakerret1:
	move	oldfunlink,r0
	jump	TSTfunlink
clearfunlinks:
	move	0(r0),r1
	move	$0,0(r0)
	move	r1,r0
TSTfunlink:
	comp	r0,$-1
	jne	clearfunlinks

	.export bakerret2
bakerret2:
	PRINT($MSG_RET)
	move	DST,_hp
	move	SpOld,Sp
	move	Vp,_garbvp
	CRET
	.data
MSG_RET: .string "CRET"

funlink:	.word 0
oldfunlink:	.word 0
gclink:		.word 0
oldgclink:	.word 0
	.text

;-------------------  NODE CODE -----------------------

	.export	Overflow
	.malign
Overflow:
	move	Sp,_ep
	move    $MSG_Overflow,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

MSG_Overflow:	.string "Overflow in update table"

	.export	Merr
Merr:
	PRINT($MSG_Merr)
	PRINTDUMP(r0)
	jump	movenext

MSG_Merr:	.string "Move on illegal node"

	.export	Serr
Serr:
	PRINT($MSG_Serr)
	PRINTDUMP(r0)
	jump	scannext

MSG_Serr:	.string "Scan on illegal node"

	.export	SOerr
SOerr:
	PRINT($MSG_SOerr)
	PRINTDUMP(r0)
	jump	scannext

MSG_SOerr:	.string "ScanO on illegal node"

;------------  Non pointer nodes

; Special cases for values that have constants in the runtime system.
	.export	Mchr
Mchr:
	move	1(r0),r1		; get value
	comp	r1,$ MINCHAR
	jlt	M20
	comp	r1,$ MAXCHAR
	jgt	M20
	TABADDR(chartab,r1,r0)	; within range
	jump	movenext

	.export	Mint
Mint:
	move	1(r0),r1		; get value
	comp	r1,$ MININTTAB
	jlt	M20
	comp	r1,$ MAXINTTAB
	jgt	M20
	TABADDR(inttab,r1,r0)	; within range
	jump	movenext

	.export	Mtag0
Mtag0:
	move	1(r0),r1		; get value
	comp	r1,$ MINTAG0
	jlt	M20
	comp	r1,$ MAXTAG0
	jgt	M20
	TABADDR(tag0tab,r1,r0)	; within range
	jump	movenext

; Node with 2 nonpointer and 0 pointer part
	.export	M20
M20:
	SINCR(_ngc20)
	PREVIOUS(20)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext

	.export	S20
S20:
	move	$2(SCAN),SCAN
	.export	SO20
SO20:
	jump	scannext

; GC of input directory node, call C routine when scanning.
; might call now if no scanning is done, i.e. old or outside.
	.export Mind
Mind:
QPREVIOUS(ind)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OLDorOUTSIDE(ind)
	move	ATMP,Vpush
	move	DTMP,Vpush
	move	r0,Vpush
	move	1(r0),DTMP
	move	Sp,Vpush
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)     ; set dirref
	move	CRETR,r1
	move	_hp,Hp
	move	Vpop,Sp
	move	Vpop,r0
	move	Vpop,DTMP
	move	Vpop,ATMP
	comp	r1,$0
	jne	movenext
	SINCR(_ngcind)
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Mind)
	move	Vpop,r0
	jump    movenext

	.export	Sind
Sind:
	move	$3(SCAN),SCAN
	.export	SOind
SOind:
	move	ATMP,Vpush
	move	DTMP,Vpush
	move	r0,Vpush
	move	1(r0),DTMP
	move	Sp,Vpush
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setdirref)     ; set dirref
	move	_hp,Hp
	move	Vpop,Sp
	move	Vpop,r0
	move	Vpop,DTMP
	move	Vpop,ATMP
	WRITEUPDATED(r0,r1,Sind)
	jump	scannext


; GC of input node, call C routine to set when scanning, 
; might call now if no scanning is done, i.e. old or outside.
	.export Minp
Minp:
QPREVIOUS(inp)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OLDorOUTSIDE(inp)
	move	ATMP,Vpush
	move	DTMP,Vpush
	move	r0,Vpush
	move	1(r0),DTMP
	move	Sp,Vpush
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)     ; set fileref
	move	CRETR,r1
	move	_hp,Hp
	move	Vpop,Sp
	move	Vpop,r0
	move	Vpop,DTMP
	move	Vpop,ATMP
	comp	r1,$0
	jne	movenext
	SINCR(_ngcinp)
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Minp)
	move	Vpop,r0
	jump    movenext

	.export	Sinp
Sinp:
	move	$3(SCAN),SCAN
	.export	SOinp
SOinp:
	move	ATMP,Vpush
	move	DTMP,Vpush
	move	r0,Vpush
	move	1(r0),DTMP
	move	Sp,Vpush
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_setfileref)     ; set fileref
	move	_hp,Hp
	move	Vpop,Sp
	move	Vpop,r0
	move	Vpop,DTMP
	move	Vpop,ATMP
	WRITEUPDATED(r0,r1,Sinp)
	jump	scannext

; GC of string node, set writeupdated when scanning unless
; node is outside. 
	.export Mstr
Mstr:
QPREVIOUS(str)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OLDorOUTSIDE(str)
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Mstr)
	move	Vpop,r0
	jump    movenext

	.export	Sstr
Sstr:
	move	$3(SCAN),SCAN
	.export	SOstr
SOstr:
	WRITEUPDATED(r0,r1,Sstr)
	jump	scannext

; Node with 3 nonpointer and 0 pointer part
	.export	M30
M30:
	SINCR(_ngc30)
	PREVIOUS(30)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext

	.export	S30
S30:
	move	$3(SCAN),SCAN
	.export	SO30
SO30:
	jump	scannext


; GC of hole node, set writeupdated when scanning unless
; node is outside. 
	.export Mhole
Mhole:
	SINCR(_ngc30)
	QPREVIOUS(hole)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OLDorOUTSIDE(hole)
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Mhole)
	move	Vpop,r0
	jump	movenext

	.export	Shole
Shole:
	move	$3(SCAN),SCAN
	.export	SOhole
SOhole:
	WRITEUPDATED(r0,r1,Shole)
	jump	scannext

; Node with 1 nonpointer and 1 pointer part
	.export	M11
M11:
	SINCR(_ngc11)
	QPREVIOUS(11)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OUTSIDE(11)	
	LINKNODE(11)
	jump	movenext

	.export	S11
S11:
	move	$2(SCAN),SCAN
	.export	SO11
SO11:
	move	$1(r0),ATMP
	move	$1,DTMP
	jump	movefirst

; Node with 1 nonpointer and 2 pointer part
	.export	M12
M12:
	SINCR(_ngccap)
QPREVIOUS(12)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OUTSIDE(12)
	LINKNODE(12)
	jump	movenext

	.export	S12
S12:
	move	$3(SCAN),SCAN
	.export	SO12
SO12:
	move	$1(r0),ATMP
	move	$2,DTMP
	jump	movefirst

; Application node
	.export	Map
Map:
	SINCR(_ngcap)
QPREVIOUS(ap)
	move	DST,r1
	move	$APG,toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OLDorOUTSIDE(ap)
	move	$APG,0(r0)	; This should never happen
	LINKNODE(ap)
	jump	movenext


	.export	Sap
Sap:
	move	$3(SCAN),SCAN
	.export	SOap
SOap:
	move	$APG,0(r0)
	move	$1(r0),ATMP
	move	$2,DTMP
	jump	movefirst

; Node with 2 nonpointer and 1 pointer part
	.export	M21
M21:
	SINCR(_ngc21)
QPREVIOUS(21)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	jump	movenext
OUTSIDE(21)
	LINKNODE(21)
	jump	movenext

	.export	S21
S21:
	move	$3(SCAN),SCAN
	.export	SO21
SO21:
	move	$2(r0),ATMP
	move	$1,DTMP
	jump	movefirst


; Vektor node with pointers
	.export	Mvek
Mvek:
	SINCR(_ngcvek)
	QPREVIOUS(vek)
	move	1(r0),r1
	move	ATMP,Vpush	; I NEED MORE REGISTERS
Mvekt1:                        ; Also used by DVEK and VAP(G)
	move	DST,ATMP	; ATMP is new address
	move	0(r0),toDST
	move	1(r0),toDST
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	$2(r0),r0
	jump	Mv2
Mv1:
	move	0(r0),toDST
	move	$1(r0),r0
	DECR(r1)
Mv2:
	comp	r1,$0
	jne	Mv1

	move	ATMP,r0
	move	Vpop,ATMP
	jump	movenext

OUTSIDE(vek)	
	LINKNODE(vek)
	jump	movenext

	.export	Svek
Svek:
	move	1(r0),r1
	add2	$2,r1
	ADDAW(r1,SCAN)
	.export	SOvek
SOvek:
	move	1(r0),DTMP
	comp	DTMP,$0
	jeq	scannext
	move	$2(r0),ATMP
	jump	movefirst


; Vektor node with nonpointers
	.export	Mdvek
Mdvek:
	SINCR(_ngcvek)
	PREVIOUS(dvek)
	move	1(r0),r1
	move	ATMP,Vpush	; I NEED MORE REGISTERS
	jump	Mvekt1		; r1 = count, r0 = node and ATMP on Vstack

	.export	Sdvek
Sdvek:
	move	1(r0),r1
	add2	$2,r1
	ADDAW(r1,SCAN)
	.export	SOdvek
SOdvek:
	jump	scannext

; Indir node
	.export	Mindir
Mindir:
	SINCR(_ngcindir)
	QPREVIOUS(indi)
	move	1(r0),r0
	move	0(r0),INDREG
	jump	oBmove(INDREG)
OUTSIDE(indi)	
	LINKNODE(indi)
	jump	movenext

	.export	Sindir
Sindir:
	move	$3(SCAN),SCAN
	.export	SOindir
SOindir:
	move	$1(r0),ATMP
	move	$1,DTMP
	jump	movefirst

; Moved node
	.export	Mmvd
Mmvd:
	SINCR(_ngcmvd)
	move	1(r0),r0
	jump	movenext

/**************************************************************************/

; Vector apply node
	.export	Mvap
Mvap:
	move    $VAPG,0(r0)
	.export	MvapG
MvapG:
	QPREVIOUS(vap)
	move	ATMP,Vpush	; I NEED MORE REGISTERS
	move	1(r0),ATMP
	move	VFlink(ATMP),r1
	comp	r1,$0
	jne	Mvap1
	SINCR(_ngcvap)
	move	funlink,VFlink(ATMP)
	move	$VFlink(ATMP),funlink
Mvap1:
	move	1(r0),ATMP
	move	VFarity(ATMP),r1
	jump	Mvekt1		; r1 = count, r0 = node and ATMP on Vstack

OUTSIDE(vap)	
	LINKNODE(vap)
	jump	movenext

	.export	Svap
Svap:
	move    $VAPG,0(r0)
	.export	SvapG
SvapG:
	move	1(r0),r1
	move	VFarity(r1),r1
	add2	$2,r1
	ADDAW(r1,SCAN)
	.export	SOvapG
SOvapG:
	move	1(r0),r1
	move	VFarity(r1),DTMP
	comp    DTMP,$0
        jeq     scannext
	move	$2(r0),ATMP
	jump	movefirst

	.export	SOvap
SOvap:
	move    $VAPG,0(r0)
	move	1(r0),r1
	move	VFarity(r1),DTMP
	comp    DTMP,$0
        jeq     scannext
	move	$2(r0),ATMP
	jump	movefirst

; Function node
	.export	Mfun
Mfun:	
	move	1(r0),r1		; p to infovector in r1
	move	VFpointer(r1),r0
Mfunz:
	move	VFlink(r1),INDREG	; I THINK is is unique on all machines
	comp	INDREG,$0
	jne	movenext
	SINCR(_ngcfun)
	move	funlink,VFlink(r1)
	move	$ VFlink(r1),funlink
	jump	movenext

	.export	Szap
Szap:
	move	$3(SCAN),SCAN
;	jump	scannext		; Must have been moved  (But maybee ????)
	.export	SOzap
SOzap:
#if 1
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Wzap1) 	; Reachable from the stack!
	move	Vpop,r0
#endif
	SINCR(_ngczap)
	/* Fall through */
	.export	SOfun
SOfun:
	move	1(r0),r1		; Infovector in r1
Sfunz:
	move	VFlink(r1),INDREG	; I THINK is is unique on all machines
	comp	INDREG,$0
	jne	scannext
	SINCR(_ngcfun)
	move	funlink,VFlink(r1)
	move	$ VFlink(r1),funlink
	move	1(r0),r1
	jump	scannext

; A ZAP node (a zapped application)
	.export	Mzap
Mzap:
	SINCR(_ngczap)
	QPREVIOUS(zap)
	move	DST,r1
	move	0(r0),toDST
	move	1(r0),toDST
	move	2(r0),toDST
	move	$MOVED,0(r0)
	move	r1,1(r0)
	move	r1,r0
	move	1(r1),r1		; prepare to do as with a FUN node
	jump	Mfunz			; r1 = FunInfo	r0 = new node

	OLDorOUTSIDE(zap)
	LINKNODE(zap)
#if 1
	move	r0,Vpush
	WRITEUPDATED(r0,r1,Wzap1) 	; Reachable from the stack!
	move	Vpop,r0
#endif
	move	1(r0),r1		; prepare to do as with a FUN node
	jump	Mfunz			; r1 = FunInfo	r0 = new node



