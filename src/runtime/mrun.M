/*#define STINGY 1 */

#include "machdep.M"
#include "tagtable.h"

#if (defined(__ANSI__) || defined(__STRICT_ANSI__) || defined(__GNUC__) || defined(__STDC__)) && !defined(__CYGWIN32__)
#define CAT2(a,b) a##b
#define CAT4(a,b,c,d) a##b##c##d
#else
#define CAT2(a,b) a/**/b
#define CAT4(a,b,c,d) a/**/b/**/c/**/d
#endif


#define NoCaseOptim 1
#define CaseTableOptim 1
#define FastTenureNoPointer 0
#define DelayTenureEval     1
#define DelayTenurePointer  1
#define NeverTenureUpdateable 1

	.data
#ifdef HPROFILE
	.word	_USE_heap
#else
	.word	_USE_none
#endif
	.text

#include "gc.h"

#define EXTRA 4000

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


#ifdef m_arm
#define GC_STACK    262144 /* 1048576 */
#define EVAL_STACK  262144 /* 1048576 */
#define PRINT_STACK 262144 /* 1048576 */
#endif

#define CONCTRANS 1

#define STATISTICS 0
#if STATISTICS
#define SINCR(a) add2 $1,a
	SDATAD
	.export	_ngc20
_ngc20:	.word	0
	.export	_ngc20i
_ngc20i:	.word	0
	.export	_ngcinp
_ngcinp:	.word	0
	.export	_ngc30
_ngc30:	.word	0
	.export	_ngc30i
_ngc30i:	.word	0
	.export	_ngc21
_ngczap:	.word	0
	.export	_ngczapi
_ngczapi:	.word	0
	.export	_ngccap
_ngccap:	.word	0
	.export	_ngcap
_ngcap:	.word	0
	.export	_ngc21
_ngc21:	.word	0
	.export	_ngc21i
_ngc21i:	.word	0
	.export	_ngc12
_ngc12:	.word	0
	.export	_ngc12i
_ngc12i:	.word	0
	.export	_ngc11
_ngc11:	.word	0
	.export	_ngc11i
_ngc11i:	.word	0
	.export	_ngcvap
_ngcvap:	.word	0
	.export	_ngcvek
_ngcvek:	.word	0
	.export	_ngcveki
_ngcveki:	.word	0
	.export	_ngcmkd
_ngcmkd:	.word	0
	.export	_ngcm
_ngcm:	.word	0
	.export	_ngcmvd
_ngcmvd:	.word	0
	.export	_ngcindir
_ngcindir:	.word	0
	.text
#else
#define SINCR(a) ;
#endif

#ifdef m_rs6000
	.data
	.malign
	.export	convtmp
convtmp:
	.word	1127219200
	.word	0
	.word	1127219200,-2147483648
#endif
#ifdef m_alpha
	.data
	.malign
	.export	cnvtmp
cnvtmp:	.word	0
	.text
#endif
#ifdef m_80386
	.data
	.malign
	.export	dtmp
	.export	dtmp1
	.export	dtmp2
dtmp:	.word	0,0
dtmp1:	.word	0,0
dtmp2:	.word	0,0
	.export	cnvtmp
	.export	ofmode
	.export	tfmode
cnvtmp:	.word	0,0
ofmode:	.word	0
tfmode:	.word	3199		; 0x00000c7f	truncation mode
	.export	fpr0
	.export	fpr1
	.export	fpr2
	.export	fpr3
	.export	fpr4
	.export	fpr5
	.export	fpr6
	.export	fpr7
	.export	fpr8
	.export	fpr9
	.export	fpr10
	.export	fpr11
	.export	fpr12
	.export	fpr13
	.export	fpr14
	.export	fpr15
fpr0:	.float	0.0e0
fpr1:	.float	0.0e0
fpr2:	.float	0.0e0
fpr3:	.float	0.0e0
fpr4:	.float	0.0e0
fpr5:	.float	0.0e0
fpr6:	.float	0.0e0
fpr7:	.float	0.0e0
fpr8:	.float	0.0e0
fpr9:	.float	0.0e0
fpr10:	.float	0.0e0
fpr11:	.float	0.0e0
fpr12:	.float	0.0e0
fpr13:	.float	0.0e0
fpr14:	.float	0.0e0
fpr15:	.float	0.0e0
	.text
#endif
#ifdef m_cray
	.data
STKDUMMY:	.word	0
	.text
#endif
#ifdef m_sparc
	.data
	.malign
	.export	cnvtmp
cnvtmp:	.word	0,0
#ifndef HPROFILE
	.text
#ifdef NOUNDERSCORE
	.export zero_g7
zero_g7:
	.asm "	sethi	%hi(old_npc),%g7\n	ld	[%g7+%lo(old_npc)],%g7\n	jmp	%g7\n	mov	0,%g7"
#else
	.export _zero_g7
_zero_g7:
	.asm "	sethi	%hi(_old_npc),%g7\n	ld	[%g7+%lo(_old_npc)],%g7\n	jmp	%g7\n	mov	0,%g7"
#endif
#endif
#endif

	.export Ejpai
	.export Ejtag
	.export ijfun
	.export Ejstr
	.export bprt
	.export t0cmp
	.export pget0
	.export pget1
	.export pget2
	.export pget3
	.export icmp
	.export pget4
	.export Ejhol
	.export cprt
	.export tprt
	.export dprt
	.export Ejdvk
	.export vprt
	.export Ejmkd
	.export Ejind
	.export Eprt
	.export Ejinp
	.export Ejint
	.export ieval
	.export gtte
	.export indev
	.export Ejzap
	.export t0prt
	.export Ejmvd
	.export iprt
	.export Ejbig
	.export indevF
	.export g2lE
	.export fev
	.export Ejvek
	.export g2mE
	.export ferr
	.export pcmp
	.export g2pE
	.export bcmpb
	.export Ejflt
	.export g2pN
	.export fevi
	.export g2sE
	.export gett
	.export fret
	.export fvek
	.export f1p
	.export Ejgcr
	.export Errgs_moved
	.export f2p
	.export iunw
	.export scmp
	.export Ejchr
	.export jfVAP
	.export tcmp
	.export dcmp
	.export Poprsb
	.export Ejpa0
	.export Ejpa1
	.export Ejpa2
	.export Ejpa3
	.export Ejpa4
	.export Ejta0
	.export vcmp
	.export pprt

	.text
;

; Handle indirection nodes
	.malign
ieval:
;;;	LBASE		; not needed, no literals
#if 0
	move	1(r0),r0
	move	0(r0),INDREG
	jump	oeval(INDREG)
#else
	move	r0,Spush
	move	1(r0),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	Spop,ATMP
	move	0(r0),0(ATMP)
	move	1(r0),1(ATMP)
	move	2(r0),2(ATMP)
#ifdef HPROFILE
	move    3(r0),3(ATMP)
#ifdef SLOP2
        move    4(r0),4(ATMP)
#endif
#endif
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,ieval_indir)
	return
#endif
	.malign
iunw:
;;;	LBASE		; not needed, no literals
	WRITEUPDATED(r0,iunw_indir)
#if 0
	move	1(r0),r0
	move	0(r0),INDREG
	jump	ounwind(INDREG)
#else
	move	r0, Spush
#if 0
	CCALL(0, _Emsg)
	move    Spop, r0
	move	r0, Spush
#endif
	jumpf	evalupdunw
#endif

	.malign
ijfun:
;;;	LBASE		; not needed, no literals
	move	1(r0),r0
	move	0(r0),INDREG
	jump	ojfun(INDREG)
	

	.malign
Poprsb:	
;;;	LBASE		; not needed, no literals
	move	Vpop,Sp
	return

	.export	Rsb
	.malign
Rsb:	
;;;	LBASE		; not needed, no literals
	return
	.export	Error
	.export	Ezap
	.export	Ehole
	.export	Emkd
	.export	Emvd
	.export	Egcret
	.export	Erru
	.export	Errj
	.export	Errmk
	.export	Errg

	.malign
unimpl:
	LBASE
	move	Sp,_ep
	CCALL(0,_Eunimpl)

	.malign
Error:
	LBASE
	move	Sp,_ep
	CCALL(0,_EError)

	.malign
Erre:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErre)

	.malign
Ezap:
	LBASE
;	move	Sp,_ep
;	CCALL(0,_EErre_zap)
	move	$Eblackholezap,0(Sp)
	jumpf	J1CPthrow

;;; jsp 941116 

	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
Eblackholezap:
	.word	$PAIR1
	.word	CC32
	.word	Eblackholezap1
;	HPROFWORDS($SYSTEMCmp_Cons_LABEL,$0)
#ifdef LINKWORD
        .word   0
#endif
Eblackholezap1:
	.word	STRING
	.word	Eblackholezap1s
	.word	0
;	HPROFWORDS($SYSTEMCmp_String_LABEL,$0)
Eblackholezap1s:
	.string	"Black hole detected in eval zap."
	.text
;;; end jsp 941116	

	.malign
Ehole:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErre_hole)

	.malign
Emkd:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErre_marked)

	.malign
Emvd:
	LBASE
	move	Sp,_ep
	CPUSHARG0(r0)
	CCALL(0,_EErre_moved)

	.data
EGS_MARKED: .string "gc marked scan"
	.text
	.malign
Errgs_marked:
	LBASE
	move	Sp,_ep
	move    $EGS_MARKED,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EGS_MOVED: .string "gc moved scan"
	.text
	.malign
Errgs_moved:
	LBASE
	move	Sp,_ep
	move    $EGS_MOVED,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(1,_EErrgc)
	.malign
Egcret:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErre_gcret)
	.malign
Erru:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErru)
	.malign
Errj:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErrj)
	.malign
Errmk:
	LBASE
	move	Sp,_ep
	CCALL(0,_EErrmk)

	.data
MGETTAG: .string "gettag"
	.text
	.malign
Errg:
	LBASE
	move	Sp,_ep
#if 1
	move    $MGETTAG,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
#else
	CCALL(0,_EErrg)
#endif

	.data
EJINT: .string "jfun on int"
	.text
	.malign
Ejint:
	LBASE
	move	Sp,_ep
	move    $EJINT,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJFLT: .string "jfun on flt"
	.text
	.malign
Ejflt:
	LBASE
	move	Sp,_ep
	move    $EJFLT,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJBIG: .string "jfun on bignum"
	.text
	.malign
Ejbig:
	LBASE
	move	Sp,_ep
	move    $EJBIG,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJCHR: .string "jfun on char"
	.text
	.malign
Ejchr:
	LBASE
	move	Sp,_ep
	move    $EJCHR,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPAI: .string "jfun on pair"
	.text
	.malign
Ejpai:
	LBASE
	move	Sp,_ep
	move    $EJPAI,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPA0: .string "jfun on pair0"
	.text
	.malign
Ejpa0:
	LBASE
	move	Sp,_ep
	move    $EJPA0,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPA1: .string "jfun on pair1"
	.text
	.malign
Ejpa1:
	LBASE
	move	Sp,_ep
	move    $EJPA1,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPA2: .string "jfun on pair2"
	.text
	.malign
Ejpa2:
	LBASE
	move	Sp,_ep
	move    $EJPA2,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPA3: .string "jfun on pair3"
	.text
	.malign
Ejpa3:
	LBASE
	move	Sp,_ep
	move    $EJPA3,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJPA4: .string "jfun on pair4"
	.text
	.malign
Ejpa4:
	LBASE
	move	Sp,_ep
	move    $EJPA4,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJTAG: .string "jfun on tag"
	.text
	.malign
Ejtag:
	LBASE
	move	Sp,_ep
	move    $EJTAG,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJTA0: .string "jfun on tag0"
	.text
	.malign
Ejta0:
	LBASE
	move	Sp,_ep
	move    $EJTA0,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJVEK: .string "jfun on vek"
	.text
	.malign
Ejvek:
	LBASE
	move	Sp,_ep
	move    $EJVEK,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJDVK: .string "jfun on dvek"
	.text
	.malign
Ejdvk:
	LBASE
	move	Sp,_ep
	move    $EJDVK,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJZAP: .string "jfun on zap"
	.text
	.malign
Ejzap:
	LBASE
	move	Sp,_ep
	move    $EJZAP,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJHOL: .string "jfun on hole"
	.text
	.malign
Ejhol:
	LBASE
	move	Sp,_ep
	move    $EJHOL,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJSTR: .string "jfun on string"
	.text
	.malign
Ejstr:
	LBASE
	move	Sp,_ep
	move    $EJSTR,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJINP: .string "jfun on input"
	.text
	.malign
Ejinp:
	LBASE
	move	Sp,_ep
	move    $EJINP,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJIND: .string "jfun on inputd"
	.text
	.malign
Ejind:
	LBASE
	move	Sp,_ep
	move    $EJINP,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJMKD: .string "jfun on marked"
	.text
	.malign
Ejmkd:
	LBASE
	move	Sp,_ep
	move    $EJMKD,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJMVD: .string "jfun on moved"
	.text
	.malign
Ejmvd:
	LBASE
	move	Sp,_ep
	move    $EJMVD,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
EJGCR: .string "jfun on gcret"
	.text
	.malign
Ejgcr:
	LBASE
	move	Sp,_ep
	move    $EJGCR,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.data
G2PE: .string "g2pE"
	.text
	.malign
g2pE:
	LBASE
	move	Sp,_ep
	move    $G2PE,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
G2SE: .string "g2sE"
	.text
	.malign
g2sE:
	LBASE
	move	Sp,_ep
	move    $G2SE,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
G2PN: .string "g2pN"
	.text
	.malign
g2pN:
	LBASE
	move	Sp,_ep
	move    $G2PN,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.data
G2LE: .string "g2lE"
	.text
	.malign
g2lE:
	LBASE
	move	Sp,_ep
	move    $G2LE,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.data
G2ME: .string "g2mE"
	.text
	.malign
g2mE:
	LBASE
	move	Sp,_ep
	move    $G2ME,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.data
G2DE: .string "g2dE"
	.text
	.malign
g2dE:
	LBASE
	move	Sp,_ep
	move    $G2DE,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)
	.data
G2DN: .string "g2dN"
	.text
	.malign
g2dN:
	LBASE
	move	Sp,_ep
	move    $G2DN,r1
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_EErrgc)

	.funend			; place literals here

; gettag, but eval first
	.malign
gtte:
;;;	LBASE
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(r0),INDREG
	jump	ogettag(INDREG)

; ordinary gettag
;;; LBASE	not needed for any of these
gett:	move	1(r0),TAGREG
	return
pget0:	move	$0,TAGREG
	return
pget1:	move	$1,TAGREG
	return
pget2:	move	$2,TAGREG
	return
pget3:	move	$3,TAGREG
	return
pget4:	move	$4,TAGREG
	return

; same as apev, but remembers node that can be updated
	.export	apevG
	.malign
apevG:
	LBASE
	move	Sp,Vpush	; save current stack pointer on the dump
	.export apunwG
#ifdef m_alpha
	jump	apunwG_1
	.malign
apunwG:
	LBASE
apunwG_1:
#else
apunwG:
#endif
	WRITEUPDATED(r0,apGskip)        ; Save address as updated
	move	$AP,0(r0)		; Wonder if this works
	move	r0,Spush	; push pointer to node
	move	1(r0),r0	; get function pointer
	move	0(r0),INDREG
	jump	ounwind(INDREG)	; unwind again.

	.export	apev
	.malign
apev:
	LBASE
	move	Sp,Vpush	; save current stack pointer on the dump
	.export apunw
#ifdef m_alpha
	jump	apunw_1
	.malign
apunw:
	LBASE
#else
apunw:
#endif
apunw_1:
	move	r0,Spush	; push pointer to node
	move	1(r0),r0	; get function pointer
	move	0(r0),INDREG
	jump	ounwind(INDREG)	; unwind again.

	.export	Jfunfew
	.malign
Jfunfew:
	LBASE
Jfunfew_1:
	; when we get here, FDISP points to the FUN node information
	DECR(ARGCREG)
	call	Mkapl		; This should be expanded inline and be CAPified
	move	r0,DTMP		; really an adress
	move	1(Sp),r0
	move	$CAP,0(r0)	; This node is canonical
	move	DTMP,1(r0)
	move	0(Sp),2(r0)
#ifdef HPROFILE
	move    VFhprof(FDISP),3(r0)
;;	move	$SYSTEM_UNKNOWN_LABEL,3(r0)
#ifdef SLOP2
        move    $0,4(r0)
#endif
#endif
	move	$2(Sp),Sp	; used in move above
	jump	apunw_1

	.malign
	.export jfAP
jfVAP:
jfAP:
	LBASE
	DECR(ARGCREG)
	call	Mkapl
	move	r0,ATMP
	move	Spop,DTMP		; really an address
	move	Spop,r0
	move	$APG,0(r0)			; UPDATE WITH APG
	move	ATMP,1(r0)
	move	DTMP,2(r0)
#ifdef HPROFILE
;;	move    3(ATMP),3(r0)
	move	$SYSTEM_UNKNOWN_LABEL,3(r0)
#ifdef SLOP2
        move    $0,4(r0)
#endif
#endif
	jump	apunw_1

/****************************************************************/
/*
**	Make application graph from args on stack
*/
; no check for heap overflow!!!
Mkapl1:
	move	$AP,toH			; AP tag to heap
	move	r0,toH			; fun pointer in r0 to heap
	move	Spop,toH		; arg on stack to heap
#ifdef HPROFILE
;;;        move    3(r0),toH       	; slop 1 from fun part
        move    $SYSTEM_UNKNOWN_LABEL,toH       	; slop 1 from fun part
#ifdef SLOP2
        move    $0,toH               	; slop 2
        move    $-5(Hp),r0
#else
        move    $-4(Hp),r0
#endif
#else
	move	$-3(Hp),r0		; the result node to r0
#endif
	.export	Mkapl			; no of supplied args in ARGCREG
#ifdef m_alpha
	jump	Mkapl_1
	.malign				; need LBASE on boundary
Mkapl:
	LBASE
Mkapl_1:
#else
Mkapl:
#endif
	DECR(ARGCREG)			; zero args? if so, return
	TSTC(ARGCREG)
	jge	Mkapl1
	return

/****************************************************************/
/*	Function nodes
 */

	.export	fununw
	.malign
fununw:
	LBASE
	move	1(r0),FDISP
	jump	VFunwind(FDISP)	

	.export	funj
	.malign
funj:
	LBASE
	move	1(r0),FDISP
	comp	ARGCREG,VFarity(FDISP)
	jlt	Jfunfew_1
#ifdef NOCOMPARE
	comp	ARGCREG,VFarity(FDISP)
#endif
	jgt	funjmany_1
; *Sp[ARGCREG] = ZAP
	jump	VFcode(FDISP)

#if defined(m_80386) || defined(m_cray)
; ATMP&FDISP are identical, fix this!
#undef ATMP
#define ATMP r1
#endif
	.malign
funjmany:
	LBASE
funjmany_1:
; on entry FDISP points to function descriptor
	move	ARGCREG,DTMP
	ADDSP2(DTMP,r0)
	move	0(r0),ATMP		; node to update
funjm1:
	move	$APG,0(ATMP)		; make AP node			UPDATE WITH APG
	move	Hp,1(ATMP)		; new fun part
	move	$-1(r0),r0
	move	0(r0),2(ATMP)		; arg part
#ifdef HPROFILE
	move	VFhprof(FDISP),3(ATMP)
#ifdef SLOP2
	move	$0,4(ATMP)
#endif
#endif
	move	Hp,0(r0)		; change stack entry
	move	Hp,ATMP			; new node to update
#ifdef HPROFILE
#ifdef SLOP2
	move	$5(Hp),Hp
#else
	move	$4(Hp),Hp
#endif
#else
	move	$3(Hp),Hp		; space for the node
#endif
	DECR(DTMP)
	comp	DTMP,VFarity(FDISP)	; still more ?
	jgt	funjm1
	move	$HOLE,0(ATMP)		; make it a hole node
#ifdef HPROFILE
	move    $SYSTEMHole_Hole_LABEL,3(ATMP)
#ifdef SLOP2
        move    $0,4(ATMP)
#endif
#endif
; *Sp[VFarity(FDISP)] = ZAP
	jump	VFcode(FDISP)
#if defined(m_80386) || defined(m_cray)
; reset ATMP
#undef ATMP
#define ATMP realATMP
#endif

;; UNWIND
	.export	unw1 
	.export	unw2 
	.export	unw3 
	.export	unw4 
	.export	unwN
	.export	unw
	.malign
unw1:
	LBASE
	move	1(r0),FDISP
	move	$1(Sp),r0
	comp	r0,0(Vp)
	jgts	juret
	move	0(Sp),r0
	move	2(r0),Spush
	jump	VFcode(FDISP)

	.malign
unw2:
	LBASE
	move	1(r0),FDISP
	move	$2(Sp),r0
	comp	r0,0(Vp)
	jgts	juret
	move	0(Sp),r0
	move	2(r0),Spush
	move	2(Sp),r0
	move	2(r0),1(Sp)
	jump	VFcode(FDISP)
	.malign
unw3:
	LBASE
	move	1(r0),FDISP
	move	$3(Sp),r0
	comp	r0,0(Vp)
	jgts	juret
	move	0(Sp),r0
	move	2(r0),Spush
	move	2(Sp),r0
	move	2(r0),1(Sp)
	move	3(Sp),r0
	move	2(r0),2(Sp)
	jump	VFcode(FDISP)
	.malign
unw4:
	LBASE
	move	1(r0),FDISP
	move	$4(Sp),r0
	comp	r0,0(Vp)
	jgts	juret
	move	0(Sp),r0
	move	2(r0),Spush
	move	2(Sp),r0
	move	2(r0),1(Sp)
	move	3(Sp),r0
	move	2(r0),2(Sp)
	move	4(Sp),r0
	move	2(r0),3(Sp)
	jump	VFcode(FDISP)

	.malign
unw5:unw6:unw7:unw8:unw9:unw10:unw11:unw12:unw13:unw14:unw15:
unwN:
unw:	; general unwind, one size fits all
	LBASE
	move	1(r0),FDISP
	move	VFarity(FDISP),ARGCREG
	ADDSP(ARGCREG)
	comp	Sp,0(Vp)
	jgts	juret
	move	$-1(Sp),Sp
	move	0(Sp),r0
unwloop:	
	move	2(r0),Spush
	move	1(r0),r0
	DECR(ARGCREG)
	TSTC(ARGCREG)
	jgt	unwloop
	jump	VFcode(FDISP)

juret:	move	Vpop,Sp
	move	-1(Sp),r0
; This is only possible if 0(r0) is AP!!! (Not FUN)
;	move	$CAP,0(r0)	; This node is canonical.
	return

/****************************************************************/
/*	Vector application nodes
 */
; same as vapev, but remembers node that can be updated
	.export	vapevG
	.malign
vapevG:
	LBASE
	move	Sp,Vpush
	; fall into ...
	.export	vapunwG
#ifdef m_alpha
	jump	vapunwG_1
	.malign
vapunwG:
	LBASE
#else
vapunwG:
#endif
vapunwG_1:
	WRITEUPDATED(r0,vapGskip)
	move	$VAP,0(r0)		; Wonder if this works
	move	r0,Spush
	move	1(r0),FDISP
	jump	VFvapunwind(FDISP)

	.export	vapev
	.malign
vapev:
	LBASE
	move	Sp,Vpush
	; fall into ...
	.export	vapunw
	.export	unw0
#ifdef m_alpha
	jump	vapunw_1
	.malign
unw0:	; to simplify the codegeneration for nullary functions.
vapunw:
	LBASE
#else
unw0:	; to simplify the codegeneration for nullary functions.
vapunw:
#endif
vapunw_1:
	move	r0,Spush
	move	1(r0),FDISP
	jump	VFvapunwind(FDISP)

	.export	vunwN
	.malign
vunwN:
	LBASE
	move	VFarity(FDISP),DTMP
vunwl:
	DECR(DTMP)
	TSTC(DTMP)
	jle	vunwo
	move	$2(r0),r1
	ADDAW(DTMP,r1)
	move	0(r1),Spush
	jump	vunwl
vunwo:
	move	2(r0),Spush
	jump	VFcode(FDISP)

	.export	vunw0
	.export	vunw1
	.export	vunw2
	.export	vunw3
	.export	vunw4
	.export	vunw5
	.export	vunw6
	.export	vunw7
	.export	vunw8
	.export	vunw9
	.export	vunw10
	.export	vunw11
	.export	vunw12
	.export	vunw13
	.export	vunw14
	.export	vunw15
vunw15:	move	16(r0),Spush
vunw14:	move	15(r0),Spush
vunw13:	move	14(r0),Spush
vunw12:	move	13(r0),Spush
vunw11:	move	12(r0),Spush
vunw10:	move	11(r0),Spush
vunw9:	move	10(r0),Spush
vunw8:	move	9(r0),Spush
; Use special cases for the most common ones because some processors
; cannot handle Spush.
vunw7:	move	8(r0),Spush
	move	7(r0),Spush
	move	6(r0),Spush
	move	5(r0),Spush
	move	4(r0),Spush
	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw6:	move	7(r0),Spush
	move	6(r0),Spush
	move	5(r0),Spush
	move	4(r0),Spush
	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw5:	move	6(r0),Spush
	move	5(r0),Spush
	move	4(r0),Spush
	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw4:	move	5(r0),Spush
	move	4(r0),Spush
	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw3:	move	4(r0),Spush
	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw2:	move	3(r0),Spush
	move	2(r0),Spush
	jump	VFcode(FDISP)
vunw1:	move	2(r0),Spush
vunw0:
	jump	VFcode(FDISP)


/****************************************************************/
/* 	String nodes
 */

	.export	strev
	.export	strevF
	.malign
strev:
strevF:					; !!! should not do writeupdated
	LBASE
	move	1(r0),ATMP
	TSTBIND(ATMP)			; end of string ?
	jne	stre1
	move	$0,1(r0)
	move	$TAG0,0(r0)		; yes: remake to nil
#ifdef HPROFILE
	move    $SYSTEMString_Nil_LABEL,2(r0)
#ifdef SLOP2
        move    $0,3(r0)
#endif
#endif
	return
stre1:
	; test for heap overflow
stre2:
	comp	Hp,_ehp
	jlth	stre3
	move	r0,Spush
	call	GARB
	move	Spop,r0

stre3:
	WRITEUPDATED(r0,strevskip)      ; Save address
	move	1(r0),ATMP		; get str pointer
	move	Hp,2(r0)		; tl part: 
	move	$STRING_F,toH		; make str node, in new heap!
	MOVIB(ATMP,DTMP)		; get the char
	INCBP(ATMP)
	move	ATMP,toH
	move	$1(Hp),Hp		; space enough for pair node
#ifdef HPROFILE
        move    $SYSTEMString_String_LABEL,toH          ; slop 1:      DW
#ifdef SLOP2
        move    $0,toH                                  ; slop 2:      DW
#endif
#endif

	TABADDR(chartab,DTMP,ATMP)
	move	$PAIR1,0(r0)		; no: remake to pair node
	move	ATMP,1(r0)		; pointer to the char in hd part
#ifdef HPROFILE
        move    $SYSTEMString_Cons_LABEL,3(r0)  	; slop 1:      DW
#ifdef SLOP2
        move    $0,4(r0)                        	; slop 2:      DW
#endif
#endif
	return
	.funend

/****************************************************************/
/* 	String nodes
 */

	.export	strnev
	.export	strnevF
	.malign
strnev:
strnevF:					; !!! Should not do writeupdated
	LBASE
; String nodes are CAFs, so they need the same special
; treatment if we want to keep them from being GCed.
	comp	r0,_startheap			; is the node outside the heap?
	jlth	strnevcaf0
	comp	r0,_endheap
	jlth	strnevcaf1
strnevcaf0:
	move	r0,Spush			; push node
	call	entercaf			; and call registration function
	move	Spop,r0				; take it off again
strnevcaf1:
;
	move	2(r0),DTMP
	comp	DTMP,$0
	jne	strne1
	move	$0,1(r0)
	move	$TAG0,0(r0)		; yes: remake to nil
#ifdef HPROFILE
	move    $SYSTEMString_Nil_LABEL,2(r0)
#ifdef SLOP2
        move    $0,3(r0)
#endif
#endif
	return
strne1:
	; test for heap overflow
strne2:
	comp	Hp,_ehp
	jlth	strne3
	move	r0,Spush
	call	GARB
	move	Spop,r0
strne3:
	WRITEUPDATED(r0,strnevskip)      ; Save address, not necessary if NIL
	move	1(r0),ATMP		; get str pointer
	move	2(r0),DTMP		; get counter
	sub2	$1,DTMP			; decrement
	move	DTMP,2(Hp)		; and store it in the new STRINGN node
	move	Hp,2(r0)		; tl part: 
	move	$STRINGN_F,0(Hp)	; make str node
	MOVIB(ATMP,DTMP)		; get the char
	INCBP(ATMP)
	move	ATMP,1(Hp)
	move	$3(Hp),Hp		; bump heap pointer
#ifdef HPROFILE
        move    $SYSTEMString_String_LABEL,toH          ; slop 1:      DW
#ifdef SLOP2
        move    $0,toH                                  ; slop 2:      DW
#endif
#endif

	TABADDR(chartab,DTMP,ATMP)
	move	$PAIR1,0(r0)		; no: remake to pair node
	move	ATMP,1(r0)		; pointer to the char in hd part
#ifdef HPROFILE
        move    $SYSTEMString_Cons_LABEL,3(r0)  	; slop 1:      DW
#ifdef SLOP2
        move    $0,4(r0)                        	; slop 2:      DW
#endif
#endif
	return
	.funend

/****************************************************************/

	.export	inpev
	.export	inpevF
	.malign
inpev:
inpevF:					; !!! should not do writeupdated
	LBASE
	; test for heap overflow
	comp	Hp,_ehp
	jlth	inpe1
	move	r0,Spush
	call	GARB
	move	Spop,r0

inpe1:
	WRITEUPDATED(r0,inpevskip)      ; Save address as updated
	move	r0,Vpush		; save node pointer
	move	2(r0),r1
	move	1(r0),r0		; push file
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG1(r1)
	CPUSHARG0(r0)
	CCALL(2,_readchar)		; read a character
	comp	CRETR,$0		; test for eof
	jge	inpe3
	move	0(Vp),r0		; node pointer back to r0
	comp	2(r0),$-1		; normal input?
	jne	inpe8			; no, don't close
	move	1(r0),r0		; push file
	CPUSHARG0(r0)
	CCALL(1,_closefile)		; close the file
inpe8:
	move	_ep,Sp
	move	_hp,Hp
	move	Vpop,r0			; node pointer back to r0
	move	$TAG0,0(r0)		; yes: list is nil.
	move	$0,1(r0)
#ifdef HPROFILE
	move    $SYSTEMInput_Nil_LABEL,2(r0)
#ifdef SLOP2
        move    $0,3(r0)
#endif
#endif
	return
inpe3:
	move	_ep,Sp
	move	_hp,Hp
	comp	CRETR,256
	jlt	inpe4
	move	$CHAR,0(Hp)
	move	CRETR,1(Hp)
	move	Hp,ATMP
	move	$2(Hp),Hp
	jump	inpe5
inpe4:
	MASKB(CRETR,DTMP)		; mask off high part
	TABADDR(chartab,DTMP,ATMP)
inpe5:
	move	Vpop,r0
	move	2(r0),DTMP		; save 2nd word
	move	Hp,2(r0)		; tl part becomes input
	move	$INPUT_F,toH		; new INPUT node
	move	1(r0),toH		; file info
	comp	DTMP,$-1		; special input node
	jeq	inpe6			; no
	add2	$1,DTMP			; yes, increment pointer
inpe6:
	move	DTMP,toH		; put seek word back
#ifdef HPROFILE
	move    $SYSTEMInput_Input_LABEL,toH
#ifdef SLOP2
        move    $0,toH
#endif
#endif

	move	$PAIR1,0(r0)		; remake to pair node
	move	ATMP,1(r0)
#ifdef HPROFILE
	move    $SYSTEMInput_Cons_LABEL,3(r0)
#ifdef SLOP2
        move    $0,4(r0)
#endif
#endif
	return
	.funend

; Evaluate a directory read
	.malign
indev:
indevF:					; !!! should not do writeupdated
	LBASE
	; test for heap overflow
	comp	Hp,_ehp
	jlth	inde1
	move	r0,Spush
	call	GARB
	move	Spop,r0

inde1:
	WRITEUPDATED(r0,indevskip)      ; Save address, possible updated
	move	r0,Spush		; save node pointer
	move	1(r0),r0		; push file
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_creaddir)		; read a character
	move	_ep,Sp
	move	_hp,Hp
	comp	CRETR,$0		; test for eof
	jne	inde3
	move	0(Sp),r0		; node pointer back to r0
	move	1(r0),r0		; push file
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_cclosedir)		; close the file
	move	_ep,Sp
	move	_hp,Hp
	move	Spop,r0			; node pointer back to r0
	move	$TAG0,0(r0)		; yes: list is nil.
	move	$0,1(r0)
#ifdef HPROFILE
	move    $SYSTEMIndir_Nil_LABEL,2(r0)
#ifdef SLOP2
        move    $0,3(r0)
#endif
#endif
	return
inde3:
	move	CRETR,ATMP
	move	Spop,r0
	move	Hp,2(r0)		; tl part becomes input
	move	$INPUTD_F,toH		; new INPUTD node
	move	1(r0),toH		; file info
	move	$1(Hp),Hp		; more space
#ifdef HPROFILE
	move    $SYSTEMIndir_Indir_LABEL,toH  	/* slop 1:      DW */
#ifdef SLOP2
        move    $0,toH       			/* slop 2:      DW */
#endif
#endif

	move	ATMP,1(r0)
	move	$PAIR1,0(r0)		; remake to pair node
#ifdef HPROFILE
        move    $SYSTEMIndir_Cons_LABEL,3(r0)  	/* slop 1:      DW */
#ifdef SLOP2
        move    $0,4(r0)       			/* slop 2:      DW */
#endif
#endif
	return
	.funend


/****************************************************************/
/** Method call **/
        .data
        .malign
#define CALLMET(n) \
        .word   0 && \
        .export CAT2(CPcallmethod,n) && \
CAT2(CPcallmethod,n): && \
        .word   FUN && \
        .word   CAT2(VCPcallmethod,n) && \
        .export CAT2(VCPcallmethod,n) && \
CAT2(VCPcallmethod,n): && \
        .word   n && \
        .word   CAT2(CPcallmethod,n) && \
        .word   CAT2(unw,n) && \
        .word   CAT2(vunw,n) && \
        .word   CAT4(J,n,CPcallmethod,n) && \
        .word   CAT4(S,n,CPcallmethod,n) && \
        .word   0 && \
        .word   0 && \
        .word   0 && \
        .word   0 && \
        .word   0

CALLMET(2)
CALLMET(3)
CALLMET(4)
CALLMET(5)
CALLMET(6)
CALLMET(7)
CALLMET(8)
CALLMET(9)
CALLMET(10)
CALLMET(11)
CALLMET(12)
CALLMET(13)
CALLMET(14)
CALLMET(15)

        .text
#define CALLMETS(n,m) \
        .export CAT4(S,n,CPcallmethod,n) && \
	.malign && \
CAT4(S,n,CPcallmethod,n): && \
	LBASE && \
	move	CAT2($,m)(Sp),Vpush && \
	jumpf	CAT4(J,n,CPcallmethod,n)

CALLMETS(2,3)
CALLMETS(3,4)
CALLMETS(4,5)
CALLMETS(5,6)
CALLMETS(6,7)
CALLMETS(7,8)
CALLMETS(8,9)
CALLMETS(9,10)
CALLMETS(10,11)
CALLMETS(11,12)
CALLMETS(12,13)
CALLMETS(13,14)
CALLMETS(14,15)
CALLMETS(15,16)
	move	Sp,_ep
	CCALL(0,_EErrcm)

	.malign
        .export J2CPcallmethod2
J2CPcallmethod2:
        .export J3CPcallmethod3
J3CPcallmethod3:
        .export J4CPcallmethod4
J4CPcallmethod4:
        .export J5CPcallmethod5
J5CPcallmethod5:
        .export J6CPcallmethod6
J6CPcallmethod6:
        .export J7CPcallmethod7
J7CPcallmethod7:
        .export J8CPcallmethod8
J8CPcallmethod8:
        .export J9CPcallmethod9
J9CPcallmethod9:
        .export J10CPcallmethod10
J10CPcallmethod10:
        .export J11CPcallmethod11
J11CPcallmethod11:
        .export J12CPcallmethod12
J12CPcallmethod12:
        .export J13CPcallmethod13
J13CPcallmethod13:
        .export J14CPcallmethod14
J14CPcallmethod14:
        .export J15CPcallmethod15
J15CPcallmethod15:
	LBASE
        move    Spop,r0
        move    0(r0),INDREG
#if CaseTableOptim
	comp	oconstr(INDREG),$ O_PAIR0
#else
	comp	INDREG,$PAIR0
	jeq	cmeval1			; gc-gen2 change from	jne	cmeval
	comp	INDREG,$PAIR0_1		; gc-gen2
	jeq	cmeval1			; gc-gen2
	comp	INDREG,$PAIR0_2		; gc-gen2
#endif
	jne	cmeval			; gc-gen2
cmeval1:
	move	Spop,ATMP		; pointer to index node
	move	1(ATMP),ARGCREG		; index in ARGCREG (evaluated)
        move    1(r0),ATMP		; pointer to vector (a function)
        move    2(r0),r0		; dictionary tuple in r0
	move	1(ATMP),INDREG		; pointer to function descriptor
	; Calling convention for the vector is a dictionary pointer in r0
	; and the index in ARGCREG.
	jump	VFcode(INDREG)		; and jump to the function
cmeval:
        call    oeval(INDREG)		; evaluate pair
	jump	cmeval1
        .funend



/****************************************************************/
/*
**	Garbage collection stuff.
*/

; If more than HCLAIM(=1024) words are claimed then NGARB is called to
; check if there are that many bytes available.
; The number of words - HCLAIMED is passed in ARGCREG
	.export	NGARB
	.malign
NGARB:
	LBASE
	move	Hp,r0
	ADDAW(ARGCREG,r0)		; compute upper bound
	comp	r0,_ehp
	jlth	NGARB1			; still enough space
	move	ARGCREG,Vpush
	call	GARB
	move	Vpop,ARGCREG
	move	Hp,r0
	ADDAW(ARGCREG,r0)		; compute upper bound
	comp	r0,_ehp
	jlth	NGARB1			; still enough space
	move	Sp,_ep
	move	Hp,_hp
	CCALL(0,_noheapleft)		; abort (no return)
	move	_ep,Sp
	move	_hp,Hp
NGARB1:
	return

#ifdef m_arm
	.export	GARB_LINK
GARB_LINK:
	.asm "	str	Ret,^\n",Vpush
#endif

	.export	GARB
	.malign
GARB:
	LBASE
	move	Hp,_hp
	move	Sp,_ep
	move	Vp,_garbvp		; for debugging only
        move    $-1,_funlink
	CCALL(0,_dogc)
	move	_hp,Hp
	move	_ep,Sp
	return

/**************************************************************************/
	SDATAD
; A pointer to the main function.  C cannot use any variable names.
	.export	_CCPmain
_CCPmain:.word	CPmain

	.data
	.export	_argvnp
	.export	CPargv
#ifdef LINKWORD
        .word   0
#endif
_argvnp:
CPargv:	.word	TAG0	; The argument node
	.word	0
	.word	0
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif

	.export	_envpnp
	.export	CPenvp
#ifdef LINKWORD
        .word   0
#endif
_envpnp:
CPenvp:	.word	TAG0
	.word	0
	.word	0
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif

	.export	_prognp
	.export	CPprogname
#ifdef LINKWORD
        .word   0
#endif
_prognp:
CPprogname:	.word	TAG0
	.word	0
	.word	0
#ifdef HPROFILE
	.word	0
#ifdef SLOP2
	.word	0
#endif
#endif

;
; _ e v a l :   i n t e r f a c e   t o   C
;
	.text
	.malign
	.export	_eval
CLABEL(_eval)
	CENTRYS(EVAL_STACK)
	move	_ep,Sp
	move	_hp,Hp
	
	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,Spush

	move	Sp,_ep
	move	Hp,_hp
	CRET


; only temporary . . . jump to nolabel is not supposed to happen.
	.export	Nolabel
Nolabel:
	LBASE
	CCALL(0,_ENolabel)

/****************************************************************/
#ifdef HPROFILE
#ifdef SLOP2
#define HPROFCHAR 	.word	$SYSTEMChar_Char_LABEL,$0
#define HPROFINT 	.word	$SYSTEMInt_Int_LABEL,$0
#define HPROFWORDS(w1,w2) 	.word	w1,w2
#define HPROFCHARTAB HPROFCHAR
#define HPROFINTTAB HPROFINT
#define HPROFWORDSTAB(w1,w2) HPROFWORDS(w1, w2)
#else /* SLOP2 */
#define HPROFCHAR 	.word	$SYSTEMChar_Char_LABEL
#define HPROFINT 	.word	$SYSTEMInt_Int_LABEL
#define HPROFWORDS(w1,w2) 	.word	w1
#define HPROFCHARTAB HPROFCHAR,$0
#define HPROFINTTAB 	.word	$SYSTEMInt_Int_LABEL, 0
#define HPROFWORDSTAB(w1,w2) 	.word	w1, 0
#endif /* SLOP2 */
#else /* HPROFILE */
#define HPROFCHAR
#define HPROFINT
#define HPROFWORDS(w1,w2)
#define HPROFCHARTAB
#define HPROFINTTAB
#define HPROFWORDSTAB(w1,w2)
#endif /* HPROFILE */

	SDATAT
	.export	C_HIATON
	.export	_HIATON
_HIATON:
C_HIATON:	.word	CHAR,-268
		HPROFCHAR
	.export	C_CINTRON
	.export	_CINTRON
_CINTRON:
C_CINTRON:	.word	CHAR,-267
		HPROFCHAR
	.export	C_CINTROFF
	.export	_CINTROFF
_CINTROFF:
C_CINTROFF:	.word	CHAR,-266
		HPROFCHAR
	.export	C_CDELAY
	.export	_CDELAY
_CDELAY:
C_CDELAY:	.word	CHAR,-265
		HPROFCHAR
	.export	C_CMHIATON
	.export	_CMHIATON
_CMHIATON:
C_CMHIATON:	.word	CHAR,-264
		HPROFCHAR
	.export	C_TONEWS
	.export	_TONEWS
_TONEWS:
C_TONEWS:	.word	CHAR,-263
		HPROFCHAR
	.export	C_CUNBUFF
	.export	_CUNBUFF
_CUNBUFF:
C_CUNBUFF:	.word	CHAR,-262
		HPROFCHAR
	.export	C_CHIATON
	.export	_CHIATON
_CHIATON:
C_CHIATON:	.word	CHAR,-261
		HPROFCHAR
	.export	C_CCBREAK
	.export	_CCBREAK
_CCBREAK:
C_CCBREAK:	.word	CHAR,-260
		HPROFCHAR
	.export	C_CCOOKED
	.export	_CCOOKED
_CCOOKED:
C_CCOOKED:	.word	CHAR,-259
		HPROFCHAR
	.export	C_CFLUSH
	.export	_CCFLUSH
_CCFLUSH:
C_CFLUSH:	.word	CHAR,-258
		HPROFCHAR
	.export	C_TOFILEA
	.export	_TOFILEA
_TOFILEA:
C_TOFILEA:	.word	CHAR,-257
		HPROFCHAR
	.export	C_TOFILE
	.export	_TOFILE
_TOFILE:
C_TOFILE:	.word	CHAR,-256
		HPROFCHAR
	.export	C_TOSTDOUT
	.export	_TOSTDOUT
_TOSTDOUT:
C_TOSTDOUT:	.word	CHAR,-255
		HPROFCHAR
	.export	C_TOSTDERR
	.export	_TOSTDERR
_TOSTDERR:
C_TOSTDERR:	.word	CHAR,-254
		HPROFCHAR
	.export	C_TOFILE3
	.export	_TOFILE3
_TOFILE3:
C_TOFILE3:	.word	CHAR,-253
		HPROFCHAR
	.export	C_TOFILE4
	.export	_TOFILE4
_TOFILE4:
C_TOFILE4:	.word	CHAR,-252
		HPROFCHAR
	.export	C_TOFILE5
	.export	_TOFILE5
_TOFILE5:
C_TOFILE5:	.word	CHAR,-251
		HPROFCHAR

	.data
	.export	C_PROCESSNO
	.export	_PROCESSNO
_PROCESSNO:
C_PROCESSNO:	.word	INT, 0, 0
	HPROFINT
	.text



/****************************************************************/
/*
**	Some assembler coded functions
**
*/
	.data
	.export	CPfail
CPfail:
	.word	FUN
	.word	VCPfail
	.export	VCPfail
VCPfail:
	.word	1
	.word	CPfail
	.word	unw1
	.word	vunw1
	.word	J1CPfail
	.word	S1CPfail
	.word	0
	.word	0
	.word	0
	.word	0
	.word	4
; All strings that may be evaluated.  They do not belong to Pfail, but this is the easiest way...
	.word	Ecmpmsg,ffloatmsg,fintrmsg,fstkovflmsg
	.text
	.export	S1CPfail
	.export	J1CPfail
	.malign
S1CPfail:
	move	$2(Sp),Vpush
J1CPfail:
	.funbegin CPfail
	jumpf	J1CPthrow

/***************************************************************/
/*
** General compare routines.
**
*/
	.export	Ecmp
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
Ecmpmsg:
	.word	$PAIR1
	.word	CC32
	.word	Ecmpmsg1
	HPROFWORDS($SYSTEMCmp_Cons_LABEL,$0)
#ifdef LINKWORD
        .word   0
#endif
Ecmpmsg1:
	.word	STRING
	.word	Ecmpmsg1s
	.word	0
	HPROFWORDS($SYSTEMCmp_String_LABEL,$0)
Ecmpmsg1s:
	.string	"Cannot compare functions."
	.text
	.malign
Ecmp:
	LBASE
	move	$Ecmpmsg,0(Sp)
	jumpf	J1CPfail

	.export	bigcmp
	.malign
bigcmp:
	LBASE
#ifdef PROFILE
	move	r0,Spush
	move	r1,Spush
	move	$-1(Vp),Vp		; Dummy entry
	move	$bigcmpp,r0
	call	Mcount
	call	Mcountpost
	move	$1(Vp),Vp		; Pop dummy
	move	Spop,r1
	move	Spop,r0
	.data
bigcmpp:	.word	0
	.text
#endif
	move	Sp,Vpush	; save stack pointer, for comparison
	jump	big1

cmploop:	
	comp	Sp,0(Vp)	; have we looked at all comparands?
	jeq	cmpeq1

; Evaluate top two graphs
cmploop1:
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	Spop,r0
	move	0(r0),INDREG
	call	oeval(INDREG)

	move	Spop,r1
	; top two pointers are popped off the stack
	; and are now in r0 and r1
big1:
	move	0(r0),INDREG		; INDREG holds 0(r0)
	jump	ocmp(INDREG)

; compare integer and char
icmp:
	move	1(r0),DTMP
	comp	DTMP,1(r1)		; values equal ?
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	DTMP,1(r1)		; values equal ?
#endif
	jgt	cmpxgt
	jump	cmploop

; compare double float
dcmp:
#if 0
#ifdef NOCOMPARE
	FCGOTO(1(r0),1(r1),dcmpgt,dcmplt)
#else
	FCOMP(1(r0),1(r1))
	FJGT(dcmpgt)
	FJLT(dcmplt)
#endif
#else
	compf	1(r0),1(r1)
	jfgt	cmpxgt
#ifdef NOCOMPARE
	compf	1(r0),1(r1)
#endif
	jflt	cmpxlt
#endif
	jump	cmploop
;;dcmpgt:	jump	cmpxgt
;;dcmplt:	jump	cmpxlt

; compare single float
scmp:
	compsf	1(r0),1(r1)
	jsfgt	cmpxgt
#ifdef NOCOMPARE
	compsf	1(r0),1(r1)
#endif
	jsflt	cmpxlt
	jump	cmploop

bcmpb:
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG1(r0)
	CPUSHARG0(r1)
	CCALL(2, _bicmp)
	move	_ep,Sp
	move	_hp,Hp
	comp	CRETR,$0
	jgt	cmpxgt
#ifdef NOCOMPARE
	comp	CRETR,$0
#endif
	jlt	cmpxlt
	jump	cmploop

; pair compare
pcmp:
#if NoCaseOptim
	move	oconstr(INDREG),DTMP
	move	0(r1),INDREG
	comp	DTMP,oconstr(INDREG)
#else
	comp	INDREG,0(r1)		; tags equal ?
#endif
	jne	cmpretcmp
	move	2(r1),Spush
	move	2(r0),Spush
	move	1(r1),Spush
	move	1(r0),Spush
	jump	cmploop1

; TAG0 compare
t0cmp:
#if NoCaseOptim
	move	oconstr(INDREG),DTMP
	move	0(r1),INDREG
	comp	DTMP,oconstr(INDREG)
#else
	comp	INDREG,0(r1)	; same tag ?
#endif
	jne	cmpretcmp
	move	1(r0),DTMP
	comp	DTMP,1(r1)
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	DTMP,1(r1)		; values equal ?
#endif
	jgt	cmpxgt
	jump	cmploop

; TAG compare
tcmp:
#if NoCaseOptim
	move	oconstr(INDREG),DTMP
	move	0(r1),INDREG
	comp	DTMP,oconstr(INDREG)
#else
	comp	INDREG,0(r1)	; same tag ?
#endif
	jne	cmpretcmp
	move	1(r0),DTMP
	comp	DTMP,1(r1)
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	DTMP,1(r1)		; values equal ?
#endif
	jgt	cmpxgt
	move	2(r1),Spush
	move	2(r0),Spush
	jump	cmploop1

; BVEK compare
bvcmp:
	move	1(r0),DTMP
	comp	DTMP,1(r1)	; compare sizes
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	DTMP,1(r1)
#endif
	jgt	cmpxgt
	comp	DTMP,$0
	jeq	cmploop		; size is 0 so there is nothing to do
	move	$1(r0),r0
	ADDAW(DTMP,r0)
	move	$1(r1),ATMP
	ADDAW(DTMP,ATMP)
bvcmpl:	comp	0(r0),0(ATMP)
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	0(r0),0(ATMP)
#endif
	jgt	cmpxgt
	move	$-1(r0),r0
	move	$-1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jne	bvcmpl
	jump	cmploop

; VEK compare
vcmp:
	move	1(r0),DTMP
	comp	DTMP,1(r1)
	jgt	cmpxgt
#ifdef NOCOMPARE
	comp	DTMP,1(r1)
#endif
	jlt	cmpxlt
	move	$2(r1),r1
	ADDAW(DTMP,r1)
	move	$2(r0),r0
	ADDAW(DTMP,r0)
	jump	vcmp1
vcmp2:
	move	$-1(r1),r1
	move	0(r1),Spush
	move	$-1(r0),r0
	move	0(r0),Spush
vcmp1:
	DECR(DTMP)
	TSTC(DTMP)
	jge	vcmp2
	jump	cmploop1

; unequal constructors, must compare constructor numbers
cmpretcmp:
	move	0(r0),INDREG
	call	ogettag(INDREG)
	move	TAGREG,CDTMP
	move	r1,r0
	move	0(r0),INDREG
	call	ogettag(INDREG)
	comp	CDTMP,TAGREG
	jlt	cmpxlt
cmpxgt:
	move	$1,BIGEQREG
	move	Vpop,Sp
	return
cmpxlt:
	move	$-1,BIGEQREG
	move	Vpop,Sp
	return

cmpeq1:
	move	$0,BIGEQREG
	move	Vpop,Sp
	return

/****************************************************************/
/*
** Print routines.
*/

/* A print buffer to avoid calling C every time */
	SDATAD
	.export _bufp
_bufp:	.word	_outbuf
	.export	_endbuf
_endbuf: .word	_outbufend
	.data
	.export	_outbuf
_outbuf: .word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0		; 16 words (64 bytes?)
	.word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.export	_outbufend
_outbufend:
	.word	0
	.text

/* 
** Add the character in DTMP to the output buffer and flush
** if it is full, or if the character is strange.
*/
#define ADDBUF(l) \
	comp	DTMP,$0 &&\
	jlt	CAT2(l,_fls) &&\
	comp	DTMP,$255 &&\
	jgt	CAT2(l,_fls) &&\
	comp	DTMP,$10 &&\
	jeq	CAT2(l,_fls) &&\
	comp	ATMP,_endbuf &&\
	jge	CAT2(l,_fls) &&\
	STIB(DTMP,ATMP) &&\
	INCBP(ATMP) &&\
	jump	CAT2(l,_end) &&\
CAT2(l,_fls): &&\
	move	r0,Vpush &&\
	move	r1,Vpush &&\
	move	Sp,_ep &&\
	move	Hp,_hp &&\
	CPUSHARG1(DTMP) &&\
	CPUSHARG0(ATMP) &&\
	CCALL(2,_printflsh) &&\
	move	_ep,Sp &&\
	move	_hp,Hp &&\
	move	$_outbuf,ATMP &&\
	move	Vpop,r1 &&\
	move	Vpop,r0 &&\
CAT2(l,_end):

#define STARTBUF \
	move	_bufp,ATMP

#define ENDBUF \
	move	ATMP,_bufp

#define FLUSHBUF \
	move	r0,Vpush &&\
	move	Sp,_ep &&\
	move	Hp,_hp &&\
	CPUSHARG1(-2) &&\
	CPUSHARG0(_bufp) &&\
	CCALL(2,_printflsh) &&\
	move	_ep,Sp &&\
	move	_hp,Hp &&\
	move	Vpop,r0 &&\
	move	$_outbuf,_bufp

	.export	_flushlow
CLABEL(_flushlow)
	CENTRY
	move	_ep,Sp
	move	_hp,Hp
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CRET

	.export	_print
CLABEL(_print)
	CENTRYS(PRINT_STACK)
	CARG0(r0)

; set registers
	move	_ep,Sp
	move	_hp,Hp

	comp	_garbvp0,$0
	jne	nested
	move	Vp,_garbvp0
nested:
	move	Sp,Vpush	; save stack pointer, for comparison
	move	r0,Spush
	jump	prtloop1

prtloop:
	comp	Sp,0(Vp)	; all printed ?
	jlts	prtloop1
	FLUSHBUF
	move	Vpop,Sp
; restore global vars
	move	Sp,_ep
	move	Hp,_hp
	CRET

prtloop1:
	move	Spop,r0

#if CaseTableOptim
	move	0(r0),INDREG
	comp	oconstr(INDREG),$ O_STRINGN
#else
	comp	0(r0),$STRINGN
	jeq	prt12			; gc-gen2
	comp	0(r0),$STRINGN_F	; gc-gen2
	jeq	prt12			; gc-gen2
	comp	0(r0),$STRINGN_1	; gc-gen2
	jeq	prt12			; gc-gen2
	comp	0(r0),$STRINGN_2	; gc-gen2
#endif
	jne	prt1
prt12:					; gc-gen2
#if 0
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG1(2(r0))		; handle strings specially
	CPUSHARG0(1(r0))
	CCALL(2,_printsn)
	move	_ep,Sp
	move	_hp,Hp
	jump	prtloop
#else
	STARTBUF
	move	2(r0),r1		; counter
	move	1(r0),r0		; pointer to next byte
prtsnl:
	DECR(r1)
	comp	r1,$0			; cannat use TSTc macro since it assumes a data register
	jlt	prtsnle
	MOVIB(r0,DTMP)
	INCBP(r0)
	ADDBUF(strn)
	jump	prtsnl
prtsnle:
	ENDBUF
	jump	prtloop
#endif
prt1:
#if CONCTRANS
#if CaseTableOptim
	move	0(r0),INDREG
	comp	oconstr(INDREG),$ O_VAP
#else
	comp	0(r0),$VAP
	jeq	prt40			; gc-gen2
	comp	0(r0),$VAP_1		; gc-gen2
	jeq	prt40			; gc-gen2
	comp	0(r0),$VAP_2		; gc-gen2
#endif
	jne	prt2
prt40:					; gc-gen2
	comp	1(r0),$VC_$40	; handle @ specially
	jne	prt2
	move	3(r0),Spush
	move	2(r0),Spush
	jump	prtloop1
#endif
prt2:
; Evaluate top graph
	move	0(r0),INDREG
	comp	INDREG,$CANON
	jlt	prdoev
	jump	oprint(INDREG)
prdoev:
	call	oeval(INDREG)
	move	0(r0),INDREG
	jump	oprint(INDREG)

iprt:
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(1(r0))
	CCALL(1,_printi)
	move	_ep,Sp
	move	_hp,Hp
	jump	prtloop

dprt:
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_printd)
	move	_ep,Sp
	move	_hp,Hp
	jump	prtloop

bprt:
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_printb)
	move	_ep,Sp
	move	_hp,Hp
	jump	prtloop

cprt:
	STARTBUF
	move	1(r0),DTMP
	ADDBUF(cprt)
	ENDBUF
	jump	prtloop

#if 0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(1(r0))
	CCALL(1,_printc)
	move	_ep,Sp
	move	_hp,Hp
	jump	prtloop
#endif

pprt:
	move	2(r0),Spush
	move	1(r0),Spush
	jump	prtloop1

tprt:
	move	2(r0),Spush
	jump	prtloop1

t0prt:
	jump	prtloop

bvprt:
	FLUSHBUF
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_printbvek)
	move	_ep,Sp
	move	_hp,Hp
	jump    prtloop

vprt:
	move	1(r0),DTMP
	move	$2(r0),r0
	ADDAW(DTMP,r0)
	jump	vprt1
vprt2:
	move	$-1(r0),r0
	move	0(r0),Spush
vprt1:
	DECR(DTMP)
	TSTC(DTMP)
	jge	vprt2
	jump	prtloop1


Eprt:
	CPUSHARG0(r0)
	CCALL(1,_printerr)
	; no return

/****************************************************************/
realfail:
	LBASE
	FLUSHBUF
	move	0(Sp),Spush
	move	Sp,_ep
	move	Hp,_hp
	CCALL(0,_fail) ; will not return

/****************************************************************/
/*
**	Some commonly used integer, character and CNIL nodes.
**	This is to avoid emmiting them each time in the code generator.
*/
	SDATAT
	.malign
	.export	CIm10		; negative integers
CIm10:	.word	INT,-10
	HPROFINTTAB
	.export	CIm9
CIm9:	.word	INT,-9
	HPROFINTTAB
	.export	CIm8
CIm8:	.word	INT,-8
	HPROFINTTAB
	.export	CIm7
CIm7:	.word	INT,-7
	HPROFINTTAB
	.export	CIm6
CIm6:	.word	INT,-6
	HPROFINTTAB
	.export	CIm5
CIm5:	.word	INT,-5
	HPROFINTTAB
	.export	CIm4
CIm4:	.word	INT,-4
	HPROFINTTAB
	.export	CIm3
CIm3:	.word	INT,-3
	HPROFINTTAB
	.export	CIm2
CIm2:	.word	INT,-2
	HPROFINTTAB
	.export	CIm1
CIm1:	.word	INT,-1
	HPROFINTTAB
	.export	_cinttab
_cinttab:
	.export	inttab
inttab:
	.export	CIp0
CIp0:	.word	INT,0
	HPROFINTTAB
	.export	CIp1
CIp1:	.word	INT,1
	HPROFINTTAB
	.export	CIp2
CIp2:	.word	INT,2
	HPROFINTTAB
	.export	CIp3
CIp3:	.word	INT,3
	HPROFINTTAB
	.export	CIp4
CIp4:	.word	INT,4
	HPROFINTTAB
	.export	CIp5
CIp5:	.word	INT,5
	HPROFINTTAB
	.export	CIp6
CIp6:	.word	INT,6
	HPROFINTTAB
	.export	CIp7
CIp7:	.word	INT,7
	HPROFINTTAB
	.export	CIp8
CIp8:	.word	INT,8
	HPROFINTTAB
	.export	CIp9
CIp9:	.word	INT,9
	HPROFINTTAB
	.export	CIp10
CIp10:	.word	INT,10
	HPROFINTTAB

	.export	_cchartab
_cchartab:
	.export	chartab
chartab:
	.export	CC0
CC0:	.word	CHAR,0
	HPROFCHARTAB
	.export	CC1
CC1:	.word	CHAR,1
	HPROFCHARTAB
	.export	CC2
CC2:	.word	CHAR,2
	HPROFCHARTAB
	.export	CC3
CC3:	.word	CHAR,3
	HPROFCHARTAB
	.export	CC4
CC4:	.word	CHAR,4
	HPROFCHARTAB
	.export	CC5
CC5:	.word	CHAR,5
	HPROFCHARTAB
	.export	CC6
CC6:	.word	CHAR,6
	HPROFCHARTAB
	.export	CC7
CC7:	.word	CHAR,7
	HPROFCHARTAB
	.export	CC8
CC8:	.word	CHAR,8
	HPROFCHARTAB
	.export	CC9
CC9:	.word	CHAR,9
	HPROFCHARTAB
	.export	CC10
CC10:	.word	CHAR,10
	HPROFCHARTAB
	.export	CC11
CC11:	.word	CHAR,11
	HPROFCHARTAB
	.export	CC12
CC12:	.word	CHAR,12
	HPROFCHARTAB
	.export	CC13
CC13:	.word	CHAR,13
	HPROFCHARTAB
	.export	CC14
CC14:	.word	CHAR,14
	HPROFCHARTAB
	.export	CC15
CC15:	.word	CHAR,15
	HPROFCHARTAB
	.export	CC16
CC16:	.word	CHAR,16
	HPROFCHARTAB
	.export	CC17
CC17:	.word	CHAR,17
	HPROFCHARTAB
	.export	CC18
CC18:	.word	CHAR,18
	HPROFCHARTAB
	.export	CC19
CC19:	.word	CHAR,19
	HPROFCHARTAB
	.export	CC20
CC20:	.word	CHAR,20
	HPROFCHARTAB
	.export	CC21
CC21:	.word	CHAR,21
	HPROFCHARTAB
	.export	CC22
CC22:	.word	CHAR,22
	HPROFCHARTAB
	.export	CC23
CC23:	.word	CHAR,23
	HPROFCHARTAB
	.export	CC24
CC24:	.word	CHAR,24
	HPROFCHARTAB
	.export	CC25
CC25:	.word	CHAR,25
	HPROFCHARTAB
	.export	CC26
CC26:	.word	CHAR,26
	HPROFCHARTAB
	.export	CC27
CC27:	.word	CHAR,27
	HPROFCHARTAB
	.export	CC28
CC28:	.word	CHAR,28
	HPROFCHARTAB
	.export	CC29
CC29:	.word	CHAR,29
	HPROFCHARTAB
	.export	CC30
CC30:	.word	CHAR,30
	HPROFCHARTAB
	.export	CC31
CC31:	.word	CHAR,31
	HPROFCHARTAB
	.export	CC32
CC32:	.word	CHAR,32
	HPROFCHARTAB
	.export	CC33
CC33:	.word	CHAR,33
	HPROFCHARTAB
	.export	CC34
CC34:	.word	CHAR,34
	HPROFCHARTAB
	.export	CC35
CC35:	.word	CHAR,35
	HPROFCHARTAB
	.export	CC36
CC36:	.word	CHAR,36
	HPROFCHARTAB
	.export	CC37
CC37:	.word	CHAR,37
	HPROFCHARTAB
	.export	CC38
CC38:	.word	CHAR,38
	HPROFCHARTAB
	.export	CC39
CC39:	.word	CHAR,39
	HPROFCHARTAB
	.export	CC40
CC40:	.word	CHAR,40
	HPROFCHARTAB
	.export	CC41
CC41:	.word	CHAR,41
	HPROFCHARTAB
	.export	CC42
CC42:	.word	CHAR,42
	HPROFCHARTAB
	.export	CC43
CC43:	.word	CHAR,43
	HPROFCHARTAB
	.export	CC44
CC44:	.word	CHAR,44
	HPROFCHARTAB
	.export	CC45
CC45:	.word	CHAR,45
	HPROFCHARTAB
	.export	CC46
CC46:	.word	CHAR,46
	HPROFCHARTAB
	.export	CC47
CC47:	.word	CHAR,47
	HPROFCHARTAB
	.export	CC48
CC48:	.word	CHAR,48
	HPROFCHARTAB
	.export	CC49
CC49:	.word	CHAR,49
	HPROFCHARTAB
	.export	CC50
CC50:	.word	CHAR,50
	HPROFCHARTAB
	.export	CC51
CC51:	.word	CHAR,51
	HPROFCHARTAB
	.export	CC52
CC52:	.word	CHAR,52
	HPROFCHARTAB
	.export	CC53
CC53:	.word	CHAR,53
	HPROFCHARTAB
	.export	CC54
CC54:	.word	CHAR,54
	HPROFCHARTAB
	.export	CC55
CC55:	.word	CHAR,55
	HPROFCHARTAB
	.export	CC56
CC56:	.word	CHAR,56
	HPROFCHARTAB
	.export	CC57
CC57:	.word	CHAR,57
	HPROFCHARTAB
	.export	CC58
CC58:	.word	CHAR,58
	HPROFCHARTAB
	.export	CC59
CC59:	.word	CHAR,59
	HPROFCHARTAB
	.export	CC60
CC60:	.word	CHAR,60
	HPROFCHARTAB
	.export	CC61
CC61:	.word	CHAR,61
	HPROFCHARTAB
	.export	CC62
CC62:	.word	CHAR,62
	HPROFCHARTAB
	.export	CC63
CC63:	.word	CHAR,63
	HPROFCHARTAB
	.export	CC64
CC64:	.word	CHAR,64
	HPROFCHARTAB
	.export	CC65
CC65:	.word	CHAR,65
	HPROFCHARTAB
	.export	CC66
CC66:	.word	CHAR,66
	HPROFCHARTAB
	.export	CC67
CC67:	.word	CHAR,67
	HPROFCHARTAB
	.export	CC68
CC68:	.word	CHAR,68
	HPROFCHARTAB
	.export	CC69
CC69:	.word	CHAR,69
	HPROFCHARTAB
	.export	CC70
CC70:	.word	CHAR,70
	HPROFCHARTAB
	.export	CC71
CC71:	.word	CHAR,71
	HPROFCHARTAB
	.export	CC72
CC72:	.word	CHAR,72
	HPROFCHARTAB
	.export	CC73
CC73:	.word	CHAR,73
	HPROFCHARTAB
	.export	CC74
CC74:	.word	CHAR,74
	HPROFCHARTAB
	.export	CC75
CC75:	.word	CHAR,75
	HPROFCHARTAB
	.export	CC76
CC76:	.word	CHAR,76
	HPROFCHARTAB
	.export	CC77
CC77:	.word	CHAR,77
	HPROFCHARTAB
	.export	CC78
CC78:	.word	CHAR,78
	HPROFCHARTAB
	.export	CC79
CC79:	.word	CHAR,79
	HPROFCHARTAB
	.export	CC80
CC80:	.word	CHAR,80
	HPROFCHARTAB
	.export	CC81
CC81:	.word	CHAR,81
	HPROFCHARTAB
	.export	CC82
CC82:	.word	CHAR,82
	HPROFCHARTAB
	.export	CC83
CC83:	.word	CHAR,83
	HPROFCHARTAB
	.export	CC84
CC84:	.word	CHAR,84
	HPROFCHARTAB
	.export	CC85
CC85:	.word	CHAR,85
	HPROFCHARTAB
	.export	CC86
CC86:	.word	CHAR,86
	HPROFCHARTAB
	.export	CC87
CC87:	.word	CHAR,87
	HPROFCHARTAB
	.export	CC88
CC88:	.word	CHAR,88
	HPROFCHARTAB
	.export	CC89
CC89:	.word	CHAR,89
	HPROFCHARTAB
	.export	CC90
CC90:	.word	CHAR,90
	HPROFCHARTAB
	.export	CC91
CC91:	.word	CHAR,91
	HPROFCHARTAB
	.export	CC92
CC92:	.word	CHAR,92
	HPROFCHARTAB
	.export	CC93
CC93:	.word	CHAR,93
	HPROFCHARTAB
	.export	CC94
CC94:	.word	CHAR,94
	HPROFCHARTAB
	.export	CC95
CC95:	.word	CHAR,95
	HPROFCHARTAB
	.export	CC96
CC96:	.word	CHAR,96
	HPROFCHARTAB
	.export	CC97
CC97:	.word	CHAR,97
	HPROFCHARTAB
	.export	CC98
CC98:	.word	CHAR,98
	HPROFCHARTAB
	.export	CC99
CC99:	.word	CHAR,99
	HPROFCHARTAB
	.export	CC100
CC100:	.word	CHAR,100
	HPROFCHARTAB
	.export	CC101
CC101:	.word	CHAR,101
	HPROFCHARTAB
	.export	CC102
CC102:	.word	CHAR,102
	HPROFCHARTAB
	.export	CC103
CC103:	.word	CHAR,103
	HPROFCHARTAB
	.export	CC104
CC104:	.word	CHAR,104
	HPROFCHARTAB
	.export	CC105
CC105:	.word	CHAR,105
	HPROFCHARTAB
	.export	CC106
CC106:	.word	CHAR,106
	HPROFCHARTAB
	.export	CC107
CC107:	.word	CHAR,107
	HPROFCHARTAB
	.export	CC108
CC108:	.word	CHAR,108
	HPROFCHARTAB
	.export	CC109
CC109:	.word	CHAR,109
	HPROFCHARTAB
	.export	CC110
CC110:	.word	CHAR,110
	HPROFCHARTAB
	.export	CC111
CC111:	.word	CHAR,111
	HPROFCHARTAB
	.export	CC112
CC112:	.word	CHAR,112
	HPROFCHARTAB
	.export	CC113
CC113:	.word	CHAR,113
	HPROFCHARTAB
	.export	CC114
CC114:	.word	CHAR,114
	HPROFCHARTAB
	.export	CC115
CC115:	.word	CHAR,115
	HPROFCHARTAB
	.export	CC116
CC116:	.word	CHAR,116
	HPROFCHARTAB
	.export	CC117
CC117:	.word	CHAR,117
	HPROFCHARTAB
	.export	CC118
CC118:	.word	CHAR,118
	HPROFCHARTAB
	.export	CC119
CC119:	.word	CHAR,119
	HPROFCHARTAB
	.export	CC120
CC120:	.word	CHAR,120
	HPROFCHARTAB
	.export	CC121
CC121:	.word	CHAR,121
	HPROFCHARTAB
	.export	CC122
CC122:	.word	CHAR,122
	HPROFCHARTAB
	.export	CC123
CC123:	.word	CHAR,123
	HPROFCHARTAB
	.export	CC124
CC124:	.word	CHAR,124
	HPROFCHARTAB
	.export	CC125
CC125:	.word	CHAR,125
	HPROFCHARTAB
	.export	CC126
CC126:	.word	CHAR,126
	HPROFCHARTAB
	.export	CC127
CC127:	.word	CHAR,127
	HPROFCHARTAB
	.export	CC128
CC128:	.word	CHAR,128
	HPROFCHARTAB
	.export	CC129
CC129:	.word	CHAR,129
	HPROFCHARTAB
	.export	CC130
CC130:	.word	CHAR,130
	HPROFCHARTAB
	.export	CC131
CC131:	.word	CHAR,131
	HPROFCHARTAB
	.export	CC132
CC132:	.word	CHAR,132
	HPROFCHARTAB
	.export	CC133
CC133:	.word	CHAR,133
	HPROFCHARTAB
	.export	CC134
CC134:	.word	CHAR,134
	HPROFCHARTAB
	.export	CC135
CC135:	.word	CHAR,135
	HPROFCHARTAB
	.export	CC136
CC136:	.word	CHAR,136
	HPROFCHARTAB
	.export	CC137
CC137:	.word	CHAR,137
	HPROFCHARTAB
	.export	CC138
CC138:	.word	CHAR,138
	HPROFCHARTAB
	.export	CC139
CC139:	.word	CHAR,139
	HPROFCHARTAB
	.export	CC140
CC140:	.word	CHAR,140
	HPROFCHARTAB
	.export	CC141
CC141:	.word	CHAR,141
	HPROFCHARTAB
	.export	CC142
CC142:	.word	CHAR,142
	HPROFCHARTAB
	.export	CC143
CC143:	.word	CHAR,143
	HPROFCHARTAB
	.export	CC144
CC144:	.word	CHAR,144
	HPROFCHARTAB
	.export	CC145
CC145:	.word	CHAR,145
	HPROFCHARTAB
	.export	CC146
CC146:	.word	CHAR,146
	HPROFCHARTAB
	.export	CC147
CC147:	.word	CHAR,147
	HPROFCHARTAB
	.export	CC148
CC148:	.word	CHAR,148
	HPROFCHARTAB
	.export	CC149
CC149:	.word	CHAR,149
	HPROFCHARTAB
	.export	CC150
CC150:	.word	CHAR,150
	HPROFCHARTAB
	.export	CC151
CC151:	.word	CHAR,151
	HPROFCHARTAB
	.export	CC152
CC152:	.word	CHAR,152
	HPROFCHARTAB
	.export	CC153
CC153:	.word	CHAR,153
	HPROFCHARTAB
	.export	CC154
CC154:	.word	CHAR,154
	HPROFCHARTAB
	.export	CC155
CC155:	.word	CHAR,155
	HPROFCHARTAB
	.export	CC156
CC156:	.word	CHAR,156
	HPROFCHARTAB
	.export	CC157
CC157:	.word	CHAR,157
	HPROFCHARTAB
	.export	CC158
CC158:	.word	CHAR,158
	HPROFCHARTAB
	.export	CC159
CC159:	.word	CHAR,159
	HPROFCHARTAB
	.export	CC160
CC160:	.word	CHAR,160
	HPROFCHARTAB
	.export	CC161
CC161:	.word	CHAR,161
	HPROFCHARTAB
	.export	CC162
CC162:	.word	CHAR,162
	HPROFCHARTAB
	.export	CC163
CC163:	.word	CHAR,163
	HPROFCHARTAB
	.export	CC164
CC164:	.word	CHAR,164
	HPROFCHARTAB
	.export	CC165
CC165:	.word	CHAR,165
	HPROFCHARTAB
	.export	CC166
CC166:	.word	CHAR,166
	HPROFCHARTAB
	.export	CC167
CC167:	.word	CHAR,167
	HPROFCHARTAB
	.export	CC168
CC168:	.word	CHAR,168
	HPROFCHARTAB
	.export	CC169
CC169:	.word	CHAR,169
	HPROFCHARTAB
	.export	CC170
CC170:	.word	CHAR,170
	HPROFCHARTAB
	.export	CC171
CC171:	.word	CHAR,171
	HPROFCHARTAB
	.export	CC172
CC172:	.word	CHAR,172
	HPROFCHARTAB
	.export	CC173
CC173:	.word	CHAR,173
	HPROFCHARTAB
	.export	CC174
CC174:	.word	CHAR,174
	HPROFCHARTAB
	.export	CC175
CC175:	.word	CHAR,175
	HPROFCHARTAB
	.export	CC176
CC176:	.word	CHAR,176
	HPROFCHARTAB
	.export	CC177
CC177:	.word	CHAR,177
	HPROFCHARTAB
	.export	CC178
CC178:	.word	CHAR,178
	HPROFCHARTAB
	.export	CC179
CC179:	.word	CHAR,179
	HPROFCHARTAB
	.export	CC180
CC180:	.word	CHAR,180
	HPROFCHARTAB
	.export	CC181
CC181:	.word	CHAR,181
	HPROFCHARTAB
	.export	CC182
CC182:	.word	CHAR,182
	HPROFCHARTAB
	.export	CC183
CC183:	.word	CHAR,183
	HPROFCHARTAB
	.export	CC184
CC184:	.word	CHAR,184
	HPROFCHARTAB
	.export	CC185
CC185:	.word	CHAR,185
	HPROFCHARTAB
	.export	CC186
CC186:	.word	CHAR,186
	HPROFCHARTAB
	.export	CC187
CC187:	.word	CHAR,187
	HPROFCHARTAB
	.export	CC188
CC188:	.word	CHAR,188
	HPROFCHARTAB
	.export	CC189
CC189:	.word	CHAR,189
	HPROFCHARTAB
	.export	CC190
CC190:	.word	CHAR,190
	HPROFCHARTAB
	.export	CC191
CC191:	.word	CHAR,191
	HPROFCHARTAB
	.export	CC192
CC192:	.word	CHAR,192
	HPROFCHARTAB
	.export	CC193
CC193:	.word	CHAR,193
	HPROFCHARTAB
	.export	CC194
CC194:	.word	CHAR,194
	HPROFCHARTAB
	.export	CC195
CC195:	.word	CHAR,195
	HPROFCHARTAB
	.export	CC196
CC196:	.word	CHAR,196
	HPROFCHARTAB
	.export	CC197
CC197:	.word	CHAR,197
	HPROFCHARTAB
	.export	CC198
CC198:	.word	CHAR,198
	HPROFCHARTAB
	.export	CC199
CC199:	.word	CHAR,199
	HPROFCHARTAB
	.export	CC200
CC200:	.word	CHAR,200
	HPROFCHARTAB
	.export	CC201
CC201:	.word	CHAR,201
	HPROFCHARTAB
	.export	CC202
CC202:	.word	CHAR,202
	HPROFCHARTAB
	.export	CC203
CC203:	.word	CHAR,203
	HPROFCHARTAB
	.export	CC204
CC204:	.word	CHAR,204
	HPROFCHARTAB
	.export	CC205
CC205:	.word	CHAR,205
	HPROFCHARTAB
	.export	CC206
CC206:	.word	CHAR,206
	HPROFCHARTAB
	.export	CC207
CC207:	.word	CHAR,207
	HPROFCHARTAB
	.export	CC208
CC208:	.word	CHAR,208
	HPROFCHARTAB
	.export	CC209
CC209:	.word	CHAR,209
	HPROFCHARTAB
	.export	CC210
CC210:	.word	CHAR,210
	HPROFCHARTAB
	.export	CC211
CC211:	.word	CHAR,211
	HPROFCHARTAB
	.export	CC212
CC212:	.word	CHAR,212
	HPROFCHARTAB
	.export	CC213
CC213:	.word	CHAR,213
	HPROFCHARTAB
	.export	CC214
CC214:	.word	CHAR,214
	HPROFCHARTAB
	.export	CC215
CC215:	.word	CHAR,215
	HPROFCHARTAB
	.export	CC216
CC216:	.word	CHAR,216
	HPROFCHARTAB
	.export	CC217
CC217:	.word	CHAR,217
	HPROFCHARTAB
	.export	CC218
CC218:	.word	CHAR,218
	HPROFCHARTAB
	.export	CC219
CC219:	.word	CHAR,219
	HPROFCHARTAB
	.export	CC220
CC220:	.word	CHAR,220
	HPROFCHARTAB
	.export	CC221
CC221:	.word	CHAR,221
	HPROFCHARTAB
	.export	CC222
CC222:	.word	CHAR,222
	HPROFCHARTAB
	.export	CC223
CC223:	.word	CHAR,223
	HPROFCHARTAB
	.export	CC224
CC224:	.word	CHAR,224
	HPROFCHARTAB
	.export	CC225
CC225:	.word	CHAR,225
	HPROFCHARTAB
	.export	CC226
CC226:	.word	CHAR,226
	HPROFCHARTAB
	.export	CC227
CC227:	.word	CHAR,227
	HPROFCHARTAB
	.export	CC228
CC228:	.word	CHAR,228
	HPROFCHARTAB
	.export	CC229
CC229:	.word	CHAR,229
	HPROFCHARTAB
	.export	CC230
CC230:	.word	CHAR,230
	HPROFCHARTAB
	.export	CC231
CC231:	.word	CHAR,231
	HPROFCHARTAB
	.export	CC232
CC232:	.word	CHAR,232
	HPROFCHARTAB
	.export	CC233
CC233:	.word	CHAR,233
	HPROFCHARTAB
	.export	CC234
CC234:	.word	CHAR,234
	HPROFCHARTAB
	.export	CC235
CC235:	.word	CHAR,235
	HPROFCHARTAB
	.export	CC236
CC236:	.word	CHAR,236
	HPROFCHARTAB
	.export	CC237
CC237:	.word	CHAR,237
	HPROFCHARTAB
	.export	CC238
CC238:	.word	CHAR,238
	HPROFCHARTAB
	.export	CC239
CC239:	.word	CHAR,239
	HPROFCHARTAB
	.export	CC240
CC240:	.word	CHAR,240
	HPROFCHARTAB
	.export	CC241
CC241:	.word	CHAR,241
	HPROFCHARTAB
	.export	CC242
CC242:	.word	CHAR,242
	HPROFCHARTAB
	.export	CC243
CC243:	.word	CHAR,243
	HPROFCHARTAB
	.export	CC244
CC244:	.word	CHAR,244
	HPROFCHARTAB
	.export	CC245
CC245:	.word	CHAR,245
	HPROFCHARTAB
	.export	CC246
CC246:	.word	CHAR,246
	HPROFCHARTAB
	.export	CC247
CC247:	.word	CHAR,247
	HPROFCHARTAB
	.export	CC248
CC248:	.word	CHAR,248
	HPROFCHARTAB
	.export	CC249
CC249:	.word	CHAR,249
	HPROFCHARTAB
	.export	CC250
CC250:	.word	CHAR,250
	HPROFCHARTAB
	.export	CC251
CC251:	.word	CHAR,251
	HPROFCHARTAB
	.export	CC252
CC252:	.word	CHAR,252
	HPROFCHARTAB
	.export	CC253
CC253:	.word	CHAR,253
	HPROFCHARTAB
	.export	CC254
CC254:	.word	CHAR,254
	HPROFCHARTAB
	.export	CC255
CC255:	.word	CHAR,255
	HPROFCHARTAB

	.export	_ctag0tab
_ctag0tab:
	.export	tag0tab
tag0tab:
	.export	CN0
CN0:	.word	TAG0,0
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN1
CN1:	.word	TAG0,1
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN2
CN2:	.word	TAG0,2
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN3
CN3:	.word	TAG0,3
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN4
CN4:	.word	TAG0,4
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN5
CN5:	.word	TAG0,5
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN6
CN6:	.word	TAG0,6
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN7
CN7:	.word	TAG0,7
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN8
CN8:	.word	TAG0,8
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN9
CN9:	.word	TAG0,9
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN10
CN10:	.word	TAG0,10
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN11
CN11:	.word	TAG0,11
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN12
CN12:	.word	TAG0,12
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN13
CN13:	.word	TAG0,13
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN14
CN14:	.word	TAG0,14
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN15
CN15:	.word	TAG0,15
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN16
CN16:	.word	TAG0,16
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN17
CN17:	.word	TAG0,17
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN18
CN18:	.word	TAG0,18
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN19
CN19:	.word	TAG0,19
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.export	CN20
CN20:	.word	TAG0,20
	HPROFWORDSTAB($SYSTEMNIL_NIL_LABEL,$0)
	.text
/******************************************************************/
#ifdef PROFILE
	.export	Mcount
Mcount:
	MCOUNTCALL
	.export	Mcountpost
Mcountpost:
	MCOUNTPOST
#endif

/**********************************/
	.data
	.malign
	.export	csbase
csbase:
	.word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.word	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	.export	_csbegin
_csbegin:
	SDATAD
	.export	_cspointer
_cspointer:	.word	_csbegin
	.data
	.export	CPcatch
CPcatch:
	.word	FUN
	.word	VCPcatch
	.export	VCPcatch
VCPcatch:
	.word	$2
	.word	CPcatch
	.word	unw2
	.word	vunw2
	.word	J2CPcatch
	.word	S2CPcatch
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	catchLT5
	.text
	.export	S2CPcatch
	.export	J2CPcatch
	.malign
S2CPcatch:
	move	$3(Sp),Vpush
J2CPcatch:
	.funbegin	CPcatch
	comp	_cspointer,$csbase
	jge	catchLL3
	.data
#ifdef LINKWORD
        .word   0
#endif
catchLT2:
	.word	STRING
	.word	catchLT3
	.word	$0
catchLT3:
	.string	"atch stack overflow"
	.text
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
catchLT5:
	.word	$PAIR1
	.word	CC99
	.word	catchLT2
	.text
	move	$catchLT5,1(Sp)
	move	$1(Sp),Sp
	jumpf	realfail
catchLL3:
; push Sp
	move	_cspointer,ATMP
#ifdef retr
	move	$-3(ATMP),ATMP
	move	retr,2(ATMP)
#else
	move	$-2(ATMP),ATMP
#endif
	move	ATMP,_cspointer
	move	Sp,0(ATMP)
	move	Vp,1(ATMP)
; check if intrflag is on, invoke action if that is the case
	comp	_intrflag,$0
	jeq	catchLL31
	call	GARB
catchLL31:
; eval first arg
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,r1
	move	$2(Sp),Sp
	move	Spop,r0
	move	0(r1),INDREG
	move	INDREG,0(r0)
	move	1(r1),1(r0)
	move	2(r1),2(r0)
; pop catch entry
	move	_cspointer,ATMP
#ifdef retr
	move	$3(ATMP),ATMP
#else
	move	$2(ATMP),ATMP
#endif
	move	ATMP,_cspointer
;
	jump	ounwind(INDREG)
	.funend

; throw
	.data
	.malign
	.export	CPthrow
CPthrow:
	.word	FUN
	.word	VCPthrow
	.export	VCPthrow
VCPthrow:
	.word	$1
	.word	CPthrow
	.word	unw1
	.word	vunw1
	.word	J1CPthrow
	.word	S1CPthrow
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1CPthrow
	.export	J1CPthrow
	.malign
S1CPthrow:
	move	$2(Sp),Vpush
J1CPthrow:
	.funbegin	CPthrow
	comp	_cspointer,$_csbegin
	jlt	throwLL3
#if 0
/* Why would we ever care if the heap is full when we are going to fail anyway? */
	comp	Hp,_ehp
	jlth	throwLT1
	call	GARB
throwLT1:
#endif
	jumpf	realfail
throwLL3:
; save argument
	move	0(Sp),r1
; pop catch entry
	move	_cspointer,ATMP
	move	1(ATMP),Vp
	move	0(ATMP),Sp
#ifdef retr
; machine stores return address in a register on call, restore this reg
	move	2(ATMP),retr
	move	$3(ATMP),ATMP
#else
	move	$2(ATMP),ATMP
#endif
	move	ATMP,_cspointer
; rearrange stack
	move	1(Sp),0(Sp)
	move	r1,1(Sp)
	move	Spop,r0
	move	$1,ARGCREG
	move	0(r0),INDREG
	jump	ojfun(INDREG)
	.funend
/**************************************/

#ifdef m_arm
	.data                   /*********** !!! ***********/
#ifdef LINKWORD
        .word   0
#endif
notimpLT2:
	.word	STRING
	.word	notimpLT3
	.word	$0
notimpLT3:
	.string	"Not implemented (floating point or div/mod)"
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
notimpmsg:
	.word	$PAIR1
	.word	CC32
	.word	notimpLT2
	.text
        .export NotImplemented
NotImplemented:
	move	$notimpmsg,0(Sp)
	jumpf	J1CPfail
#endif

	.data
#ifdef LINKWORD
        .word   0
#endif
ffloatLT2:
	.word	STRING
	.word	ffloatLT3
	.word	$0
ffloatLT3:
	.string	"Floating exception (or divide by zero)"
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
ffloatmsg:
	.word	$PAIR1
	.word	CC32
	.word	ffloatLT2
	.text
	.export	_failfloat
CLABEL(_failfloat)
/*	CENTRY*/
	move	Sp,_ep
	move	Hp,_hp
#ifdef m_sparc
; Vp may be corrupted if the signal comes from C code.
; Use a temporary stack until fail can reset it.
	move	$tmpvstk,Vp
#endif
	CCALL(0,_cleansig)
	move	_ep,Sp
	move	_hp,Hp
	move	$ffloatmsg,0(Sp)
	jumpf	J1CPfail


	.data
#ifdef LINKWORD
        .word   0
#endif
fintrLT2:
	.word	STRING
	.word	fintrLT3
	.word	$0
fintrLT3:
	.string	"Interrupt"
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
fintrmsg:
	.word	$PAIR1
	.word	CC32
	.word	fintrLT2
	.text
	.export	_failintr
CLABEL(_failintr)
	move	$fintrmsg,0(Sp)
	jumpf	J1CPfail

	.data
#ifdef LINKWORD
        .word   0
#endif
fstkovflLT2:
	.word	STRING
	.word	fstkovflLT3
	.word	$0
fstkovflLT3:
	.string	"Stack overflow"
	.data
	.malign
#ifdef LINKWORD
        .word   0
#endif
fstkovflmsg:
	.word	$PAIR1
	.word	CC32
	.word	fstkovflLT2
	.text
	.export	_failstkovfl
CLABEL(_failstkovfl)
/*	CENTRY*/
	move	$10(Sp),Sp	; Get some space to recover in
	move	$10(Vp),Vp	; dito
	move	$fstkovflmsg,0(Sp)
	jumpf	J1CPfail

#ifdef m_sparc
; A hack for SUN4.
	.data
	.malign
	.word	0,0,0,0,0,0
tmpvstk: .word	0
	.text
#endif

	.malign
	.export	entercaf
entercaf:
	LBASE
	move	Sp,_ep
	move	Hp,_hp
	CCALL(0,_centercaf)
	move	_ep,Sp
	move	_hp,Hp
	return

#if 1
/***************************************************************/
/*
** String compare routine.
**
*/
	.export	lstrcmp
	.malign
lstrcmp:
	LBASE
 jumpf bigcmp
#ifdef PROFILE
	move	r0,Spush
	move	r1,Spush
	move	$-1(Vp),Vp		; Dummy entry
	move	$strcmpp,r0
	call	Mcount
	call	Mcountpost
	move	$1(Vp),Vp		; Pop dummy
	move	Spop,r1
	move	Spop,r0
	.data
strcmpp:	.word	0
	.text
#endif
	move	Sp,Vpush		; save stack pointer

; Enter with pointer in r0, r1
lstrloop:
	move	0(r0),ATMP
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_TAG
#else
	comp	ATMP,$TAG0		; first is nil
	jeq	lstrnil1
	comp	ATMP,$TAG0_1		; gc-gen2
	jeq	lstrnil1
	comp	ATMP,$TAG0_2		; gc-gen2
#endif
	jeq	lstrnil1		; gc-gen2
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_PAIR1
#else
	comp	ATMP,$PAIR1
	jeq	lstrcons1		; first is cons
	comp	ATMP,$PAIR1_1		; gc-gen2
	jeq	lstrcons1		; first is cons
	comp	ATMP,$PAIR1_2		; gc-gen2
#endif
	jeq	lstrcons1		; gc-gen2
	; must evaluate first
	move	r1,Spush
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	Spop,r1
	jump	lstrloop		; must be a TAG0 or PAIR1 now

; first is cons
lstrcons1:
	move	0(r1),ATMP
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_TAG0
#else
	comp	ATMP,$TAG0
	jeq	cmpxgt			; first is greater
	comp	ATMP,$TAG0_1		; gc-gen2
	jeq	cmpxgt			; first is greater
	comp	ATMP,$TAG0_2		; gc-gen2
#endif
	jeq	cmpxgt			; gc-gen2
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_PAIR1
#else
	comp	ATMP,$PAIR1
	jeq	lstrcons0r		; gc-gen2
	comp	ATMP,$PAIR1_1		; gc-gen2
	jeq	lstrcons0r		; gc-gen2
	comp	ATMP,$PAIR1_2		; gc-gen2
#endif
	jne	lstrcons11		; must evaluate second
lstrcons0r:				; gc-gen2
	; two conses, compare characters
	move	1(r0),ATMP		; pointer to first char
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_CHAR
#else
	comp	0(ATMP),$CHAR
	jeq	lstrcc1r		; gc-gen2
	comp	0(ATMP),$CHAR_1		; gc-gen2
	jeq	lstrcc1r		; gc-gen2
	comp	0(ATMP),$CHAR_2		; gc-gen2
#endif
	jne	lstrcc1
lstrcc1r:
	move	1(ATMP),DTMP		; get char
	move	1(r1),ATMP		; pointer to second char
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_CHAR
#else
	comp	0(ATMP),$CHAR
	jeq	lstrcc2r		; gc-gen2
	comp	0(ATMP),$CHAR_1		; gc-gen2
	jeq	lstrcc2r		; gc-gen2
	comp	0(ATMP),$CHAR_2		; gc-gen2
#endif
	jne	lstrcc2
lstrcc2r:
	comp	DTMP,1(ATMP)
	jlt	cmpxlt
#ifdef NOCOMPARE
	comp	DTMP,1(ATMP)
#endif
	jgt	cmpxgt
	move	2(r0),r0		; equal: now continue
	move	2(r1),r1		; with the tails
	jump	lstrloop
	

; evaluate first char
lstrcc1:
	move	r0,Spush
	move	r1,Spush
	move	ATMP,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,ATMP
	move	Spop,r1
	move	Spop,r0
	jump	lstrcc1r

; evaluate second char
lstrcc2:
	move	DTMP,Vpush
	move	r0,Spush
	move	r1,Spush
	move	ATMP,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,ATMP
	move	Spop,r1
	move	Spop,r0
	move	Vpop,DTMP
	jump	lstrcc2r

; evaluate second
lstrcons11:
	move	r0,Spush
	move	r1,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,r1
	move	Spop,r0
	jump	lstrcons1

; first is nil
lstrnil1:
	move	0(r1),ATMP		; get tag
lstrnil11:
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_TAG0
#else
	comp	ATMP,$TAG0
	jeq	cmpeq1			; they are equal
	comp	ATMP,$TAG0_1		; gc-gen2
	jeq	cmpeq1			; they are equal
	comp	ATMP,$TAG0_2		; gc-gen2
#endif
	jeq	cmpeq1			; gc-gen2
#if CaseTableOptim
	comp	oconstr(ATMP),$ O_PAIR1
#else
	comp	ATMP,$PAIR1
	jeq	cmpxlt			; first is smaller
	comp	ATMP,$PAIR1_1		; gc-gen2
	jeq	cmpxlt			; first is smaller
	comp	ATMP,$PAIR1_2		; gc-gen2
#endif
	jeq	cmpxlt			; gc-gen2
	; must evaluate r1
	move	r1,r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(r0),ATMP
	jump	lstrnil11		; ATMP must be TAG0 or PAIR1 now
#endif

;;;;  Forcing
fret:
	return

f2p:	; 2 pointers
	move	r0,Spush
	move	1(r0),r0
	move	0(r0),INDREG
	call	oforce(INDREG)
	move	0(Sp),r0
	move	2(r0),r0
	move	0(r0),INDREG
	call	oforce(INDREG)
	move	Spop,r0
	return

f1p:	; 1 pointer
	move	r0,Spush
	move	2(r0),r0
	move	0(r0),INDREG
	call	oforce(INDREG)
	move	Spop,r0
	return

fvek:	; vektor
	move	r0,Spush
	move	1(r0),DTMP
	move	$2(r0),r0
floop:
	move	r0,Spush
	move	DTMP,Vpush

	move	0(r0),r0
	move	0(r0),INDREG
	call	oforce(INDREG)

	move	Vpop,DTMP
	move	Spop,r0
	move	$1(r0),r0
	DECR(DTMP)
	TSTC(DTMP)
	jgt	floop
	move	Spop,r0
	return

fevi:	; evaluate indirection
#if 0
	move	0(r0),INDREG
	call	oeval(INDREG)
#else
	move	r0,Spush
	move	1(r0),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	Spop,ATMP
	move	0(r0),0(ATMP)
	move	1(r0),1(ATMP)
	move	2(r0),2(ATMP)
#ifdef HPROFILE
	move    3(r0),3(ATMP)
#ifdef SLOP2
        move    4(r0),4(ATMP)
#endif
#endif
	move	ATMP,DTMP
	WRITEUPDATED(DTMP,fevi_indi)
	move	0(r0),INDREG
	jump	oforce(INDREG)
#endif

fev:	; evaluate
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	0(r0),INDREG
	jump	oforce(INDREG)

	.malign
ferr:	; should not happen
	LBASE
	move	Sp,_ep
	CCALL(0,_forceerr)

	.data
	.export	CPforce
CPforce:
	.word	FUN
	.word	VCPforce
	.export	VCPforce
VCPforce:
	.word	1
	.word	CPforce
	.word	unw1
	.word	vunw1
	.word	J1CPforce
	.word	S1CPforce
	.word	0
	.word	0
	.word	0
	.word	0
	.word	0
	.text
	.export	S1CPforce
	.export	J1CPforce
	.malign
S1CPforce:
	move	$2(Sp),Vpush
J1CPforce:
	.funbegin CPforce
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oforce(INDREG)
	jumpf	evalupdunw		; for simplicity	

	.export	_forceit
CLABEL(_forceit)
	CENTRY
	move	_ep,Sp
	move	_hp,Hp
	
	move	Spop,r0
	move	0(r0),INDREG
	call	oforce(INDREG)
	move	r0,Spush

	move	Sp,_ep
	move	Hp,_hp
	CRET

	.export	_fixgarbvp
CLABEL(_fixgarbvp)
	CENTRY
	move	Vp,_garbvp
	CRET

#ifdef TAGAREA
tagarea:
	.word	INT, DFLOAT, BIGNUM, CHAR, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4, TAG, TAG0, VEK, BVEK, DVEK, ZAP, HOLE, STRING, INPUT, INPUTD, AP, APG, CAP, FUN, VAP, VAPG, MARKED, MOVED, GCRET
#endif

