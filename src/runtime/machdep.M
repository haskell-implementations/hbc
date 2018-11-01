#include "tagtable.h"

; VFarity	the arity of the funtion
; VFpointer	pointer to function node
; VFunwind	after unwind, i.e. chk arg & rearrange
; VFcode	function code (after rearrange)
; VFvapunwind	unwind of vektor applikation
; VFstingy	pointer to stingy code, or 0 if not available
; VFhprof	heap profiling stuff
; VFlink	link field used during GC
; VFnref	number of entries in VFrefs
; VFrefs	table of references to global entities

; Reg          Type
; r0		AM	Misc
; r1		AM	Misc
; ARGCREG	DM	Argument count in jfun, callfun
; TAGREG	DM	Tag value from gettag
; BIGEQREG	DM	Return value fom bigeq
; INDREG	AM	Used in dispatch
; GCSTART	A	_startheap during GC
; GCEND		A	_endheap during GC
; GCCUR		A	_curheap during GC
; CRETR		D	return value from C
; DTMP		D	scratch
; CDTMP		D	scratch used only in bigcmp, must survive ogettag, usually DTMP
; ATMP		A	scratch
; FDISP		A	used in function node dispatch
;
; DTMP, ATMP must be distinct
; GCx must be distinct from all other regs when used
;
; A - Address register
; D - Data register
; M - Used in mcode generator

; LBASE			360 only
; CCALL(m,a)		Call C routine a with n arguments
; CENTRY		Entry code for C routine
; CRET			Return from C routine
; CARGn(a)		Get C argument n into a
; CPUSHARGn(a)		Push C argument (before CCALL)
; DECR(d)		Decrement d
; TSTC(d)		Set condition code (if DECR&sub doesn't)
; ADDAW(c,d)		Add a word offset to an address
; ADDSP(c)		Add a word count to sp
; ADDSP2(c,d)		Add a word count to sp, result in second op
; TABADDR(s,c,d)	Address of table entry c, 2 word/elem
; MOVIB(s,d)		Get a byte
; STIB(s,d)		Store a byte
; MASKB(d)		Byte mask
; INCBP(a)		Increment byte pointer
; TSTBIND(a)		Test byte indirect
; MCOUNTCALL		Call the profiling routine
; MCOUNTPOST		Reset after profiling has been called
; JUMPREG(a)		jump to address in reg

#ifdef m_vax
#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r9

#define GCSTART r5
#define GCEND r6
#define GCCUR r7
#define CRETR r0
#define DTMP r4
#define CDTMP DTMP
#define ATMP r2
#define FDISP r3

#define LBASE		; nothing
#define CCALL(m,a) 	.asm "	calls	^,^",m,a
#define CENTRY		.asm "	.word	0xfff"
#define CRET		.asm "	ret"
#define CARG(m,a)	.asm "	movl	4*^+4(ap),^",@m,a
#define CPUSHARG(a)	.asm "	movl	^,-(sp)",a
#define DECR(a)		.asm "	decl	^",a
#define TSTC(a)		; CC already set
#define ADDAW(s,d)	.asm "	moval	(^)[^],^",d,s,d
#define ADDSP(c)	.asm "	moval	(%ep)[^],%ep",c
#define ADDSP2(c,d)	.asm "	moval	(%ep)[^],^",c,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	.asm "	ashl	$4,^,^\n	addl2	$^,^",c,d,s,d
#else
#define TABADDR(s,c,d)	.asm "	movaq	^[^],^",s,c,d
#endif
#define MOVIB(s,d)	.asm "	movzbl	(^),^",s,d
#define STIB(s,d)	.asm "	movb	^,(^)",s,d
#define MASKB(s,d)	.asm "	movzbl	^,^",s,d
#define INCBP(p)	.asm "	incl	^",p
#define TSTBIND(r)	.asm "	tstb	(^)",r
#define MCOUNTCALL	.asm "	?!!"
#define MCOUNTPOST	.asm "	?!!"
#endif

#ifdef m_32000
#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r3

#define GCSTART _startheap
#define GCEND _endheap
#define GCCUR _curheap
#define CRETR r0
#define DTMP r4
#define CDTMP r5
#define ATMP r2
#define FDISP r5

#define LBASE		; nothing
#ifdef SYSV
#define CCALL(m,a) 	.asm "	jsr	^\n	adjspb	$^*-4",a,m
#else
#define CCALL(m,a) 	.asm "	jsr	^\n	adjspb	-4*^",a,m
#endif
#define CENTRY		.asm "	enter	[r3,r4,r5,r6,r7],0\n	sprd	fp,tos"
#define CRET		.asm "	lprd	fp,tos\n	exit	[r3,r4,r5,r6,r7]\n	ret	0"
#define CARG(m,a)	.asm "	movd	4*^+8(fp),^",@m,a
#define CPUSHARG(a)	.asm "	movd	^,tos",a
#define DECR(a)		.asm "	addqd	-1,^",a
#ifdef SYSV
#define TSTC(a)		.asm "	cmpd	^,$0",a
#else
#define TSTC(a)		.asm "	cmpd	^,0",a
#endif
#define ADDAW(s,t)	.asm "	addr	0(^)[^:d],^",t,s,t
#define ADDSP(c)	.asm "	addr	0(^)[^:d],^",Sp,c,Sp
#define ADDSP2(c,x)	.asm "	addr	0(^)[^:d],^",Sp,c,x
#ifdef HPROFILE
#define TABADDR(s,c,d)	?!! Can't remember the name of the shift ops on 32k right now /LA
#else
#define TABADDR(s,c,d)	.asm "	addr	^[^:q],^",s,c,d
#endif
#define MOVIB(s,d)	.asm "	movzbd	0(^),^",s,d
#define STIB(s,d)	.asm "	movb	^,0(^)",s,d
#define MASKB(s,d)	.asm "	movzbd	^,^",s,d
#define INCBP(p)	.asm "	addqd	1,^",p
#ifdef SYSV
#define TSTBIND(r)	.asm "	cmpb	0(^),$0",r
#else
#define TSTBIND(r)	.asm "	cmpb	0(^),0",r
#endif
#define JUMPREG(a)	.asm "	jump	0(^)",a
#define MCOUNTCALL	.asm "	addr	4(sp),r4\n	lprd	fp,r4\n	jump	mcount"
#define MCOUNTPOST	.asm "	ret"
#endif

#ifdef m_68000
; r0-r7 <=> a0-a7 r8-r15 <=> d0-d7
; d1 is used as a scratch register, both here and in the code generator.
#define ARGCREG	r10
#define TAGREG r10
#define BIGEQREG r10
#define INDREG r4

#define GCSTART r14
#define GCEND r13
#define GCCUR r12
#define CRETR r8
#define DTMP r11
#define CDTMP DTMP
#define ATMP r2
#define FDISP r3

#define LBASE		; nothing
#define CCALL(m,a) 	.asm "	jsr	a\n	addl	^*4,^",m,Vp
#define CENTRY		.asm "	link	a6,#-16\n	moveml	#0x3f3e,a7@-"
#define CRET		.asm "	moveml	a7@+,#0x7cfc\n	unlk	a6\n	rts"
#define CARG(m,a)	.asm "	movl	a6@(4*^+8),^",@m,a
#define CPUSHARG(a)	.asm "	movl	^,a7@-",a
#define DECR(a)		.asm "	subql	#1,^",a
#define TSTC(a)		; CC already set
#define ADDAW(s,d)	.asm "	movl	^,d1\n	asll	#2,d1\n	addl	d1,^",s,d
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,d)	.asm "	movl	^,^\n	movl	^,d1\n	asll	#2,d1\n	addl	d1,^",Sp,d,c,d
#ifdef HPROFILE
#define TABADDR(s,c,d)  .asm "	movl	#^,^\n	movl	^,d1\n	asll	#4,d1\n	addl	d1,^",s,d,c,d
#else
#define TABADDR(s,c,d)	.asm "	movl	#^,^\n	movl	^,d1\n	asll	#3,d1\n	addl	d1,^",s,d,c,d
#endif
#define MOVIB(s,d)	.asm "	movb	^@,^\n	andl	#255,^",s,d,d
#define STIB(s,d)	.asm "	movb	^,^@",s,d
#define MASKB(s,d)	.asm "	movb	^,^\n	andl	#255,^",s,d,d
#define INCBP(p)	.asm "	addql	#1,^",p
#define TSTBIND(r)	.asm "	tstb	^@",r
#define MCOUNTCALL	.asm "	movl	a6,savea6\n	lea	a7@(4),a6\n	jmp	mcount"
#define MCOUNTPOST	.asm "	movl	savea6,a6\n	rts\n	.data\nsavea6:	.long	0\n	.text"
#define JUMPREG(a)	.asm "	jmp	^@",a
#endif

#ifdef m_360

	.export	callit
callit:
.asm "	using	callit,14"
.asm "	s	10,=8"			; make room
.asm "	st	14,0(10)"		; save ret addr
.asm "	st	11,4(10)"		; save base
.asm "	br	15"
.asm "	ltorg"

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r3

#define GCSTART _startheap
#define GCEND _endheap
#define GCCUR _curheap
#define CRETR r0
#define DTMP r4
#define CDTMP DTMP
#define ATMP r2
#define FDISP r15

#define LBASE		.asm "	using	*,11\n	lr	11,15"
#define CCALL(m,a) 	.asm "	s	10,=72\n	l	15,=a\n	balr	14,15\n	la	10,4*^+72(10)",m
#define CENTRY		.asm "	stm	14,11,12(13)\n	st	13,4(r10)\n	lr	13,10\n	lr	11,15\n	using	*,11\n	la	7,4"
#define CRET		.asm "	l	13,4(0,13)\n	lm	14,11,12(13)\n	br	14"
#define CARG(m,a)	.asm "	l	^,4*^+72(13)",m,a
#define CPUSHARG(a)	.asm "	st	^,72(10)",a
#define DECR(a)		.asm "	bctr	^,0",a
#define TSTC(a)		.asm "	ltr	^,^",a,a
#define ADDAW(s,t)	.asm "	lr	0,^\n	ar	0,0\n	ar	0,0\n	ar	^,0"	,s,t
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,x)	.asm "	lr	^,^\n	ar	^,^\n	ar	^,^\n	ar	^,^"	,x,c,x,x,x,x,x,Sp
#ifdef HPROFILE
#define TABADDR(s,c,d)	.asm "	lr	^,^\n	sll	^,4\n	a	^,=^"	,d,c,d,d,s
#else
#define TABADDR(s,c,d)	.asm "	lr	^,^\n	sll	^,3\n	a	^,=^"	,d,c,d,d,s
#endif
#define MOVIB(s,d)	.asm "	ic	^,0(^)\n	n	^,=255",d,s,d
#define MASKB(s,d)	.asm "	lr	^,^\n	n	^,=255",d,s,d
#define INCBP(p)	.asm "	la	^,1(^)",p,p
#define TSTBIND(r)	.asm "	cli	0(^),0",r
#define MCOUNTCALL	.asm "	?!!"
#endif

#ifdef m_80386
#if defined(sun386) || defined(i386) && defined(sun) || defined(linux)
#define NOUNDERSCORE
#endif

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r4

#define GCSTART _startheap
#define GCEND _endheap
#define GCCUR _curheap
#define CRETR r1
#define DTMP r2
#define CDTMP dtmp
#define realATMP r3
#define ATMP realATMP
#define FDISP r3

; The code generator needs to overwrite the top element of the stack
; sometimes.  To make sure there is something to overwrite we push
; a 0 on entry and pop it again on exit.
#define LBASE		; nothing
#define CCALL(m,a) 	.asm "	fstp	%st(0)\n	call	^\n	addl	$^^,%esp\n	fldz",a,@m
#define CENTRY		.asm "	pushl	%ebp\n	movl	%esp,%ebp\n	subl	$16,%esp\n	pushl	%edi\n	pushl	%esi\n	pushl	%ebx\n	pushl	%ebp\n	fldz"
#define CRET		.asm "	fstp	%st(0)\n	popl	%ebp\n	popl	%ebx\n	popl	%esi\n	popl	%edi\n	leave\n	ret"
#define CARG(m,a)	.asm "	movl	^^^(%ebp),^",@m,a
#define CPUSHARG(a)	.asm "	pushl	^",a
#define DECR(a)		.asm "	decl	^",a
#define TSTC(a)		; CC already set
#define ADDAW(s,t)	.asm "	leal	(^,^,4),^",t,s,t
#define ADDSP(c)	.asm "	leal	(^,^,4),^",Sp,c,Sp
#define ADDSP2(c,x)	.asm "	leal	(^,^,4),^",Sp,c,x
#ifdef HPROFILE
#define TABADDR(s,c,d)	.asm "	movl	^,^\n	shll	$4,^\n	addl	$^,^",c,d,d,s,d
#else
#define TABADDR(s,c,d)	.asm "	leal	^(,^,8),^",s,c,d
#endif
#define MOVIB(s,d)	.asm "	movzbl	(^),^",s,d
#define STIB(s,d)	.asm "	movb	#,(^)",s,d
#define MASKB(s,d)	.asm "	movl	^,^\n	andl	$255,^",s,d,d
#define INCBP(p)	.asm "	incl	^",p
#define TSTBIND(r)	.asm "	cmpb	$0,(^)",r
#define MCOUNTCALL	.asm "	movl	%ebp,saveebp\n	leal	4(%esp),%ebp\n	movl	^,%eax\n	jmp	mcount",r0
#define MCOUNTPOST	.asm "	movl	saveebp,%ebp\n	ret\n	.data\nsaveebp:	.long	0\n	.text"
#define JUMPREG(a)	.asm "	jmp	*^",a

#endif

#ifdef m_rtpc
#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r3

#define GCSTART _startheap
#define GCEND _endheap
#define GCCUR _curheap
#define CRETR r12
#define DTMP r4
#define CDTMP DTMP
#define realATMP r2
#define ATMP realATMP
#define FDISP r12

#define LBASE		; nothing
#define CCALL(m,a) 	.asm " .globl ^\n .globl ~\n stm r6,-76(r1)\n cal r1,-76(r1)\n get  r0,$^\n bali r15,~\n cal r1,76(r1)\n lm r6,-76(r1)\n",a,a,a,a
#define CENTRY		.asm " stm r6,-76(r1)\n cal r1,-76(r1)\n"
#define CRET		.asm " cal r1,76(r1)\n lm r6,-76(r1)\n br r15\n"
#define CARG(m,a)	.asm " cas ^,!,r0\n",a,m
#define CPUSHARG0(a)	move a, r12
#define CPUSHARG1(a)	move a, r13
#define CPUSHARG2(a)	move a, r14
#define CPUSHARG3(a)	move a, r15
#define DECR(a)		.asm " sis ^,1",a
#define TSTC(a)		; CC already set
#define ADDAW(s,d)	.asm " ai ^,^\n ai ^,^\n ai ^,^\n ai ^,^\n",d,s,d,s,d,s,d,s
#define ADDSP(c)	.asm " ai ^,^\n ai ^,^\n ai ^,^\n ai ^,^\n",Sp,c,Sp,c,Sp,c,Sp,c
#define ADDSP2(c,x)	.asm " mr ^,^\n sli ^,2\n ai ^,^\n",x,c,x,x,Sp
#ifdef HPROFILE
#define TABADDR(s,c,d)	.asm " mr ^,^\n sli ^,4\n get r4,$^\n a ^,r4\n",d,c,d,s,d
#else
#define TABADDR(s,c,d)	.asm " mr ^,^\n sli ^,3\n get r4,$^\n a ^,r4\n",d,c,d,s,d
#endif
#define MOVIB(s,d)	.asm " lcs ^,0(^)\n",d,s
#define MASKB(s,d)	.asm " nilz ^,^,255\n",d,s
#define INCBP(p)	.asm " inc ^,1\n",p
#define TSTBIND(r)	.asm " lcs r4,0(^)\n cis r4,0\n",r
#define MCOUNTCALL	.asm "# no profiling done"
#define MCOUNTPOST	.asm "# no profiling done"
#endif

#ifdef m_cray

#define SDATAT .data

#define NOCOMPARE
#define NOUNDERSCORE

;These must be the same as in machine
#define ARGCREG r11
#define TAGREG r11
#define BIGEQREG r11
#define INDREG r3

; Should place these in B-registers
#define GCSTART _startheap
#define GCEND   _endheap
#define GCCUR   _curheap

#define CRETR r9
#define DTMP r12
#define CDTMP DTMP

; ATMP /= FDISP /= INDREG
#define realATMP r2
#define ATMP realATMP
#define FDISP r2

	.asm	"|EXT|STOFERR"
	.asm	"|EXT|STOFJMP"
	.asm	"|EXT|$STKOFEN:p"

#define LBASE		; nothing
#define CCALL(m,a) 	.asm "* CCALL\n|A0|B00\n|B10|A0\n|B11|A1\n|B12|A2\n|B13|A3\n|B14|A4\n|B15|A5\n|B16|A6\n|B17|A7\n|T02|S2\n|T03|S3\n|T04|S4\n|T05|S5\n|T06|S6\n|T07|S7\n|S2|9\n|A6|&\n|S2|S2<24\n|S3|^\n|A6|A7-A6\n|S3|S2!S3\n|0,A6|S3\n|R|P.~\n|A0|B10\n|B00|A0\n|A1|B11\n|A2|B12\n|A3|B13\n|A4|B14\n|A5|B15\n|A6|B16\n|A7|B17\n|S2|T02\n|S3|T03\n|S4|T04\n|S5|T05\n|S6|T06\n|S7|T07\n",m,m,a

#define CPUSHARG(a)	.asm "* CPUSHARG\n|S0|^\n|-1,A7|S0\n",a

#define CENTRY		 .asm "* CENTRY\n|A0|B66\n|A5|17\n|A2|B66\n|0,A0|B77,A5\n|A0|A2+A5\n|A5|16\n|B02|A2\n|B01|A6\n|0,A0|T70,A5\n|A5|5033\n|A5|A5+A2\n|A2|B67\n|A0|A2-A5\n|B66|A5\n|A2|B02\n|JAM|#2\n%1\n|A3|B02\n|A0|A3-A2\n|JAN|STOFJMP\n|A3|24\n|A2|A2+A3\n|B03|A2\n|A7|B66\n|J|#3\n%2\n|A6|STKDUMMY\n|R|P.$STKOFEN\n|J|#1\n%3\n"

#define CRET		.asm "* CRET\n|A0|B02\n|A1|17\n|A2|B02\n|B77,A1|0,A0\n|A0|A2+A1\n|A1|16\n|T70,A1|0,A0\n|B66|A2\n|J|B00\n"

#define CARG(m,a)	.asm "* CARG\n|^|B01\n|^|^+1,^\n",a,a,m,a

#define DECR(r)		.asm "* DECR\n|S1|1\n|^|^-S1\n",r,r
#define TSTC(a)		comp a,$0
#define ADDAW(ir,r)	.asm "* ADDAW\n|B11|A1\n|A1|^\n|^|^+A1\n|A1|B11\n",ir,r,r
#define ADDSP(c)	.asm "* ADDSP\n|B11|A1\n|A1|^\n|A6|A6+A1\n|A1|B11\n",c
#define ADDSP2(ir,r)	.asm "* ADDSP2\n|^|^\n|^|^+A6\n",r,ir,r,r
#ifdef HPROFILE
#define TABADDR(l,r1,r2) ?!!
#else
#define TABADDR(l,r1,r2) .asm "* TABADDR\n|B11|A1\n|A1|~\n|^|^\n|^|^+^\n|^|^+A1\n|A1|B11\n",l,r2,r1,r2,r2,r2,r2,r2
#endif
#define MOVIB(r1,r2)	.asm "* MOVIB\n|^|0,^\n",r2,r1
#define MASKB(r1,r2)	.asm "* MASKB\n|^|255\n|^|^\&^\n",r2,r2,r1,r2
#define INCBP(r)	.asm "* INCBP\n|^|^+1",r,r
#define TSTBIND(r)	.asm "* TSTBIND\n|S1|0,^\n|S2|255\n|S0|S2\&S1\n",r

#define MCOUNTCALL	.asm "* no profiling done"
#define MCOUNTPOST	.asm "* no profiling done"
#define ADDBYTEPTR(a,b)	.asm "!X!X!"
#define CVTBYTELEN(r)	.asm "!X!X!"
#endif

#ifdef m_mips

#define NOCOMPARE
#define NOUNDERSCORE
#define SDATAT .asm	".sdata"
#define SDATAD .asm	".sdata"

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r31

#define GCSTART r13
#define GCEND r12
#define GCCUR r11
#define CRETR r26
#define DTMP r5
#define CDTMP DTMP
#define ATMP r3
#define FDISP r4
#define rd r29
#define ehpr r22
#define retr r23

#define LBASE		; nothing
#define CCALL(m,a) 	.asm "	move	$16,$sp\n	subu	$sp,72\n	and	$sp,$sp,-8\n	sw	$31,60($sp)\n	jal	^\n	lw	$31,60($sp)\n	move	$sp,$16\n	lw	^,ehp",a,ehpr
#define CENTRY		.asm "	subu	$sp,256\n	sd	$16,20($sp)\n	sd	$18,28($sp)\n	sd	$20,36($sp)\n	sd	$22,44($sp)\n	sd	$4,52($sp)\n	sd	$6,60($sp)	\n	sw	$31,16($sp)\n	lw	^,ehp",ehpr
#define CRET		.asm "	ld	$16,20($sp)\n	ld	$18,28($sp)\n	ld	$20,36($sp)\n	ld	$22,44($sp)\n	ld	$4,52($sp)\n	ld	$6,60($sp)	\n	lw	$31,16($sp)\n	addu	$sp,256\n	j	$31"
#define CARG0(a)	move	r28,a
#define CARG1(a)	move	r29,a
#define CPUSHARG0(a)	move	a,r28
#define CPUSHARG1(a)	move	a,r29
#define CPUSHARG2(a)	move	a,r30
#define CPUSHARG3(a)	move	a,r31
#define DECR(a)		.asm "	subu	^,1",a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	move	s,rd && .asm "	sll	^,^,2",rd,rd && add2 rd,d
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,d)	move	c,rd && .asm "	sll	^,^,2",rd,rd && add Sp,rd,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,4\n	la	^,^",d,rd,s && add2 rd,d
#else
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,3\n	la	^,^",d,rd,s && add2 rd,d
#endif
#define MOVIB(s,d)	.asm "	lb	^,0(^)\n	and	^,255",d,s,d
#define STIB(s,d)	.asm "	sb	^,0(^)",s,d
#define MASKB(s,d)	.asm "	move	^,^\n	and	^,255",d,s,d
#define INCBP(p)	.asm "	addu	^,1",p
#define TSTBIND(r)	.asm "	lb	^,0(^)",rd,r && comp rd,$0
#define JUMPREG(r)	.asm "	j	^",r
#define MCOUNTCALL	return
#define MCOUNTPOST	return
#endif

#ifdef m_alpha

#define BYTESPERWORD 8

#define SDATAT .asm "	.rdata"

#define NOCOMPARE
#define NOUNDERSCORE

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r12

#define GCSTART r11
#define GCEND r10
#define GCCUR r9
#define CRETR r0
#define DTMP r5
#define CDTMP DTMP
#define ATMP r3
#define FDISP r4
#define rd r18
#define ehpr r22
#define retr r26
#define rSP r20
#define rd2 r28
#define Canon r19

#define CLABEL(l)	.malign && l: && LBASE
#define LBASE		.asm "	ldgp	$gp,0($27)"
#define CCALL(m,a) 	.asm "	bis	$sp,$sp,$14\n	subq	$sp,80,$sp\n	and	$sp,-8,$sp\n	stq	$26,64($sp)\n	jsr	$26,^\n	ldgp	$gp,0($26)\n	ldq	$26,64($sp)\n	bis	$14,$14,$sp",a && move _ehp,ehpr && move $CANON,Canon
#define CENTRY		.asm "	subq	$sp,256,$sp\n	stq	$9,16($sp)\n	stq	$10,24($sp)\n	stq	$11,32($sp)\n	stq	$12,40($sp)\n	stq	$13,48($sp)\n	stq	$14,56($sp)\n	stq	$26,64($sp)"  && move _ehp,ehpr && move $CANON,Canon
#define CRET		.asm "	ldq	$9,16($sp)\n	ldq	$10,24($sp)\n	ldq	$11,32($sp)\n	ldq	$12,40($sp)\n	ldq	$13,48($sp)\n	ldq	$14,56($sp)\n	ldq	$26,64($sp)\n	addq	$sp,256,$sp\n	ret	$31,($26),1"
#define CARG0(a)	move	r16,a
#define CARG1(a)	move	r17,a
#define CPUSHARG0(a)	move	a,r16
#define CPUSHARG1(a)	move	a,r17
#define CPUSHARG2(a)	move	a,r18
#define CPUSHARG3(a)	move	a,r19
#define DECR(a)		.asm "	subq	^,1",a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	adda	s,d
#define ADDSP(c)	ADDAW(c,rSP)
#define ADDSP2(c,d)	move	$0(Sp),d && adda c,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,5\n	lda	^,^",d,rd,s && add2 rd,d
#else
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,4\n	lda	^,^",d,rd,s && add2 rd,d
#endif
#define MOVIB(s,d)	.asm "	ldq_u	^,0(^)\n	extbl	^,^,^",d,s,d,s,d
#define STIB(s,d)	.asm "	.set	noat\n	ldq_u	^,0(^)\n	insbl	^,^,^\n	mskbl	^,^,^\n	bis	^,^,^\n	stq_u	^,0(^)\n	.set	at",rd,d, s,d,rd2, rd,d,rd, rd,rd2,rd, rd,d
#define MASKB(s,d)	.asm "	bis	^,^,^\n	and	^,255,^",s,s,d, d,d
#define INCBP(p)	.asm "	addq	^,1",p
#define TSTBIND(r)	MOVIB(r,rd) && comp rd,$0
#define MCOUNTCALL	return
#define MCOUNTPOST	return
#define JUMPREG(a)	.asm "	bis	^,^,$27\n	jmp	$31,($27)",a,a

#endif

#ifdef m_sparc
#include "../m_sparc/reg.h"

#ifdef SOLARIS
#define NOUNDERSCORE
#endif

#define ARGCREG	Argcreg
#define TAGREG Tagreg
#define BIGEQREG Bigeqreg
#define INDREG Indreg

#define GCSTART r8
#define GCEND r9
#define GCCUR r10
#define CRETR r16
#define DTMP r12
#define CDTMP DTMP
#define ATMP r3
#define FDISP r4
#define rd r25
#define ehpr Ehpr
#define vpr Vpr
#define retr Ret

#define LBASE		; nothing
#define CCALL(m,a) 	move	retr,Vpush && move Vp,_vp && .asm "	call	^,^\n	nop",a,m && move _vp,Vp && move _ehp,ehpr && move $CANON,Canon && move Vpop,retr
#define CENTRY		.asm "	save	%sp,-96,%sp" && move _vp,Vp && move _ehp,ehpr && move $CANON,Canon
#define CRET		move Vp,_vp && .asm "	ret	\n	restore"
#define CARG0(a)	move	r8,a
#define CARG1(a)	move	r9,a
#define CPUSHARG0(a)	move	a,r16
#define CPUSHARG1(a)	move	a,r17
#define CPUSHARG2(a)	move	a,r18
#define CPUSHARG3(a)	move	a,r19
#define DECR(a)		.asm "	dec	^",a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	move	s,rd && .asm "	sll	^,2,^",rd,rd && add2 rd,d
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,d)	move	c,rd && .asm "	sll	^,2,^",rd,rd && add Sp,rd,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,4,^\n	set	^,^",d,d,s,rd && add2 rd,d
#else
#define TABADDR(s,c,d)	move	c,d && .asm "	sll	^,3,^\n	set	^,^",d,d,s,rd && add2 rd,d
#endif
#define MOVIB(s,d)	.asm "	ldub	[^],^",s,d
#define STIB(s,d)	.asm "	stb	^,[^]",s,d
#define MASKB(s,d)	.asm "	mov	^,^\n	and	^,255,^",s,d,d,d
#define INCBP(p)	.asm "	inc	^",p
#define TSTBIND(r)	.asm "	ldub	[^],^",r,rd && comp rd,$0
#define JUMPREG(r)	.asm "	jmp	^\n	nop",r
#define MCOUNTCALL	.asm "	mov	^,%o0\n	set	mcount,^\n	jmp	^\n	nop",r0,rd,rd
#define MCOUNTPOST	move retr,DTMP && add2 $4,vpr && move 0(vpr),retr && add2 $4,vpr && move _ehp,ehpr && move $CANON,Canon && .asm "	jmpl	^+8,%g0\n	nop",DTMP
#endif

#ifdef m_C

#define NOCOMPARE
#define NOUNDERSCORE

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r9

#define GCSTART r5
#define GCEND r6
#define GCCUR r7
#define CRETR r0
#define DTMP r4
#define CDTMP DTMP
#define ATMP r2
#define FDISP r3

#define LBASE		; nothing
#define CCALL(m,a) 	.asm "	XXX dispatch(^)",a
#define CENTRY		.asm "	XXX (arg1, arg2, arg3) {"
#define CRET		.asm "	XXX return SEMI"
#define CARG(m,a)	.asm "	XXX ^ = arg^ SEMI",a,m
#define CPUSHARG(a)	.asm "	XXX *--sp = ^ SEMI",a
#define DECR(a)		.asm "	XXX --^ SEMI",a
#define TSTC(a)		.asm "	XXX TSTC(^) SEMI",a
#define ADDAW(s,d)	.asm "	moval	(^)[^],^",d,s,d
#define ADDSP(c)	.asm "	moval	(%ep)[^],%ep",c
#define ADDSP2(c,d)	.asm "	moval	(%ep)[^],^",c,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	?!!
#else
#define TABADDR(s,c,d)	.asm "	movaq	^[^],^",s,c,d
#endif
#define MOVIB(s,d)	.asm "	movzbl	(^),^",s,d
#define MASKB(s,d)	.asm "	movzbl	^,^",s,d
#define INCBP(p)	.asm "	incl	^",p
#define TSTBIND(r)	.asm "	tstb	(^)",r
#define MCOUNTCALL	.asm "	?!!"
#define MCOUNTPOST	.asm "	?!!"
#endif

#ifdef m_arm

/* If you change register assignments, remember to change in m_arm/tmp.h !! */

#define NOCOMPARE
#define NOUNDERSCORE

#define ARGCREG	 r2
#define TAGREG   r2
#define BIGEQREG r2
#define INDREG   r4

#define GCSTART r5     /* !!! This is until I find free registers */
#define GCEND r6
#define GCCUR r7

#define CRETR r0      /* !!! guess this is the same as r0 */
#define DTMP r3
#define CDTMP DTMP
#define ATMP r2
#define FDISP r14
#define rd r9         /* !!! wild guess */
#define ehpr Ehpr

#define LBASE		; nothing

#define CCALL(m,a)	.asm "	bl	PreCCALL\n	bl	^\n	bl	PostCCALL",a	
#define CENTRY		.asm  "	mov	ip,Vpr\n	stmfd	Vpr!,{r4,r5,r6,r7,r8,r9,fp,ip,lr,pc}\n	sub	fp,ip,#4\n	cmp	Vpr,sl\n	bllt	|x$stack_overflow|\n	bl	PostCENTRY"
#define CENTRYS(s)	.asm  "	mov	ip,Vpr\n	stmfd	Vpr!,{r4,r5,r6,r7,r8,r9,fp,ip,lr,pc}\n	sub	fp,ip,#4\n	sub	ip,Vpr,^\n	cmp	ip,sl\n	bllt	|x$stack_overflow_1|\n	bl	PostCENTRY",s
#define CRET		.asm "	bl	PreCRET\n	ldmea	fp,{r4,r5,r6,r7,r8,r9,fp,Vpr,pc}^\n"

#define CARG0(a)	move	r0,a  /* !!! This is dangerous but     */
#define CARG1(a)	move	r1,a  /* works as the runtime is now.  */
#define CPUSHARG0(a)	move	a,r0  /* Be aware of problems if       */
#define CPUSHARG1(a)	move	a,r1  /* calculations are done between */
#define CPUSHARG2(a)	move	a,r2  /* CPUSHARGs (or CARGs).         */
#define CPUSHARG3(a)	move	a,r3

#define DECR(a)		.asm "	sub	^,^,#1",a,a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	move s,rd && .asm "	add	^,^,^,lsl #2",d,d,rd
#define ADDSP(c)	.asm "	add	^,^,^,lsl #2",Sp,Sp,c
#define ADDSP2(c,d)	move c,rd && .asm "	add	^,^,^,lsl #2",d,Sp,rd
#ifdef HPROFILE
#define TABADDR(s,c,d)	move	c,d && move $s,rd && .asm "	add	^,^,^,lsl #4",d,rd,d
#else
#define TABADDR(s,c,d)	move	c,d && move $s,rd && .asm "	add	^,^,^,lsl #3",d,rd,d
#endif
#define MOVIB(s,d)	.asm "	ldrb	^,[^]",d,s
#define MASKB(s,d)	.asm "	and	^,^,#255",d,s
#define INCBP(p)	.asm "	add	^,^,#1",p,p
#define TSTBIND(r)	.asm "	ldrb	^,[^]",rd,r && comp rd,$0

#define MCOUNTCALL	.asm "	#?!!MCOUNTCALL"
#define MCOUNTPOST	.asm "	#?!!MCOUNTPOST"
#endif

#ifdef m_rs6000

#define SDATAT .data
#define CLABEL(s) .asm "	.globl	~\n	.csect	^[DS]",s,s && s: && .asm "	.long	~,TOC[tc0],0\n	.csect .text[PR]\n~:",s,s
#define NOUNDERSCORE

#define ARGCREG	r21
#define TAGREG r21
#define BIGEQREG r21
#define INDREG r22

#define GCSTART r13
#define GCEND r12
#define GCCUR r11
#define CRETR r27
#define DTMP r5
#define CDTMP DTMP
#define ATMP r3
#define FDISP r4
#define ehpr r16
#define rd r20

#define LBASE		; nothing
#define CCALL(m,a)	move Vp,r21 && move $-50(Vp),Vp && .asm "	mflr	30\n	bl	~\n	mtlr	30",a && move _ehp,ehpr && move r21,Vp
#define CENTRY		.asm "	mflr	0\n	stm	13,-76(1)\n	st	0,8(1)\n	stu	1,-192(1)" && move _ehp,ehpr
#define CRET		.asm "	l	1,0(1)\n	l	0,8(1)\n	mtlr	0\n	lm	13,-76(1)\n	br"

#define CARG0(a)	move	r27,a
#define CARG1(a)	move	r28,a
#define CPUSHARG0(a)	move	a,r27
#define CPUSHARG1(a)	move	a,r28
#define CPUSHARG2(a)	move	a,r29
#define CPUSHARG3(a)	move	a,r30
#define DECR(a)		.asm "	ai	^,^,-1",a,a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	move	s,rd && .asm "	sli	^,^,2",rd,rd && add2 rd,d
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,d)	move	c,rd && .asm "	sli	^,^,2",rd,rd && add Sp,rd,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	move	c,d && .asm "	sli	^,^,4",d,d && move $ s,rd && add2 rd,d
#else
#define TABADDR(s,c,d)	move	c,d && .asm "	sli	^,^,3",d,d && move $ s,rd && add2 rd,d
#endif
#define MOVIB(s,d)	.asm "	lbz	^,0(^)",d,s
#define STIB(s,d)	.asm "	stb	^,0(^)",s,d
#define MASKB(s,d)	.asm "	andil.	^,^,255",d,s
#define INCBP(p)	.asm "	ai	^,^,1",p,p
#define TSTBIND(r)	.asm "	lbz	^,0(^)",rd,r && comp rd,$0
#define MCOUNTCALL	return
#define MCOUNTPOST	return
#define JUMPREG(a)	.asm "	mtctr	^\n	bctrl",a
#endif

#ifdef m_hppa
#define TAGTABLE .data && .asm "	.SPACE $TEXT$" && .asm "	.SUBSPA $LIT$"

#define NOCOMPARE
#define NOUNDERSCORE

#define SDATAT	.data

#define ARGCREG	r2
#define TAGREG r2
#define BIGEQREG r2
#define INDREG r11

#define GCSTART r10
#define GCEND r9
#define GCCUR r8
#define CRETR r20
#define DTMP r5
#define CDTMP DTMP
#define ATMP r3
#define FDISP r4
#define rd r6
#define ehpr r12
#define Canon r7
#define retr r26

#define LBASE		; nothing
#define CCALL(m,a) 	move	retr,Vpush && move Vp,_vp && .asm "	.import ^,code\n	bl	^,%r2\n	nop",a,a && move _vp,Vp && move _ehp,ehpr && move $CANON,Canon && move Vpop,retr
#define CENTRY		.asm "	stw	%r2,-20(0,%r30)\n	copy	%r3,%r1\n	copy	%r30,%r3\n	stwm	%r1,128(0,%r30)\n        stw %r18,8(0,%r3)\n        stw %r17,12(0,%r3)\n        stw %r16,16(0,%r3)\n        stw %r15,20(0,%r3)\n        stw %r14,24(0,%r3)\n        stw %r13,28(0,%r3)\n        stw %r12,32(0,%r3)\n        stw %r11,36(0,%r3)\n        stw %r10,40(0,%r3)\n        stw %r9,44(0,%r3)\n        stw %r8,48(0,%r3)\n        stw %r7,52(0,%r3)\n        stw %r6,56(0,%r3)\n        stw %r5,60(0,%r3)\n        stw %r4,64(0,%r3)" && move _vp,Vp && move _ehp,ehpr && move $CANON,Canon
#define CRET		move Vp,_vp && .asm "	ldw	-20(0,%r3),%r2\n        ldw 8(0,%r3),%r18\n        ldw 12(0,%r3),%r17\n        ldw 16(0,%r3),%r16\n        ldw 20(0,%r3),%r15\n        ldw 24(0,%r3),%r14\n        ldw 28(0,%r3),%r13\n        ldw 32(0,%r3),%r12\n        ldw 36(0,%r3),%r11\n        ldw 40(0,%r3),%r10\n        ldw 44(0,%r3),%r9\n        ldw 48(0,%r3),%r8\n        ldw 52(0,%r3),%r7\n        ldw 56(0,%r3),%r6\n        ldw 60(0,%r3),%r5\n        ldw 64(0,%r3),%r4\n	ldo	64(%r3),%r30\n	ldwm	-64(0,%r30),%r3\n	bv,n	0(%r2)"
#define CARG0(a)	move	r18,a
#define CARG1(a)	move	r17,a
#define CPUSHARG0(a)	move	a,r18
#define CPUSHARG1(a)	move	a,r17
#define CPUSHARG2(a)	move	a,r16
#define CPUSHARG3(a)	move	a,r15
#define DECR(a)		sub2	1,a
#define TSTC(a)		comp	a,$0
#define ADDAW(s,d)	.asm	"" && lsh $2,s,rd && add2 rd,d
#define ADDSP(c)	ADDAW(c,Sp)
#define ADDSP2(c,d)	.asm	"" && lsh $2,c,rd && add Sp,rd,d
#ifdef HPROFILE
#define TABADDR(s,c,d)	lsh	$4,c,rd && move $ s,d && add2 rd,d
#else
#define TABADDR(s,c,d)	lsh	$3,c,rd && move $ s,d && add2 rd,d
#endif
#define MOVIB(s,d)	.asm "	ldb	0(0,^),^",s,d
#define STIB(s,d)	.asm "	stb	^,0(0,^)",s,d
#define MASKB(s,d)	.asm "	copy	^,^\n	extru	^,31,8,^",s,d,d,d
#define INCBP(p)	.asm "	ldo	1(^),^",p,p
#define TSTBIND(r)	.asm "	ldb	0(0,^),^",r,rd && comp rd,$0
#define MCOUNTCALL	return
#define MCOUNTPOST	return
#define JUMPREG(a)	.asm "	bv,n	0(^)",a

#endif /* m_hppa */

#ifndef JUMPREG
#define JUMPREG(a)   "No jumpreg macro for this machine"
#endif

#ifdef CARG
#define CARG0(a)	CARG(0,a)
#define CARG1(a)	CARG(1,a)
#endif

#ifdef CPUSHARG
#define CPUSHARG0(x)	CPUSHARG(x)
#define CPUSHARG1(x)	CPUSHARG(x)
#define CPUSHARG2(x)	CPUSHARG(x)
#define CPUSHARG3(x)	CPUSHARG(x)
#endif

#ifndef SDATAD
#define SDATAD	.data
#endif
#ifndef SDATAT
#define SDATAT	.text
#endif

#define LINKWORD 1

#ifndef CENTRYS
#define CENTRYS(s) CENTRY
#endif

#ifndef CLABEL
#define CLABEL(s) s:
#endif

#ifndef BYTESPERWORD
#define BYTESPERWORD 4
#endif

#ifndef TAGTABLE
#define TAGTABLE SDATAD
#endif
