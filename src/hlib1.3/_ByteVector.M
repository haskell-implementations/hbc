#include "../runtime/machdep.M"
; Some more machine dependent stuff.

#if BYTESPERWORD == 4
#define BYTESPERWORD1 3
#define LOGBYTESPERWORD 2
#endif
#if BYTESPERWORD == 8
#define BYTESPERWORD1 7
#define LOGBYTESPERWORD 3
#endif

#define PSTRING PAIR0
#define INT2 DFLOAT

#define CVTBYTELEN(r)	add2 $ BYTESPERWORD1,r && rsh2 $LOGBYTESPERWORD,r

#ifdef m_vax
#define CMPIIB(a,b)	.asm "	cmpb	(^),(^)",a,b
#endif /* m_vax */

#ifdef m_32000
#define CMPIIB(a,b)	.asm "	cmpb	0(^),0(^)",a,b
#endif /* m_32000 */

#ifdef m_68000
#define CMPIIB(a,b)	.asm "	movb	(^),d0\n	cmpb	(^),d0",a,b
#endif /* m_68000 */

#ifdef m_360
			???
#endif /* m_360 */

#ifdef m_80386
#define CMPIIB(a,b)	.asm "	movb	(^),%al\n	cmpb	(^),%al",a,b
#endif /* m_80386 */

#ifdef m_rtpc
			???
#endif /* m_rtpc */

#ifdef m_cray
			???
#endif /* m_cray */

#ifdef m_mips
#define RCMP1 r11
#define RCMP2 r12
#define RECOMPAREIIB comp RCMP1,RCMP2
#endif /* m_mips */

#ifdef m_alpha
#define RCMP1 r10
#define RCMP2 r11
#define RECOMPAREIIB comp RCMP1,RCMP2

#define GETLENGTH(r,d) .asm "	ldl	^,8(^)\n	ldl	^,12(^)\n	subq	^,^,^",d,r,rd,r,rd,d,d
#define GETLOWER(r,d) .asm "	ldl	^,8(^)",d,r
#define GETUPPER(r,d) .asm "	ldl	^,12(^)",d,r
#define ALLOCINT2 move $ INT2,0(Hp) && move $2(Hp),Hp 
#define SETLOWER(x) move x,rd && .asm "	stl	^,-8($21)",rd
#define SETUPPER(x) move x,rd && .asm "	stl	^,-4($21)",rd
#define SETADDLOWER(x,y) add x,y,rd && SETLOWER(rd)
#define SETADDUPPER(x,y) add x,y,rd && SETUPPER(rd)
#endif /* m_alpha */

#ifdef m_sparc
#define RCMP1 r8
#define RCMP2 r9
#endif /* m_sparc */

#ifdef m_C
			XXX
#endif /* m_C */

#ifdef m_arm
			???
#endif /* m_arm */

#ifdef m_rs6000
#define RCMP1 r12
#define RCMP2 r13
#endif /* m_rs6000 */

#ifdef m_hppa
#define RCMP1 r8
#define RCMP2 r9
#define RECOMPAREIIB comp RCMP1,RCMP2
#endif /* m_hppa */

#ifndef GETLENGTH
; works for 4 byte words
#define GETLENGTH(r,d) move 2(r),d && sub2 1(r),d
#define GETLOWER(r,d) move 1(r),d
#define GETUPPER(r,d) move 2(r),d
#define ALLOCINT2 move $ INT2,0(Hp) && move $3(Hp),Hp
#define SETLOWER(x) move x,-2(Hp)
#define SETUPPER(x) move x,-1(Hp)
#define SETADDLOWER(x,y) SETLOWER(x) && add2 y,-2(Hp)
#define SETADDUPPER(x,y) SETUPPER(x) && add2 y,-1(Hp)
#endif

#if !defined(CMPIIB) && defined(RCMP1) && defined(RCMP2)
#define CMPIIB(x,y) MOVIB(x,RCMP1) && MOVIB(y,RCMP2) && comp RCMP1,RCMP2
#endif

#ifndef ADDHP
; add to Hp. The asm makes the stack simulator flush.
#define ADDHP(r) .asm "" && adda DTMP,Hp
#endif

#ifndef ADDBYTEPTR
#define ADDBYTEPTR(s,d)	add2 s,d
#endif


;
; Packed strings are stored as follows:
; 3 words (tag and two pointers)
; PSTRING   .                .
;           |                |
; 2 ints    INT2 low high    |
; string area                DVEK size data
; The low and high index indicates offsets into the data area
; where the current string starts and ends.  This way
; many strings can share the same DVEK to get different
; strings (and we get a fast substr operation).
;

	.data
#ifdef HPROFILE
#include "../mcode/magic.h"
	.word	_USE_heap
#ifdef SLOP2
#define HPROFHP	move	$ByteVector_LABEL, toH
#define HWORDS  .word	ByteVector_LABEL
#else
#define HPROFHP	move	$ByteVector_LABEL, toH && 	move 	$0, toH
#define HWORDS  .word	ByteVector_LABEL,0
#endif

#else
	.word	_USE_none
#define HPROFHP
#define HWORDS
#endif

; NIL
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__nilBV
C__ByteVector$__nilBV:
	.word	$VAPG
	.word	VC__ByteVector$__nilBV
	.word	$0
	.export	VC__ByteVector$__nilBV
VC__ByteVector$__nilBV:
	.word	$0
	.word	C__ByteVector$__nilBV
	.word	unw0
	.word	vunw0
	.word	J0C__ByteVector$__nilBV
	.word	S0C__ByteVector$__nilBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S0C__ByteVector$__nilBV
	.export	J0C__ByteVector$__nilBV
	.malign
S0C__ByteVector$__nilBV:
	move	$1(Sp),Vpush
J0C__ByteVector$__nilBV:
	.funbegin	C__ByteVector$__nilBV 0
	call    entercaf
	.data
	.word	0
I00:
	.word INT2,0,0
	HWORDS

	.word	0
B0:
	.word	DVEK
	.word	0
	; nothing
	HWORDS

	.text
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$ PSTRING,0(r0)
	move	$I00,1(r0)
	move	$B0,2(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return
	.funend

; LENGTH
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__lengthBV
C__ByteVector$__lengthBV:
	.word	FUN
	.word	VC__ByteVector$__lengthBV
	.export	VC__ByteVector$__lengthBV
VC__ByteVector$__lengthBV:
	.word	$1
	.word	C__ByteVector$__lengthBV
	.word	unw1
	.word	vunw1
	.word	J1C__ByteVector$__lengthBV
	.word	S1C__ByteVector$__lengthBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C__ByteVector$__lengthBV
	.export	J1C__ByteVector$__lengthBV
	.malign
S1C__ByteVector$__lengthBV:
	move	$2(Sp),Vpush
J1C__ByteVector$__lengthBV:
	.funbegin	C__ByteVector$__lengthBV 1
	move	1(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__lengthBV,1(r0)
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	1(r0),ATMP		; pointer to INT2 node
	GETLENGTH(ATMP,DTMP)		; get length into DTMP
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$INT,0(r0)
	move	DTMP,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return
	.funend

; INDEX
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__indexBV
C__ByteVector$__indexBV:
	.word	FUN
	.word	VC__ByteVector$__indexBV
	.export	VC__ByteVector$__indexBV
VC__ByteVector$__indexBV:
	.word	$2
	.word	C__ByteVector$__indexBV
	.word	unw2
	.word	vunw2
	.word	J2C__ByteVector$__indexBV
	.word	S2C__ByteVector$__indexBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$2
	.word	LT45
	.word	C_Prelude$__error
	.text
	.export	S2C__ByteVector$__indexBV
	.export	J2C__ByteVector$__indexBV
	.malign
S2C__ByteVector$__indexBV:
	move	$3(Sp),Vpush
J2C__ByteVector$__indexBV:
	.funbegin	C__ByteVector$__indexBV 2
	move	2(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__indexBV,1(r0)
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate the string
	move	r0,0(Sp)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate the index
	comp	$0,1(r0)
	jle	LL45
LL42:
	.data
	.word	$0
LT45:
	.word	$PAIR1
	.word	CC105
	.word	LT42
	HWORDS
	.word	$0
LT42:
	.word	STRINGN
	.word	LT43
	.word	$19
	HWORDS
LT43:
	.string	"ndexBV out of range"
	.text
	move	$LT45,1(Sp)
	move	$1(Sp),Sp
	jumpf	J1C_Prelude$__error
LL45:
	move	0(Sp),ATMP
	move	1(ATMP),ATMP
	GETLENGTH(ATMP,DTMP)		; compute length
	comp	1(r0),DTMP		; is index greater than length?
	jge	LL42

	GETLOWER(ATMP,DTMP)
	add2	1(r0),DTMP		; compute real index into the DVEK

	move	0(Sp),ATMP
	move	2(ATMP),ATMP		; pointer to DVEK node
	move	$2(ATMP),ATMP		; pointer to data area
	ADDBYTEPTR(DTMP,ATMP)		; pointer to the char we want
	MOVIB(ATMP,DTMP)		; and get it

	move	Vpop,Sp
	move	-1(Sp),r0
	move	$CHAR,0(r0)
	move	DTMP,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return
	.funend

; SUBSTR
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__substrBV
C__ByteVector$__substrBV:
	.word	FUN
	.word	VC__ByteVector$__substrBV
	.export	VC__ByteVector$__substrBV
VC__ByteVector$__substrBV:
	.word	$3
	.word	C__ByteVector$__substrBV
	.word	unw3
	.word	vunw3
	.word	J3C__ByteVector$__substrBV
	.word	S3C__ByteVector$__substrBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$4
	.word	LT69
	.word	C_Prelude$__error
	.word	LT76
	.word	LT82
	.text
	.export	S3C__ByteVector$__substrBV
	.export	J3C__ByteVector$__substrBV
	.malign
S3C__ByteVector$__substrBV:
	move	$4(Sp),Vpush
J3C__ByteVector$__substrBV:
	.funbegin	C__ByteVector$__substrBV 3
	move	3(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__substrBV,1(r0)
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)			; evaluate the string
	move	r0,0(Sp)

	move	2(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)			; evaluate end
	move	r0,2(Sp)

	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)			; evaluate start
	comp	$0,1(r0)
	jle	LL65
LL62:
	.data
	.word	$0
LT69:
	.word	$PAIR1
	.word	CC115
	.word	LT66
	HWORDS
	.word	$0
LT66:
	.word	STRINGN
	.word	LT67
	.word	$26
	HWORDS
LT67:
	.string	"ubstrBV start out of range"
	.text
	move	$LT69,2(Sp)
	move	$2(Sp),Sp
	jumpf	J1C_Prelude$__error

LL65:
	move	0(Sp),ATMP
	move	1(ATMP),ATMP
	GETLENGTH(ATMP,DTMP)		; compute length
	comp	1(r0),DTMP		; is start greater than length?
	jgt	LL62

LL61:
	move	2(Sp),r0		; end
	comp	$0,1(r0)
	jle	LL73
LL70:
	.data
	.word	$0
LT76:
	.word	$PAIR1
	.word	CC115
	.word	LT73
	HWORDS
	.word	$0
LT73:
	.word	STRINGN
	.word	LT74
	.word	$24
	HWORDS
LT74:
	.string	"ubstrBV end out of range"
	.text
	move	$LT76,2(Sp)
	move	$2(Sp),Sp
	jumpf	J1C_Prelude$__error

LL73:
	comp	1(r0),DTMP		; is end greater than length?
	jgt	LL70

LL69:
	move	1(r0),DTMP		; end
	move	1(Sp),ATMP
	comp	1(ATMP),DTMP		; start <= end ?
	jle	LL77
	.data
	.word	$0
LT82:
	.word	$PAIR1
	.word	CC115
	.word	LT79
	HWORDS
	.word	$0
LT79:
	.word	STRINGN
	.word	LT80
	.word	$19
	HWORDS
LT80:
	.string	"ubstrBV end < start"
	.text
	move	$LT82,2(Sp)
	move	$2(Sp),Sp
	jumpf	J1C_Prelude$__error

LL77:
	comp	Hp,_ehp
	jlth	LT83
	call	GARB

LT83:
	move	0(Sp),ATMP
	move	1(ATMP),ATMP
	GETLOWER(ATMP,DTMP)			; old lower bound

	move	Hp,ATMP
	ALLOCINT2
	move	1(Sp),r0
	SETADDLOWER(1(r0),DTMP)			; new lower bound = start + old lower
	move	2(Sp),r0
	SETADDUPPER(1(r0),DTMP)			; new upper bound = end + old lower
	HPROFHP

	move	0(Sp),r0
	move	2(r0),DTMP			; pointer to DVEK

	move	Vpop,Sp
	move	-1(Sp),r0
	move	$ PSTRING,0(r0)
	move	ATMP,1(r0)
	move	DTMP,2(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return
	.funend

;
; APPENDPS
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__appendBV
C__ByteVector$__appendBV:
	.word	FUN
	.word	VC__ByteVector$__appendBV
	.export	VC__ByteVector$__appendBV
VC__ByteVector$__appendBV:
	.word	$2
	.word	C__ByteVector$__appendBV
	.word	unw2
	.word	vunw2
	.word	J2C__ByteVector$__appendBV
	.word	S2C__ByteVector$__appendBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C__ByteVector$__appendBV
	.export	J2C__ByteVector$__appendBV
	.malign
S2C__ByteVector$__appendBV:
	move	$3(Sp),Vpush
J2C__ByteVector$__appendBV:
	.funbegin	C__ByteVector$__appendBV 2
	move	2(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__appendBV,1(r0)
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,0(Sp)

	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	r0,1(Sp)

	move	1(r0),r0			; bounds node
	GETLENGTH(r0,DTMP)			; length of second string
	move	DTMP,Vpush

	move	0(Sp),r0
	move	1(r0),r0			; bounds node
	GETLENGTH(r0,DTMP)			; length of first string
	move	DTMP,Vpush
	add2	1(Vp),DTMP			; total length

	move	DTMP,Vpush			; save for later use
	move	DTMP,ARGCREG			; here is the length
	CVTBYTELEN(ARGCREG)			; round up to # of words to word
	move	ARGCREG,Vpush
	add2	$20,ARGCREG			; add some slop
	call	NGARB				; and make sure there is room
	move	Vpop,DTMP			; get the saved rounded size
	move	$0(Hp),Spush			; save vector address
	move	$DVEK,toH			; tag
	move	DTMP,toH			; size
	move	Hp,ATMP				; base of packed string
	ADDHP(DTMP)				; allocate space
	HPROFHP

; size of first string at 0(Vp), size of second at 1(Vp)

; copy first string
	move	1(Sp),r0
	move	1(r0),r1
	GETLOWER(r1,DTMP)
	move	2(r0),r0			; vector node
	move	$2(r0),r0			; base of string
	ADDBYTEPTR(DTMP,r0)			; start of string
	move	1(Vp),DTMP			; # bytes to copy
	move	r0,r1
	ADDBYTEPTR(DTMP,r1)			; r1 is upper bound
	jump	aloop1x
aloop1:
	MOVIB(r0,DTMP)
	INCBP(r0)
	STIB(DTMP,ATMP)
	INCBP(ATMP)
aloop1x:
	comp	r0,r1				; end of string?
	jlt	aloop1
		
; copy second string
	move	2(Sp),r0
	move	1(r0),r1
	GETLOWER(r1,DTMP)
	move	2(r0),r0			; vector node
	move	$2(r0),r0			; base of string
	ADDBYTEPTR(DTMP,r0)			; start of string
	move	2(Vp),DTMP			; # bytes to copy
	move	r0,r1
	ADDBYTEPTR(DTMP,r1)			; r1 is upper bound
	jump	aloop2x
aloop2:
	MOVIB(r0,DTMP)
	INCBP(r0)
	STIB(DTMP,ATMP)
	INCBP(ATMP)
aloop2x:
	comp	r0,r1				; end of string?
	jlt	aloop2

; make the bounds node
	move	Hp,ATMP
	ALLOCINT2
	SETLOWER($0)				; lower bound is 0
	SETUPPER(Vpop)				; upper bound is length
	HPROFHP

	move	$2(Vp),Vp			; pop sizes

	move	Spop,DTMP			; get address to vector node
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$ PSTRING,0(r0)
	move	ATMP,1(r0)
	move	DTMP,2(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return
	.funend

; PACKSTRING
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__packBV
C__ByteVector$__packBV:
	.word	FUN
	.word	VC__ByteVector$__packBV
	.export	VC__ByteVector$__packBV
VC__ByteVector$__packBV:
	.word	$1
	.word	C__ByteVector$__packBV
	.word	unw1
	.word	vunw1
	.word	J1C__ByteVector$__packBV
	.word	S1C__ByteVector$__packBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$1
	.word	C_Prelude$__length
	.text
	.export	S1C__ByteVector$__packBV
	.export	J1C__ByteVector$__packBV
	.malign
S1C__ByteVector$__packBV:
	move	$2(Sp),Vpush
J1C__ByteVector$__packBV:
	.funbegin	C__ByteVector$__packBV 1
	move	1(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__packBV,1(r0)

; Compute length and evaluate all chars in a list.
	move	$0,DTMP				; length is 0 so far
	move	0(Sp),r0			; get first list cell
slenloop:
	move	DTMP,Vpush
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	Vpop,DTMP
	move	0(r0),INDREG
	comp	oconstr(INDREG),$O_TAG0		; is it nil?
	jeq	slenend
	add2	$1,DTMP
	move	r0,Spush
	move	DTMP,Vpush
	move	1(r0),r0			; get head pointer
	move	0(r0),INDREG
	call	oeval(INDREG)			; and evaluate
	move	Vpop,DTMP
	move	Spop,r0
	move	2(r0),r0			; get tail
	jump	slenloop
slenend:
; length is now in DTMP
	move	DTMP,Vpush			; save for later use
	move	DTMP,ARGCREG			; here is the length
	CVTBYTELEN(ARGCREG)			; round up to # of words to word
	move	ARGCREG,Vpush
	add2	$20,ARGCREG			; add some slop
	call	NGARB				; and make sure there is room
	move	Vpop,DTMP			; get the saved rounded size
	move	$0(Hp),Spush
	move	$DVEK,toH			; tag
	move	DTMP,toH			; size
	move	Hp,ATMP				; base of packed string
	ADDHP(DTMP)				; allocate space
	HPROFHP

	move	1(Sp),r0			; get first list cell
fillloop:
	move	0(r0),INDREG
	comp	oconstr(INDREG),$O_TAG0		; is it nil?
	jeq	fillend
	move	1(r0),r1			; head pointer
	move	1(r1),DTMP			; get the character
	STIB(DTMP,ATMP)				; store the byte
	INCBP(ATMP)				; and point to next
	move	2(r0),r0			; get tail
	jump	fillloop
fillend:
; the vector has been filled with characters, create lower&upper node, and main node
	move	Hp,ATMP
	ALLOCINT2
	SETLOWER($0)
	SETUPPER(Vpop)
	HPROFHP

	move	Spop,DTMP			; saved pointer to start of vector
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$ PSTRING,0(r0)
	move	ATMP,1(r0)
	move	DTMP,2(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return

; COMPAREPS
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__compareBV
C__ByteVector$__compareBV:
	.word	FUN
	.word	VC__ByteVector$__compareBV
	.export	VC__ByteVector$__compareBV
VC__ByteVector$__compareBV:
	.word	$2
	.word	C__ByteVector$__compareBV
	.word	unw2
	.word	vunw2
	.word	J2C__ByteVector$__compareBV
	.word	S2C__ByteVector$__compareBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C__ByteVector$__compareBV
	.export	J2C__ByteVector$__compareBV
	.malign
S2C__ByteVector$__compareBV:
	move	$3(Sp),Vpush
J2C__ByteVector$__compareBV:
	.funbegin	C__ByteVector$__compareBV 2
	move	2(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__compareBV,1(r0)
	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate second arg
	move	r0,1(Sp)
	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate first arg

#ifdef m_80386
; we are almost out of registers, *sigh*
#define r1 r4
#endif
	move	1(Sp),r1

; r0&r1 point at the nodes to compare
	comp	r0,r1			; check id same node
	jeq	cpsteq
	comp	1(r0),1(r1)		; or same bounds
	jne	cpst1
	comp	2(r0),2(r1)		;    and vector
	jeq	cpsteq
cpst1:
	move	1(r0),ATMP		; bounds node
	move	2(r0),r0		; vector node
	move	$2(r0),r0		; start of string
	GETLOWER(ATMP,DTMP)		; lower bound
	ADDBYTEPTR(DTMP,r0)		; start of string
	GETLENGTH(ATMP,DTMP)
	move	DTMP,Vpush

	move	1(r1),ATMP		; bounds node
	move	2(r1),r1		; vector node
	move	$2(r1),r1		; start of string
	GETLOWER(ATMP,DTMP)		; lower bound
	ADDBYTEPTR(DTMP,r1)		; start of string
	GETLENGTH(ATMP,DTMP)

	move	Vpop,ATMP
; r0&r1 now point at the strings, ATMP&DTMP has the lengths
cpstl:
	comp	ATMP,$0
	jeq	cpstend			; first string has ended
	comp	DTMP,$0
	jeq	cpstgt			; second string has ended, but first has not, so first is greater

	sub2	$1,ATMP
	sub2	$1,DTMP

	CMPIIB(r0,r1)			; compare bytes
	jgt	cpstgt
#ifdef NOCOMPARE
	RECOMPAREIIB
#endif
	jlt	cpstlt
	INCBP(r0)
	INCBP(r1)
	jump	cpstl

#ifdef m_80386
#undef r1
#endif

cpstend:
	move	DTMP,CDTMP		; yuck! make sure both lengths are in registers that we can compare
	move	ATMP,TAGREG
	comp	TAGREG,CDTMP
	jne	cpstlt			; first have ended, but not second, so first is less
	; both have ended
	
cpsteq:
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$INT,0(r0)
	move	$0,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return
	
cpstlt:
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$INT,0(r0)
	move	$-1,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return
	
cpstgt:
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$INT,0(r0)
	move	$1,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return

;;;;;
; UNPACKPS
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__unpackBV
C__ByteVector$__unpackBV:
	.word	FUN
	.word	VC__ByteVector$__unpackBV
	.export	VC__ByteVector$__unpackBV
VC__ByteVector$__unpackBV:
	.word	$1
	.word	C__ByteVector$__unpackBV
	.word	unw1
	.word	vunw1
	.word	J1C__ByteVector$__unpackBV
	.word	S1C__ByteVector$__unpackBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S1C__ByteVector$__unpackBV
	.export	J1C__ByteVector$__unpackBV
	.malign
S1C__ByteVector$__unpackBV:
	move	$2(Sp),Vpush
J1C__ByteVector$__unpackBV:
	.funbegin	C__ByteVector$__unpackBV 1
	move	1(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__unpackBV,1(r0)

	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)		; evaluate arg
	move	r0,0(Sp)

	move	1(r0),ATMP
	GETLENGTH(ATMP,DTMP)		; get length
	comp	DTMP,$0
	jne	LL25			; not empty
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$TAG0,0(r0)		; return []
	move	$0,1(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 2(r0)
#ifdef SLOP2
	move 	$0, 3(r0)
#endif
#endif
	return
LL25:
	comp	Hp,_ehp
	jlth	LT40
	call	GARB
LT40:
	move	0(Sp),r0
	move	1(r0),ATMP
	GETLOWER(ATMP,DTMP)		; get lower index
	move	2(r0),ATMP		; pointer to DVEK node
	move	$2(ATMP),ATMP		; pointer to data area
	ADDBYTEPTR(DTMP,ATMP)		; pointer to the char we want
	MOVIB(ATMP,DTMP)		; and get it
	TABADDR(chartab,DTMP,ATMP)
	move	ATMP,Spush

	move	Hp,ATMP
	ALLOCINT2
	move	1(r0),r0		; pointer to bounds node
	GETLOWER(r0,DTMP)		; get lower index
	add2	$1,DTMP
	SETLOWER(DTMP)
	GETUPPER(r0,DTMP)
	SETUPPER(DTMP)
	HPROFHP

	move	$ PSTRING,toH
	move	ATMP,toH		; new bounds node
	move	Spop,ATMP		; pop address to saved CHAR node
	move	0(Sp),r0
	move	2(r0),toH		; and old DVEK
	HPROFHP

	move	$VAP,toH		; build node to unpack rest
	move	$VC__ByteVector$__unpackBV,toH
	move	$-5(Hp),toH
	HPROFHP

	move	Vpop,Sp
	move	-1(Sp),r0
	move	$PAIR1,0(r0)		; cons node
	move	ATMP,1(r0)		; with CHAR head
	move	$-3(Hp),2(r0)		; and unpackBV tail
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return
	.funend

;
; CPOINTERTOPS
	.data
	.malign
	.word	$0
	.export	C__ByteVector$__cPointerToBV
C__ByteVector$__cPointerToBV:
	.word	FUN
	.word	VC__ByteVector$__cPointerToBV
	.export	VC__ByteVector$__cPointerToBV
VC__ByteVector$__cPointerToBV:
	.word	$2
	.word	C__ByteVector$__cPointerToBV
	.word	unw2
	.word	vunw2
	.word	J2C__ByteVector$__cPointerToBV
	.word	S2C__ByteVector$__cPointerToBV
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.word	$0
	.text
	.export	S2C__ByteVector$__cPointerToBV
	.export	J2C__ByteVector$__cPointerToBV
	.malign
S2C__ByteVector$__cPointerToBV:
	move	$3(Sp),Vpush
J2C__ByteVector$__cPointerToBV:
	.funbegin	C__ByteVector$__cPointerToBV 2
	move	2(Sp),r0
	move	$ZAP,0(r0)
	move	$VC__ByteVector$__cPointerToBV,1(r0)

	move	0(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)			; evaluate CPointer
	move	r0,0(Sp)

	move	1(Sp),r0
	move	0(r0),INDREG
	call	oeval(INDREG)			; evaluate length
	move	r0,1(Sp)

	move	1(r0),DTMP			; get the length

	move	DTMP,Vpush			; save for later use
	move	DTMP,ARGCREG			; here is the length
	CVTBYTELEN(ARGCREG)			; round up to # of words to word
	move	ARGCREG,Vpush
	add2	$20,ARGCREG			; add some slop
	call	NGARB				; and make sure there is room
	move	Vpop,DTMP			; get the saved rounded size
	move	$0(Hp),Spush			; save vector address
	move	$DVEK,toH			; tag
	move	DTMP,toH			; size
	move	Hp,ATMP				; base of packed string
	ADDHP(DTMP)				; allocate space
	HPROFHP

; copy data
	move	0(Vp),DTMP			; get size
	move	1(Sp),r0
	move	1(r0),r0			; start of data
	move	r0,r1
	ADDBYTEPTR(DTMP,r1)			; end of data
	jump	cloop1x
cloop1:
	MOVIB(r0,DTMP)
	INCBP(r0)
	STIB(DTMP,ATMP)
	INCBP(ATMP)
cloop1x:
	comp	r0,r1				; end of string?
	jlt	cloop1

; make the bounds node
	move	Hp,ATMP
	ALLOCINT2
	SETLOWER($0)				; lower bound is 0
	SETUPPER(Vpop)				; upper bound is length
	HPROFHP

	move	Spop,DTMP			; get address to vector node
	move	Vpop,Sp
	move	-1(Sp),r0
	move	$ PSTRING,0(r0)
	move	ATMP,1(r0)
	move	DTMP,2(r0)
#ifdef HPROFILE
	move	$ByteVector_LABEL, 3(r0)
#ifdef SLOP2
	move 	$0, 4(r0)
#endif
#endif
	return
	.funend

#ifdef HPROFILE
	.malign
ByteVector_LABEL:
	.word	HEAPMAGIC
	.word	$lib_LABEL
	.word   $BV_LABEL
	.word   $bv_LABEL
	.word   $BV_LABEL
	.word   $BV_LABEL

lib_LABEL:		.string "hlib1.3"
BV_LABEL:	.string	"_ByteVector"
bv_LABEL:	.string	"(bytevector)"
#endif
