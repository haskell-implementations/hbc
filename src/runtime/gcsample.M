#ifdef HPROFILE
#include "machdep.M"
#include "../mcode/magic.h"

#include "gc.h"

#define STATISTICS 0
#if STATISTICS
#define SINCR(a) add2 $1,a
#else
#define SINCR(a)
#endif

	.word	_USE_heap

	.pragma	GC_ON

/*************************************************************************
 * 									 *
 *	New sampling scheme, written in Mcode for efficiency		 *
 *									 *
 *************************************************************************/

        .export SAMPLE 
	.malign
SAMPLE:
	LBASE
        move    Hp,_hp
        move    Sp,_ep
        move    $-1,_funlink
        CCALL(0,_DoSample)
        move    _hp,Hp
        move    _ep,Sp
        return

        .export _samplegc
CLABEL(_samplegc)
	CENTRYS(GC_STACK)
        CARG1(r1)
        CARG0(r0)
        move    $0,Vpush
        move    $NGCRET,Vpush
        move    r1,Vpush
        move    r0,Vpush
        move    _ep,Sp
        move    _hp,Hp
        move    _startheap,GCSTART
        move    _endheap,GCEND
        move    _curheap,GCCUR
        move    $-1,_funlink
        move    Sp,gcSp
        jump    sgcpop

	.export sallmoved
        .export sgcpop
        .export sgcd

sallmoved:
        move    r0,0(r1)
sgcpop:
        move    Vpop,r0
        move    Vpop,r1
sgcd:   move    0(r0),INDREG
        jump    ogcsamp(INDREG)		; calls the sampling gc entry	

/*
 *	Sampling garbage collection for nodes with 2 nonpointer parts
 */

        .export sgc20
sgc20:
        comp    r0,GCSTART
        jlth    sallmoved
        comp    r0,GCEND
        jgeh    sallmoved

        comp    r0,Hp
        jgeh    sgc20i
        comp    r0,GCCUR
        jgeh    sallmoved

sgc20i:
        move    Hp,ATMP
        move    0(r0),toH
        move    1(r0),toH
        move    2(r0),toH               /* slop 1:      DW */
#ifdef SLOP2
        move    3(r0),toH               /* slop 2:      DW */
#endif
        move    $MOVED,0(r0)
        move    ATMP,1(r0)
        move    ATMP,0(r1)

	move    _nodelist2, 2(r0)	; link node to list
	move	r0, _nodelist2		;

        jump    sgcpop

/*
 *	Sampling garbage collection for nodes with 3 nonpointer parts
 */

	.export	sgc30
sgc30:
	comp	r0,GCSTART
	jlth	sallmoved
	comp	r0,GCEND
	jgeh	sallmoved

	comp	r0,Hp
	jgeh	sgc30i
	comp	r0,GCCUR
	jgeh	sallmoved

sgc30i:
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move    3(r0),toH               /* slop 1:      DW */
#ifdef SLOP2
        move    4(r0),toH               /* slop 2:      DW */
#endif
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move    _nodelist3, 2(r0)      ; link node to list
        move    r0, _nodelist3         ;

	jump	sgcpop



/*
 *      Sampling garbage collection for nodes with 1 nonpointer and 1 pointer 
 *	part.
 */

	.export	sgc11
sgc11:
	comp	r0,GCSTART
	jlth	sgc11h
	comp	r0,GCEND
	jgeh	sgc11h

	comp	r0,Hp
	jgeh	sgc11i
	comp	r0,GCCUR
	jgeh	sallmoved

sgc11i:
	move	Hp,ATMP
	move	0(r0),toH
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
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move    _nodelist2, 2(r0)      ; link node to list
        move    r0, _nodelist2         ;

	jump	sgcpop

	.export	sgc11h
sgc11h:
	move    0(r0),Spush     ; set mark
        move    r0,Spush        ;

	move    $MARKED,0(r0)   
	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	sgcd


/*
 *      Sampling garbage collection for nodes with 1 nonpointer and 2 pointer 
 *	parts.
 */

	.export	sgc12
sgc12:
	comp	r0,GCSTART
	jlth	sgc12h
	comp	r0,GCEND
	jgeh	sgc12h

	comp	r0,Hp
	jgeh	sgc12i
	comp	r0,GCCUR
	jgeh	sallmoved

sgc12i:
	move	Hp,ATMP
	move	0(r0),toH
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
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move    _nodelist3, 2(r0)      ; link node to list
        move    r0, _nodelist3         ;

	jump	sgcpop

	.export	sgc12h
sgc12h:
	move    0(r0),Spush     ; set mark
        move    r0,Spush        ;

	move	$MARKED,0(r0)	;

	move	$2(r0),Vpush
	move	2(r0),Vpush
	move	r0,0(r1)
	move	$1(r0),r1
	move	1(r0),r0
	jump	sgcd


/*
 *      Sampling garbage collection for nodes with 2 nonpointer and 1 pointer 
 *      parts.
 */

	.export	sgc21
sgc21:
	comp	r0,GCSTART
	jlth	sgc21h
	comp	r0,GCEND
	jgeh	sgc21h

	comp	r0,Hp
	jgeh	sgc21i
	comp	r0,GCCUR
	jgeh	sallmoved

sgc21i:
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	Hp,Vpush
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
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

	move    _nodelist3, 2(r0)      ; link node to list
        move    r0, _nodelist3         ;

	jump	sgcpop

sgc21h:
        move    0(r0),Spush     ; set mark
        move    r0,Spush        ;

        move    $MARKED,0(r0)   ;
 
        move    r0,0(r1)
        move    $2(r0),r1
        move    2(r0),r0
        jump    sgcd

/*
 *      Sampling garbage collection for VEK nodes. 
 */

	.export	sgcvek
sgcvek:
	move	1(r0),DTMP
	; fall into ...
	.export	sgcvekt
sgcvekt:
	comp	r0,GCSTART
	jlth	sgcvekh
	comp	r0,GCEND
	jgeh	sgcvekh

	comp	r0,Hp
	jgeh	sgcveki
	comp	r0,GCCUR
	jgeh	sallmoved
	
sgcveki:
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move 	r0, ATMP		/* DW */
	move	$2(r0),r0

	.export	sgcv1
sgcv1:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	sgcv1end		/* DW */	
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	sgcv1

sgcv1end:
	move    0(r0),toH               ; slop 1:       DW
#ifdef SLOP2
        move    1(r0),toH               ; slop 2:       DW
#endif

	move    _nodelistvek, 2(ATMP)   ; link node to list
        move    ATMP, _nodelistvek      ;

        jump 	sgcpop			;		DW


	.export	sgcvekh
sgcvekh:	
	move	0(r0),Spush	; set mark
	move	r0,Spush	; 

	move	$MARKED,0(r0)	;

	move	r0,0(r1)
	move	$2(r0),r0
	.export	sgcv2
sgcv2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	sgcpop
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	sgcv2


/*
 *      Sampling garbage collection for basic vector nodes
 */

	.export	sgcdvek
sgcdvek:
	comp	r0,GCSTART
	jlth	sallmoved
	comp	r0,GCEND
	jgeh	sallmoved

	comp	r0,Hp
	jgeh	sgcbvi
	comp	r0,GCCUR
	jgeh	sallmoved

sgcbvi:
	move	Hp,Vpush	; save new address
	move	1(r0),DTMP
	add2	$2,DTMP		; DTMP is the counter.
	move	r0,ATMP		; ATMP points to the words in the vector.
sgcbv1:	move	0(ATMP),toH
	move	$1(ATMP),ATMP
	DECR(DTMP)
	TSTC(DTMP)
	jne	sgcbv1
	move	$MOVED,0(r0)
	move	Vpop,ATMP
	move	ATMP,1(r0)
	move    ATMP,0(r1)
	jump	sgcpop
 
/*
 *	Sampling garbage collection for a ZAP node (a zapped application)
 */

	.export	sgczap
sgczap:
	comp	r0,GCSTART
	jlth	sgczap1
	comp	r0,GCEND
	jgeh	sgczap1

	comp	r0,Hp
	jgeh	sgczapi
	comp	r0,GCCUR
	jgeh	sgczap1

sgczapi:
; r0 points to an overwritten application.
	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	2(r0),toH
	move	3(r0),toH	/* slop1:	DW */
#ifdef SLOP2
	move	4(r0),toH	/* slop2:	DW */
#endif
	move	$0,toH			; spare word to allow updating
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)

        move    _nodelist3, 2(r0)      ; link node to list
        move    r0, _nodelist3         ;
 
	move	1(ATMP),FDISP		; prepare to do as with a FUN node
	jump	sgcfunz

sgczap1:
	move	r0,0(r1)
	move	1(r0),FDISP
	jump	sgcfunz

/*
 *	Sample garbage collection of an input node.
 */

	.export	sgcinp
sgcinp:
        move    r0,Vpush
        move    r1,Vpush
        move    1(r0),DTMP
        move    Sp,_ep
        move    Hp,_hp
        CPUSHARG0(DTMP)
        CCALL(1,_setfileref)
        move    _ep,Sp
        move    _hp,Hp
        move    Vpop,r1
        move    Vpop,r0
	jump    sgc30

/*
 *	Sample garbage collection of a directory node.
 */

	.export	sgcind
sgcind:
        move    r0,Vpush
        move    r1,Vpush
        move    1(r0),DTMP
        move    Sp,_ep
        move    Hp,_hp
        CPUSHARG0(DTMP)
        CCALL(1,_setdirref)
        move    _ep,Sp
        move    _hp,Hp
        move    Vpop,r1
        move    Vpop,r0
        jump    sgc30

/*
 *	Sample garbage collection of a FUN node
 */

        .export sgcfun
sgcfun:
        move    1(r0),FDISP             ; p to infovector in FDISP
        move    VFpointer(FDISP),0(r1)
; check (& set) the link
sgcfunz:
        move    VFlink(FDISP),DTMP
        comp    DTMP,$0
        jne     sgcfun1
        move    _funlink,VFlink(FDISP)
        move    $ VFlink(FDISP),_funlink
; push the references
        move    VFnref(FDISP),DTMP      ; number of refs
        move    $ VFrefs(FDISP),r0       ; first entry
        jump    sgcv2
sgcfun1:
        jump    sgcpop


/*
 *	Sample garbage collection of a VAP node.
 */

	.export	sgcvap
sgcvap:
	move	1(r0),ATMP
	move	VFlink(ATMP),DTMP
	comp	DTMP,$0
	jne	sgcvapX1
	move	_funlink,VFlink(ATMP)
	move	$ VFlink(ATMP),_funlink
	move	VFnref(ATMP),DTMP
	move	$ VFrefs(ATMP),ATMP
sgcvap2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	sgcvapX1
	move	ATMP,Vpush
	move	0(ATMP),Vpush
	move	$1(ATMP),ATMP
	jump	sgcvap2

sgcvapX1:
	move	1(r0),ATMP
	move	VFarity(ATMP),DTMP

	comp	r0,GCSTART
	jlth	sgcvapXh
	comp	r0,GCEND
	jgeh	sgcvapXh

	comp	r0,Hp
	jgeh	sgcvapXi
	comp	r0,GCCUR
	jgeh	sallmoved
	
sgcvapXi:
#if 0
   /*****************************************************************/
   /*								    */
   /*			Start of Wadler GC stuff		    */
   /*								    */
   /*****************************************************************/

	comp    1(r0), $VCPs2_1         ; fst pattern selector?    	  -- DW
        jne     sgcvapWadler1           ; branch if not     		  -- DW

        move    2(r0), ATMP             ; get argument to fst selector    -- DW
        comp    0(ATMP), $CANON         ; is it canonical?		  -- DW
;   !!!     jgeh    sgcvapWadler1           ; branch if not            	  -- DW	
        jlt    sgcvapWadler1           ; branch if not            	  -- DW	

        move    $VC_id, 1(r0)           ; replace fst (x,y) with id x     -- DW 
        move    1(ATMP), 2(r0)          ;

	jump	sgcvapWadler2		;

sgcvapWadler1:
        comp    1(r0), $VCPs2_2         ; snd pattern selector?    	  -- DW
        jne     sgcvapWadler2           ; branch if not     	          -- DW
 
        move    2(r0), ATMP             ; get argument to fst selector
        comp    0(ATMP), $CANON         ; is it canonical?
;   !!!     jgeh    sgcvapWadler2           ; branch if not                   -- DW
        jlt    sgcvapWadler2           ; branch if not                   -- DW
        
        move    $VC_id, 1(r0)           ; replace snd (x,y) with id y     -- DW 
        move    2(ATMP), 2(r0)          ;

	jump	sgcvapWadler2		;

#endif
sgcvapWadler2:

	move	Hp,ATMP
	move	0(r0),toH
	move	1(r0),toH
	move	$MOVED,0(r0)
	move	ATMP,1(r0)
	move	ATMP,0(r1)
	move    r0, ATMP                /* DW */
	move	$2(r0),r0

	.export	sgcvX1
sgcvX1:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	sgcvX1end
	move	Hp,Vpush
	move	$1(Hp),Hp
	move	0(r0),Vpush
	move	$1(r0),r0
	jump	sgcvX1

sgcvX1end:
	move    0(r0),toH		; slop 1:	DW
#ifdef SLOP2
	move    1(r0),toH		; slop 2:	DW
#endif

	move    _nodelistvap, 2(ATMP)   ; link node to list
        move    ATMP, _nodelistvap      ;

	jump 	sgcpop

	.export	sgcvapXh
sgcvapXh:	
	move    0(r0),Spush     ; set mark
        move    r0,Spush        ;

	move	$MARKED,0(r0)	;

	move	r0,0(r1)
	move	$2(r0),r0

	.export	sgcvX2
sgcvX2:
	DECR(DTMP)
	TSTC(DTMP)
	jlt	sgcpop
	move	r0,Vpush
	move	0(r0),Vpush
	move	$1(r0),r0
	jump    sgcvX2


/*
 *	Sample garbage collection of a marked node
 */

        .export sgcmkd
sgcmkd:
        move    r0,0(r1)
        jump    sgcpop

/*
 *	Sample garbage collection of a gcm (?!) node
 */

        .export sgcm
sgcm:
        move    Vpop,0(r1)
        jump    sgcpop

/* 
 *      Sample garbage collection of a moved node
 */

        .export sgcmvd
sgcmvd:
        move    1(r0),0(r1)
        jump    sgcpop

/*
 *	Sample garbage collection of a gcret node
 */

	.export	sgcret
; reset all funlinks
sgcret:
	move	_funlink,r0
	jump	sgcret0
sgcret1:
	move	r0,r1
	move	0(r0),r0
	move	$0,0(r1)
  	add2	$1,_nfunlink
sgcret0:
	comp	r0,$-1
	jne	sgcret1

; reset all marked nodes
sgcret3:
	comp	Sp,gcSp
	jeq	sgcret2
	move	Spop,r0
	move	Spop,0(r0)
	jump	sgcret3
sgcret2:
	move	Sp,_ep
	move	Hp,_hp
	CRET

; INDIR node, just short-circuit it
	.export	sgcindir
sgcindir:
	move	1(r0),r0
	jump	sgcd

	.pragma	GC_OFF


/*************************************************************************
 *									 *
 *			End of new sampling scheme			 *
 *									 *
 *************************************************************************/	

#define MAGIC 	.word	HEAPMAGIC

	.data
	.malign
	.export	SYSTEMChar_Char_LABEL
SYSTEMChar_Char_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMChar_LABEL
	.word $Char_LABEL
	.word $Char_LABEL

	.export	SYSTEMCmp_Cons_LABEL
SYSTEMCmp_Cons_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMCmp_LABEL
	.word $Cons_LABEL
	.word $List_LABEL

	.export	SYSTEMCmp_String_LABEL
SYSTEMCmp_String_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMCmp_LABEL
	.word $String_LABEL
	.word $List_LABEL

	.export	SYSTEMHole_Hole_LABEL
SYSTEMHole_Hole_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMHole_LABEL
	.word $Hole_LABEL
	.word $Hole_LABEL

	.export	SYSTEMInt_Int_LABEL
SYSTEMInt_Int_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMInt_LABEL
	.word $Int_LABEL
	.word $Int_LABEL

	.export	SYSTEM_SYSTEM_LABEL
SYSTEM_SYSTEM_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEM_LABEL
	.word $SYSTEM_LABEL
	.word $SYSTEM_LABEL

	.export _CSYSTEM_SYSTEM_LABEL
_CSYSTEM_SYSTEM_LABEL:
	MAGIC
	.word $Cruntime_LABEL
	.word $Cruntime_LABEL
	.word $CSYSTEM_LABEL
	.word $CSYSTEM_LABEL
	.word $CSYSTEM_LABEL

	.export SYSTEM_UNKNOWN_LABEL
SYSTEM_UNKNOWN_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEM_LABEL
	.word $UNKNOWN_LABEL
	.word $UNKNOWN_LABEL

	.export	SYSTEMInput_Input_LABEL
SYSTEMInput_Input_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMInput_LABEL
	.word $Input_LABEL
	.word $Input_LABEL

	.export	SYSTEMInput_Nil_LABEL
SYSTEMInput_Nil_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMInput_LABEL
	.word $Nil_LABEL
	.word $List_LABEL

	.export	SYSTEMInput_Cons_LABEL
SYSTEMInput_Cons_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMInput_LABEL
	.word $Cons_LABEL
	.word $List_LABEL

	.export	SYSTEMIndir_Indir_LABEL
SYSTEMIndir_Indir_LABEL:
	MAGIC
	.word $runtime_LABEL
        .word $runtime_LABEL
        .word $SYSTEMIndir_LABEL
        .word $Indir_LABEL
        .word $Indir_LABEL

	.export	SYSTEMIndir_Nil_LABEL
SYSTEMIndir_Nil_LABEL:
	MAGIC
	.word $runtime_LABEL
        .word $runtime_LABEL
        .word $SYSTEMIndir_LABEL
        .word $Nil_LABEL
	.word $List_LABEL
 
	.export	SYSTEMIndir_Cons_LABEL
SYSTEMIndir_Cons_LABEL:
	MAGIC
	.word $runtime_LABEL
        .word $runtime_LABEL
        .word $SYSTEMIndir_LABEL
        .word $Cons_LABEL
	.word $List_LABEL

	.export	SYSTEMString_String_LABEL
SYSTEMString_String_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMString_LABEL
	.word $String_LABEL
	.word $String_LABEL

	.export	SYSTEMString_Nil_LABEL
SYSTEMString_Nil_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMString_LABEL
	.word $Nil_LABEL
	.word $List_LABEL

	.export	SYSTEMString_Cons_LABEL
SYSTEMString_Cons_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMString_LABEL
	.word $Cons_LABEL
	.word $List_LABEL

	.export	SYSTEMNIL_NIL_LABEL
SYSTEMNIL_NIL_LABEL:
	MAGIC
	.word $runtime_LABEL
	.word $runtime_LABEL
	.word $SYSTEMNIL_LABEL
	.word $NIL_LABEL
	.word $List_LABEL

id_LABEL: 	.string "id"                      	/* DW */

		.export Char_LABEL
Char_LABEL: 	.string "Char"                         	/* DW */

		.export Float_LABEL
Float_LABEL:  	.string "Float"                        	/* DW */

		.export Hole_LABEL
Hole_LABEL:  	.string "Hole"                        	/* DW */

		.export Int_LABEL
Int_LABEL:  	.string "Int"                          	/* DW */

		.export String_LABEL
String_LABEL: 	.string "String"			/* DW */

Nil_LABEL: 	.string "[]"				/* DW */

Cons_LABEL: 	.string ":"				/* DW */

NIL_LABEL:	.string "NIL"				/* DW */

Input_LABEL:   	.string "Input"                		/* DW */

Indir_LABEL:   	.string "Indir"                		/* DW */

List_LABEL:	.string "List"				/* DW */

runtime_LABEL: 	.string "runtime"                      	/* DW */

Cruntime_LABEL: 	.string "runtime(C)"           	/* DW */

		.export SYSTEM_LABEL
SYSTEM_LABEL: 	.string "SYSTEM"                       	/* DW */

CSYSTEM_LABEL: 	.string "SYSTEM(C)"                    	/* DW */

SYSTEMChar_LABEL: 	.string "SYSTEM(Char)"        	/* DW */

SYSTEMCmp_LABEL: 	.string "SYSTEM(Cmp)"        	/* DW */

SYSTEMHole_LABEL: 	.string "SYSTEM(Hole)"        	/* DW */

SYSTEMId_LABEL:		.string "SYSTEM(Id)"		/* DW */

SYSTEMInput_LABEL: 	.string "SYSTEM(Input)"        	/* DW */

SYSTEMIndir_LABEL: 	.string "SYSTEM(Inputdir)"     	/* DW */

SYSTEMInt_LABEL: 	.string "SYSTEM(Int)"        	/* DW */

SYSTEMString_LABEL: 	.string "SYSTEM(String)"       	/* DW */

SYSTEMNIL_LABEL: 	.string "SYSTEM(NIL)"       	/* DW */

UNKNOWN_LABEL:		.string "SYSTEM(UNKNOWN)"

#else
	.export	sgc20
sgc20:
	.export	sgc30
sgc30:
	.export	sgc11
sgc11:
	.export	sgc12
sgc12:
	.export	sgc21
sgc21:
	.export	sgcvek
sgcvek:
	.export	sgczap
sgczap:
	.export	sgcfun
sgcfun:
	.export	sgcvap
sgcvap:
	.export	sgcmkd
sgcmkd:
	.export	sgcmvd
sgcmvd:
	.export	sgcret
sgcret:
	.export	sgcinp
sgcinp:
	.export	sgcind
sgcind:
	.export	sgcdvek
sgcdvek:
	.export	sgcindir
sgcindir:
	return
#endif
