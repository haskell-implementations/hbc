#include "machdep.M"
; Code for EVAL; UPDATE 1; UNWIND
; This code occurs when a variable is returned and is very common.
; R0 has a pointer to the node to return.
	.export	evalupdunw
	.malign
evalupdunw:
	LBASE
	move	0(r0),INDREG            ; Fetch tag
	comp	INDREG,$CANONDATA       ; Is it a constructor? (true if >= CANONDATA)
	jge	evdata			; Evaluated after unevaluated
	comp	INDREG,$CANON           ; A function or a CAP? (true if >= CANON and < CANONDATA)
	jge	ev1			; Evaluated after unevaluated
	call	oeval(INDREG)           ; All other nodes can be evaluated!
ev1:
	move	r0,ATMP
	move	Spop,r0
	move	0(ATMP),INDREG          ; Get tag of result
	move	INDREG,0(r0)
	move	1(ATMP),1(r0)
	move	2(ATMP),2(r0)
#ifdef HPROFILE
	move    3(ATMP),3(r0)
#ifdef SLOP2
        move    4(ATMP),4(r0)
#endif
#endif
	jump	ounwind(INDREG)

evdata:
	move	r0,ATMP
	move	Spop,r0
	move	0(ATMP),0(r0)
	move	1(ATMP),1(r0)
	move	2(ATMP),2(r0)
#ifdef HPROFILE
	move    3(ATMP),3(r0)
#ifdef SLOP2
        move    4(ATMP),4(r0)
#endif
#endif
	move	Vpop,Sp
	return
