#include "machdep.M"
; Code for EVAL; UPDATE 1; UNWIND
; This code occurs when a variable is returned and is very common.
; R0 has a pointer to the node to return.
	.export	evalupdunw
	.malign
evalupdunw:
	LBASE
	move	0(r0),INDREG
	call	oeval(INDREG)
	move	Spop,ATMP
	move	$INDIR,0(ATMP)
	move	r0,1(ATMP)
	move	0(r0),INDREG
	jump	ounwind(INDREG)

