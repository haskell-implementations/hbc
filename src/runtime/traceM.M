#include "machdep.M"

/*
** Tracing
*/
	.export	do_enter
do_enter:
	LBASE
#ifdef retr
	move	retr,DTMP
#else
	move	0(Vp),DTMP
#endif
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(DTMP)
	CCALL(1,_cdo_enter)
	move	_ep,Sp
	move	_hp,Hp
	return

	.export	do_return
do_return:
	LBASE
	move	r0,Spush		; save r0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_cdo_return)
	move	_ep,Sp
	move	_hp,Hp
	move	Spop,r0			; restore r0
	return

	.export	do_unwind
do_unwind:
	LBASE
	move	r0,Spush		; save r0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_cdo_unwind)
	move	_ep,Sp
	move	_hp,Hp
	move	Spop,r0			; restore r0
	return

	.export do_evalupdunw
do_evalupdunw:
	LBASE
	move	r0,Spush		; save r0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_cdo_evalupdunw)
	move	_ep,Sp
	move	_hp,Hp
	move	Spop,r0			; restore r0
	return

	.export	do_jfun
do_jfun:
	LBASE
	move	r0,Spush		; save r0
	move	Sp,_ep
	move	Hp,_hp
	CPUSHARG0(r0)
	CCALL(1,_cdo_jfun)
	move	_ep,Sp
	move	_hp,Hp
	move	Spop,r0			; restore r0
	return

	.export	do_jglobal
do_jglobal:
	LBASE
	move	Sp,_ep
	move	Hp,_hp
	CCALL(0,_cdo_jglobal)
	move	_ep,Sp
	move	_hp,Hp
	return

