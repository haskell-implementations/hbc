#include "machdep.M"

/*
** No tracing
*/
	.export	do_enter
do_enter:
	return

	.export	do_return
do_return:
	return

	.export	do_unwind
do_unwind:
	return

	.export do_evalupdunw
do_evalupdunw:
	return

	.export	do_jfun
do_jfun:
	return

	.export	do_jglobal
do_jglobal:
	return
