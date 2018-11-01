#include "../runtime/machdep.M"
#ifdef m_mips
	.export	getgp
getgp:
	.asm	"	move	$2,$gp"
	.asm	"	j	$31"
#endif
