module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "mmemcheck.t"
#include "machine.tt"
export mbigop, mbigjrel, mstrop, mstrjrel;

-- Bigcmp is a beast! 

rec
local use (regind r _) n = r = n
   || use (regrel r _) n = r = n
   || use       a       n = false
in
   intor01 (reg 0) (reg 1)  = ([], false)
|| intor01 (reg 1) (reg 0)  = ([], true)
|| intor01 (reg 0)    b     = ([Mmove b (reg 1)], false)
|| intor01 (reg 1)    b     = ([Mmove b (reg 0)], true)
|| intor01    a    (reg 1)  = ([Mmove a (reg 0)], false)
|| intor01    a    (reg 0)  = ([Mmove a (reg 1)], true)
|| intor01 a b & (use a 0 & use b 0) = ([Mmove b (reg 1);Mmove a(reg 0)],false)
|| intor01 a b & (use a 1 & use b 0) = ([Mmove a (reg 1);Mmove b(reg 0)], true)
|| intor01 a b = ([Mmove a (reg 0); Mmove b (reg 1)], false)
end
and bigcmp abS =
	let (a.b.S) = abS in
	let (c, flip) = intor01 a b
	in
		c @
		[Mcall "bigcmp"; 
		 if flip then Mcompare (const 0) bigeqreg
		         else Mcompare bigeqreg (const 0)
		]

and mbigjrel svf t G op l V S =
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	and ls = mstrlbl l
	in (	Mcom ("BIG"@strop op @ "; JFALSE "@ls).
		bigcmp S @
		Mjcond ((mopcc o ccneg o ccrev o ccop) op) ls.c,
			d, allWpush,Wuse.Wuse.allWpush,0)

and mbigop svf t G op V S =
	let rec (c, d, u.uv, us, k) = M svf t 0 G V1 allpopS
	and    (ar, V1) = 
		case u in
		  Wpush: pushV, allpopV
		||Wuse : let r = reg (gdreg V S) in
				(r, r.allpopV)
		end
	in (	Mcom ("BIG"@strop op).
		bigcmp S @
		Mboolcc ((mopcc o ccrev o ccop) op) ar.c,
			d, allWpush,Wuse.Wuse.allWpush,0)
and strcmp abS =
	let (a.b.S) = abS in
	let (c, flip) = intor01 a b
	in
		c @
		[Mcall "lstrcmp"; 
		 if flip then Mcompare (const 0) bigeqreg
		         else Mcompare bigeqreg (const 0)
		]

and mstrjrel svf t G op l V S =
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	and ls = mstrlbl l
	in (	Mcom ("STR"@strop op @ "; JFALSE "@ls).
		strcmp S @
		Mjcond ((mopcc o ccneg o ccrev o ccop) op) ls.c,
			d, allWpush,Wuse.Wuse.allWpush,0)

and mstrop svf t G op V S =
	let rec (c, d, u.uv, us, k) = M svf t 0 G V1 allpopS
	and    (ar, V1) = 
		case u in
		  Wpush: pushV, allpopV
		||Wuse : let r = reg (gdreg V S) in
				(r, r.allpopV)
		end
	in (	Mcom ("STR"@strop op).
		strcmp S @
		Mboolcc ((mopcc o ccrev o ccop) op) ar.c,
			d, allWpush,Wuse.Wuse.allWpush,0)
end
