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
#include "machine.tt"
#include "../misc/flags.t"
export mupdate, mupdateindir;
rec mupdate svf t g G tg m V aS =
        let l = if small tg then 2 else 3 in
	let (a.S) = aS in
	let (c, d, uv, us, k) = M svf t g G V S in
	let (ac, ar) = intoareg a V (a.S) in
	let (bc, br) = intoareg (indS (m-1) S) V (reg ar.a.S)
	in (	Mcom ("UPDATE "@itos m).
		ac @ bc @
		Mmove (regind ar 0) (regind br 0).
		Mmove (regind ar 1) (regind br 1).
	        (if l = 2 then [] else [Mmove (regind ar 2) (regind br 2)]) @
		(if ProfileHeap then
		    [ Mmove (regind ar l) (regind br l)
--		    ; Mmove (regind ar (l+1)) (regind br (l+1))
		    ]
		 else
		    []
		) @
		c,
			d, uv, Wuse.us, k)
and small Gbint = true
||  small Gbchar = true
||  small Gbsfloat = sfloatsize = 1
||  small Gbdfloat = dfloatsize = 1
||  small _ = false
and mupdateindir svf t g G m V aS =
	let ts = tmplbl t in
	let (a.S) = aS in
	let (c, d, uv, us, k) = M svf (t+1) g G V S in
	let (ac, ar) = intoareg a V (a.S) in
	let (bc, br) = intoareg (indS (m-1) S) V (reg ar.a.S)
	in (	Mcom ("UPDATEINDIR "@itos m).
		ac @ bc @
		Mmove mindirtag (regind br 0).
		Mmove (reg ar)  (regind br 1).
#if 1
	        Mmove (glob "_tp") (reg ar).
	        Mcompare (reg ar) (const 0).
	        Mjcond eq ts.
	        Mmove (regrel ar (-1)) (reg ar).
	        Mmove (reg br) (regind ar 0).
                Mmove (reg ar) (glob "_tp").
	        Mcompare (reg ar) (glob "_etp").
	        Mjcond gt ts.
                Mmove (const 0) (glob "_ehp").
                Mmove (const 0) (glob "_tp").
	        Mlabel ts.
#endif
		c,
			d, uv, Wuse.us, k)
end
