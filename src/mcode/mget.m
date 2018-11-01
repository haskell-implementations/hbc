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
export mget, mgetf, mgettag, mgetmethod;
rec
mget svf t g G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newV (regind r 1) u V c
	in ( Mcom"GET". cr @ c1, d, uv, Wuse.us, k)
and
-- float!!!
mgetf dbl svf t g G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S
	and    (c1, V1) = newV (regind r 1) u V c
	in ( Mcom"GETF". cr @ c1, d, uv, Wuse.us, k)
and
mgettag svf t g G V aS =
	let (a.S) = aS in
	let rec (c, d, u.uv, us, k) = M svf t g G V1 S2
        and    (c2, V2, S2) = freereg (regof tagreg) V S
	and    (c1, V1) = newV tagreg u V2 c
	in ( 	c2 @
	        Mcom"GETTAG".
		intoreg0 a (
		Mmove (regind 0 0) indreg.
		Mcalltag ogettag (regof indreg).c1), d, uv, Wuse.us, k)
and
mgetmethod svf metno t g G V aS =
	let (a.S) = aS in
	let (cr, r) = intoareg a V S in
	let rec (c, d, uv, u.us, k) = M svf t g G V S1
	and    (c1, S1) = newS (regind r (metno+2)) u S c
	in ( Mcom "GETMETHOD".cr @ c1, d, uv, Wuse.us, k)

-- Make sure reg r is not in use.
and freereg r V S =
    if rused r V then
	let nr = gdreg V S in
	([Mmove (reg r) (reg nr)], upds r nr V, S)
    else if rused r S then
	let nr = gareg V S in
	([Mmove (reg r) (reg nr)], V, upds r nr S)
    else
	([], V, S)

and upds r nr (reg p.S)      & (r = p) = reg nr .      upds r nr S
 || upds r nr (regind p x.S) & (r = p) = regind nr x . upds r nr S
 || upds r nr (regrel p x.S) & (r = p) = regrel nr x . upds r nr S
 || upds r nr (S as popS._)            = S
 || upds r nr (S as popV._)            = S
 || upds r nr []	               = []
 || upds r nr (a.S)                    = a . upds r nr S
end
