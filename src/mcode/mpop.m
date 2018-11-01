module
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "mprint.t"
#include "handmade.mcode.t"
export mpop, mpopv;
mpop svf t g G n V S = 
	let (c, d, uv, us, k) = M svf t g G V (tail n S) in
   	let rec mp 0 S c = c
	     || mp n (popS.S) c = Mmove (Srel n) Sp.c
	     || mp n (_.S) c = mp (n-1) S c 
	     || mp _ _ _ = fail "mpop,mp"
        in ( Mcom("POP "@itos n@" "@prstk 6 S).mp n S c,
	   	d, uv, prepWuse n us, k)
and
mpopv svf t g G n V S = 
	let (c, d, uv, us, k) = M svf t g G (tail n V) S in
   	let rec mp 0 V c = c
	     || mp n (popV.V) c = Mmove (Vrel n) Vp.c
	     || mp n (_.V) c = mp (n-1) V c 
	     || mp _ _ _ = fail "mpopv,mp"
        in ( Mcom("POPV "@itos n).mp n V c,
	   	d, prepWuse n uv, us, k)
end
