module 
#include "../expr/id.t"
#include "mcodedef_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "Wuse.t"
#include "mutil.t"
#include "mutil1.t"
#include "mregs.t"
#include "mstrlbl.t"
#include "mstrid.t"
#include "handmade.mcode.t"
#include "mconstr.t"
export mvap;
rec mvap svf t G m i V S =
	let is = mstrid i in
        let cstr = idtopstr i in
	let (c, d, v, s, k) = mcvek svf (cstr, cstr) t G m mvaptag (idlit('V'.is)) V S in
	(c, is.d, v, s, k)
end
