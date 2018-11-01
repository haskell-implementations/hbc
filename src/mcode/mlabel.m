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
export mlabel;
mlabel svf t G l =
	let (c, d, uv, us, k) = mmemcheck svf t G allpopV allpopS
	in (Mcom "LABEL".Mlabel(mstrlbl l).c, d, allWpush, allWpush, 0)
end
