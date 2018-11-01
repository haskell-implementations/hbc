module
#include "mcodedef_t.t"
#include "machine.tt"
export gareg, gdreg, gfreg, regof, rused;
rec
    rused r (reg p.S)       = r = p | rused r S
 || rused r (regind p _.S)  = r = p | rused r S
 || rused r (regrel p _.S)  = r = p | rused r S
 || rused r (popS.S)        = false
 || rused r (popV.S)        = false
 || rused r []		    = false
 || rused r (a.S)           = rused r S
 
and greg (r.regs) V S = if ~(rused r S | rused r V) then
				r
			else
				greg regs V S

and gareg V S = greg Aregs V S
and gdreg V S = greg Dregs V S
and gfreg V S = greg Fregs V S
and regof (reg r) = r
end
