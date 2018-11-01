module -- gout
#include "../Gcode/Gcodedef_t.t"
#include "peepopt.t"
#include "ppeepopt.t"
#include "jumpopt.t"
#include "stackstub.t"
export gopt;
rec
      gopt' = ppeepopt o jopt o peepopt
and   gopt :: Gcodes -> Gcodes
and   gopt l = map goptp l
and   goptp (n, gs, None) = (n, stub (gopt' gs), None)
||    goptp (n, gs, Some gs') = (n, gopt' gs, Some (gopt' gs'))
end
