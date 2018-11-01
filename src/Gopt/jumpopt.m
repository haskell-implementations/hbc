module
#include "../Gcode/Gcodedef_t.t"
#include "jfun.t"

export jopt;
    jopt gs = case jfun (LABEL Notalabel . gs) in 
		 LABEL Notalabel . gs' : gs'
              || _ : fail "jopt: bad return from jfun"
              end
end
