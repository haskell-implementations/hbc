module -- goptgm
-- Do some small improvements for the real G-machine
#include "../Gcode/Gcodedef_t.t"
export goptgm;
#if 0
rec
    goptgm [] = []
||  goptgm (CONSTR _ g a b.UPDATE _ n.POP m.RET.gs) & (n=m+1) = CUPDRET g a b m.goptgm gs
||  goptgm (BCONSTR g.UPDATE _ n.POP m.RET.gs) & (n=m+1) = BUPDRET g m.goptgm gs
||  goptgm (AMODE _.gs) = goptgm gs
||  goptgm (POP n.POP m.gs) = POP (n+m).goptgm gs
||  goptgm (g.gs) = g . goptgm gs
#else
    goptgm gs = gs
#endif
end
