module -- ppeepopt
-- improvements after the code reorganisation
#include "../Gcode/Gcodedef_t.t"
export ppeepopt;
rec
    ppeepopt [] = []
||  ppeepopt (BCONSTR _.GET _.gs) = ppeepopt gs
||  ppeepopt (BCONSTR _.GETTAG.gs) = ppeepopt gs
||  ppeepopt (POP n.POP m.gs) = ppeepopt (POP (n+m).gs)
||  ppeepopt (POPV n.POPV m.gs) = ppeepopt (POPV (n+m).gs)
||  ppeepopt (POP 0.gs) = ppeepopt gs
||  ppeepopt (POPV 0.gs) = ppeepopt gs
||  ppeepopt (PUSH n1.EVAL ns.POP 1.PUSH n2.gs) & (n1=n2) = PUSH n1.EVAL ns.ppeepopt gs
||  ppeepopt (PUSH 0.MOVE k.POP 1.gs) = MOVE (k-1).ppeepopt gs
||  ppeepopt (g.gs) = g . ppeepopt gs
end
