module -- peepopt
-- do some small improvements
#include "../Gcode/Gcodedef_t.t"
export peepopt;
rec
    peepopt [] = []
||  peepopt ((g1 as CONSTR _ _ _ _).(g2 as UPDATE _ _).(g3 as POP _).UNWIND.gs) =
	g1.g2.g3.RET.peepopt gs
||  peepopt ((g1 as CONSTBLOCK _).(g2 as UPDATE _ _).(g3 as POP _).UNWIND.gs) =
	g1.g2.g3.RET.peepopt gs
||  peepopt ((g1 as BCONSTR _).(g2 as UPDATE _ _).(g3 as POP _).UNWIND.gs) =
	g1.g2.g3.RET.peepopt gs
||  peepopt (BCONSTR _.GET _.gs) = peepopt gs
||  peepopt (BCONSTR _.GETTAG.gs) = peepopt gs
||  peepopt (BASICOP _ _ ORD.gs) = peepopt gs
||  peepopt (BASICOP _ _ CHR.gs) = peepopt gs
||  peepopt (BASICOP _ _ TAG.gs) = peepopt gs
||  peepopt (PUSH n.MOVE n1.gs) & (n+1 = n1) = peepopt gs
||  peepopt ((g1 as ANNOT "VECTORDEF").(AMODE _).g2.g3.(AMODE _).g4.gs) = g1.g2.g3.g4.peepopt gs	-- must skip next optim if this is as vectordef
||  peepopt (CASE 2 [(0,0,l1); (1,0,l2)] Notalabel.gs) =
	JFALSE l1.JMP l2.peepopt gs
||  peepopt (PUSH _.GETTAG.CASE 1 [(0,_,l1)] _.gs) = JMP l1.peepopt gs
||  peepopt (POP n.POP m.gs) = POP (n+m).peepopt gs
||  peepopt (POPV n.POPV m.gs) = POPV (n+m).peepopt gs
||  peepopt (SPLIT bs _ _.gs) & (~ Or bs) = peepopt gs
-- Remove all AMODE since thay are not used now.
-- !! Don't change this without fixing mcode/bigcmp !!
||  peepopt (AMODE _.gs) = peepopt gs
||  peepopt (g.gs) = g . peepopt gs
and notGET (GET _) = false
||  notGET _ = true
end
