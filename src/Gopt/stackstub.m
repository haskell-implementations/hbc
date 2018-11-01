module --stackstub
#include "../Gcode/Gcodedef_t.t"
#include "../misc/flags.t"
#include "../Gcode/Gprint.t"
#include "jfun1.t"

export stub;

rec stub :: List Gcode -> List Gcode
and stub gs = if StackStubbing then dostub gs else gs
and dostub gs = 
    let bs = choplist ablock gs in
    let gs' = concmap (\ (_,_,gs).gs) bs
    in  trace ("Gcode:\n"@prgs gs @ "----\nBlocks:\n" @ unlines (map prbs bs) @ "----\nStubbed\n" @ prgs gs')
        gs

and ablock gs = 
    case take nj gs in
       (gs', [])     : ((entry (hd gs'), exits (last gs'), gs'),     [])
    || (gs', g.gs'') : ((entry (hd gs'), exits g,          gs'@[g]), gs'')
    end

and entry (LABEL (Label n)) = n
||  entry _ = -1

and exits (JMP (Label n)) = [n]
||  exits (JFALSE (Label n)) = [n; -1]
||  exits (CASE _ cs l) = 
    let getl (Label n) = [n]
    ||  getl _ = []
    in  mkset (getl l @ concmap (getl o thd) cs)
||  exits _ = []

and nj (JMP _) = false
||  nj (JFUN _) = false
||  nj (JGLOBAL _ _) = false
||  nj (JMETHOD _ _) = false
||  nj (RET) = false
||  nj (UNWIND) = false
||  nj (CASE _ _ _) = false
||  nj (JFALSE _) = false
||  nj _ = true


and prgs gs = Gprint gs
and prbs (n, es, gs) = "BLK "@itos n@" "@show_list show_int es@"\n"@prgs gs
and unlines xs = concmap (\x.x@"\n") xs
and thd (_,_,x) = x
end
