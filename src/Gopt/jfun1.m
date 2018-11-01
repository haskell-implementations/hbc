module
#include "../Gcode/Gcodedef_t.t"
#include "jfunutil.t"

export nlab, empty, bend, bhead, bblock, bopt, extractb, addlabel;
rec
    nj (JMP _) = false
||  nj (JFUN _) = false
||  nj (JGLOBAL _ _) = false
||  nj (JMETHOD _ _) = false
||  nj (RET) = false
||  nj (UNWIND) = false
||  nj (CASE _ _ _) = false
||  nj _ = true
and nlab (LABEL (Label _)) = false
||  nlab _ = true
and empty (_, _, []) = true
||  empty _ = false
and bend l [] = (l, [])
||  bend l (LABEL (Label _).g) = bend l g
||  bend l g =
	let (c, r) = take nj g in
	case r in
	   JMP (Label n)._ : (n, c)
	|| i._ : (-1, c@[i])
	|| [] : (l, c)
	end
and bhead (LABEL Notalabel._) = 0   -- marks start of function
||  bhead (LABEL (Label n)._) = n
||  bhead _ = -1
and bblock (g.gs) =
	let (b, r) = take nlab gs in
	let c = g.b in
	((bhead c, bhead r, c), r)
and bopt (e, x, c) = let (nx, nc) = bend x c in (e, nx, nc)
and extractb x (b.bs) = let (e,_,_)=b in
	if x = e then b
	else extractb x bs
||  extractb x [] = fail ("extractb on "@itos x)
and addlabel (0, _, c) = c
||  addlabel (e, _, c) = LABEL (Label e).c
end
