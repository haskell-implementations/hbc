module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "mtrans.t"
#include "misc.t"
export mklbl, mkglob, mkconst;
rec
    mklbl lbl cx = emitlbl lbl "=" (cx.".*")
and
    mkglob a = emit "VWD" ("40/0,24/" @ (fixclbl a))
and
    mkconst i =
	if i >= 0 then
	     emit "CON" ("D'" @ (itos i))
	else
	     emit "CON" ("-D'" @ (itos (-i)))
end
