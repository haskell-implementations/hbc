module
#include "../expr/constr_t.t"
#include "../expr/tinfo.t"
#include "../expr/ttype.t"
#include "Expr_t.t"
export Emkint, Emkbool, Emkchar;
rec
    Emk s t it n = Econstr (Cconstr s t it n (false,[],[]) []) []
and Emkint i = Emk (itos i) Tint ITint i
and Emkbool b = Emk (if b then "true" else "false") Tbool ITbool (if b then 1 else 0)
and Emkchar c = Emk ['\''; c; '\''] Tchar ITchar (ord c)
end
