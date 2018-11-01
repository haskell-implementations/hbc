module
#include "../misc/misc.t"
#include "../expr/id_t.t"
#include "../expr/types_t.t"
#include "../expr/impexp_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/ttype.t"
#include "cast.t"
#include "cons.t"
#include "cno.t"
#include "id.h"
export Cexpr, Tconst, show_Cexpr;
rec type Cexpr = 
	Lam Id Cexpr + 
	Var Id + 
	Ap Cexpr Cexpr + 
	Comb String Univ + 
	Constant Tconst + 
	Fail +
        Konstr Int Int (List Bool)		-- con #, number of components, strict info
and type Tconst = CInt Int + CChar Char + CString String + CSFloat String + CInteger String + CDFloat String
and show_Cexpr (Lam i e) = "(\\"@pprid i@"."@show_Cexpr e@")"
||  show_Cexpr (Var i) = pprid i
||  show_Cexpr (Ap e1 e2) = "("@show_Cexpr e1@" "@show_Cexpr e2@")"
||  show_Cexpr (Comb i _) = i
||  show_Cexpr (Konstr i k _) = "CON"@itos i@"_"@itos k
||  show_Cexpr (Constant (CInt i)) = itos i
||  show_Cexpr (Constant (CChar c)) = ['\''; c]
||  show_Cexpr (Constant (CString s)) = '"'.s
||  show_Cexpr (Constant (CDFloat s)) = s
||  show_Cexpr (Constant (CSFloat s)) = s
||  show_Cexpr (Constant (CInteger s)) = s
||  show_Cexpr (Fail) = "Fail"
end
