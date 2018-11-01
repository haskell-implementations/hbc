module -- make the program strict
-- Semistrict still allows letrec for data and lazy input because it evals.
-- Strict is completely strict and cannot be mixed with lazy code.
#include "../misc/flags.t"
#include "../expr/einfo_t.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"
#include "../misc/misc.t"
#include "../expr/id.t"
#include "../expr/constrfun.t"
#include "../transform/hexpr.t"
export mkstrict;
rec
    ms (e as Einfo strict _) = e
||  ms e = Einfo strict e
and
    mks (Ecase e pl dp) =
	Einfo strict
	      (Ecase (mks e)
		    (map (\(c,il,e).(c, il, mks e)) pl) (mks dp))
 || mks (Elet r dl e) =
	(Elet r (map f dl) (mks e)
	where rec f (i, Elaml is e) =
		    (i, Elaml is (mks e))
	       || f (i, e) = if r & Semistrict then (i, e) else (i, mks e))
 || mks (Econstr c _) & (isstring c) = expandstring (cname c)
 || mks (Econstr c el) = Econstr c (map mks el)
 || mks (Efailmatch n) = Efailmatch n
 || mks (e as Ecfunction _ _) = e
 || mks (Eidapl i []) =
	if Semistrict then Eidapl i []
	else Einfo noeval (Eidapl i [])
 || mks (Eidapl i el) =
	if arity_of_id i <= length el then
		Einfo strict (Eidapl i (map mks el))
	else
		Eidapl i (map mks el)
 || mks (Elaml il e) = Elaml il (mks e)
 || mks (e as Einfo noeval _) = e
 || mks (Einfo strict e) = ms (mks e)
 || mks (Einfo i e) = Einfo i (mks e)

and expandstring "" = Econstr hcnil []
||  expandstring (c.cs) = Econstr hccons [Emkchar c; expandstring cs]

and mkstrict (e as Emodule i es bss) =
    if Strict | Semistrict then
	Emodule i es (map (mapsnd mks) bss)
    else
	e
end
