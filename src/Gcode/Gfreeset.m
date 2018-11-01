module
#include "../misc/setofid.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"

export Gfreeset;
rec
Gfreeset e = f e
where rec
	fl el = reduce (\e.\p.Iu (f e) p) [] el

and	f (Ecase e pl de) = Iu (f e) 
				    (Iu (fl (map (\(_,_,e).e) pl)) 
				        (f de))
 ||	f (Elet _ d e) = Iu (fl (map(\(_,e).e) d)) (f e)
 ||	f (Eidapl i el) = Iu [i] (fl el)
 ||	f (Econstr i el) = fl el
 ||	f (Einfo _ e) = f e
 ||     f (Efailmatch _) = []
 ||     f (Ecfunction _ _) = []
 ||	f e = fail ("Gfreeset "@pr e)
 
end
