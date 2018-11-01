module -- casetr
#include "../ExprE/Expr_t.t"
#include "../misc/misc.t"
export casetr;
rec
    casetr (Ecase (Ecase e1 cl1 d1) cl2 d2) = 
	let f x = Ecase x cl2 d2 in
	Ecase e1 (mapthd (pushcase f) cl1) (pushcase f d1)
#if 0
||  casetr (Ecase e1 cl e2) = Ecase (casetr e1) (mapthd casetr cl) (casetr e2)
||  casetr (Elet r dl e) = Elet r (mapsnd casetr dl) (casetr e)
||  casetr (Emodule i expl dl) = Emodule i expl (map (mapsnd casetr) dl)
||  casetr (Econstr c el) = Econstr c (map casetr el)
||  casetr (Efailmatch n) = Efailmatch n
||  casetr (Ecfunction b n) = Ecfunction b n
||  casetr (Eidapl i es) = Eidapl i (map casetr es)
||  casetr (Elaml is e) = Elaml is (casetr e)
||  casetr (Einfo f e) = Einfo f (casetr e)
--  Eap, Elam,  Evar cannot occur
#else
||  casetr (Eidapl i es) =
	let n = findcase es in
	let f x = Eidapl i (updl es n x) in
	case select n es in
	    Ecase e cl d : Ecase e (mapthd (pushcase f) cl) (pushcase f d)
	end
||  casetr e = e
#endif
and updl (x.xs) 1 y = y.xs
||  updl (x.xs) n y = x.updl xs (n-1) y
and findcase (Ecase _ _ _._) = 1
||  findcase (_.es) = 1 + findcase es
and
    pushcase f (Ecase e2 cl2 d2) = Ecase e2 (mapthd (pushcase f) cl2) (pushcase f d2)
||  pushcase f (Efailmatch n) = Efailmatch n
||  pushcase f e = f e--Ecase e cl d
end
