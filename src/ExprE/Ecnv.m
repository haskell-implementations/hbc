module -- Ecnv
--
-- convert to more compact tree form
-- expr is used during syntactical transformations and Expr thereafter
--
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../expr/types_t.t"
#include "../expr/einfo_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/id_t.t"
#include "../transform/exprs.t"
#include "../transform/hexpr.t"
#include "Expr_t.t"

export Ecnv;
rec
    Ecnv e = (
	case e in	
	    mkmodule i _ imp (Some exp) (mkbrec dl) : Emodule i exp [Ebconv dl]
	||  _ : Econv e
	end)
where rec
    Econv (mkap f a) = Eap (Econv f) (Econv a)
 || Econv (mklam (mkident i) e) = Elam i (Econv e)
 || Econv (mkcase e (p.pl)) = Ecase (Econv e) (reverse (map Epbc pl)) (Epbi p)
 || Econv (mkletv (mkbrec d) e) =  case Ebconv d in
					[] : Econv e
				   || dl : Elet true dl (Econv e)
				   end
 || Econv (mkletv d e) =           case Ebconv d in
					[] : Econv e
				   || dl : Elet false dl (Econv e)
				   end
 || Econv (mkident ii) = Evar ii
 || Econv (mkconstr c el) = Econstr c (map Econv el)
 || Econv (mkfailmatch n) = Efailmatch n
 || Econv (mkinfo t e) = Einfo t (Econv e)
 || Econv (mkcfunction b i) = Ecfunction b i
 || Econv e = fail ("No match in Econv: "@ppr e)
 -- mkmodule, mkconst, mkerror, mkas, mkcondp cannot occur
and
    Ebconv d = (
	map fixpairs (filter notype (smashand d))
	where rec smashand (mkband d1 d2) = smashand d1 @ smashand d2
	      ||  smashand mkbnull = []
	      ||  smashand d = [d]
	and       notype (mkbtype tt lat _ _) = false
	      ||  notype _ = true
	and       fixpairs (mkbpat [(mkident(i), e)]) = (i, Econv e))
and
    Epbc (mkconstr c el, e) =
	(c, map (\(mkident i).i) el, Econv e)
and
    Epbi (mkident i, e) & (isdummy i) = Econv e
end
