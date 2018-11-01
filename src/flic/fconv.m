/* Revisions

1) 	Etof returns a list of (string, flic) pairs 
	instead of a flic expression.
	This represents the top-level letrec.
				SLPJ 27 Apr 88

2)	Dealing with case/fail is exported to before lamba-lifting (fconstr.m)
				SLPJ 28 Apr 88

End of revisions */

module
#include "../ExprE/Expr_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/id.t"
#include "../expr/einfo_t.t"
#include "Flic_t.t"
#include "fconvname.t"
export Etof;

rec
    fap (Fap (Fname "[!]") f) a  =  Fannot (Annot0 "!") (Fap f a)
||  fap (Fname "[LAM!]") f       =  Fannot (Annot0 "LAM!") f
||  fap (Fname "[*]") a          =  Fannot (Annot0 "*") a
||  fap f a                      =  Fap f a
and
    icnv i = convname (idtostr i)
and
    fccnv (c as Cconstr _ _ _ d _ l) =
    	case constrtype c in
	   Gint :      Fnumber d
	|| Gchar :     Fchar (chr d)
	|| Gstring s : Fstring s
        || Gsfloat sf : fail "fccnv: no floats"
        || Gdfloat sf : fail "fccnv: no floats"
        || Ginteger si:fail "fccnv: no integers"
	|| Gtype :     fail "fccnv: unexpected constructor"
	end
and
    aps f args = revitlist (\a.\f.fap f a) args f
and
    F (Elet is_rec ies e)= let (is,es) = split ies in
			  Flet is_rec (map icnv is) (map F es) (F e)
||  F (Econstr c [])	= fccnv c
||  F (Eidapl i es)	= aps (Fname (icnv i)) (map F es)
||  F (Elaml is e)	= reduce (\i.Flam (icnv i)) (F e) is
--||  F (Einfo (annot i) e)  =  Fannot (Annot0 i) (F e)
||  F (Einfo _ e)	= F e
||  F (Econstr _ _)	= fail "F: unexpected Econstr" 
||  F (Ecase _ _ _)	= fail "F: unexpected Ecase"
||  F (Efailmatch _)	= fail "F: unexpected Efailmatch"
||  F (Eap f a)		= fail "F: unexpected Eap"
||  F (Elam i e)	= fail "F: unexpected Elam"
||  F (Evar i)		= fail "F: unexpected Evar"

and Etof (Emodule _ _ defs) = concmap (map (\(id,expr). (idtostr id, F expr))) defs
    -- Etof returns a list of (id, flic-expression) pairs

end
