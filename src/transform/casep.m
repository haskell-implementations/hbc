module -- condp
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/pprint.t"
#include "failcase.t"

export condjoin;
    condjoin d pes = cj (map snd pes)
	where cj (e.es) = j es (ext e)
	    where rec j [] e = e
		   || j (mkcase e [(i, mkfailmatch 1); et; ef] . es) ed =
			j es (fmkcase e [(i, ed); et; ef])
		   || j (e.es) _ = mkerror ("[34] Bad conditional pattern (overlap?) near "@ppr e)
	    and       ext (mkcase e [(i, mkfailmatch 1); et; ef]) =
			fmkcase e [(i, d); et; ef]
		   || ext e = e
end
