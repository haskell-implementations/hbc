module -- imptuples
#include "../misc/triple.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/ttype_t.t"
#include "../expr/impexp_t.t"
#include "../expr/types_t.t"
#include "../misc/flags.t"
-- Special hack for tuples.
export imptuples;
rec
    tuples = (for 2 30 f
    	where f n = mkimpeqtype (mkkarrows (rept n mkkground) mkkground) (mktcons (mkids (tchar.'#'.itos n)) (tvars n))
		     [mkcons (mkids (tupstr n)) (false, [], []) (combine3 (tvars n, (f where rec f = false.f),(f where rec f=None.f))) false]
		     (tupder n) 
	             false true)

and tchar = if H1_3 then 'P' else '_'

-- This is a temporary hack!!
and tupder n = 
    if Curry then
	if n <= 5 & Derived then Some (map mkids (["_Eq"; "_Ord"; "_Ix"] @ if H1_3 then ["_Show"; "_Read"] else ["_Binary"; "_Text"])) else Some []
    else
	None

and imptuples = mkimport (mkids "_Prelude") [] [] tuples false [] [] false None

end
