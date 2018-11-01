module
#include "cexpr.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../misc/util.t"
#include "../rename/renenv.t"
#include "imisc.t"
#include "state.t"
#include "dynlib.t"

export idsubst;

rec look (i as mkid n _ _ _) l = 
    let rec
	ccall = n = -999
    and asmname = (if ccall then "" else "C") @ asmid i
    and	look' [] = ifail ("id lookup: "@prid i)
    ||  look' (Internal i2 v._) & (eqid i i2) = v
    ||  look' (DynLib _ h.bs) = 
				case dlsym h asmname in
				   No _  : look' bs
				|| Yes e : 
				   if ccall then Comb (prid i) e 
				   else Comb (prid i) (dlcall e)
				end
    ||  look' (_.bs) = look' bs
    in look' l

and idsubst l e = ids e
where rec ids (Ap e1 e2) = Ap (ids e1) (ids e2)
||        ids (Var (mkids s)) = ifail ("idsubst of mkids "@s)
||        ids (Var i) = look i l
||        ids e = e
end
