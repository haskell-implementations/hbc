module


----- Does not work.  Needs to move default code.
----- see rename/himport.m chgid.m








#include "../expr/id.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Esubst.t"
#include "treewalk.t"
export cflow;
rec
    cflow (Emodule i e iess) = Emodule i e (map (mapsnd (flowe [])) iess)
and flowe vcs e = treewalk (flowf vcs) e
and flowf vcs (Ecase (ie as Eidapl i []) cies de) (Ecase _ ncies nde) =
        case findit eqid i vcs in
	    Some (c, is) & (~exists isdummy is) : simpflow c is nde ncies
        ||  _ : Ecase ie (map (\(c,is,e). (c,is,flowe ((i,(c,is)).vcs) e)) cies) nde
        end
||  flowf vcs oe ne = ne
and simpflow c is d [] = d
||  simpflow c is d ((c1,is1,e1).cies) & (cno c = cno c1) = Ealphasubst (combine(is1,is)) e1
||  simpflow c is d (_.cies) = simpflow c is d cies
and findit _  _ [] = None
||  findit eq x ((y,r).ys) & (eq x y) = Some r
||  findit eq x (_.ys) = findit eq x ys
end
