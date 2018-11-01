module
#include "../expr/types_t.t"
#include "../expr/impexp_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../rename/renenv.t"
#include "../transform/hexpr.t"
#include "cexpr.t"
#include "state.t"
export mkprt;
rec mkprt state i = 
    case rfind Ktype i (st_env state) in
       (ii as mkid n _ (idi_type _ t _ ti _ _) _) :
         if get_has_existential ti then
	   No ("Cannot show existential "@i)
         else
           let fun = mkident (showid ii)
           and tvars = getTvars t in
           let args = map mkident (map argid tvars @ [mkids (pre@"x")]) in
           let body = mkcase (last args) (map mk1pat (get_component_types_from_tinfo ti)) in
	   Yes (mkbrec (mkbpat[(apply fun args, body)]))
    || _ : No ("Not a type "@i)
    end
and pre = "__"
and argid v = mkids (pre@"f"@itos v)
and apply f [] = f
||  apply f (x.xs) = apply (mkap f x) xs
and mk1pat (mkcons i _ tbs _) =
    let vars = for 1 (length tbs) (\n.mkident (mkids (pre@"y"@itos n))) in
    let cname = idtostr i in
    (apply (mkident (mkids cname)) vars,
     if null vars then
	 mkconst (cstring (tl cname))
     else
	 cnc (concs (mkconst (cstring ("("@tl cname)).map2 show1 vars tbs)) (mkconst (cstring ")")) )
and cnc x y = mkap (mkap (mkident (mkids "_@")) x) y
and concs [x] = x
||  concs (x.xs) = cnc x (cnc (mkconst (cstring " ")) (concs xs))
and show1 v (t,_,_) = mkap (showt t) v
and showt (mktvar v) = mkident (argid v)
||  showt (t as mktcons i ts) = 
    if stringp t then
	mkident (mkids "_show_String")
    else
	apply (mkident (showid i)) (map showt ts)
and stringp (mktcons l [mktcons c []]) = eqid l hiList & eqid c hiChar
||  stringp (mktcons l []) = eqid l hiString
||  stringp _ = false
and showid i = mkids ("_show"@idtostr i)
end
