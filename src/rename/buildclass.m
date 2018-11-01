module -- buildclass
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../misc/util.t"
#include "renenv.t"
#include "classutil.t"
export buildsuper;
rec buildsuper env =
    -- The superclass structure is stored as an association list from
    -- class id to a superclass list.  The list contains pairs of class
    -- id and access path to the id.
    let cs = filter id_isclass (rids Ktype env) in
    let css = map (\(i as mkid _ _ (idi_class (clsi _ t _ _ _ _)) _).(i, tsup t)) cs in
    let ncss = transclos css in
/*
(
    trace (show_list (\(i,is).prid i@":"@show_list (show_pair(prid,show_list show_int)) is@"\n") ncss)
)
*/
    (ncss, map (idtostr o fst) (filter (\(i,xs).member eqid i (map fst xs)) ncss))

and tsup (mkcdecl ts _) = 
    let ts' = [ mkassert c v ;; mkassert c v <- ts ]
    in  combine (map aname (sortcon ts'), map (\x.[x]) (from 0))
and aname (mkassert i _) = i
||  aname (mkaerror s) = fail ("aname: "@s)

-- The transitive closure algoritm is slow, but OK if the superclass chains are
-- short.
and transclos css =
    let css' = onestep css in
    if And (map2 (\(_,xs).\(_,ys).length xs = length ys) css css') then
	css
    else
	transclos css'
and assoceqid x xs = assocdefeq eqid x xs (trace ("assoceq in buildsuper "@dprid x) [])
--and assoceqid x xs = assocdefeq eqid x xs (fail ("assoceq in buildsuper "@dprid x))
and onestep css = map (step css) css
and step css (i, xs) = (i, xs@
    let ixs = map fst xs in
    mkseteq (\(x,_).\(y,_).eqid x y) (concmap (\(x,is).map (\(y,js).(y,is@js)) (filter (\(y,_).~member eqid y ixs) (assoceqid x css))) xs))
end
