module -- subst
--
-- substitute an expression for an identifier
--
#include "id.t"
#include "types_t.t"
#include "einfo_t.t"
#include "pprint.t"
#include "../misc/util.t"

export subst, substl;
rec
    subst s i t = substl [(i,s)] t
and unid (mkident i) = i
and substl al t = substx t
where rec
    substx t =
	case t in
	   (mkap f a) :  mkap (substx f) (substx a)
	|| (mklam e1 e) : mklam e1 (substx e)
	|| (mkcase e cl) : mkcase (substx e) (map substp cl)
	|| (mkletv b e) : mkletv (substb b) (substx e)
	|| (mkident i) : assocdefeq eqid i al t
	|| (mkmodule i fl il el b) : mkmodule i fl il el (substb b)
	|| (mkconst _) : t
	|| (mkcfunction _ _) : t
	|| (mkconstr c el) : mkconstr c (map substx el)
	|| (mkcondp p c) : mkcondp (substx p) (substx c)
	|| (mklazyp p) : mklazyp (substx p)
	|| (mkinfo t e) : mkinfo (substt t) (substx e)
	|| (mkerror _) : t
	|| (mkfailmatch _) : t
        || (mkas i p) : mkas (unid (assocdefeq eqid i al (mkident i))) (substx p)
        || (mkwhere ges b) : mkwhere (mapsnd substx ges) (substb b)
        || _ : fail ("No match in subst: "@ppr t)
	end
and substb d =
	case d in
	   (mkbrec b) : mkbrec (substb b)
	|| (mkband b1 b2) : mkband (substb b1) (substb b2)
	|| (mkblocal b1 b2) : mkblocal (substb b1) (substb b2)
	|| (mkbpat pl) : mkbpat (map substp pl)
	|| (mkbmulti p e) : let (np,ne) = substp (p,e) in mkbmulti np ne
	|| (mkbtype _ _ _ _) : d
	|| (mkbnull) : d
	|| (mkbpragma _) : d
	|| (mkberror _) : d
	|| _ : fail ("substb "@prdefg 0 d)
	end
and
    substp (p, e) = (p, substx e)
and
    substt (spark is) = spark (map (\i.unid (assocdefeq eqid i al (mkident i))) is)
||  substt t = t
end
