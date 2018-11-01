module -- lettrans
--
-- various transformations on letexpressions
--
#include "../misc/flags.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../misc/misc.t"
#include "../misc/util.t"

export remdeep, andify;
rec
    andify [] = mkbnull
||  andify [a] = a
||  andify (a.b) = mkband a (andify b)
and mapfs f = map (\(p,e).(f p, f e))
and
    getbind (mkbrec d) = getbind d
||  getbind (mkband d1 d2) = getbind d1 @ getbind d2
||  getbind (mkbpat [(p,e)]) = [mkbpat [(re p, re e)]]
||  getbind (mkblocal d1 d2) = getbind d1 @ getbind d2
||  getbind (mkbmulti p e) = fail "getbind mkbmulti"
||  getbind (mkbnull) = []
||  getbind (mkbpragma _) = []
||  getbind (mkbtype _ _ _ _) = []
||  getbind (b as mkberror _) = [b]
and
    mlet [] e = e
||  mlet bs e = mkletv (mkbrec (andify bs)) e
and
    re (mkap f a) = mkap (re f) (re a)
||  re (mklam i e) = mklam i (re e)
||  re (mkcase e cl) = mkcase (re e) (mapfs re cl)
||  re (mkletv b e) = mlet (getbind b) (re e)
||  re (e as mkident _) = e
||  re (mkmodule i fixl impl expl b) =
	mkmodule i fixl impl expl (mkbrec (andify (getbind b)))
||  re (e as mkconst _) = e
||  re (e as mkerror _) = e
||  re (e as mkcfunction _ _) = e
||  re (mkas i e) = mkas i (re e)
||  re (mkcondp p c) = mkcondp (re p) (re c)
--  lazyp cannot occur
||  re (mkinfo t e) = mkinfo t (re e)
||  re (mkconstr c es) = mkconstr c (map re es)
||  re (e as mkfailmatch _) = e
and
    remdeep e = re e
end
