module --curry0
-- Initial transformations, before rename.
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/pragma_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/ttype.t"
#include "../transform/misc.t"
#include "../syntax/listgen.h"
#include "../misc/flags.t"
export curry0, cmkif;
rec curry0 e = ispec (cc (ff e))
and
    ff t =
	case t in
	    mkap (mkap (mkap (mkident (mkids "Pif")) c) t) e : cmkif (ff c) (ff t) (ff e)
	|| (mkap f a) :  mkap (ff f) (ff a)
	|| (mklam e1 e) : mklam e1 (ff e)
	|| (mkcase e cl) : mkcase (ff e) (map ffp cl)
	|| (mkletv b e) : mkletv (ffb b) (ff e)
	|| (mkident i) : mkident i
	|| (mkconst _) : t
	|| (mkerror _) : t
	|| (mkas i p) : mkas i (ff p)
	|| (mkcondp p c) : mkcondp (ff p) (ff c)
	|| (mklazyp p) : mklazyp (ff p) 
	|| (mkconstr c el) : mkconstr c (map (ff) el)
	|| (mkfailmatch _) : t
	|| (mkinfo t e) : mkinfo t (ff e)
	|| (mklistf n es) : mlistf n (map ff es)
	|| (mklistg e qs) : if MonadCompr then remmonad e qs else mklistg (ff e) (map ffq qs)
	|| (mkhmodule a b c d e) : mkhmodule a b c d (ffb e)
	|| (mkwhere ges b) : mkwhere (map (\(g,e).ff g, ff e) ges) (ffb b)
--	|| (mkcfunction _ _) : t
	|| (mkdo s) : ff (expandstmt s)
	|| (mkrecord c es _) : mkrecord (ff c) (mapsnd ff es) []
	end
and remmonad e qs =
	let rec f (mkqfilter e . qs) = mksexpstmt (mkapl "_guard" [e]) (f qs)
	||      f (mkqgen p e . qs) = mksbind p e (f qs)
	||      f (mkqlet b . qs) = mkslet b (f qs)
	||      f [] = mksexp (mkapl "_return" [e])
	in  ff (mkdo (f qs))
and mlistf L_FROM_TO    es = mkapl "_enumFromTo"     es
||  mlistf L_FROM       es = mkapl "_enumFrom"       es
||  mlistf L_FROM_BY_TO es = mkapl "_enumFromThenTo" es
||  mlistf L_FROM_BY    es = mkapl "_enumFromThen"   es
and mkapl s es = reduce (\a.\f.mkap f a) (mkident (mkpids s)) (reverse es)
and
    ffq (mkqfilter e) = mkqfilter (ff e)
||  ffq (mkqgen p e) = mkqgen (ff p) (ff e)
||  ffq (mkqlet b) = mkqlet (ffb b)
and ffb (mkbrec b) = mkbrec (andify (crunchf (map ffb1 (unand b))))
||  ffb b = andify (crunchf (map ffb1 (unand b)))
and ffb1 d =
	case d in
	   mkbpat pl : mkbpat (map ffp pl)
        || mkbmulti p e : mkbmulti (ff p) (ff e)
	|| mkbclass t b : mkbclass t (ffb b)
	|| mkbinstance t b x : mkbinstance t (ffb b) x
        || mkbview t ot cs b : mkbview t ot cs (ffb b)
	|| d : d
	end
--and ffi i = i
and unand (mkband b1 b2) = unand b1 @ unand b2
||  unand b = [b]
and andify [b] = b
||  andify (b.bs) = mkband b (andify bs)
and crunchf (mkbpat [(p, e)].bs) = pickf (leftmost p) [(p, e)] bs
||  crunchf (b.bs) = b.crunchf bs
||  crunchf [] = []
and pickf n pes (mkbpat [(p, e)].bs) & (leftmost p = n) = pickf n ((p,e).pes) bs
||  pickf _ pes bs = mkbpat (reverse pes).crunchf bs
and
    ffp (p, e) = (ff p, ff e)
and cmkif c t e = mkcase c [(cmtrue, t); (cmfalse, e)]
-- True and False cannot be overridden by user because they are constructors in Prelude (I hope!!)
and cmtrue = mkident (mkpids "_True")
and cmfalse = mkident (mkpids "_False")

and ithen = mkident (mkpids "_>>")
and ibind = mkident (mkpids "_>>=")
and mkap2 f a1 a2 = mkap (mkap f a1) a2

and expandstmt (mksexp e) = e
||  expandstmt (mksexpstmt e s) = mkap2 ithen e (expandstmt s)
||  expandstmt (mkslet b s) = mkletv b (expandstmt s)
||  expandstmt (mksbind p e s) = mkap2 ibind e (mklam p (mkinfo stmtpat (expandstmt s)))

-- specialize instances
and ispec (mkhmodule a b c d e) = mkhmodule a b c d (ispecb e)
and ispecb b =
	let ispecs = getispecs b
	and insts  = getinsts b in
	let newinsts = map (geninst insts) ispecs in
	addbind newinsts b
and geninst insts (t as mktcons _ ts) =
	case filter (matches t) insts in
	   [mkbinstance (mkidecl _ clsid _ s) b d] : mkbinstance (mkidecl [] clsid ts s) b d
	|| _ : mkberror ("[96] Illegal instance specialized: "@prttype t)
	end
and matches (mktcons clsid ts) (mkbinstance (mkidecl _ clsid' ts' _) _ _) =
	eqid clsid clsid' & And (map2 matcht ts' ts)
and matcht (mktvar _) _ = true
||  matcht (mktcons i ts) (mktcons i' ts') = eqid i i' & And (map2 matcht ts ts') & length ts = length ts'
||  matcht _ _ = false
and addbind [] b = b
||  addbind n (mkbrec b) = mkbrec (mkband b (andify n))
||  addbind n b = mkband b (andify n)
and getispecs (mkbrec b) = getispecs b
||  getispecs (mkband b1 b2) = getispecs b1 @ getispecs b2
||  getispecs (mkbpragma (Pspecinst t)) = [t]
||  getispecs _ = []
and getinsts (mkbrec b) = getinsts b
||  getinsts (mkband b1 b2) = getinsts b1 @ getinsts b2
||  getinsts (b as mkbinstance _ _ _) = [b]
||  getinsts _ = []

----
and cc e = 
  let rec
    ff t =
	case t in
	   (mkap (mkident (mkids "_cFunction")) (mkident i)) : mkcfunction true i
	|| (mkap (mkident (mkids "_cVariable")) (mkident i)) : mkcfunction false i
	|| (mkap f a) : ffap f [a]
	|| (mklam e1 e) : mklam e1 (ff e)
	|| (mkcase e cl) : mkcase (ff e) (map ffp cl)
	|| (mkletv b e) : mkletv (ffb b) (ff e)
	|| (mkident i) : mkident i
	|| (mkconst _) : t
	|| (mkerror _) : t
	|| (mkas i p) : mkas i (ff p)
	|| (mkcondp p c) : mkcondp (ff p) (ff c)
	|| (mklazyp p) : mklazyp (ff p) 
	|| (mkconstr c el) : mkconstr c (map (ff) el)
	|| (mkfailmatch _) : t
	|| (mkinfo t e) : mkinfo t (ff e)
	|| (mklistf n es) : mlistf n (map ff es)
	|| (mklistg e qs) : mklistg (ff e) (map ffq qs)
	|| (mkhmodule a b c d e) : mkhmodule a b c d (ffb e)
	|| (mkwhere ges b) : mkwhere (map (\(g,e).ff g, ff e) ges) (ffb b)
--	|| (mkcfunction _ _) : t
	|| (mkrecord c es _) : mkrecord (ff c) (mapsnd ff es) []
	end
  and ffap (mkap f a) xs = ffap f (a.xs)
  ||  ffap (mkident (mkids "_ccall")) (mkident cfun . xs) =
        mkaps (mkident (mkids "_ccallCV")) [mkcfunction true cfun; mklist (map (mkap (mkident (mkids "_toCU"))) (map ff xs))]
  ||  ffap e xs = mkaps (ff e) (map ff xs)
  and mklist = reduce (\ x . \ xs . mkap (mkap (mkident (mkpids "_:")) x) xs) (mkident (mkpids "_[]"))
  and mkaps e [] = e
  ||  mkaps f (x.xs) = mkaps (mkap f x) xs
  and ffq (mkqfilter e) = mkqfilter (ff e)
  ||  ffq (mkqgen p e) = mkqgen (ff p) (ff e)
  ||  ffq (mkqlet b) = mkqlet (ffb b)
  and ffp (p, e) = (ff p, ff e)
  and ffb d =
	case d in
	   mkbpat pl : mkbpat (map ffp pl)
        || mkbmulti p e : mkbmulti (ff p) (ff e)
	|| mkbclass t b : mkbclass t (ffb b)
	|| mkbinstance t b x : mkbinstance t (ffb b) x
	|| mkbrec b : mkbrec (ffb b)
	|| mkband b1 b2 : mkband (ffb b1) (ffb b2)
	|| d : d
	end
 in  if CCall then ff e else e
end

