module -- import
--
#include "../misc/triple.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/types_t.t"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../misc/htable.t"
#include "../misc/oset.t"
#include "../misc/sort.t"
#include "imptuples.t"
#include "renenv.t"
#include "importlib.t"
#include "himport.t"
#include "renameutil.t"
export importenv, badmodname, importe;
rec
    cdef (mkcdecl _ (mkassert i _)) = i

and pretab = [(i, Some (MI s, idtostr i)) ;; (mkimport (mkids s) x y ents z w v q a)<-corelib; ent<-ents; i<-gettc ent ]

-- Generate a-list for possible types and classes in one import
and gettcids (mkimpimport m imps rens) = let s = idtostr m in (m, map (gettcid1 s rens) imps)
and gettcid1 m rens (mkexpid i) = gettcc m rens i
||  gettcid1 m rens (mkexpidall i) = gettcc m rens i
||  gettcid1 m rens (mkexpidsome i is) = gettcc m rens i
||  gettcid1 m rens (mkexpidmodall i) = fail "mkexpidmodall in interface import"
and gettcc m rens i = (assocdefeq eqid i rens i, Some (MI m, idtostr i))

and gettc (mkimpclass _ c _ _) = [cdef c]
||  gettc (mkimpsyn _ t _) = [tname t]
||  gettc (mkimpeqtype _ t _ _ _ _) = [tname t]
||  gettc (mkimptype _ t _) = [tname t]
||  gettc (mkimpview _ t _ _) = [tname t]
||  gettc _ = []

and gettcv imp = impids imp

and getv (mkimpclass _ _ b _) = map fst (flatsyns (listify b))
||  getv (mkimpeqtype _ _ ats _ _ _) = map (\(mkcons i _ _ _).i) ats
||  getv (mkimpid i _ _ _) = [i]
||  getv (mkimpids is _ _) = fail "mkimpids in getv"
||  getv _ = []

and mktable tab = makehtbl (mapfst (\ (mkids s).s) tab)
and looktable s tab = lookuphtbl tab s None

and tr1ent mid itabs tab imp = (
    case imp in
       mkimpid i t f ots :    mkimpid (trid i) (trtype t) (trfinfo f) (oapply (mapfst trtype) ots)
    || mkimptype k t p :      mkimptype k (trtype t) p
    || mkimpeqtype k t ats od iso vis : let t' = trtype t in mkimpeqtype k t' (map (trat (odef (tname t') mstr) (assocdef mid itabs [])) ats) (oapply (map trid) od) iso vis
    || mkimpsyn k t1 t2 :     mkimpsyn k (trtype t1) (trtype t2)
    || mkimpclass k c b fs :  let c' = trcdecl c in mkimpclass k c' (trbind (odef (ccname c') mstr) (assocdef mid itabs []) b) fs
    || mkimpinstance i b fs : mkimpinstance (tridecl i) b fs
    || mkimpview k t tof ats : let t' = trtype t in mkimpview k t' (trtype tof) (map (trat (odef (tname t') mstr) (assocdef mid itabs [])) ats)
    || _ :                    fail "Bad import in tr1ent"
    end
where
rec trfinfo f = f
and mstr = idtostr mid
and odef (mkidi _ (Some (MI m,_)) _ _) _ = m
||  odef _ m = m
and trtype (mktcons i ts) = mktcons (trid i) (map trtype ts)
||  trtype (mktap i ts) = mktap i (map trtype ts)
||  trtype (t as mktvar _) = t
||  trtype (t as mkterror _) = t
||  trtype (mktcontype c t) = mktcontype (map trassert c) (trtype t)
and trassert (mkassert i vs) = mkassert (trid i) vs
and trid (i as mkids s) = 
       case splitat '.' s in
          ((mod as (_._)), id as (c._)) & (isalnum (last mod)) : mkidi ('_'.id) (Some (MI mod, '_'.id)) mstr [""]
       || _ : mkidi s (looktable s tab) mstr [""]
       end
and trtabid mdef tb (i as mkids s) = mkidi s (assocdef i tb (Some (MI mdef, s))) mstr [""]
and trcdecl (mkcdecl c a) = mkcdecl (map trassert c) (trassert a)
and tridecl (mkidecl c i ts m) = mkidecl (map trassert c) (trid i) (map trtype ts) m
and trbind md n (mkband b1 b2) = mkband (trbind md n b1) (trbind md n b2)
||  trbind md n mkbnull = mkbnull
||  trbind md n (mkbsign is t) = mkbsign (map (trtabid md n) is) (trtype t)
||  trbind md n _ = fail "Bad class body in trbind"
and trat md n (mkcons i (uni,vs, c) tbs flg) = mkcons (trtabid md n i) (uni, vs, map trassert c) (mapfst3 trtype tbs) flg
)

and ccname (mkcdecl _ (mkassert i _)) = i
and itname (mkidecl _ _ [t] _) = tname t -- ASSERT
and ctname (mkidecl _ i _ _) = i

and underive t None = []
||  underive t (Some is) = 
    let ks = cpart t 
    and rt = tpart t 
    and m = getminame (fst (id_orignames (tname t)))
    in  map (\ci.mkimpinstance (mkidecl ks ci [rt] m) true []) is -- fs info is [] for derived insts -- ASSERT

and keepren show fltr rens imp = 
    let fids = map (idtostr o expid) fltr in
    let remq true imp = imp
    ||	remq false imp = remunqual imp in
(
 (let (b, imps) =
    case imp in
       mkimpid i t f ots      : (keep i,                            [mkimpid (trid i) (trtype t) (trfinfo f) (oapply (mapfst trtype) ots)])
    || mkimptype k t p        : (keep (tname t),                    [mkimptype k (trtype t) p])
    || mkimpview k t tof ats  : (keep (tname t),		    [mkimpview k (trtype t) (trtype tof) (map trat ats)])
    || mkimpeqtype k t ats od iso _ : (keep (tname t),                  let t' = trtype t
								      and od' = oapply (map trid) od
								      and vis = keep (tname t) & allexp (tname t) in
								      if vis | H1_3  -- XXX wrong
								      then [mkimpeqtype k t' (map trat ats) od' iso vis ]
								      else [mkimptype k t' (0, false)] @ underive t od')
    || mkimpsyn k t1 t2       : (keep (tname t1),                   [mkimpsyn k (trtype t1) (trtype t2)])
    || mkimpclass k c b fs    : (keep (cdef c),                     [mkimpclass k (trcdecl c) (trbind b) fs])
    || mkimpinstance i b fs   : (keep (itname i) | keep (ctname i), [mkimpinstance (tridecl i) b fs])
    || mkimpids is t f : fail "mkimpids in keepren"
    || _ : fail "no match in keepren"
    end
 in if QualifiedNames then map (remq b) imps else if b then imps else [] )
where
-- Yuk, text editor copying is easy, abstraction is tedious.  I'll use the former.
rec trfinfo f = f
and tab = map (\(mkids s, mkids t).(s, t)) rens
and keep (mkidi s _ _ _) = if show then mem s fids else ~mem s fids
and allexp i = ~show | chkexp (idtostr i) fltr
and chkexp i (mkexpid i'._) & (i=idtostr i') = false
||  chkexp i (mkexpidall i'._) & (i=idtostr i') = true
||  chkexp i (mkexpidsome i' _._) & (i=idtostr i') = true
||  chkexp i (_.es) = chkexp i es
||  chkexp i _ = fail ("Bad id in chkexp "@ i)
and trtype (mktcons i ts) = mktcons (trid i) (map trtype ts)
||  trtype (mktap i ts) = mktap i (map trtype ts)
||  trtype (t as mktvar _) = t
||  trtype (t as mkterror _) = t
||  trtype (mktcontype c t) = mktcontype (map trassert c) (trtype t)
and trassert (mkassert i vs) = mkassert (trid i) vs
and trid (mkidi s on mstr pres) = mkidi (assocdef s tab s) on mstr pres
and trcdecl (mkcdecl c a) = mkcdecl (map trassert c) (trassert a)
and tridecl (mkidecl c i ts m) = mkidecl (map trassert c) (trid i) (map trtype ts) m
and trbind (mkband b1 b2) = mkband (trbind b1) (trbind b2)
||  trbind mkbnull = mkbnull
||  trbind (mkbsign is t) = mkbsign (map trid is) (trtype t)
||  trbind _ = fail "Bad class body in trbind"
and trat (mkcons i (uni, vs, c) tbs flg) = mkcons (trid i) (uni, vs, map trassert c) (mapfst3 trtype tbs) flg
)

and keepfix ids (mkfixid is f) = mkfixid (filter (\(mkids s).mem s ids) is) f

and flatids (mkimpids is t f) = map (\i.mkimpid i t f None) is
||  flatids i = [i]

and addsels ients ents = if ~H1_3 then ients else
	let sels = [ (mi, idtostr s) ;; 
		    	mkimpeqtype x1 t ats x3 x4 x5 <- ients;
		        (mi, w) <- [id_orignames (tname t)];
		        mkcons y1 y2 tbs y3 <- ats;
		        (z1, z2, Some s) <- tbs ] in
	if null sels then ients
	else
	    let is = [ i ;; mkimpid i t f ots <- ients ] in
	    [ mkimpid i t f ots ;; mkimpid i t f ots <- ents; mem (id_orignames i) sels & ~ mem i is ] @ ients

-- Translate one import into its new names etc.
          --         mkimport Id (List Impid) (List Fixid) (List Impid) Bool (List Expid) (List (Id#Id)) Bool (Option Id)
and get1import (i as mkimport mid imports     fixs         oents        show fltr         rens           qual asname) =
        let ents = concmap flatids oents in
        let mstr = idtostr mid in
        let imptabs = map gettcids imports in
        let imptab = concmap snd imptabs in
	let localtab = map (\i.(i, Some (MI mstr, idtostr i))) (difference (concmap gettcv ents) (map fst imptab)) in
	let tab = if H1_3 then mktable (localtab @ pretab @ imptab) else mktable (pretab @ imptab @ localtab) in
	-- process each of the imported entities
	let ents' = map (tr1ent mid imptabs tab) ents in
	-- keep only visible entities and do final renaming
        let ents'' = addsels (concmap (keepren show fltr rens) ents') ents' in
        let eids = map goname (concmap impids ents'') in
        let fixs' = map (keepfix eids) fixs in
	let mstr' = tl (case asname in Some (mkids s) : s || _ : mstr end) @ "." in
	let ents''' = if qual then map remunqual ents'' else ents'' in
	(fixs', addqual mstr' ents''')

and goname i = idtostr i --- i seems to be a mkids?!?  snd (id_orignames i)

and oget None = []
||  oget (Some l) = l

and gettcuse imp = ounion (gettctuse imp) (gettccuse imp)

and gettctuse imp =
    case imp in
       mkimpid i t f ots :    tctutype t @ oget (oapply (reduce ounion [] o map (tctutype o fst)) ots)
    || mkimptype _ t p :      []
    || mkimpview _ _ tof ats : reduce ounion (tctutype tof) (map tctuat ats)
    || mkimpeqtype _ t ats od _ _ : reduce ounion [] (map tctuat ats)
    || mkimpsyn _ t1 t2 :     tctutype t2
    || mkimpclass _ c b _ :   tctubind b
    || mkimpinstance i b _ :  tctuidecl i
    || mkimpids is t f :      fail "mkimpids in gettctuse"
    || mkimpimport _ _ _ :    []
    || mkimpctype _ _ :       []
    end
and tctutype (mktcons i ts) = reduce ounion [i] (map tctutype ts)
||  tctutype (mktap _ ts) = reduce ounion [] (map tctutype ts)
||  tctutype (mktvar _) = []
||  tctutype (mkterror _) = []
||  tctutype (mktcontype c t) = tctutype t
and tctuidecl (mkidecl c i ts m) = reduce ounion [] (map tctutype ts)
and tctubind (mkband b1 b2) = ounion (tctubind b1) (tctubind b2)
||  tctubind mkbnull = []
||  tctubind (mkbsign is t) = tctutype t
||  tctubind _ = fail "Bad class body in tctubind"
and tctuat (mkcons i (_, _, c) tbs _) = reduce ounion [] (map (tctutype o fst3) tbs)

and gettccuse imp =
    case imp in
       mkimpid i t f ots :    tccutype t @ oget (oapply (reduce ounion [] o map (tccutype o fst)) ots)
    || mkimptype _ t p :      []
    || mkimpview _ _ _ ats :  reduce ounion [] (map tccuat ats)
    || mkimpeqtype _ t ats od _ _ : reduce ounion (tccuod od) (map tccuat ats)
    || mkimpsyn _ t1 t2 :     []
    || mkimpclass _ c b _ :   ounion (tccucdecl c) (tccubind b)
    || mkimpinstance i b _ :  tccuidecl i
    || mkimpids is t f :      fail "mkimpids in gettccuse"
    || mkimpimport _ _ _ :    []
    || mkimpctype _ _ :       []
    end
and tccuod None = []
||  tccuod (Some is) = omkset is
and tccutype (mktcons i ts) = []
||  tccutype (mktap _ ts) = []
||  tccutype (mktvar _) = []
||  tccutype (mkterror _) = []
||  tccutype (mktcontype c t) = omkset (map tccuassert c)
and tccuassert (mkassert i v) = i
and tccucdecl (mkcdecl c a) = omkset (map tccuassert c)
and tccuidecl (mkidecl c i ts m) = reduce ounion (omkset (i.map tccuassert c)) (map tccutype ts)
and tccubind (mkband b1 b2) = ounion (tccubind b1) (tccubind b2)
||  tccubind mkbnull = []
||  tccubind (mkbsign is t) = tccutype t
||  tccubind _ = fail "Bad class body in tccubind"
and tccuat (mkcons i (_, _, c) tbs _) = reduce ounion [] (map tccuassert c . map (tccutype o fst3) tbs)

and getorig (mkidi _ (Some on) _ _) = on
||  getorig i = trace ("getorig failed "@idtostr i) (MI "?", "?")

and mltid (mkidi s _ _ _) (mkidi s' _ _ _) = s < s'
and oltid (mkidi _ on _ _) (mkidi _ on' _ _) = on < on'
and oeqid (mkidi _ on _ _) (mkidi _ on' _ _) = on = on'
--and eqidq (i1 as mkidi _ _ _ p1) (i2 as mkidi _ _ _ p2) = 
--	eqid i1 i2 & not (null (intersect p1 p2))

and remunqual imp = changeimpmod (filter (\x.x~="")) imp

and addqual mstr ents = if QualifiedNames then map (changeimpmod (\ pres . mstr.pres)) ents else ents

and changeimpmod g imp = (
    case imp in
       mkimpid i t f ots :    mkimpid (trid i) t f ots
    || mkimpview k t tof ats : mkimpview k (trtype t) tof (map trat ats)
    || mkimptype k t p :      mkimptype k (trtype t) p
    || mkimpeqtype k t ats od iso vis : mkimpeqtype k (trtype t) (map trat ats) od iso vis
    || mkimpsyn k t1 t2 :     mkimpsyn k (trtype t1) t2
    || mkimpclass k c b fs :  mkimpclass k (trcdecl c) (trbind b) fs
    || mkimpinstance i b fs : imp
    || _ :                    fail "Bad import in tr1ent"
    end
where
rec
    trtype (mktcons i ts) = mktcons (trid i) ts
||  trtype (t as mktap _ _)  = t
||  trtype (t as mktvar _) = t
||  trtype (t as mkterror _) = t
||  trtype (mktcontype c t) = mktcontype c (trtype t)
and trcdecl (mkcdecl c (mkassert cl v)) = mkcdecl c (mkassert (trid cl) v)
and trat (mkcons i vsc tbs flg) = mkcons (trid i) vsc tbs flg
and trid (mkidi s n m pres) = mkidi s n m (g pres)
and trbind (mkband b1 b2) = mkband (trbind b1) (trbind b2)
||  trbind mkbnull = mkbnull
||  trbind (mkbsign is t) = mkbsign (map trid is) (trtype t)
||  trbind _ = fail "Bad class body in trbind"
)

and renamed (mkidi old (Some (_, new)) _ _) = old ~= new
-- Change types that have been renamed on import via another interface.
-- This has to be done since lookup is on new name, not original.
-- (dorename is not called for the Prelude since no types can be renamed in it.)
and dorename [] ents = ents
||  dorename rt ents = map (dorename1 (map (\ (i as mkidi _ (Some (_, s)) _ _).(s, i)) rt)) ents
-- DOUBLE YUK!!
and dorename1 rt imp = (
    case imp in
       mkimpid i t f ots :    mkimpid i (trtype t) f (oapply (mapfst trtype) ots)
    || mkimpview k t tof ats :    mkimpview k (trtype t) (trtype tof) (map trat ats)
    || mkimptype k t p :      mkimptype k (trtype t) p
    || mkimpeqtype k t ats od iso vis : mkimpeqtype k (trtype t) (map trat ats) (oapply (map trid) od) iso vis
    || mkimpsyn k t1 t2 :     mkimpsyn k (trtype t1) (trtype t2)
    || mkimpclass k c b fs :  mkimpclass k (trcdecl c) (trbind b) fs
    || mkimpinstance i b fs : mkimpinstance (tridecl i) b fs
    || _ :                    fail "Bad import in tr1ent"
    end
where
rec
    trtype (mktcons i ts) = mktcons (trid i) (map trtype ts)
||  trtype (mktap i ts) = mktap i (map trtype ts)
||  trtype (t as mktvar _) = t
||  trtype (t as mkterror _) = t
||  trtype (mktcontype c t) = mktcontype (map trassert c) (trtype t)
and trassert (mkassert i v) = mkassert (trid i) v
and trid (i as mkidi s _ _ _) = assocdef s rt i
and trcdecl (mkcdecl c a) = mkcdecl (map trassert c) (trassert a)
and tridecl (mkidecl c i ts m) = mkidecl (map trassert c) (trid i) (map trtype ts) m
and trbind (mkband b1 b2) = mkband (trbind b1) (trbind b2)
||  trbind mkbnull = mkbnull
||  trbind (mkbsign is t) = mkbsign is (trtype t)
||  trbind _ = fail "Bad class body in trbind"
and trat (mkcons i (uni, vs, c) tbs flg) = mkcons i (uni, vs, map trassert c) (mapfst3 trtype tbs) flg
)

and eqcdecl (mkcdecl k1 a1) (mkcdecl k2 a2) = And (map2 eqassert (a1.k1) (a2.k2))
and eqassert (mkassert i1 v1) (mkassert i2 v2) = eqid i1 i2 & v1 = v2
||  eqassert _ _ = false
and eqidecl (mkidecl k1 i1 t1 _) (mkidecl k2 i2 t2 _) = And (map2 eqassert k1 k2) & eqid i1 i2 & And (map2 eqtype t1 t2)

and ordimp (mkimpid _ _ _ _) = 0
||  ordimp (mkimptype _ _ _) = 1
||  ordimp (mkimpeqtype _ _ _ _ _ _) = 1
||  ordimp (mkimpsyn _ _ _) = 3
||  ordimp (mkimpclass _ _ _ _) = 4
||  ordimp (mkimpinstance _ _ _) = 5
||  ordimp (mkimpview _ _ _ _) = 6
-- Could do this faster, but it's to tedious.
and eqimp (mkimpid i1 _ _ _) (mkimpid i2 _ _ _) = eqid i1 i2
||  eqimp (mkimpview _ t1 _ _) (mkimpview _ t2 _ _) = eqtype t1 t2
||  eqimp (mkimptype _ t1 _) (mkimptype _ t2 _) = eqtype t1 t2
||  eqimp (mkimpeqtype _ t1 _ _ _ _) (mkimpeqtype _ t2 _ _ _ _) = eqtype t1 t2
||  eqimp (mkimpsyn _ t1 _) (mkimpsyn _ t2 _) = eqtype t1 t2
||  eqimp (mkimpclass _ c1 _ _) (mkimpclass _ c2 _ _) = eqcdecl c1 c2
||  eqimp (mkimpinstance i1 _ _) (mkimpinstance i2 _ _) = eqidecl i1 i2
||  eqimp _ _ = false
and remdup eq (x1.(xs as x2._)) & (eq x1 x2) = remdup eq xs
||  remdup eq (x.xs) = x.remdup eq xs
||  remdup eq [] = []
and geteqtypeid (mkimpeqtype _ t _ _ _ _) = [tname t]
||  geteqtypeid _ = []
and specok is (mkimptype _ t _) = ~member eqid (tname t) is
||  specok _ _ = true
-- Remove duplicate entities and pick the most specific type.
and ismkimpinstance (mkimpinstance _ _ _) = true
||  ismkimpinstance _ = false
and pickspec ents =
    if H1_3 then
	let (insts, ninsts) = partition ismkimpinstance ents in
        let gs = groupsort (\ m1 . \ m2 . let n1 = ordimp m1 and n2 = ordimp m2 in
					  n1 < n2 | n1 = n2 & oltid (impid m1) (impid m2)) ninsts in
--trace (show_list (show_list dprid) [ map impid e ;; e <- gs]) (
	let ninsts' = map (joinimp o eqfirst) gs in
--trace (show_list dprid [ impid e ;; e <- ninsts'])
	let ents' = ninsts' @ insts in
	let eqtypes = concmap geteqtypeid ents' in
	filter (specok eqtypes) ents'
    else
	---- let ents' = ents in ----remdup eqimp (sort (<) ents) in
	let eqtypes = concmap geteqtypeid ents in
	filter (specok eqtypes) ents
and joinimp [imp] = imp
||  joinimp (is as (imp . _)) = let ps = getps is in changeimpmod (\ _ . ps) imp
and getps is = mkset [ p ;; mkidi s mi x ps <- map impid is; p <- ps ]
and eqfirst imps = 
  let eq (mkimpeqtype _ _ _ _ _ _) = true || eq _ = false
  in  filter eq imps @ filter (not o eq) imps

and isNone None = true
||  isNone _ = false

--and qprid (mkidi s _ _ ps) = s @ "[" @ mix ps "," @ "]"

and dupchk eq lt = 
    filter (not o allsameeq eq) o groupsort lt o filter (\ (mkidi _ _ _ ps) . mem "" ps)

and modname (mkidi _ (Some (MI m,_)) _ _) = m
||  modname _ = ""

and viamodname (mkidi _ (Some (MI mo,_)) m _) = m
||  viamodname _ = ""
-- Compute an export list for a module in case M.. is used
and expent f r (mkimpid i _ _ _) & (f i) = [mkexpid (rfind Kvalue (idtostr i) r)]
||  expent f r (mkimpsyn _ t _) & (f (tname t)) = [mkexpidall (rfind Ktype (idtostr (tname t)) r)]
||  expent f r (mkimptype _ t _) & (f (tname t)) = [mkexpid (rfind Ktype (idtostr (tname t)) r)]
||  expent f r (mkimpview _ t _ _) & (f (tname t)) = [mkexpidall (rfind Ktype (idtostr (tname t)) r)]
||  expent f r (mkimpeqtype _ t ats _ _ vis) & (f (tname t)) = 
    if vis then
  	[mkexpidall (rfind Ktype (idtostr (tname t)) r)] @
	if ~H1_3 then [] else
	[mkexpid (rfind Kvalue (idtostr s) r) ;; mkcons y1 y2 tbs y3 <- ats; (z1, z2, Some s) <- tbs ]
    else
	[mkexpid (rfind Ktype (idtostr (tname t)) r)]
||  expent f r (mkimpclass _ c _ _) & (f (cdef c)) = [mkexpidall (rfind Ktype (idtostr (cdef c)) r)]
||  expent f _ _ = []

and oidi s = remdup eqid s -- MKIDI

and importe clsfun curmod u0 envp preimps imps =
        let (pfixss, pentss) = split (map get1import preimps) in
        let ( fixss,  entss) = split (map get1import    imps) in
	let fixs = conc (fixss@pfixss)
        and pents = conc pentss
        and nents = conc  entss in
        let u = u0 + length imps in
        let ents = nents@pents in
        let tdefs = concmap gettc ents
        and vdefs = concmap getv ents
        and tuses = oidi (reduce ounion [] (map gettcuse nents)) in
	let (u, closenv) = addnonclosed clsfun tdefs nents u in
((if ImpDebug then strace ("import 1\nnents: "@show_list (\i.primpid i@" ==> "@show_list prid (gettcuse i)) nents@"\ntdefs: "@show_list pprid (xflt tdefs)@"\nvdefs: "@show_list pprid (xflt vdefs)@"\ntuses: "@show_list pprid (xflt tuses))
else \x.x) where xflt l = if Test then l else filter (not o inprelude o getorig) l)
(
        let noorig = filter (\(mkidi _ l _ _).isNone l) tuses in
        let noclose =  (filter (\ (MI m, s).m~=curmod & clsfun s) (odifference (omkset (map getorig tuses)) (omkset (map getorig tdefs)))) in
	let dupids = dupchk eqid mltid vdefs @ dupchk eqid mltid tdefs in
	let dupvis = dupchk eqid oltid vdefs @ dupchk eqid oltid tdefs in
        let badimps = concmap getbadimps (preimps@imps) in
	let badrenames = filter badrename (concmap getrename (preimps@imps)) in
	if Curry then
	    if dupimports imps then
		(u, rnil, "[107] Name clash for imported module names")
	    else if ~Relax & ~null noorig then
		(u, rnil, "[46] Identifiers without original names in interface files: "@mixmap prid noorig ", ")
            else if ~H1_3 & ~Relax & ~null noclose then
		(u, rnil, "[47] Module does not import: "@mixmap (\(m,s).tl (getminame m)@"."@tl s) noclose ", ")
            else if ~Interactive & ~AllowRedef & ~null dupids then
		(u, rnil, "[48] Multiply defined: "@mixmap (\l.mixmap nprid l ",") dupids ";  ")
            else if ~H1_3 & ~AllowRedef & ~null dupvis then
		(u, rnil, "[49] Multiple visible names: "@mixmap (\ss.mixmap prid ss ", ") dupvis "; ")
            else if ~Relax & ~null badimps then
		(u, rnil, "[89] Bad import specifier for: "@mixmap prid badimps ", ")
            else if ~H1_3 & Pedantic & ~null badrenames then
		(u, rnil, "[91] Bad renaming: "@mixmap (\(f,t).prid f@" to "@prid t) badrenames ", ")
            else
(if ImpDebug then strace ("import 2\nnents:\n"@mix (map primpid nents) "\n"@
                         (if Debug then "\npents:\n"@mix (map primpid pents) "\n" else "")) (\x.x) else (\x.x))
(
                let nents' = filter (notthis curmod) nents in
                let (env, u1) = genimpenv (rjoin closenv envp) fixs (pickspec (pents @ dorename (filter renamed tdefs) nents')) u in
                let mdle = rlist Kmodule (map2 (mkidmodule env nents) imps (from u0)) in
		let xenv = rjoin mdle env in
		if ImpDebug then
		    strace ("import 3\n"@show_Renv xenv) (u1, xenv, "")
		else
		    (u1, xenv, "")
)
	else
	    let rec (env1, u1) = genimpenv envp fixs pents u
            and     (env2, u2) = genimpenv envp fixs nents u1
            and     xenv = rjoin env2 env1 in
	    if ImpDebug then
		strace ("import 4\n"@show_Renv xenv) (u2, xenv, "")
	    else
		(u2, xenv, "")
)

and dupimports imps =
    let inames = mkset [ mid ;; (mkimport mid imports fixs oents show fltr rens qual asname) <- imps ]
    and anames = [ aid ;; (mkimport mid imports fixs oents show fltr rens qual (Some aid)) <- imps ]
    in  anysame (inames @ anames)

and mkidmodule env nents (mkimport mid _ _ _ show fltr _ qual _) u =
    let es = map (idtostr o expid) fltr in
    let s = idtostr mid in
    let f = if ~H1_3 then (\i.viamodname i = s)
	    else if qual then (\i.false) 
	    else (\i.viamodname i = s & show = mem (idtostr i) es)
    in  mkid u s (idi_module (concmap (expent f env) nents)) Noorigname

and addnonclosed clsfun tdefs nents u =
    if ~H1_3 then
	(u, rnil)
    else
	let stdefs = omksetby oeqid oltid tdefs in
	let filt l = filter (\ (mkidi _ (Some (_, s)) _ _) . clsfun s) l in
	let ts = filt (odifferenceby oeqid oltid (omksetby oeqid oltid (map hd (groupsort oltid (concmap gettctuse nents)))) stdefs)
	and cs = filt (odifferenceby oeqid oltid (omksetby oeqid oltid (map hd (groupsort oltid (concmap gettccuse nents)))) stdefs) in
	let (its,u') = Umap mtypeid ts u in
	let (ics,u'') = Umap mclassid cs u' in
        let itcs = its @ ics in
--	(if null its then (\x.x) else trace ("addnonclosed "@show_list dprid itcs))
	(u'', rmany Ktype itcs)
and mtypeid (i as (mkidi s (Some mi) _ _)) u =
    let rec ti = mktinfo tn 0 false false [] false false None
	and tn = mktcons ii []
	and ii = mkid u s (idi_type mkkground tn 0 ti [] None) (Orignames Vimported Nonfix mi)
    in  (ii, u+1)
and mclassid (i as (mkidi s (Some mi) _ _)) u =
    let rec cli = clsi (mkkarrow mkkground mkkground) (fail ("mclassid: "@s)) [] [] [] 0
	and ii = mkid u s (idi_class cli) (Orignames Vimported Nonfix mi)
    in  (ii, u+1)

and getrename (mkimport _ _ _ _ _ _ rens _ _) = rens
and badrename (i1, i2) = iscap i1 ~= iscap i2

and frommodule (mkimpid i _ _ _) = modname i
||  frommodule (mkimpsyn _ t _) = modname (tname t)
||  frommodule (mkimptype _ t _) = modname (tname t)
||  frommodule (mkimpview _ t _ _) = modname (tname t)
||  frommodule (mkimpeqtype _ t _ _ _ _) = modname (tname t)
||  frommodule (mkimpclass _ c _ _) = modname (cdef c)
||  frommodule (mkimpinstance (mkidecl _ _ (t._) "_") _ _) = modname (tname t) -- Just a guess
||  frommodule (mkimpinstance (mkidecl _ _ _ m) _ _) = m

and notthis m i = m ~= frommodule i | ~RevVis

and badmodname id = ~AllowRedef & head (length preludename) (idtostr id) = preludename
-- Handle import of Prelude
and importenv (mkids curmod) r2 u imps = 
    let corelib' = if H1_3 then [] else corelib in
    case partition (\(mkimport id _ _ _ _ _ _ _ _).idtostr id = preludename) imps in
	([], _ ) : importe (\x.true) curmod u r2 (corelib'@implib) imps
    ||  (ps, ns) : importe (\x.true) curmod u r2 (imptuples.corelib'@map setprel ps) ns
    end

and setprel (imp as mkimport _ _ _ _ show exps rens qual asname) =
    if H1_3 then imp else
    let (mkimport id imps fixs ents _ _ _ _ _) = hd implib in
    mkimport id imps fixs ents show exps rens qual asname

-- Check if imported stuff is defined, and imported correctly.
-- Not very efficient, but this is the price you pay for naming everything!
and getbadimps (mkimport mid _ _ imps _ specs _ _ _) = concmap (getbadspec (concmap flatids imps)) specs
and getbadspec imps (mkexpid i) =
    case locateimp imps i in
       Some (mkimpid _ _ _ _) : []
    || Some (mkimptype _ _ _) : []
    || Some (mkimpeqtype _ _ _ _ _ _) : []
    || Some (mkimpsyn _ _ _) & (H1_3) : []
    || _ : [i]
    end
||  getbadspec imps (mkexpidall i) =
    case locateimp imps i in
       Some (mkimpeqtype _ _ _ _ _ _) : []
    || Some (mkimpview _ _ _ _) : []
    || Some (mkimpclass _ _ _ _) : []
    || Some (mkimpsyn _ _ _) : []
    || _ : [i]
    end
||  getbadspec imps (mkexpidsome i _) =
    case locateimp imps i in
       Some (mkimpeqtype _ _ _ _ _ _) : []
    || Some (mkimpclass _ _ _ _) : []
    || _ : [i]
    end
||  getbadspec _ (mkexpidmodall i) = [i]
and locateimp [] _ = None
||  locateimp ((i as mkimpid i1 _ _ _)._) i2 & (eqq i1 i2) = Some i
||  locateimp ((i as mkimptype _ t _)._) i2 & (eqq (tname t) i2) = Some i
||  locateimp ((i as mkimpview _ t _ _)._) i2 & (eqq (tname t) i2) = Some i
||  locateimp ((i as mkimpeqtype _ t _ _ _ _)._) i2 & (eqq (tname t) i2) = Some i
||  locateimp ((i as mkimpsyn _ t _)._) i2 & (eqq (tname t) i2) = Some i
||  locateimp ((i as mkimpclass _ c _ _)._) i2 & (eqq (cdef c) i2) = Some i
||  locateimp (_.is) i = locateimp is i

and eqq (mkids (s1 as (_.s1t))) (mkids (s2 as (_.s2t))) = s1 = s2 | isuppm s1t  &
    let rec f ('.'.ss) = ss = s2t
    ||      f (_.s) = f s
    ||      f "" = false
    in f s1t
||  eqq i1 i2 = i1 = i2
and isuppm ('_'.cs) = isuppm cs
||  isuppm (c._) = isupper c
||  isuppm _ = false

and strace s x = if s=s then trace s x else fail "??"
end
