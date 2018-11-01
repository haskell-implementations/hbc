module -- ttype
#include "../funnos.h"
#include "../transform/misc.t"
#include "../transform/hexpr.t"
#include "../expr/types_t.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "id.t"
#include "id_t.t"
#include "ttype_t.t"
#include "constr_t.t"

export prttype, Tint, Tbool, Tchar, Tlist, Tsfloat, Tinteger, Trational, Tdfloat,
        Tarr, Tvar, Tvar1, Tvar2, Typerec, normtype, teqid, getTconstrs,
	Ttuple, tvars, getTvars, flattype, eqtype, tpart, tname, Tstring, gettinfo, cpart,
	lprttype, hprttype, synexpand, tsubst, synexpandall, clsname, prcdecl, pridecl, iclsname, itype,
	xmkcontype, cdecl2type, idecl2type, is_syn, getallTvars, getavars,
        show_Kind, mkkarrows, getkargs, hprcontext, ty2as, as2ty;
rec
    prttype t = if Curry then hprttype t else lprttype t
and lprttype (mktvar v) = lprtvar v
 || lprttype (mktcons i []) = oprid i
 || lprttype (mktap i lt) = "(" @ lprtvar i @ concmap (\x." "@lprttype x) lt @ ")"
 || lprttype (mktcons i lt) =
	case idtostr i in
	    "P->" : "("@lprttype(hd lt)@"->"@lprttype(hd (tl lt))@")"
	||  "_#2" : "("@lprttype(hd lt)@" # "@lprttype(hd (tl lt))@")"
	||  _ : "(" @ oprid i @ concmap (\x." "@lprttype x) lt @ ")"
	end
 || lprttype (mkterror s) = "Error: "@s
 || lprttype (mktcontype ts t) = "("@mixmap lpras ts ", " @ ") => " @ lprttype t
and lpras (mkassert c vs) = oprid c @ " " @ mix (map lprtvar vs) " "
||  lpras (mkaerror e) = "ERROR "@e
and lprtvar v = 
    (if v < 0 then "?" else "")@
    let v = if v < 0 then -v+1 else v in
    '*'.if v < 10 then [chr(ord 'a' + v)] else 't'.itos v
and Tint  = mktcons hiInt []
and Tbool = mktcons hiBool []
and Tchar = mktcons hiChar []
and Tlist t = mktcons hiList [t]
and Tsfloat  = mktcons hiSFloat []
and Tdfloat  = mktcons hiDFloat []
and Tinteger  = mktcons hiInteger []
and Trational  = mktcons hiRational []
and Tarr t1 t2 = mktcons hiARROW [t1; t2]
and Tstring = mktcons hiString []
and Tvar n = mktvar n
and Tvar1 = mktvar 1
and Tvar2 = mktvar 2
and tvars n = for 1 n mktvar
and mktup n = mktcons (hituple n) (tvars n)
and tuples = map mktup (from 2)
and Ttuple n = select (n-1) tuples
and Typerec fv fc fa (mktvar n) = fv n
 || Typerec fv fc fa (mktap v l) = fa v (map (Typerec fv fc fa) l)
 || Typerec fv fc fa (mktcons c l) = fc c (map (Typerec fv fc fa) l)
 || Typerec fv fc fa t = fail ("Typerec:"@prttype t)
and getTvars (mktcontype _ t) = getTvars t			-- There must be no unused type vars in the assertion!
||  getTvars (mkterror _) = []
||  getTvars (mktvar v) = [v]
||  getTvars (mktap v l) = reduce union [v] (map getTvars l)
||  getTvars (mktcons _ l) = reduce union [] (map getTvars l)
and getTconstrs (mktcontype _ t) = getTconstrs t			-- There must be no unused type constrs in the assertion!
||  getTconstrs (mkterror _) = []
||  getTconstrs (mktvar v) = []
||  getTconstrs (mktap _ l) = reduce union [] (map getTconstrs l)
||  getTconstrs (mktcons c l) = reduce union [c] (map getTconstrs l)
and getallTvars (mktcontype _ t) = getallTvars t			-- There must be no unused type vars in the assertion!
||  getallTvars (mkterror _) = []
||  getallTvars (mktvar v) = [v]
||  getallTvars (mktap v l) = v.concmap getTvars l
||  getallTvars (mktcons _ l) = concmap getTvars l
and flattype (mktcons (mkid _ _ (idi_type _ _ _ (mktinfo _ _ _ f _ _ _ _) _ _) _) []) = f
||  flattype _ = false
and dummycon n = mkcons (mkids "?") (false,[],[]) (for 0 (n-1) (\x.(Tvar x,false,None))) false
and eqtype (mktvar n) (mktvar m) = n = m
||  eqtype (mktcons i1 ts1) (mktcons i2 ts2) = teqid i1 i2 & And (map2 eqtype ts1 ts2)
||  eqtype (mktap i1 ts1) (mktap i2 ts2) = i1 = i2 & And (map2 eqtype ts1 ts2)
||  eqtype (mktcontype k1 t1) (mktcontype k2 t2) = 
	length k1 = length k2 & And (map2 eqassert k1 k2) & eqtype t1 t2
||  eqtype _ _ = false
and eqassert (mkassert c1 v1) (mkassert c2 v2) = teqid c1 c2 & v1 = v2
||  eqassert _ _ = false
-- ** LML hack
-- Special eqid test for types.  Since undeclared types are allowed, there may be types that are udef (id = 0)
-- in the prelude.  Allow these to compare equal to another if the strings are equal.
-- ** Haskell Prelude hack
-- Also allow different numbers if we are allowing redefinitions
and teqid (mkid n1 s1 _ _) (mkid n2 s2 _ _) = n1 = n2 & n1 ~= 0 | (n1 = 0 | n2 = 0 | AllowRedef) & s1 = s2
||  teqid i1 i2 = eqid i1 i2

and tpart (mktcontype _ t) = t
||  tpart t = t
and cpart (mktcontype ts _) = ts
||  cpart _ = []
and tname t =
    case tpart t in
	mktcons i _ : i
    ||	mkterror _ : dummyid
    end
and getavars aas = (concmap f aas where f (mkassert _ vs) = vs || f _ = [])

-- Print a Haskell type
and hprttype t = hpr false t
and hpr _ (mktvar v) = hprtvar v
||  hpr _ (mktcons i []) = oprid i
||  hpr np (mktap i ts) = paren np (hprtvar i @ " " @mix (map (hpr true) ts) " ")
||  hpr np (mktcons i ts) =
    let l = length ts in
    case idtostr i in
	"P->" & (l=2) : paren np (hpr true (hd ts) @ " -> " @ hpr false (hd (tl ts)))
    ||	"PPrelude.->" & (l=2) : paren np (hpr true (hd ts) @ " -> " @ hpr false (hd (tl ts)))
    ||  "PList" : "["@hpr false (hd ts)@"]"
    ||  "_[]" : "["@hpr false (hd ts)@"]"
    ||  "_Prelude.[]" : "["@hpr false (hd ts)@"]"
    ||  "_List" & (~Curry) : "["@hpr false (hd ts)@"]"
    ||  '_'.'#'.s & (stoi s = l) : "("@mix (map (hpr false) ts) ", " @")"
    ||  'P'.'#'.s & (stoi s = l) : "("@mix (map (hpr false) ts) ", " @")"
    ||  'P'.'P'.'r'.'e'.'l'.'u'.'d'.'e'.'.'.'#'.s & (stoi s = l) : "("@mix (map (hpr false) ts) ", " @")"
--    ||  '_'.c.cs & (~isalpha c) : paren np (hpr true (hd ts) @ " " @ c.cs @ " " @ hpr false (hd (tl ts)))
    ||  _ : paren np (oprid i @ " " @mix (map (hpr true) ts) " ")
    end
||  hpr _ (mktcontype ts t) = hprcontext ts @ hpr false t
||  hpr _ (mkterror s) = "mkterror "@s
and hpras (a as mkassert c v) = --oprid c @ " " @ hprtvar v
        hpr false (as2ty a)
||  hpras (mkaerror e) = "ERROR "@e
and hprcontext ts = "("@mixmap hpras ts ", " @ ") => "
and hprtvar v = 
    (if v < 0 then "?" else "")@
    let v = if v < 0 then -v-1 else v in
    if v < 26 then [chr (ord 'a' + v)] else 'a'.itos v
and paren true s = "("@s@")"
||  paren false s = s
and gettinfo (mkid 0 _ _ _) = mktinfo (fail "gettinfo Ttype") 0 false false [] false false None
||  gettinfo (mkid _ _ (idi_type _ _ _ ti _ _) _) = ti
||  gettinfo (mkid _ _ (idi_view _ _ (idi_type _ _ _ ti _ _)) _) = ti
||  gettinfo i = fail ("gettinfo on "@prid i@" ("@itos (id_no i)@")")
-- Expand top level type synonym.
and synexpand (mktcons (mkid _ _ (idi_syn _ src _ dst) _) ts) =
    let (mktcons _ vs) = tpart src in
    let al = map2 (\(mktvar n).\t.(n,t)) vs ts in
    tsubst al dst
||  synexpand t = t
and is_syn (mktcons (mkid _ _ (idi_syn _ _ _ _) _) _) = true
||  is_syn _ = false
-- Expand all type synonyms
and synexpandall t =
    case while is_syn synexpand t in
	(t' as (mktvar _)) : t'
    ||  mktap v ts : mktap v (map synexpandall ts)
    ||  mktcons i ts : mktcons i (map synexpandall ts)
    ||  mkterror _ : t
    ||  mktcontype k t : mktcontype k (synexpandall t)
    end
and tsubst al (v as mktvar n) = assocdef n al v
||  tsubst al (mktcons i ts) = mktcons i (map (tsubst al) ts)
||  tsubst al (mktap n ts) =
        let ts' = map (tsubst al) ts
	in  case assocdef n al (mktvar n) in
	       mktvar n' : mktap n' ts'
	    || mktcons i ts : mktcons i (ts@ts')
	    || mktap n ts : mktap n (ts@ts')
	    || t : fail ("tsubst ap "@prttype t)
	    end
||  tsubst al (mktcontype k t) = mktcontype (map (ksubst al) k) (tsubst al t)
||  tsubst _ t = fail ("tsubst "@prttype t)
and ksubst al (mkassert c vs) = mkassert c [ n ;; v <- vs; (mktvar n) <- [assocdef v al (mktvar v)] ]
||  ksubst al a = a

and clsname (mkcdecl _ (mkassert ci _)) = ci
and iclsname (mkidecl _ ci _ _) = ci
and itype (mkidecl _ _ [t] _) = t -- ASSERT
and prcdecl c = prttype (cdecl2type c)
and pridecl (i as mkidecl _ _ _ m) = prttype (idecl2type i)@"{-#FROMMODULE "@tl m@"#-}"
and cdecl2type (mkcdecl ks (a as mkassert ci v)) = xmkcontype ks (as2ty a)
||  cdecl2type (mkcdecl _ (mkaerror s)) = mkterror s
and idecl2type (mkidecl ks ci ts _) = xmkcontype ks (mktcons ci ts)
and xmkcontype [] t = t
||  xmkcontype k t = mktcontype k t

-- Make type vars go from 0 and up
and normtype t =
	let tvs = getTvars t in
        let al = combine (tvs, from 0) in
	TRtype' al t
and TRtype' ss (mktcontype k t) = mktcontype (map (TRcon ss) k) (TRtype' ss t)
||  TRtype' ss (mktcons i ts) = mktcons i (map (TRtype' ss) ts)
||  TRtype' ss (mktvar v) = mktvar (assocdef v ss v)
||  TRtype' ss (mktap v ts) = mktap (assocdef v ss v) (map (TRtype' ss) ts)
||  TRtype' ss t = fail ("Bad TRtype' "@prttype t)
and TRcon ss (mkassert ci vs) = mkassert ci (map (\v.assocdef v ss v) vs)

---- Kind stuff
and mkkarrows [] k = k
||  mkkarrows (k.ks) k' = mkkarrow k (mkkarrows ks k')
and getkargs (mkkarrow k1 k2) = k1 . getkargs k2
||  getkargs _ = []
and show_Kind k = showk false k
and showk p mkkground = "*"
||  showk p (mkkarrow k1 k2) = (if p then "(" else "")@showk true k1@" -> "@showk false k2@(if p then ")" else "")
||  showk p (mkkvar i) = 'v'.itos i

and ty2as (mktcons c tvs) = mkassert c (map (\ (mktvar v).v) tvs)
and as2ty (mkassert c vs) = mktcons c (map mktvar vs)
||  as2ty (mkaerror s) = mkterror s
end
