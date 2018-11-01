module -- rename
--
-- This module contains various functions used in rename.

#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/error.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/tinfo.t"
#include "../expr/pprint.t"
#include "../expr/pragma_t.t"
#include "../expr/booltree.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../misc/sort.t"
#include "renenv.t"
#include "../transform/misc.t"
#include "../main/topexprs.t"		/* curmod */

export sdef, mid, newidenti, isconstri, ismulti, addc, getcs,
	etag, findid, getimpid, renexplist, listify, flatsyns, iscap, fixtype,
	isexpid, impid, isimpimport, istypeish, isctype, isval, isclass, hrenexplist, 
	mkff, mkffv, buildinstid, cliof, badid, badidq, defstr, mkcompound, mkcompprel,
        hasext, extractpragmas, specname, setpref, qhprttype, qfixid, qfixcontext;
rec

    sdef i e = mkbpat [(mkident i, e)]
and mid s (u, i) = mkid u s i
and newidenti i n = mkident (mknewids "T" n (idtostr i))
and isconstri env (mkids s) = 
	case rfind Kvalue s env in
	    mkid _ _ (idi_constr _ _ _ _ _ _ _) _ : true
	||  _ : false
	end
||  isconstri env i = id_isconstr i
and setpref p (c.s) = c.p@s
and ismulti cp p =
	case leftmost p in
	    mkconst _ : true
        ||  mkident i : cp i
	||  mkas _ _  : true
	||  mkinfo _ p : ismulti cp p
        ||  mkbrack _ _ : true 
	||  mkrecord _ _ _ : true
        ||  e : fail ("Weird pattern "@ppr e)
	end
and addc cs cp x = mem x cs | cp x
and getcs (mkbrec d) = getcs d
||  getcs (mkband d1 d2) = getcs d1 @ getcs d2
||  getcs (mkblocal d1 d2) = getcs d2
||  getcs (mkbtype _ ats _ _) = map (\(mkcons c _ _ _).c) ats
||  getcs (mkbview _ _ ats _) = map (\(mkcons c _ _ _).c) ats
||  getcs _ = []
and etag ff l u i = (rlist Kvalue (map2 tagid l (from u))
	where tagid (mkids s) n = let rec ii = mkid n s i (ff ii) in ii
	||    tagid (i as mkid _ s _ _) _ = i)
and getimpid (mkimpid i _ _ _) = i

-- Methods and method vectors are in a special pile.
and kof ('M'.'M'._) = Kmeth
||  kof ('V'.'V'._) = Kmeth
||  kof _ = Kvalue
and kof' s = if Curry then kof s else Kvalue
and findid s env cont =
	    case rfind (kof' s) s env in
	       mkid 0 _ _ _ : mkerror("[33] Undefined identifier " @ tl s)
	    || id : cont id
	    end

and renexplist expl denv = reduce (\s.\(er, ex).
	case rfind Kall s denv in
	    mkid 0 _ _ _ : (("[85] Export id "@tl s@" not defined").er, ex)
	||  (id as mkid no _ (idi_type _ _ _ ti _ _) _) :
		let cs = map (\(mkcons i _ _ _).mkexpid i) (get_cs_from_tinfo ti) in
		(er, mkexpid id.cs @ ex)
	||  id : (er, mkexpid id.ex)
	end)
	([], [])
	expl
and hrenexplist expl denv =
    let cs = 
        map
        (\e.
	case e in
	    mkexpid s : chk mkexpid [Ktype; Kvalue] s denv
        ||  mkexpidall s :
	    case chk mkexpidall [Ktype] s denv in
	        Yes [mkexpidall (ei as mkid u s (idi_type k t m (mktinfo tt n nf ff ats ef iso vw) ids ois) on)] :
                    let sels = map mkexpid (mkseteq eqid [ s ;; (mkcons x1 x2 tbis x4) <- ats;
                                                              (t,b,Some s) <- tbis ]) in
		    Yes (mkexpidall ei . sels)
            ||  x : x
            end
	||  mkexpidsome s is :
	    case chk mkexpidall [Ktype] s denv in
	        Yes [mkexpidall (mkid u s (idi_type k t m (mktinfo tt n nf ff ats ef iso vw) ids ois) on)] :
		    let ss = map idtostr is in
                    let fixsel (t,b,Some s) = (t,b,Some (uuname ss s))
                     || fixsel x = x in
		    let ats' = [mkcons (uuname ss i) ii (map fixsel tbis) b ;; (mkcons i ii tbis b) <- ats] in
                    let sels = map mkexpid (mkseteq eqid [ s ;; (mkcons x1 x2 tbis x4) <- ats';
                                                              (t,b,Some s) <- tbis; mem (idtostr s) ss ]) in
		    Yes (mkexpidall (mkid u s (idi_type k t m (mktinfo tt n nf ff ats' ef iso vw) ids ois) on) . sels)
	    ||  Yes [mkexpidall (mkid u s (idi_class (clsi k cd iits cs insts n)) on)] :
		    let ss = map idtostr is in
		    let iits' = [(d,uuname ss i,t) ;; (d,i,t) <- iits] in
		    Yes [mkexpidall (mkid u s (idi_class (clsi k cd iits' cs insts n)) on)]

	    ||  x : x
            end
	||  mkexpidmodall s : chk mkexpidmodall [Kmodule] s denv
	end)
	expl in
    let (es, is) = reduce clsfy ([],[]) cs
    in  (es, is)
and uuname ss (i as mkid u s ii on) = 
	if mem s ss then
	    i
	else
	    mkid u ("__"@s) ii on
and clsfy (Yes x) (e,i) = (e, x @ i)
||  clsfy (No s)  (e,i) = (s.e, i)
and chk f [] s env = No ("[53] Export id "@oprid s@" not defined")
||  chk f (k.ks) s env = 
        case rfind k (idtostr s) env in
	    mkid 0 _ _ _ : chk f ks s env
        ||  i : Yes [f i]
        end

and listify (mkband b1 b2) = listify b1 @ listify b2
||  listify (mkbnull) = []
||  listify b = [b]
and flatsyns ss = concmap (\(mkbsign ids t).map (\id.(id, t)) ids) ss
and isexpid (mkexpid _) = true
||  isexpid _ = false
and impid (mkimpid i _ _ _) = i
||  impid (mkimptype _ t _) = tname t
||  impid (mkimpview _ t _ _) = tname t
||  impid (mkimpeqtype _ t _ _ _ _) = tname t
||  impid (mkimpsyn _ t _) = tname t
||  impid (mkimpclass _ (mkcdecl _ (mkassert ci _)) _ _) = ci
||  impid (mkimpinstance (mkidecl _ ci _ _) _ _) = ci
||  impid (mkimpimport i _ _) = i
and isimpimport (mkimpimport _ _ _) = true
||  isimpimport _ = false
and istypeish (mkimptype _ _ _) = true
||  istypeish (mkimpeqtype _ _ _ _ _ _) = true
||  istypeish (mkimpsyn _ _ _) = true
||  istypeish (mkimpview _ _ _ _) = true
||  istypeish _ = false
and isclass (mkimpclass _ _ _ _) = true
||  isclass _ = false
and isctype (mkimpctype _ _) = true
||  isctype _ = false
and isval (mkimpid _ _ _ _) = true
||  isval _ = false
-- Inefficient because there is no full lazyness!!
and mkff v (mkids m) fixs id = 
    let s = idtostr id in
    Orignames v
              (assocdef s (concmap (\(mkfixid ids f).map (\i.(idtostr i, f)) ids) fixs) Nofixity) 
              (MI m, s)
and mkffv (mkids m) fixs exps id = 
    let s = idtostr id in
    Orignames (if member eqid id exps then Vexported else Vprivate)
              (assocdef s (concmap (\(mkfixid ids f).map (\i.(idtostr i, f)) ids) fixs) Nofixity) 
	      (MI m, s)

#define TR(x)
-- The idi_inst id is used to collect info about instances and also serves as the id of the instance
-- method vector.  It has to contain the names of all the instance operators
-- reserve numbers for both methods, and functions in the method vector.
and buildinstid ff u (mkidecl k ci [it] ms) tn flg loc fs = -- ASSERT
    let u = u+2 + length k in
    let nci = iclsname tn
    and (mktcons nti _) = itype tn in
    let vis = if loc then if id_is_visible nci | id_is_visible nti then Vexported else Vprivate else Vimported in
    let vecs = 
	case tn in
	   mkidecl _ ci [it] _ : vecstr ci it -- ASSERT
	end in
    let modname = if ms="_" then fst (id_orignames (tname it)) else MI ms in
    let on = ff (mkidi vecs (Some (modname, vecs)) "??????" [""]) in
    let ii = (mkid u vecs (idi_inst tn (bldmets vis tn (u+1) on fs) flg) on) in 
    (updvis vis ii, u+length (cliof tn)+1)

and ar (finfo _ x y z w) = finfo 1 x y z w
and bldmets vis t u (Orignames _ f (mi,xxxx)) fs =
TR(trace ("bldmets "@xxxx)) (
    let fi s t' k t m =
	if UseForceArity then
	    let a = countarrows t + length k in
	    case fs m a in
	    finfo _ _ st frs oi : 
		    finfo a [] st frs oi
	    end
	else f_unk in
    let! (mkidecl k ci [it] _) = t in -- ASSERT
    map2 (\(d,m,t0).\u.let s = methstr ci it m in
		      let t = let vs = getTvars t0 in let tr = combine (vs, map Tvar (from 500)) in tsubst tr t0 in
		      let! (mktcontype (mkassert _ [tv].xs) xtt) = t in -- ASSERT
		      let tt = xmkcontext xs xtt in
		      let tr = tsub tv it tt in
		      TR(trace("bldmethod "@s))
	              mkid u s (idi_var (var_global (fi s t k tr m)) 
			       (Ohastype tr (getTvars tr) None) None) 
			       (Orignames vis f (mi, s))) 
	 (cliof t) (from u)
)

and xmkcontext [] t = t
||  xmkcontext ts t = mktcontype ts t

and tsub v tr (mktcontype ts t) = mktcontype ts (tsub v tr t)
||  tsub v tr t = 
    let f v' ts' = if v ~= v' then mktap v' ts' else
	    case tr in
	       mktvar n' : mktap n' ts'
	    || mktcons i ts : mktcons i (ts@ts')
	    || mktap n ts : mktap n (ts@ts')
	    || t : fail ("tsub ap "@prttype t)
	    end
    in  Typerec (\x.if x = v then tr else mktvar x) mktcons f t
and cliof (mkidecl _ (mkid _ _ (idi_class (clsi _ _ cli _ _ _)) _) _ _) = cli
||  cliof _ = []		-- Can happen during erroneous declarations.
--||  cliof t = fail ("cliof "@pridecl t)
and idstrof i = 
    let ss = 
        case id_orignames i in
            (_, "") : idtostr i
        ||  (_, s)  : s
        end
    in
    case ss in
	'P'.s : '$'.s		-- not to confuse e.g. PList with _List
    ||  _  .s : s
    end
and qidstrof i = if ~H1_3 then idstrof i else tl (idtostr (qfixid "?" i))
and vecstr ci t = "VV"@compdelim@qidstrof ci@compdelim@flatstr (synexpandall t)
and methstr ci t oi = "MM"@compdelim@qidstrof ci@compdelim@flatstr (synexpandall t)@compdelim@idstrof oi
and defstr ci s = "DD"@compdelim@qidstrof ci@compdelim@idstrof s
and flatstr (mktcons ti ts) = qidstrof ti @ concmap (\t.'~'.flatstr t) ts
||  flatstr (mktap _ ts) = "a" @ concmap (\t.'~'.flatstr t) ts
||  flatstr (mktvar _) = "a"

and mkcompound xs = mix xs compdelim
and compdelim = if H1_3 then "`" else "."
and mkcompprel s = if H1_3 then "Prelude."@s else s

-- Check if an id conforms to the stupid Haskell rules.
and badid i = normalid i & iscap i ~= needcap i
and badidq (mkid u s x y) = badid (mkid u (dropqual s) x y)
||  badidq i = badid i
and normalid (mkid _ ('_'._) _ _) = true
||  normalid _ = false
and iscap i =
    let (_.c._) = idtostr i
    in  c = ':' | isupper c | 192 <= ord c & ord c <= 222 & c ~= '×'
and needcap (mkid _ _ (idi_var _ _ _) _) = false
||  needcap (mkid _ _ (idi_method _ _ _) _) = false
||  needcap _ = true

and hasext cs = exists extvar cs
and extvar (mkcons _ (_,vs,_) _ _) = ~ null vs

and extractpragmas :: Binding -> (Binding # (List (Id#(List Pragma))))
and extractpragmas b = 
    case extps b in
    (b', ps) : (b', sortbyid ps)
    end
and extps (mkband b1 b2) = 
    let (b1', ps1) = extps b1
    and (b2', ps2) = extps b2
    in  (mkband b1' b2', ps1@ps2)
||  extps (mkblocal b1 b2) = 
    let (b1', ps1) = extps b1
    and (b2', ps2) = extps b2
    in  (mkblocal b1' b2', ps1@ps2)
||  extps (mkbrec b) = let (b', ps) = extps b in (mkbrec b', ps)
||  extps (mkbpragma p) = (mkbnull, [p])
||  extps b = (b, [])
and sortbyid = map (\l.(fst (hd l), map snd l)) o groupsort (\(x,_).\(y,_).ltid x y) o concmap addid
and addid (p as Pspecialize i ts) = [(i, p)]
||  addid (Pspecinst _) = []

and remsp (c1.' '.c2.cs) & (~isalnum c1 | ~isalnum c2) = c1.remsp (c2.cs)
||  remsp (c.cs) = c.remsp cs
||  remsp "" = ""
and specname (c.s) t = 
--let r = 
c . remsp (qhprttype "?" (normtype (synexpandall t))) @ "`" @ s
--in  trace ("specname "@(c.s)@"::"@hprttype t@"='"@r@"'") r

and qhprttype mod t = hprttype (fixtype mod t)
and fixtype mod t = if ~H1_3 then t else
    let rec f (mktcons i ts) = mktcons (qfixid mod i) (map f ts)
	||  f (mktap n ts) = mktap n (map f ts)
	||  f (mktcontype ks t) = mktcontype (qfixcontext mod ks) (f t)
	||  f t = t
    in  f t
and qfixcontext mod ks = if ~H1_3 then ks else map (\ (mkassert i v) . (mkassert (qfixid mod i) v)) ks
and qfixid mod (i as mkid n olds ii on) = if ~H1_3 then i else
  case olds in
     'P'.'P'.'r'.'e'.'l'.'u'.'d'.'e'.'.'.'#'.s : mkid n ('P'.'#'.s) ii on
  || "PPrelude.->" : mkid n "P->" ii on
  || "_Prelude.()" : mkid n "_()" ii on
  || "_Prelude.[]" : mkid n "_[]" ii on
  || _ :
	    let s =
		case on in
		   Orignames _ _ mi : qfixmi mod olds mi
		|| Noorigname : fail ("qfixid "@olds) --curmod@"."@olds
		end
	    in  mkid n s ii on
  end
||  qfixid _ (mkids (_.s)) = trace ("qfixid "@cmod@"."@s) mkids (cmod@"."@s)
||  qfixid mod (mkidi olds (Some mi) m ps) =
    mkidi (qfixmi mod olds mi) (Some mi) m ps
and cmod = if Prelude then preludename else curmod
and qfixmi mod olds mi =
    case mi in
       (_, _.s as (c._)) & (inprelude mi & (s = "->" | s = "[]" | s = "()" | c = '#' | Prelude & mod ~= "?")) : /*trace ("qfixmi1 "@olds)*/ olds
    || (MI m, _) & (m = mod) : /*trace ("qfixmi2 "@olds)*/ olds
    || (MI m, _.s) : 
	if inprelude mi then preludename@"."@s else /*trace ("qfixmi3 "@m@"."@s)*/ m@"."@s
    end
end
