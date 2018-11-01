module -- rename
--
-- This module does the renaming.  The renaming is the process in which
-- every identifier gets a unique name.  This greatly simplifies further
-- transformations since there will be no risk of name clashes after the
-- renaming.
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../expr/tinfo.t"
#include "../expr/types_t.t"
#include "../expr/impexp_t.t"
#include "../expr/idtab.t"
#include "../expr/impexp.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../expr/constrfun.t"
#include "../expr/pragma_t.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../transform/cutil.t"		/* idtoconstr */
#include "../transform/misc.t"
#include "../transform/lettrans.t"
#include "../transform/hexpr.t"		/* hiundefined */
#include "import.t"
#include "renenv.t"
#include "renamedef.t"
#include "renameutil.t"
#include "renametype.t"
#include "buildclass.t"
#include "buildinsts.t"
#include "classutil.t"
#include "deriv.t"
#include "kind.t"

#include "../main/topexprs.t"

export rename, renexpr, renpat, expandrecord;
rec
    renexpr fid ff env u e = renexpro false fid ff env u e
and
    renpat  fid ff env u e = renexpro true  fid ff env u e
and
    renexpro ispat fid ff env u e =
	let renlex u (mklt c) = (u, mklt c)
        ||  renlex u (mkltint c) = (u, mkltint c)
        ||  renlex u (mkltid c) = (u, mkltid c)
        ||  renlex u (mkltsym c) = (u, mkltsym c)
	||  renlex u (mkunq e) =
			  let (u1,ne) = renexpro ispat fid ff env u e
			  in (u1 , mkunq ne)
	in
	case e in
	   mkap f a :
	    let (u1, fn) = renexpro ispat fid ff env u f in
	    let (u2, an) = renexpro ispat fid ff env u1 a in
	    (u2, mkap fn an)
	|| mklam (mkident i as (mkids s)) e1 & (~isconstri env i & ~isdummy i) :
            let rec ii = mkid u s idi_varu (ff ii) in
	    let (u1, en) = renexpro ispat fid ff (rjoin1 env Kvalue ii) (u+1) e1 in
	    (u1, if Pedantic & badid ii then mkerror ("[4] Bad identifier in pedantic mode: "@prid ii) else mklam (mkident ii) en)
	|| mklam e1 (mkinfo stmtpat e2) :
	    let nid = mkident (mkids "I") in
            let def = [ (dollar, mkinfo (stmtfailpoint e1) 
                                        (mkident (mkpids "_zero"))) ] in
	    renexpro ispat fid ff env u (mklam nid (mkcase nid ([(e1, e2)]@def) ))
        || mkinfo (stmtfailpoint e1) e2 :
	    let (u1, (e1', e2')) = renpb fid ff env u (e1, e2) in
	    (u1, mkinfo (stmtfailpoint e1') e2')
	|| mklam e1 e2 :
	    let nid = mkident (mkids "I") in
	    renexpro ispat fid ff env u (mklam nid (mkcase nid [(e1, e2)]))
	|| mkcase exp casel :
	    let (u1, e1) = renexpro ispat fid ff env u exp in
	    let (u2, cl) = mapstate (renpb fid ff env) u1 casel in
	    (u2, mkcase e1 cl)
	|| mkletv def exp :
	    let (u1, defn, defs) = rendef fid ff env u def in
	    let (u2, en) = renexpro ispat fid ff (rjoin defs env) u1 exp in
	    (u2, mkletv defn en)
	|| mkident i & (isdummy i) : (u, dollar)
	|| mkident (mkids s) : (u, findid s env mkident)
	|| mkident _ : (u, e)	-- already renamed, from FLIC
	|| mkconst _ : (u, e)
	|| mkcfunction _ _ : (u, e)
        || mkbrack [] llex  :
			 let (u1,nllex) = mapstate renlex u llex
			 in (u1, mkbrack (rgetcenv env) nllex)
			  --  the grammarlist should be empty from Read

	|| mkas (mkids s) e :
	    let (u1, e1) = renexpro ispat fid ff env u e in
	    (u1, findid s env (\i.mkas i e1))
	|| mkcondp p c :
	    let (u1, pn) = renexpro ispat fid ff env u p in
	    let (u2, cn) = renexpro ispat fid ff env u1 c in
	    (u2, mkcondp pn cn)
	|| mklazyp p:
	    let (u1, pn) = renexpro ispat fid ff env u p in
	    (u1, mklazyp pn)
	|| mkinfo (restr vs t) e :
	    let (u1, e1) = renexpro ispat fid ff env u e in
	    (u1, mkinfo (restr vs (rentype env t)) e1)
	|| mkinfo (spark is) e :
	    let (u1, e1) = renexpro ispat fid ff env u e in
	    (u1, mkinfo (spark (map (\(mkids s).rfind Kvalue s env) is)) e1)
	|| mkinfo t e :
	    let (u1, e1) = renexpro ispat fid ff env u e in
	    case (t, e1) in
	       (position f l, mkident i) :
	           if eqid i hiundefined then
		       let f' = if Interactive then "<interactive session>" else f in
		       (u1, mkap (mkap (mkident hi_undefined) (mkconst (cstring f'))) (mkconst (cint l)))
		   else
		       (u1, mkident i)
	    || _ : (u1, mkinfo t e1)
            end
	|| mklistg e qs :
	    let (u1, e1, qs1) = renq fid ff env [] e u qs in
	    (u1, mklistg e1 qs1)
        || mkwhere ges def :
	    let (u1, defn, defs) = rendef fid ff env u (mkbrec def) in
            let envn = (rjoin defs env) in
	    let (gso, eso) = split ges in
	    let (u2, gs) = mapstate (renexpro ispat fid ff envn) u1 gso in
            let (u3, es) = mapstate (renexpro ispat fid ff envn) u2 eso in
	    (u3, mkwhere (combine (gs, es)) defn)
	|| mkrecord c ies _ :
	    let	(is, es) = split ies in
	    let	(u', es') = mapstate (renexpro ispat fid ff env) u es in
	    let (u'', c') = renexpro ispat fid ff env u' c in
	    (u'', 
	    case reduce (\ (mkids s) . \ (es, is) . 
			 case rfindselid s env in 
			    mkids _ : (mkerror ("[99] Unknown field identifier: "@tl s).es, is)
			 || i : (es, i.is) 
			 end) ([], []) is in
               ([], is') : expandrecord ispat c' is' es'
	    || (e._,_) : e
	    end
	    )
	-- mkmodule, mkerror, mkconstr, mkfailmatch cannot occur
	end

and expandrecord ispat c is es =
    if anysameeq eqid is then mkerror ("[101] Duplicate fields: "@mix (map oprid is) ", ") else
    let ies = combine (is, es) in
        case c in
	   mkident (mkid _ _ (idi_constr tt _ tbis _ _ flg _) _) & (flg) :
	       let cfs = [ (i,b) ;; (t,b,Some i) <- tbis ]
	       and rfs = is
	       and udef i b = if ispat then dollar 
			      else if b then mkerror ("[108] Uninitialized strict field: "@prid i)
			      else mkap (mkident hifail) (mkconst (cstring ("undefined field "@prid i)))
	       in  --trace (show_list dprid cfs @ show_list dprid rfs)
		   case diffeq eqid rfs (map fst cfs) in
		      [] : mkapl c (map (\ (i,b) . assocdefeq eqid i ies (udef i b)) cfs)
		   || is : mkerror ("[98] Extra/duplicate fields: "@mix (map oprid is) ", ")
		   end
	|| _ : if ispat then mkerror ("[97] Not a record constructor: "@ppr c) else
	    -- this is the update function
            let ts = [ t ;; (mkid u s (idi_var ii ot (Some t)) x) <- is]
	    and msg = ppr (mkrecord c ies [])
	    in if length ts ~= length ies | not (allsameeq eqid ts) then
		   mkerror ("[111] Bad fields in record update: "@msg)
	       else if length ts = 0 then
		   mkerror ("[106] Empty record update")
	       else
                   let is_record (mkcons _ _ tbis f) = f & null (diffeq eqid is [ s ;; (t,b,Some s) <- tbis ]) in
		   let (mkid _ _ (idi_type _ _ _ ti _ _) _ . _) = ts in
		   case filter is_record (get_cs_from_tinfo ti) in
		      [] : mkerror ("[100] No updateable constructors: "@msg)
		   || cs : mkrecord c ies cs
		   end
	end
and mkapl s es = reduce (\a.\f.mkap f a) s (reverse es)

and renq fid ff env xs e u [] =
        let (u1, e1) = renexpr fid ff env u e in
	(u1, e1, reverse xs)
||  renq fid ff env xs e u (mkqfilter f.qs) =
        let (u1, e') = renexpr fid ff env u f in
	renq fid ff env (mkqfilter e'.xs) e u1 qs
||  renq fid ff env xs e u (mkqlet b.qs) =
	let (u1, b', defs) = rendef fid ff env u b in
	let env' = rjoin defs env in
	renq fid ff env' (mkqlet b'.xs) e u1 qs
||  renq fid ff env xs e u (mkqgen p g.qs) =
	let vl = filter (not o isconstri env) (getids p) in
	let vle = rjoin (etag ff vl u idi_varu) env in
	let (u1, p') = renpat  fid ff vle (u+length vl) p in
	let (u2, e') = renexpr fid ff env u1 g in
	renq fid ff vle (mkqgen p' e'.xs) e u2 qs

and isplus (mkids "_+") = true
||  isplus _ = false
and renpb fid ff env u (p, e) =
	let vl = filter (\i.~(isconstri env i | NPlusK&Curry&isplus i)) (getids p) in	-- I hate n+1 patterns!
        let env' = etag ff vl u idi_varu in
	let vle = rjoin env' env in
	let (u1, pn) = renpat  fid ff vle (u+length vl) p in
	let (u2, en) = renexpr fid ff vle u1 e in
	if Pedantic then
	    case filter badid (rids Kall env') in
		[] : (u2, (pn, en))
	    ||  is : (u1, (pn, mkerror ("[5] Bad identifiers in pedantic mode: "@mixmap oprid is ", ")))
	    end
	else
	    (u2, (pn, en))

and rename u (mkmodule i fixs impl exp def) =
	    let rec (u0, nienv, _) = importenv (mkids "") ienv u [mkimport (mkids "_lmlimports") [] [] impl false [] [] false None]
	    and     ienv = rperm nienv
	    and     expl = case exp in Some es : es || None : xgetexps menv end
	    and     (u1, defn, menv) = rendef dummyid (mkffv i fixs (map expid nexpl)) ienv u0 def
	    and     exps = map (idtostr o expid) expl
	    and     (errl, nexpl) = renexplist exps menv in
	    let ne = mkmodule i fixs [] (Some nexpl) defn in
	    (u1, [], ienv, [], "", 
	     (if null errl then
		ne
	     else -- we mustn't hide any errors, so make an appl.
		mker ne (concmap (\x.x@"\n") errl)
	     )
	    )
||  rename u (mkhmodule id oexps imps fixs b) =
            let rec (u0, nienv, imperrs) = importenv id (rsetct (rjoin xmenv ienv) cts) u imps
	    and     ienv = raddpragma (rperm nienv) pragmas
            and     ienvi = addmets ienv
            and     xmenv = if RevVis then menv else rnil
	    and     exps = let allexps = xgetexps menv in case oexps in Some es : concmap (replcurmod id allexps) es || None : allexps end
	    and     ff = mkffv id fixs (map expid nexpl)
            and     (u1, b', menv) = rendef dummyid ff (raddkt ktable (rsetct ienvi cts)) u0 (if NoToprec then b else mkbrec b)
	    and     nenv = rjoin denv (rjoin menv ienv)	-- derived, module, and imported env
	    and     (errl, nexpl) = hrenexplist exps nenv
	    and     (kerrs, ktable) = if HigherKind then computekinds b' else ([], [])
	    and     ne = mkmodule id [] [] (Some nexpl) bbb
	    and     dbl = dbldefs menv ienv
	    and     (css, circs) = buildsuper nenv
	    and     (eder, denv, db, u21) = solvederiv AutoDerive ff menv ienvi rnil u1
	    and     (cts, instmsgs) = buildinsts css nenv
            and     (bbb, pragmas0) = extractpragmas (mkband b' db)
	    and     (pragmas, u2) = Umap renpragma pragmas0 u21
	    and     badders = getbadders b'
            in 
--trace (show_list (\ (i, k) . prid i@":"@show_Kind k) ktable)
--trace (show_Renv ienvn0)
--trace (show_list (\x.x) (rstrs Ktype menv))
--trace ("u1="@itos u1@",u2="@itos u2)
	    (u2, filter id_isinst (rids Kmeth nenv), ienvi, 
	     pragmas, idtostr id,
	     if badmodname id then
		 mkerror ("[6] Illegal module name: "@oprid id)
	     else if imperrs ~= "" then
		 mkerror imperrs
	     else if dbl ~= [] then
		 mkerror ("[8] Redefinition of imported entity: "@mix dbl ", ")
	     else if badders ~= [] then
		 mkerror (hd badders)
	     else if null errl & eder ~= [] then
		 mkerror ("[7] Bad deriving for: "@mix eder ", ")
	     else if circs ~= [] then
		 mkerror ("[9] Circular class structure "@mix circs ", ")
	     else if errl~=[] then
		 mker ne (concmap (\x.x@"\n") errl)
             else if instmsgs ~= [] then
		 mkerror (mix instmsgs "\n")
	     else if kerrs ~= [] then
		 mkerror (mix kerrs "\n")
	     else
		 ne)

-- we mustn't hide any errors, so make an appl.
and mker ne msg = mkap (mkerror msg) ne

-- Replace export of current module in export list with all top level entities.
and replcurmod modid allexps (mkexpidmodall i) & (eqid modid i) = allexps
||  replcurmod _ _ e = [e]

and xgetexps env = concmap xexp (filter (\i.let (c.cs) = idtostr i in c = '_' & ~null cs) (rids Kall env))
and xexp (i as mkid _ _ (idi_var _ _ None) _) = [mkexpid i]
||  xexp (i as mkid _ _ (idi_type _ _ _ _ _ _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_syn _ _ _ _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_class _) _) = [mkexpidall i]
||  xexp (i as mkid _ _ (idi_view _ _ _) _) = [mkexpidall i]
||  xexp _ = []

-- Check for multiple definitions in the symbol table
and dbldefs nenv env = 
    if AllowRedef then [] 
    else map tl (rdbldefs Kvalue nenv env @ rdbldefs Ktype nenv env)

and renpragma :: (Id#(List Pragma)) -> Int -> ((Id#(List Pragma)) # Int)
and renpragma (i, ps) u = 
	let (ps', u') = Umap (renpragma1 i) ps u
	in  ((i, conc ps'), u')
and renpragma1 i (Pspecialize _ ts) u =
	case i in
	   mkid _ s info (Orignames vis fix (mi,_)) :
	     let ps = map2 (\ (t, oi) .\u.
		let s' = specname s t in
		let i' = mkid u s' info (Orignames vis fix (mi,s')) in
		Pspecialize i' [(t, oi)]) ts (from u)
	     in (ps, u+length ts)
	end
||  renpragma1 _ p u = ([p], u)

and getbadders (mkbrec b) = getbadders b
||  getbadders (mkband b1 b2) = getbadders b1 @ getbadders b2
||  getbadders (mkblocal b1 b2) = getbadders b1 @ getbadders b2
||  getbadders (mkberror (s as ('['.'2'.'9'.']'._))) = [s]
||  getbadders _ = []
end

