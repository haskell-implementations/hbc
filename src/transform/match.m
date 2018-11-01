module -- match
--
-- remove all pattern matching
--
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/subst.t"
#include "../expr/einfo_t.t"
#include "../expr/booltree.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../rename/multi.t"
#include "../funnos.h"
#include "hexpr.t"
#include "misc.t"
#include "case.t"
#include "lettrans.t"
#include "cutil.t"

export remmatch;
rec
    multielim fn p e u =
	let its = gettids p in
       case mkseteq eqid (map fst its) in
	   [] & (PatVarError) : (mkberror ("[37] Pattern binding does not bind any variables: "@ppr p), u)
       ||  is : if PatBindUpdate & length is > 1 then
		    let rec l = length is
		    and patid = mkident (mknewids "z" u fn)
		    and [p'; e'] = copyids is [p; (mkconstr (CTuple l) (map mkident is))] (u+1)
		    and patbind = mkbpat [(patid, mkcase e [(p', mkap (mkap leftpat e')
							             (mkconstr (CTuple l) 
								      (map mkident is)))])]
		    and exprs = andify [mkbpat [(mkident i, prestr i its (pbselect patid k l))];; (i, k) <- 
					                                          combine (is, [0..])]
		    in
			(mkbrec (mkblocal patbind exprs), u+1+l)
		else
                    let l = length is in
		    (andify (map2 (\i.\k.let [p'; ei]=copyids is [p; mkident i] k in 
					 mkbpat [(mkident i, prestr i its (pselect e ei p'))]) is (map (\n.u+n*l) (from 0))),
		     u+l*l)
       end
and prestr i its e =
--trace (prid i@" "@show_list (show_pair(prid,show_option prttype)) its)
	case assocdefeq eqid i its None in
	   None : e
	|| Some t : mkinfo (restr [] t) e
	end
and gettids (mkap f a) = gettids f @ gettids a
 || gettids (mkident i) & (isdummy i) = []
 || gettids (mkinfo (restr _ t) (mkident i)) = [(i, Some t)]
 || gettids (mkident i) = [(i, None)]
 || gettids (mkconstr _ es) = concmap gettids es
 || gettids (e as mkbrack _ lexl) = map (,None) (getids e)
 || gettids (mkas i e) = (i, None).gettids e
 || gettids (mkcondp p c) = gettids p
 || gettids (mklazyp p) = gettids p
 || gettids (mkinfo _ e) = gettids e
 || gettids _ = []
and leftpat = 
    let lpfinfo = var_pre (finfo 2 [] (bttt,btff) 2 None)
    and lptype = Ohastype (Tarr (Tvar 1) (Tarr (Tvar 2) (Tvar 1))) [1; 2] None
    and lporig = Orignames Vimported Nofixity (MI preludeBuiltin, "Pleftpat")
    in  mkident (mkid Fleftpat "Pleftpat" (idi_var lpfinfo lptype None) lporig)

and copyids is es u =
    let tr = map2 (\(i as mkid _ s f on).\n.(i, mkident (mkid n s f on))) is (from u) in
    map (substl tr) es
and matchp fn (mkcondp p c) u =
	let (nc, u1) = match fn c u in (mkcondp p nc, u1)
||  matchp _ p u = (p, u)
and match fn e u =
	case e in
	   mkap f a :
		let (f1, u1) = match fn f u in
		let (a1, u2) = match fn a u1 in
		(mkap f1 a1, u2)
	|| mklam i e :	Uap (\e1.mklam i e1) (match fn e u)
	|| mkcase e pl :
		let (e1, u1) = match fn e u in
                let (pl', u1') = remwhere fn e1 (concmap remsimplewhere pl) u1 in
		let (pl1, u2) = Umap (\(p, e).\u.
			let (np, u1) = matchp fn p u in
			let (ne, u2) = match fn e u1 in
			(np, ne),u2) pl' u1' in
		caseelim fn (mkcase e1 pl1) u2
	|| mkletv b e :
		let (b1, u1) = matchbind fn b u in
		let (e1, u2) = match fn e u1 in
		(mkletv b1 e1, u2)
	|| mkident _ : (e, u)
	|| mkmodule i fs is es b : Uap (mkmodule i fs is es) (matchbind fn b u)
	|| mkerror _ : (e, u)
	|| mkcfunction _ _ : (e, u)
	|| mkconstr c el : Uap (mkconstr c) (Umap (match fn) el u)
	|| mkinfo t e : Uap (mkinfo t) (match fn e u)
	|| mkfailmatch _ : (e, u)	-- generated by remwhere
	|| e : fail ("No match in match "@ppr e)
	-- mkconst, mkas, mkcondp, mkfailmatch cannot occur
	end
and
    matchbind fn b u =
	case b in
	   mkband b1 b2 :
		let (b11, u1) = matchbind fn b1 u in
		let (b21, u2) = matchbind fn b2 u1 in
		(mkband b11 b21, u2)
	|| mkbrec b : Uap mkbrec (matchbind fn b u)
	|| mkbmulti p e : 
	        let (b, u1) = (multielim fn p e u) in matchbind "?" b u1		
	|| mkbpat [((i as mkident ii), e)] : Uap (\e1.mkbpat [(i, e1)]) (match (idtostr ii) e u)
	|| mkberror _ : (b, u)
	|| mkbnull : (b, u)
	|| mkblocal b1 b2 :
		let (b11, u1) = matchbind fn b1 u in
		let (b21, u2) = matchbind fn b2 u1 in
		(mkblocal b11 b21, u2)
	|| (b as (mkbtype _ _ _ _)) : (b, u)
        || (b as (mkbpragma _)) : (b, u)
	|| b : fail ("No match in matchbind: "@prdefg 0 b)
	end
and remmatch e u = match "?" e u
and and2 x y = mkap (mkap (mkident hiand) x) y
and remsimplewhere (mkcondp p g, mkwhere ges (mkbrec mkbnull)) = map (\ (c,e).(mkcondp p (and2 g c), e)) ges
||  remsimplewhere (        p,   mkwhere ges (mkbrec mkbnull)) = map (\ (c,e).(mkcondp p c,          e)) ges
||  remsimplewhere pe = [pe]
-- Stupid version of where removal for now
and remwhere fn e0 [] u = ([], u)
||  remwhere fn e0 ((mkcondp p g, mkwhere ges b).pes) u =
    -- move condition (from matching integer literals etc)
    remwhere fn e0 ((p, mkwhere (map (\ (c,e). (and2 g c, e)) ges) b).pes) u
||  remwhere fn e0 ((p, mkwhere ges b).pes) u =
#if 0
-- Save code (time?) by using y
    let nc = mkcase e0 pes in
    let y = mkident (mknewid "y" u) in
    let de = case p in mkident _ : [] || _ : [(mkident dummyid, y)] end in
    let ne = mkcase e0 ((p, mkletv b (reduce mkif y ges)).de) in
    ([(mkident dummyid, mkletv (mkbpat [(y, nc)]) ne)], u+1)
#else
    let flg = false in
    let nc = mkcase e0 pes in
    let y = if flg then nc else mkident (mknewids "y" u fn) in
    let de = case p in mkident _ : [] || _ : [(mkident dummyid, y)] end in
    let ne = mkcase e0 ((p, mkletv b (reduce mkif y ges)).de) in
    ([(mkident dummyid, if flg then ne else mkletv (mkbpat [(y, nc)]) ne)], u+1)
#endif
||  remwhere fn e0 (pe.pes) u =
    let (pes', u') = remwhere fn e0 pes u in
    (pe.pes', u')
and mkif (c, t) e = mkcase c [(mkconstr hctrue [], t); (mkconstr hcfalse [], e)]
and mlet mkbnull e = e
||  mlet b e = mkletv b e
end