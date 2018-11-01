module -- renamedef
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/subst.t"
#include "../expr/id.t"
#include "../expr/pprint.t"
#include "../expr/pragma_t.t"
#include "../transform/misc.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../misc/sort.t"
#include "../transform/lettrans.t"	/* andify */
#include "../transform/failcase.t"      /* tfail */
#include "../transform/hexpr.t"
#include "renenv.t"
#include "renametype.t"
#include "fun.t"
/*import renexpr:	*a->Renv->Int->Texpr->(Int#Texpr);*/
#include "rename.t"	/* rename and renamedef are mutually recursive! */
#include "renameutil.t"
#include "renameclass.t"

export rendef, rendef';
rec
    rd bad fid ff env u cp dd =
        if Pedantic then
	    let (u', d', env') = rdx bad fid ff env u cp dd in
	    let d'' = case filter bad (rids Kall env') in
		          [] : d'
		      ||  is : mkberror ("[18] Bad identifiers in pedantic mode: "@mixmap oprid is ", ")
		      end
            in (u', d'', env')
	else
	    rdx bad fid ff env u cp dd
and
    rdx bad fid ff env u cp dd =
	case dd in
	   mkbrec d :
	    let rec (uu, nd, ne) = rd bad fid ff (rjoin ne env) u (addc (getcs d) cp) d in
	    let d' = if syncirc ne then 
		         mkberror "[19] Circular type synonyms."
                     else
			 nd in
	    (uu, mkbrec d', ne)
	|| mkband d1 d2 :
	    let (u1, nd1, e1) = rd bad fid ff env u cp d1 in
	    let (u2, nd2, e2) = rd bad fid ff env u1 cp d2 in
	    (u2, mkband nd1 nd2, rjoin e1 e2)
	|| mkblocal d1 d2 :
	    let (u1, nd1, e1) = rd bad fid ff env u cp d1 in
	    let (u2, nd2, e2) = rd bad fid ff (rjoin e1 env) u1 (addc (getcs d1) cp) d2 in
	    (u2, mkblocal nd1 nd2, e2)
	|| mkbpat [pe as (p,e)] & (ismulti cp p | isvarwhere p e) :
	    let ((b, ex), u1) = multi (newfid fid topfid) cp ff env pe u in
	    (u1, b, ex)
	|| mkbpat [(mkident (i as mkids s), e)] :
		let (u2, en) = renexpr (newfid fid i) ff env (u+1) e in
		let rec id = mkid u s idi_varu (ff id) in
		if s = "_" then
		    (u2, sdef dummyid en, rnil)
		else
		    (u2, sdef id en, rone Kvalue id)
        || mkbpat (pes as ((p,e)._)) & (Curry & (ismulti cp p | isvarwhere p e)) :
		-- The parser&curry0 may have messed it up.  They don't know any better.
		   let (bexs, u1) = Umap (multi (newfid fid topfid) cp ff env) pes u in
	           let (bs, exs) = split bexs in
	           (u1, andify bs, reduce rjoin rnil exs)
	|| mkbpat pl : 
		case fixfun cp pl in
                   (pl' as (mkbpat ((mkident fid',_)._))) : rd bad fid' ff env u cp pl'
		|| (e as mkberror _) : (u, e, rnil)
                end
	|| mkbtype t l _ iso : 
#if 0
	    if EvalClass & oktoeval iso l then
		let bi = mkbinstance (mkidecl [] (mkpids "_Eval") [tpart t] "_") mkbnull None in
		let (u1, nd1, e1) = renbtype ff env u dd in
		let (u2, nd2, e2) = reninst false fid ff env u1 bi in
		(u2, mkband nd1 nd2, rjoin e1 e2)
	    else
#endif
		renbtype ff env u (if EvalClass & ~RelaxEval then addEval dd else dd)
	|| mkbview t ot cs b :
	    let (viewstr as (_.ct.ts)) = idtostr (tname t) in
	    let vstr = '_'.tolower ct.ts in
            let (u1, bt, e1) = renbtype ff env u (mkbtype t cs None false) in
	    let s = mkbsign [mkids vstr] (xmkcontype (cpart t) (arrow ot (tpart t))) in
            let (u2, bd, _ ) = rendef fid ff (rjoin e1 env) u1 (mkbrec (mkband s b)) in
            let ot' = rentype env ot in
            let rec f (mkid u s (ii as idi_type _ _ _ _ _ _) on) = mkid u s (idi_view ot' vid ii) on
            ||      f (mkid u s (idi_constr a b c d e f _) on) = mkid u s (idi_constr a b c d e f (Some it)) on
            ||      f i = i
            and     rext = rmaptemp f e1
            and     it = rfindid Ktype (tname t) rext
            and tids = case bt in mkbtype _ cs _ _ : [ ti ;; (mkcons id cx tbs b) <- cs; (t,b,s) <- tbs; ti <- getTconstrs t ] || _ : [] end
            and isrec = member eqid it tids
	    and (b', vid) =
             case bt in
               mkbtype t' cs' _ _ :
                 case bd in
                    mkbrec (mkband (mkbsign _ st) (mkbpat [(mkident i, e)])) :
                      if idtostr i = vstr then
                          let (mkid n _ ii (Orignames _ f (m,s))) = i in
                          let ni = mkid n ("__"@(ct.ts)) ii (Orignames Vexported f (m,'_'.viewstr)) in
                          let e' = subst (mkident ni) i e in
                          if isrec then
                              (mkberror ("[116] View type may not be recursive: "@oprid it), dummyid)
                          else
                              (mkband (mkband (mkbsign [ni] st) (mkbpat [(mkident ni, e')])) (mkbview t' ot' cs' mkbnull), ni)
                      else
                          (mkberror ("[114] view type and transformation name do not agree: "@(ct.ts)@", "@tl vstr), dummyid)
                 || _ : (mkberror "[113] Bad view transformation binding", dummyid)
                 end
             || _ : (mkband bt bd, dummyid)
             end
	    in (u2, b', rext)
	|| mkbctype _ _ : renbctype ff env u dd
	|| mkberror _ : (u, dd, rnil)
	|| mkbnull : (u, dd, rnil)
	|| mkbpragma p : (u, renpragma env p, rnil)
	|| mkbsyn t1 t2 : rensyntype ff env u dd
        || mkbclass t b : renclass fid ff env u dd
        || mkbinstance _ _ _ : reninst (~EvalInst) fid ff env u dd
        || mkbdefault ts : (u, mkbdefault (map (rentype env) ts), rnil)
        || mkbsign is t :
	       let is' = map (\(mkids s).rfind Kvalue s env) is in
	       (u,
	        (if exists (\i.id_no i=0) is' then
		   mkberror ("[20] Unknown id among "@mix (map oprid is) ",")
	        else
		   mkbsign is' (rentype env t)),
	        rnil)
	end
and renpragma env (Pspecialize (mkids s) ts) = 
            let i = rfind Kvalue s env in
            if id_no i = 0 /*| ~id_is_exported i*/ then
		mkberror ("[95] Illegal value specialized: "@tl s)
            else
		mkbpragma (Pspecialize i (map (\ (t, oi) . (rentype env t, renoptid env oi)) ts))
||  renpragma env (p as Pspecinst t) = mkbpragma p	-- ignore it

#if 0
and oktoeval true [mkcons _ _ [(mktap _ _, _, _)] _] = BadEvalInst
||  oktoeval _ _ = true
#else
and addEval (mkbtype t l None      iso) = mkbtype t l (Some [hiEval]) iso
||  addEval (mkbtype t l (Some ds) iso) = mkbtype t l (Some (hiEval.ds)) iso
#endif

and renoptid env None = None
||  renoptid env (Some (mkids s)) = Some (rfind Kvalue s env)

and id_is_exported (mkid _ _ _ (Orignames Vexported _ _)) = true
||  id_is_exported _ = false

and newfid (mkid 0 _ _ _) i = i
||  newfid (fid as mkids _) i & (eqid fid topfid) = i
||  newfid fid _ = fid
and topfid = mkids "_PATTERNBIND"

and multi fid cp ff env (p, e) u =
        let pi = (filter (not o cp) (getids p)) in
	let ex = etag ff pi u idi_varu in
	let (u1, pn) = renpat  fid ff (rjoin ex env) (u+length pi) p in
	let (u2, en) = renexpr fid ff env u1 e in
	let I = newidenti fid u2 in
	((mkbrec (mkblocal (mkbpat [(I,expwhere en)]) (mkbmulti pn I)), ex), u2+1)

and expwhere (mkwhere ges mkbnull) = expw ges
||  expwhere (mkwhere ges b) = mkletv b (expw ges)
||  expwhere e = e
and expw ges = reduce mkif (tfail "No match in pattern") ges
and mkif (c, t) e = mkcase c [(mkident hitrue, t); (mkident hifalse, e)]
and isvarwhere (mkident _) (mkwhere _ _) = true
||  isvarwhere _ _ = false

and rendef fid ff env u dd = rendef' badid fid ff env u dd
and rendef' bad fid ff env u dd =
	let (un, b, eext) = rd bad fid ff env u (isconstri env) dd in
	if anysame (rstrs Kvalue eext) | anysame (rstrs Ktype eext) then
	    let dups = (map hd o filter (\is.length is > 1) o groupsort (<) o rstrs Kall) eext in
	    (u,
	     mkberror ("[21] Multiple definition in binding: " @ mixmap tl dups ", "),
	     rnil)
	else
	    (un, b, eext)
----
and arrow f a = mktcons (mkids "P->") [f; a]
and tolower c = chr (ord c + 32)
end
