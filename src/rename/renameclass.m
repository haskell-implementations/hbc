module -- renameclass
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/einfo_t.t"
#include "../expr/einfo.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/booltree.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../misc/oset.t"
#include "../misc/flags.t"
#include "../transform/misc.t"	/* countarrows */
#include "../transform/hexpr.t"	/* hiEval */
#include "../type/conutil.t"	/* subsume */
#include "renameutil.t"
#include "renenv.t"
#include "renametype.t"
#include "renamedef.t"
#include "classutil.t"
export renclass, reninst, makemetid, makedefid, renin;
rec renclass fid ff env u0 (mkbclass cl b) =
	case cl in
	    mkcdecl _ (mkassert (mkids s) [cu]): -- ASSERT
		let rec   ci = mkid u s (idi_class cli) (ff ci)
		and      (cl' as mkcdecl aas' (k1 as (mkassert mi _))) = rencls ci env cl
		and    (tsgns, u) = Umap (mknewtvars [cu]) (concmap flatsign (extractsign b)) u0
		and     iits = map2 (doiit env (u+1) [k1] (id_visibility ci) cli mi ff) tsgns (from 0)
		and     menv = rlist Kvalue ( (map (\(_,i,_).i) iits) @ map (\(i,_,_).i) iits )
		and     eenv = rjoin1 menv Ktype ci
		and        k = rgetkind ci env
		and      cli = getclassinfo k env cl' iits
                and     emsg = prttype (cdecl2type cl)
		and   badaas = [msg ;; mkaerror msg <- aas' ]
		and (u', b', defs) = rendef fid ff (rjoin eenv env) (u+1+2*length iits) b in
		(u', (if ~chkdefs defs menv then 
		          mkberror ("[10] Extra definitions in class "@emsg)
		      else if ~all (\(_, t).mem cu (getTvars (tpart t))) tsgns then
			  mkberror ("[11] Type variable not in all methods for class "@emsg)
		      else if ~all (\(_, t).~mem cu (getavars (cpart t))) tsgns then
			  mkberror ("[12] Constrained variable in class "@emsg)
		      else if not (null badaas) then
			  mkband (mkberror (hd badaas)) (mkbclass cl' b')
		      else
			  mkbclass cl' b' ), eenv)
	end

and mknewtvars cus (i, t) u =
    let vs = odifference (omkset (getTvars t)) (omkset cus) in
    let s = map2 (\v.\n.(v, mktvar n)) vs (from u) in
    let t' = tsubst s t in
    ((i, t'), u + length vs)

and extractsign b = filter issign (listify b)
and issign (mkbsign _ _) = true
||  issign _ = false
and flatsign (mkbsign is t) = map (\i.(i, t)) is
and doiit env u k vis cli ms ff (i, t) n =
    let s = idtostr i in
    let t' = rentype env t in
    let t'' = mktcontype (k @ cpart t') (tpart t') in
    (updvis vis (makedefid u n s ms t'' ff),
     makemetid u n s cli ff,
     t'')

and rencls ci env (mkcdecl k (mkassert _ v)) = 
    mkcdecl (sortcon (map (rencontext env) k)) (mkassert ci v)
||  rencls ci _ _ = mkcdecl [] (mkaerror "[13] Malformed class declaration")

-- could do better than f_unk
and makedefid u n s mi tt ff =
    let fi s t = if UseForceArity then
--trace ("makedefid-fa "@s@" :: "@prttype t)
(
		    let a = countarrows t in
		    finfo a [] (bttt, btff) (-1) None
)
		else f_unk in
    let ds = defstr mi (mkids s) in 
    let rec ii = mkid (u+2*n+1) ds (idi_var (var_global (fi ds tt)) (Ohastype tt (getTvars tt) None) None) (ff ii) in ii

and ma n (clsi _ _ iits _ _ ns) = 
        case select (n+1) iits in 
        (_,_,t) : countarrows t - 1	-- don't count own context
	end
and makemetid u n s cli ff = 
        let di = mkid (u+2*n) s (idi_method [n] (ma n cli) cli) noorigname in
	         mkid (u+2*n) s (idi_method [n] (ma n cli) cli) (ff di)

and classorig (clsi _ (mkcdecl _ (mkassert i _)) _ _ _ _) = getminame (fst (id_orignames i))
and chkdefs defs menv = null (difference (rstrs Kvalue defs) (rstrs Kvalue menv)) & null (rstrs Ktype defs)

and setinstmod ff (mkidecl x y z _) = 
    let (Orignames _ _ (MI m,_)) = ff (fail "setinstmod") in
    mkidecl x y z m

and reninst expl fid ff env u (mkbinstance idcl0 b _) =
        let idcl = setinstmod ff idcl0 in
        let rec (u', b', defs) = rendef' badidq fid ff env u b
        and (iid, u'') = buildinstid ff u' idcl idcl' false true (\_.\_.f_unk)
	and iits = cliof idcl'
        and idcl' = renin env idcl
        and b'' = case chkinst defs iits in
		     [] : let bd = case idcl' in mkidecl _ ci _ _ & (EvalClass & expl & eqid ci hiEval) : mkberror ("[110] Explicit Eval instances not allowed") || _ : b' end
		     	  in  mkbinstance idcl' bd (Some iid)
		  || is : mkberror ("[14] Instance declaration contains extra bindings: "@prttype (idecl2type idcl)@", "@mix (map tl is) ", ")
		  end
	in
-- Cannot add instance methods here since they cannot be computed without
-- the environment, i.e. a data loop.
--trace ("reninst "@pprid iid@" "@itos u'')
	(u'', b'', rone Kmeth iid)

and chkinst defs iits = difference (rstrs Kvalue defs) (map (\(_,m,_).idtostr m) iits) @ rstrs Ktype defs

#define TR(x)
-- The code is a little weird here to avoid data dep. loops.
and renin env (idcl as mkidecl k oi [t] ms) = -- ASSERT
--        let s = idtostr oi in
    	let i = rfindid Ktype oi (TR(trace "renin1") env) in
	let k''' = map (rencontext env) k in
        let k' = subsume k''' in	   -- Get rid of unneeded contexts (why doesn't this cause a data dependency loop?)
	let (it, msg) =
	    case rentype (TR(trace "renin2") env) t in
		mkterror s : (mktcons (mkid 0 s idi_varu noorigname) [], s)
	    ||  nt : (nt, "")
	    end in
        let k'' = if msg ~= "" then
	              [mkaerror msg] 
		  else if ~id_isclass i then
		      [mkaerror ("[16] Unknown class in instance declaration "@pridecl idcl)]
		  else if Pedantic & anysame (getallTvars t) then
		      [mkaerror ("[17] Multiple type variables in instance declaration "@pridecl idcl)]
		  else
		      k'
        in
	mkidecl k'' i [it] ms -- ASSERT
--)
end
