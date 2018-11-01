module -- himport
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/pprint.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/einfo_t.t"
#include "../expr/constr_t.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "../expr/tinfo.t"
#include "../expr/einfo.t"
#include "renameutil.t"
#include "renametype.t"
#include "renenv.t"
#include "renameclass.t"
#include "classutil.t"

#define TR(x)

-- Sort out the imported stuff to a module.
export genimpenv;
rec genimpenv env fixs ents u =
        let env = if H1_3 then rmkonametbl env else env in
        (if ImpDebug /* & Test*/ then strace ("genimpenv:\n"@mix (map primpid ents) "\n") else \x.x) (
        let flatfix = concmap (\(mkfixid ids f).map (\i.(idtostr i, f)) ids) fixs in
        let ff = newff flatfix in
        -- Split into parts
	let types = filter istypeish ents
        and classes = filter isclass ents
	and vals = filter isval ents
	and insts = filter isinst ents in

	let rec xenv = rjoin ctenv (rjoin (rlist Kmeth (TR(trace "instl") instl)) (rjoin (rlist Ktype (tenvl@cenvl)) (rlist Kvalue (venvl@conl@metl@defl))))
	and (tenvl, u0) = gentype ff (TR(trace "gentype") env) types u
        and (cenvl, u1) = genclass ff (TR(trace "genclass") env) classes u0
        and (ctenvl, u11) = genctype ff (TR(trace "genctype") env) (filter isctype ents) u1
	and ctenv = reduce rjoin rnil ctenvl
	and seltypes = concmap getseltypes tenvl
	and (venvl, u2) = genval seltypes ff (TR(trace "genval") env) vals u11
	and conl = gencon tenvl
	and metl = genmet cenvl
	and defl = gendef cenvl
	and (instl, u3) = Umap (ginst ff (TR(trace "ginst") env)) insts u2
        in
(if Test then strace ("genimpenv "@show_list show_int (map length [types; classes; vals; insts])) else (\x.x))
	(xenv, TR(trace "u3") u3)
)

and newff fixs _ (mkidi s (Some on) _ _) = 
    Orignames Vimported
              (assocdef s fixs Nofixity) 
              on
||  newff fixs true (mkid _ s _ (Orignames _ _ on)) =
    Orignames Vimported
              (assocdef s fixs Nofixity) 
              on
--||  newff _ _ i = trace ("newff "@pprid i) (Orignames Vimported Nofixity (MI "", idtostr i)) --!!! temp!!!
||  newff _ _ i = fail ("newff "@pprid i)

and ginst ff env (mkimpinstance ot flg fs) u =
        buildinstid (ff false) u ot (renin env ot) flg false (\s.\_.assocdef (idtostr s) (mapfst idtostr fs) f_unk)

and mkidpres ps u s ii ff = map (\p. mkid u (setpref p s) ii ff) (specialid s ps)

and specialid ('_'._) ps = ps
||  specialid s       ps = [""]

and getseltypes (ii as (mkid _ _ (idi_type _ _ _ ti _ _) _)) =
    [ (idtostr s, ii) ;; (mkcons ci ee tbis true) <- get_cs_from_tinfo ti; (t,b,Some s) <- tbis ]
||  getseltypes _ = []

and gentype ff env types u = Uconcmap (gent1 ff env) types u
and gent1 ff env (mkimptype k t (nc,fl)) u =
    case tpart t in
    	mktcons (i as mkidi s _ _ pres) tvs : 
	    let tn = rentype env t in
	    let ti = mktinfo tn nc false fl [] false false None in
            let rec (iis as (ii . _)) = mkidpres pres u s (idi_type k tn (length tvs) ti (gettypeinsts env ii) None) (ff false i) in
	    if null pres then ([], u) else
	    (iis, u+1)
    ||  mktcons i tvs : 
	    fail ("gent1 mkimptype "@pprid i)
    end
||  gent1 ff env (mkimpeqtype k t cs od iso vis) u =
    let od' = case od in None : Some [] || Some xs : Some (map (\i.rfindid Ktype i env) xs) end in	-- no auto derive
    case tpart t in
	mktcons (i as mkidi s _ _ pres) tvs : 
	    let iso' = iso | ~NewType & case cs in [mkcons _ (_,[],[]) [(_,s,_)] _] : s || _ : false end in
            let rec (iis as (ii . _)) = map (\ pref .
		let (u1, t', cs') = renbt true pref (ff true) env u t cs in
		let ti = mktinfo t' (length cs') false (isflat cs') cs' (hasext cs') iso' None in
		mkid u1 (setpref pref s) (idi_type k t' (length tvs) ti (gettypeinsts env ii) od') (ff false i))
		    pres in
	    if null pres then ([], u) else
	    (iis, u + length cs + 1)
    ||  mktcons i tvs : 
	    fail ("gent1 mkimpeqtype "@pprid i)
    end
||  gent1 ff env (mkimpview k t tof cs) u = 
#if 0
    fail "impview"
#else
    case tpart t in
	mktcons (i as mkidi s _ _ pres) tvs : 
            let rec (iis as (ii . _)) = map (\ pref .
                let rec
                    viewft = rentype env' (xmkcontype (cpart t) (arrow tof (tpart t)))
                and oviewft = Ohastype viewft (getTvars viewft) None
                and on = case ff false i in 
                            Orignames v f (m,s) : Orignames v f (m,'_'.s)
                         end
                and env' = rjoin1type env tyid
		and vid = mkid u ('_'.s) (idi_var var_unknown oviewft None) on
		and (u1, t', cs') = renbt true pref (ff true) env' (u+1) t cs
		and vt = fail "gent1 view vt"
		and ti = mktinfo t' (length cs') false (isflat cs') cs' (hasext cs') false (Some vt)
		and it = idi_type k t' (length tvs) ti (gettypeinsts env ii) None
                and tof' = rentype env tof
		and iv = idi_view tof' vid it
                and tyid = mkid u1 (setpref pref s) it (ff false i)
                and tyidv = mkid u1 (setpref pref s) iv (ff false i)
                in  tyidv)
		    pres in
	    if null pres then ([], u) else
	    (iis, u + length cs + 2)
    ||  mktcons i tvs : 
	    fail ("gent1 mkimpview "@pprid i)
    end
#endif
||  gent1 ff env (mkimpsyn k t1 t2) u =
    case tpart t1 in
	mktcons (i as mkidi s _ _ pres) tvs : 
	    let (iis as (ii . _)) = mkidpres pres u s (idi_syn k (rentype env t1) (length (getTvars t1)) (rentype env t2)) (ff false i) in
	    if null pres then ([], u) else
	    (iis, u+1)
    ||  mktcons i tvs : 
	    fail ("gent1 mkimpsyn "@pprid i)
    end
||  gent1 _ _ ii _ = fail ("No match in gent1 on :"@primpid ii)

and genclass ff env types u = Uconcmap (gencl1 ff env) types u
and gencl1 ff env (mkimpclass k (cl as mkcdecl _ (mkassert (i as mkidi s on _ pres) _)) b fis) u = -- !!FIS
	let its = flatsyns (listify b) in
        let u1 = u + 2 * length its in
        let iis = map (\ pref . 
		       let rec (cl1, ms) = rencls pref (TR(trace ("gencl1:"@s)) env) i on cli u cl its (ff false)
		       and cli = getclassinfo k env cl1 ms in
		       mkid u1 (setpref pref s) (idi_class cli) (ff false i)
	    ) pres in
	if null pres then ([], u) else
        (iis, u1 + 1)
||  gencl1 ff env (mkimpclass _ (cl as mkcdecl _ (mkassert i _)) b _) u = fail ("gent1 mkcdecl "@pprid i)
||  gencl1 _ _ ii _ = fail ("No match in gencl1 on :"@primpid ii)

and genctype ff env cts u = Umap (gentc1 ff env) cts u
and gentc1 ff env0 (mkimpctype (tt as (mktcons (i as mkidi s _ _ pres) tts)) prods) u =
	if null pres then (rnil, u) else
	let rec (iiis as (iii . _))  = mkidpres pres u s (idi_type mkkground ttn (length tts) ti (gettypeinsts rnil iii) None) (ff false i)
	and     ti   = mktinfo ttn (length atln) false (isflat atln) atln (hasext atln) false None
	and     (u1,ttn,atln,ctenvobj) = renbct (ff false) env (u+1) tt prods
	and     tenv = rmany Ktype iiis
	and	(iiiis as (iiii . _))  = mkidpres pres u1 s (idi_conctype ttn prods) (ff false i)
	and	tenv' = rmany Kvalue iiiis
	and     env  = rjoin tenv env0 
	and 	env'  = rjoin tenv' env
	in (rjoin tenv' (rjoin (rjoin tenv (renbtenv atln)) (rcone ctenvobj)), u1)

and getmi (Some (mi, _)) = mi
and rencls pref env ms on cli u (mkcdecl aas a) its f =
    let a' = rencontext env a in
    let cl' = mkcdecl (sortcon (map (rencontext env) aas)) a' in
    let rec its' = map2 (\n.\(i,tt).
			 let tt' = rentype env tt in
			 let s0 = idtostr i in
			 let g _ = Orignames Vimported Nofixity (getmi on, s0) in
			 let s = setpref pref s0 in
			 let mi = makemetid u n s cli g in
			 let ctt = mktcontype (a'.cpart tt') (tpart tt') in
			 let di = makedefid u n s ms ctt g in
			 (di, mi, ctt))
			(from 0) its
    in (cl', its')

and olen None = 0
||  olen (Some l) = length l
and mkspec env s (Orignames vi xf (m, _)) u (t, fi) =
    let t' = rentype env t in
    let ot = Ohastype t' (getTvars t') None in
    let s' = specname s t' in
    let v = var_global fi in
    let i = mkid u s' (idi_var v ot None) (Orignames vi xf (m, s')) in
    (i, t', fi)
and genval seltypes ff env vals u = Uconcmap (genv1 seltypes ff env) vals u
and genv1 seltypes ff env (mkimpid (oi as mkidi s _ _ pres) t f ots) u = 
    let t' = rentype env t in
    let v = var_global f in
    let on = ff false oi in
    let ot = Ohastype t' (getTvars t') (oapply (map2 (mkspec env s on) (from (u+1))) ots) in
    let osel = lookup s seltypes in
    let is = mkidpres pres u s (idi_var v ot osel) on in
    (is, u+1+olen ots)
||  genv1 _ _ _ ii _ = fail ("genv1 "@primpid ii)

and isflat ats = all (\(mkcons _ _ xs _).length xs = 0) ats

and gencon sis = concmap genc1 sis
and genc1 (mkid _ _ (idi_type _ _ _ ti _ _) _) = map (\(mkcons i _ _ _).i) (get_cs_from_tinfo ti)
||  genc1 (vw as mkid _ _ (idi_view _ _ (idi_type _ _ _ ti _ _)) _) = 
	map (\(mkcons (mkid u s (idi_constr a b c d e f _) on) _ _ _).mkid u s (idi_constr a b c d e f (Some vw)) on) (get_cs_from_tinfo ti)
||  genc1 _ = []

and genmet sis = concmap genm1 sis
and genm1 (mkid _ _ (idi_class (clsi _ _ its _ _ _)) _) = map (\(_,i,_).i) its
||  genm1 _ = []
-- should not make duplicates XXX
and gendef sis = concmap gend1 sis
and gend1 (mkid _ _ (idi_class (clsi _ _ its _ _ _)) _) = map (\(i,_,_).i) its
||  gend1 _ = []

and isinst (mkimpinstance _ _ _) = true
||  isinst _ = false

and lookup i [] = None
||  lookup i ((i',s).iss) = if i=i' then Some s else lookup i iss

----
and arrow f a = mktcons (mkids "P->") [f; a]

and strace s x = if s=s then trace s x else fail "??"
end
