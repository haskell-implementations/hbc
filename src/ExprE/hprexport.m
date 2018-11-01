module -- hprexport
-- Print the export list of a module.
#include "../misc/triple.t"
#include "Expr_t.t"
#include "../expr/einfo_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/constr_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../expr/constrfun.t"
#include "Eprint.t"
#include "../expr/einfo.t"
#include "../expr/pragma_t.t"
#include "../misc/flags.t"
#include "../misc/util.t"
#include "../misc/misc.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../expr/tinfo.t"
#include "../rename/renameutil.t"
#include "../rename/renenv.t"
#include "../main/topexprs.t"		/* pragmas, curmod */
#include "../type/subst.t"		/* normtype */
export hprexport, prettypre;
rec hexpinfo insts env sinf (Emodule i oooexps _) =
        let ooexps = reverse (fixup oooexps) in					-- reversing makes the interface file look nicer
        let mids = idtostr i in
        let esyns = filter id_issyn (map expid ooexps) in			-- exported synonyms
        let oexps = map (eexpsyn esyns) ooexps in				-- expand synonyms that are not visible
	let exps = concmap expimp oexps in					-- add stuff from exported "import"
        let eids = concmap ecollid exps in					-- identifiers that are (re-)exported from this interface
        let instids = yydup (filter (expinst (idtostr i) eids exps) insts) in	-- instance ids that need exporting
	let ids = concmap collid exps in					-- all identifiers in the interface (except for instance-decls)
        let iexps = filter (\e.id_visibility (expid e) = Vimported) exps in	-- identifiers that are not from this module
        let iexpsi = map expid iexps in						-- same but with mkexp* stripped
        let xids = mkseteq eqid (ids @ concmap getclsid instids) in		-- all identifiers in the interface (including instance-decls)
        let iids = filter (\i.id_visibility i = Vimported &			-- identifiers (types&classes) that are visible in the interface,
			      ~(member eqid i iexpsi | (~H1_3 & idinprelude i))) xids in	--           imported (not from Prelude), but not exported.
        let iiexps = map mkexpid iids in					-- ids that needs original name mentioned
	let nonexp = filter (not o id_is_visible) ids in			-- in the interface, but not visible
        let badexp = filter (not o okexp) oexps in				-- invalid exports ids
	let dupexp = getdups ltid (map expid oexps) in				-- duplicate exports
        (badexp, dupexp, nonexp, iexps, iiexps, eids, remduptype exps, instids)

and remduptype exps =
    let id_istype (mkid _ _ (idi_type _ _ _ _ _ _) _) = true || id_istype _ = false in
    let ts = [ ii ;; mkexpidall ii <- exps; id_istype ii ] in
    let f (mkexpid (ii as mkid _ _ (idi_type _ t _ ti _ od) _)) = ~member eqid ii ts
    ||  f _ = true
    in  filter f exps

and hprexport insts env (e as Emodule i _ _) sinf =
        let (badexp, dupexp, nonexp, iexps, iiexps, eids, exps, instids) = hexpinfo insts env sinf e in
        if Curry & badexp ~= [] then
	    No ("[86] Bad export specifier for: "@mixmap (oprid o expid) badexp ", ")
        else if dupexp ~= [] then
	    No ("[87] Duplicates in export list: "@mixmap oprid dupexp ", ")
	else if Curry & nonexp ~= [] & ExportCheck then 
	    No ("[88] Module "@hprid i@" should export "@mixmap hprid (mkseteq eqid nonexp) ", ") 
	else 
	    let impimps = (group eqmodule o remdupi o sort ltorig) (iexps@iiexps)
            and uimpimps = (map (getminame o fst o id_orignames o expid o hd) o group eqmodule o remdupi o sort ltorig) iiexps in
	    Yes ("interface "@hprid i@" where {\n"@
		 usinginfo env uimpimps @
		 (if H1_3 then "" else concmap hmkimps impimps) @
		 concmap hprfix eids @
		 mix (map (prexp sinf) exps@prinst sinf instids) ";\n"@
		 (if H1_3 & null eids & null exps & null instids then ";" else "")@
		 "\n}\n")
||  hprexport _ _ e _ = fail ("No match in hprexport: "@pr e)

and eqmodule i1 i2 = getminame (fst (id_orignames (expid i1))) = getminame (fst (id_orignames (expid i2)))
and eqorig e1 e2 = ieqorig (expid e1) (expid e2)
and ltorig e1 e2 = iltorig (expid e1) (expid e2)
and ieqorig (mkid _ _ _ on1) (mkid _ s2 _ on2) = on1 = on2
and iltorig (mkid _ _ _ on1) (mkid _ s2 _ on2) = on1 < on2
--and xeq i1 i2 = eqid i1 i2 & idtostr i1 = idtostr i2

and prsel false (Some i) s = "{ " @ hprid i @ " ::" @ s @ "}"
||  prsel true  (Some i) s = hprid i @" ::"@s
||  prsel _ _ s = s

and isnormalkind 0 mkkground = true
||  isnormalkind n (mkkarrow mkkground k) = isnormalkind (n-1) k
||  isnormalkind _ _ = false

and prdata sinf dta t ti ofpart = 
      let cs = get_cs_from_tinfo ti in
      let tvs = reduce union (getTvars t) (map (\(mkcons _ _ ys _).reduce union [] (map (getTvars o fst3) ys)) cs) in
      let al = combine (tvs, map mktvar (from 0)) in
      let fixal false vs al & (H1_3) = [(v, mktvar (if mem v vs then -x-1 else x));; (v,mktvar x) <- al]
      ||  fixal _ _ _ = al in
      let t' = lTRtype al t
      and cs' = map (\(mkcons i (uni,a,cx) ys flg). let al = fixal uni a al in mkcons i (uni, a, map (TRcon1 al) cx) (mapfst3 (lTRtype al) ys) flg) cs in
      let ofstr = case ofpart in None : "" || Some ot : " of "@phprttype (lTRtype al ot) end in
      dta@kprttype t'@ofstr@" = "@mix (map (\ (mkcons i (_, _, cx) ys flg).hprcx cx@noqprid i@(if flg then " { " else "")@mix (map (\(t,b,sel).prsel flg sel ((if b & H1_3 then " !" else " ")@phprttype t@if b & ~H1_3 then "{-#STRICT#-}" else "")) ys) (if flg then ", " else "") @(if flg then " }" else "")) cs') " | "

and prexp sinf (mkexpid (i as (mkid _ _ (idi_var (var_global f) (Ohastype t _ _) _) _))) =
        let si = hprid (fixid i) in
	si@" :: "@xhprttype (normtype t)@"  "@hprfinfo true si f@hprpragmas sinf si i
||  prexp sinf (mkexpid (i as (mkid _ _ (idi_var _ (Ohastype t _ _) _) _))) =
	hprid (fixid i)@" :: "@xhprttype (normtype t)
||  prexp sinf (mkexpidall (mkid _ _ (idi_type _ t _ ti _ od) _)) =
      let dta = if NewType & get_isotype ti then "newtype " else "data "
      in  prdata sinf dta t ti None @ prd od
||  prexp sinf (mkexpidall (mkid _ _ (idi_view ot _ (idi_type _ t _ ti _ _)) _)) =
      prdata sinf "view " t ti (Some ot)
||  prexp sinf (mkexpid (ii as mkid _ _ (idi_type _ t _ _ _ _) _)) =
    if Curry then
      "data "@kprttype (normtype t)
    else
      prexp sinf (mkexpidall ii)
||  prexp sinf (mkexpidall (mkid _ _ (idi_class (clsi _ c dmts _ _ _)) _)) =
    let (tc.tms) = normtypes (cdecl2type c . [ t ;; (xx,m,t) <- dmts ]) in
    let dmts = map2 (\ (xx,m,_) . \ t . (xx,m,t)) dmts tms in
      "class "@kprttype tc@" where {\n"@mix (map prdmt dmts) ";\n"@"\n    }"
||  prexp sinf (mkexpid (mkid _ _ (idi_class (clsi _ c dmts _ _ _)) _)) =
      "class "@kprttype (cdecl2type c)
||  prexp sinf (mkexpidall (mkid _ _ (idi_syn _ t1 n t2) _)) = expsyn t1 t2
||  prexp sinf (mkexpid (mkid _ _ (idi_syn _ t1 n t2) _)) = expsyn t1 t2
||  prexp sinf (mkexpidmodall _) = ""
||  prexp sinf e = if Curry then "-- !! STRANGE EXPORT: "@prexpid e else "--"

and expsyn t1 t2 =
      let [t1'; t2'] = normtypes [t1; t2] in
      "type "@kprttype t1'@" = "@xhprttype t2'

and TRcon1 ss (mkassert ci vs) = mkassert ci (map (\ v.(let (mktvar y) = assocdef v ss (mktvar v) in y)) vs)
-- !!!
and hprcx [] = ""
||  hprcx cx = "("@mixmap hpras cx ", " @ ") => "
and hpras (a as mkassert c vs) = --oprid c @ " " @ hprtvar v
        xhprttype (as2ty a)
||  hpras (mkaerror e) = "ERROR "@e
and hprtvar v = if v < 26 then [chr (ord 'a' + v)] else 'a'.itos v
-- !!!

and kprttype t = kprttype' (fixtype curmod t)
and kprttype' (mktcontype k t) = hprcontext k @ kprttype' t
||  kprttype' (t as mktcons i tvs) = 
    let k = get_id_kind i in
    if ~HigherKind | isnormalkind (length tvs) k then hprttype t
    else "("@oprid i @ " :: " @ show_Kind k @ ") " @ mix (map hprttype tvs) " "

and phprttype (t as mktvar _) = xhprttype t
||  phprttype (t as mktcons _ []) = xhprttype t
||  phprttype (t as mktcons i _ ) & (atomic (idtostr i)) = xhprttype t
||  phprttype (t as mktap _ []) = xhprttype t
||  phprttype t = "("@xhprttype t@")"
and atomic "_[]" = true
||  atomic (_.'#'._) = true
||  atomic _ = false
and okexp (mkexpid    (mkid _ _ (idi_var _ _ _) _)) = true
||  okexp (mkexpidall (mkid _ _ (idi_type _ _ _ i _ _) _)) = ~(abstracttype i)
||  okexp (mkexpid    (mkid _ _ (idi_type _ _ _ _ _ _) _)) = true
||  okexp (mkexpidall (mkid _ _ (idi_class _) _)) = true
||  okexp (mkexpid    (mkid _ _ (idi_class _) _)) = H1_3
||  okexp (mkexpidall (mkid _ _ (idi_view _ _ _) _)) = true
||  okexp (mkexpidall (mkid _ _ (idi_syn _ _ _ _) _)) = true
||  okexp (mkexpid    (mkid _ _ (idi_syn _ _ _ _) _)) = H1_3
||  okexp (mkexpidmodall (mkid _ _ (idi_module _) _)) = true
||  okexp _ = false
and xmkcontext [] t = t
||  xmkcontext ts t = mktcontype ts t
and abstracttype (mktinfo _ _ _ _ [] _ _ _) = true
||  abstracttype _ = false
and prdmt (_,m,mktcontype (_.xs) t) = "    "@noqprid m @ " :: " @ xhprttype (xmkcontext xs t)
and prd _ = ""
--and prd None = " deriving ()"
--||  prd (Some is) = " deriving ("@mix (map hprid is) ", " @ ")"
and hprfix i =
    case id_fixity i in
	Infix  n : "infix  "@itos n@" "@opr i@";\n"
    ||  InfixL n : "infixl "@itos n@" "@opr i@";\n"
    ||  InfixR n : "infixr "@itos n@" "@opr i@";\n"
    ||  Nofixity : ""
    ||  FPrefix _ : fail "hbc does not support prefix operstors"
    ||  FPostfix _ : fail "hbc does not support postfix operstors"
    ||  Nonfix : fail "hbc does not support nonfix"
    end
and opr i =
	case prid i in
	   s as c . _ & (isalpha c | isiso c) : "`"@s@"`"
	|| s : s
	end

and hprid i = if hd (idtostr i) = 'P' then "{-:\""@idtostr i@"\":-}" else oprid i

and noqprid (mkid n s t v) = hprid (mkid n (dropqual s) t v)
||  noqprid i = hprid i

-- Collect ids that must be exported from an export id
and collid (mkexpid (i as (mkid _ _ (idi_var _ (Ohastype t _ _) _) _))) = collidt t
||  collid (mkexpidall (mkid _ _ (idi_type _ t _ ti _ od) _)) = 
      let cs = get_cs_from_tinfo ti in
      collidt t @ concmap (\(mkcons i _ ys _).concmap (\(t,b,_).collidt t) ys) cs
||  collid (mkexpidall (mkid _ _ (idi_view ot _ (idi_type _ t _ ti _ _)) _)) = 
      let cs = get_cs_from_tinfo ti in
      collidt ot@collidt t @ concmap (\(mkcons i _ ys _).concmap (\(t,b,_).collidt t) ys) cs
||  collid (mkexpid (i as mkid _ _ (idi_type _ _ _ _ _ od) _)) = []
||  collid (mkexpidall (mkid _ _ (idi_class (clsi _ t dmts _ _ _)) _)) =
      collidt (cdecl2type t) @ concmap (\(_,_,t).collidt t) dmts
||  collid (mkexpid (mkid _ _ (idi_class (clsi _ t _ _ _ _)) _)) = collidt (cdecl2type t)
||  collid (mkexpidall (mkid _ _ (idi_syn _ _ _ t) _)) = collidt t
||  collid _ = []
and collidt (mktcontype ts t) = map (\(mkassert i _).i) ts @ collidt t
||  collidt (mktcons i ts) = i . concmap collidt ts
||  collidt (mktap _ ts) = concmap collidt ts
||  collidt _ = []
-- Collect exported ids from an export id
and ecollid (mkexpid (i as (mkid _ _ (idi_var _ _ _) _))) = [i]
||  ecollid (mkexpidall (i as mkid _ _ (idi_type _ _ _ ti _ od) _)) = 
	i . concmap (\(mkcons i _ tbs _).i . [ s ;; (t,b,Some s) <- tbs ]) (get_cs_from_tinfo ti)
||  ecollid (mkexpidall (i as mkid _ _ (idi_view _ _ (idi_type _ _ _ ti _ od)) _)) = 
	i . concmap (\(mkcons i _ tbs _).i . [ s ;; (t,b,Some s) <- tbs ]) (get_cs_from_tinfo ti)
||  ecollid (mkexpid (i as mkid _ _ (idi_type _ _ _ _ _ od) _)) = [i]
||  ecollid (mkexpidall (i as mkid _ _ (idi_class (clsi _ t iits _ _ _)) _)) = i . map (\(_,i,_).i) iits
||  ecollid (mkexpid (i as mkid _ _ (idi_class _) _)) = [i]
||  ecollid (mkexpidall (i as mkid _ _ (idi_syn _ _ _ _) _)) = [i]
||  ecollid _ = []
and ecollidt (mktcontype ts t) = ecollidt t
||  ecollidt (mktcons i ts) = [i]
||  ecollidt _ = []

and metsof (mkid _ _ (idi_class (clsi _ _ xxx _ _ _)) _) = map (\ (_,x,_).x) xxx
and prinststr sinf (xi as mkid _ _ (idi_var (var_global f) _ _) _) i =
	let (finfo a _ (s,t) _ _) = f in
	case assocdefeq eqid xi sinf (a, s, t) in
	   (-1,_,_) : ""
	|| (a,s,t)  : 
		let si = hprid i in
		hprfinfo false si (finfo a [] (s,t) (-1) None)
	end
-- An instance declaration should be exported if the class or the type is
-- exported.  I presume that both type and class also have to be externally visible.
and prinst sinf instids =
    map (\(mkid _ _ (idi_inst (t as mkidecl _ ci _ m) ms der) _)."instance "@xhprttype (normtype (idecl2type t))@(if der then " {-# DERIVED #-}" else "")@
    (if H1_3 then "" else
     case m in
        "" : ""
     || "_" : ""
     || _.ms : " {-# FROMMODULE "@ms@" #-}"
     end)@
     (if der then "" else concmap2 (prinststr sinf) ms (metsof ci))
     ) instids
and expinst mid eids exps (mkid n s (idi_inst t _ der) (Orignames _ _ (onm,_))) =
--trace ("expinst "@pridecl t)
    case t in
	mkidecl _ _ [mktvar _] _ : true -- ASSERT
    ||	mkidecl _ ci [mktcons ti _] _ : -- ASSERT
-- always show instances explicitely			(~der | abstype ti exps) &		-- show non-derived, and derived for abstypes
		if InstWithCT then
		    (member eqid ci eids | member eqid ti eids | (AllowRedef & getminame onm = mid) ) &
		    (id_is_visible ci & id_is_visible ti)
		else
		    -- New braindead rules: export all instances.
		    -- We relax it a little, don't export if both C and T are in the Prelude,
		    -- or if C and/or T are defined locally but not exported.
		    ( ~ idinprelude ci | ~ idinprelude ti ) &
		    id_is_visible ci & id_is_visible ti
    end
||  expinst _ _ _ _ = false
and getclsid (mkid _ _ (idi_inst t _ _) _) = collidt (idecl2type t)

-- Check if type is exported as an abstract type
and abstype i [] = false
||  abstype i (mkexpid j._) & (eqid i j) = true
||  abstype i (_.es) = abstype i es
and hmkimps [] = fail "hmkimps"
||  hmkimps (es as e0 . _) =
    let m = getminame (fst (id_orignames (expid e0))) in
    let ids = concmap ecollid es in
    let renids = if H1_3 then [] else filter (\i.idtostr i ~= oname i) ids in
    "import "@tl m@"("@mixmap (hmkimp1 renids) es ", "@")"@
    case renids in
	[] : ";\n"
    ||  sns : let ons = mkset (map (\i.(tl (oname i), tl (idtostr i))) sns) in
              " renaming (" @ mix (map (\(on,nn).optparen on@" to "@optparen nn) ons) ", " @ ");\n"
    end
and hmkimp1 renids e = optparen (tl (onamex (expid e))) @ 
    (case e in 
	 mkexpid _ : "" 
     ||  mkexpidall (mkid _ _ (idi_class _) _) : "(..)"
     ||  mkexpidall (mkid _ _ (idi_syn _ _ _ _) _) : "(..)"
     ||  mkexpidall (mkid _ _ (idi_type _ _ _ ti _ _) _) : 
             let cis = map (\(mkcons i _ _ _).i) (get_cs_from_tinfo ti) in
	     if null (intereq eqid renids cis) then
		 "(..)"
	     else
		 "("@ mixmap (optparen o tl o oname) cis ", " @")"
     ||  mkexpidall (mkid _ _ (idi_view _ _ (idi_type _ _ _ ti _ _)) _) : 
             let cis = map (\(mkcons i _ _ _).i) (get_cs_from_tinfo ti) in
	     if null (intereq eqid renids cis) then
		 "(..)"
	     else
		 "("@ mixmap (optparen o tl o oname) cis ", " @")"
     ||  _ : fail ("No match in hmkimp1: "@prexpid e)
     end)
and expimp (mkexpidmodall (mkid _ _ (idi_module es) _)) = es
||  expimp i = [i]

and oname i = snd (id_orignames i)
and onamex i =
    case oname i in
       _.'#'.cs as (c._) & (isdigit c) : 'P'.rept (stoi cs - 1) ','
    || s : s
    end

-- remove duplications in export list
and remdupi (x1.x2.xs) & (oname (expid x1) = oname (expid x2)) =
    if expmore x1 x2 then
	remdupi (x1.xs)
    else
	remdupi (x2.xs)
||  remdupi (x.xs) = x.remdupi xs
||  remdupi [] = []
-- does first export more than second
and expmore _ (mkexpid _) = true
||  expmore _ _ = false

-- Expand synonyms that are not externally visible.
and eexpsyn es e = (eexps e
where rec
    eexps (mkexpid i) = mkexpid (iexpsyn i)
||  eexps (mkexpidall i) = mkexpidall (iexpsyn i)
||  eexps (mkexpidmodall i) = mkexpidmodall (iexpsyn i)
and iexpsyn (mkid u s iv on) = mkid u s (ivexp iv) on
and ivexp (idi_var vi (Ohastype t n ois) osel) = idi_var vi (Ohastype (tsynexp t) n (oapply (map (\ (i,t,f).(i, tsynexp t, f))) ois)) osel
||  ivexp (idi_type k t x (mktinfo y i a b cs e iso vw) z w) = idi_type k (tsynexp t) x (mktinfo y i a b (map (\(mkcons q e tbs flg).(mkcons q e (mapfst3 tsynexp tbs) flg)) cs) e iso vw) z w
||  ivexp (idi_view ot vw (idi_type k t x (mktinfo y i a b cs e iso vwt) z w)) = idi_view (tsynexp ot) vw (idi_type k (tsynexp t) x (mktinfo y i a b (map (\(mkcons q e tbs flg).(mkcons q e (mapfst3 tsynexp tbs) flg)) cs) e iso vwt) z w)
||  ivexp (idi_syn k t1 n t2) = idi_syn k t1 n (tsynexp t2)
||  ivexp (idi_class (clsi k t iits ss ins n)) = idi_class (clsi k t (mapthd tsynexp iits) ss ins n)
||  ivexp iv = iv
and invissyn (mktcons (i as mkid _ _ (idi_syn _ _ _ _) _) _) = 
    ~ (H1_3 & Prelude) & -- no expansion in 1.3 Prelude files.
    (SynExpand | ~(member eqid i es | idinprelude i | id_visibility i = Vimported))
||  invissyn _ = false
and tsynexp t =
    case while invissyn synexpand t in
	mktvar _ : t
    ||  mktap _ _ : t
    ||  mktcons i ts : mktcons i (map tsynexp ts)
    ||  mkterror _ : t
    ||  mktcontype k t : mktcontype k (tsynexp t)
    end
)
and idinprelude i = ~Curry | (inprelude (id_orignames i))	-- for lmlc -fboth pretend that everything comes from the prelude

and ltidecl (mkid _ _ (idi_inst (mkidecl _ c1 [t1] s1) _ _) _) -- ASSERT
	    (mkid _ _ (idi_inst (mkidecl _ c2 [t2] s2) _ _) _) = ltid c1 c2 | eqid c1 c2 & ltid (tname t1) (tname t2)

and yydup is =
let is' = sort ltidecl is in
let r = xxdup is'
in
--trace ("yydup "@show_list prid is@show_list prid r)
r

-- temporary(?) to remove duplicated instances
and xxdup ((i1 as (mkid _ _ (idi_inst (d1 as mkidecl _ c1 t1 s1) _ _) _)).
	   (i2 as (mkid _ _ (idi_inst (d2 as mkidecl _ c2 t2 s2) _ _) _)).is) & (eqid c1 c2 & And (map2 (\t1.\t2.eqtype (normtype t1) (normtype t2)) t1 t2)) =
    if s1 = "_" then xxdup (i2.is)
    else xxdup (i1.is)
||  xxdup (i.is) = i.xxdup is
||  xxdup [] = []

and usinginfo env strs =
    let strs' = [] in -- if H1_3 then [] else strs in
    let pl = length preludename in
    case difference ((map tl o filter (\s.head pl s ~= preludename & head 4 s ~= "_LML") o map idtostr o rids Kmodule) env) (map tl strs') in
       [] : ""
    || ss : "{-# IMPORTING "@mix ss ", " @ " #-}\n"
    end

and fixup es = if Curry then es else filter notconstr es
and notconstr (mkexpid (mkid _ _ (idi_constr _ _ _ _ _ _ _) _)) = false
||  notconstr _ = true

and hprpragmas sinf si i = 
	case assocdefeq eqid i pragmas [] in
	   [] : ""
	|| ps : hprspec sinf i si (crunch ps)
	end
and hprspec sinf i si ts = "{-# SPECIALIZE "@si@" :: "@mix (map (prttypestr sinf i) ts) ", " @ " #-}"
and crunch ps = (conc (mapfilter f ps)
	where f (Pspecialize _ ts) = Some (map fst ts)
	   || f _ = None)
and prttypestr sinf i t = 
	let tstr = prttype t in
	let n = specname (idtostr i) t in
	let sstr =
	    case assocdefeq eqidstr n sinf (-1,fail"",fail"") in
	       (-1,_,_) : ""
	    || (a,s,t)  : 
		let si = hprid i in
		hprfinfo true si (finfo a [] (s,t) (-1) None)
	    end
	in
--(if X4 then trace ("SPEC-str "@idtostr i@" "@sstr) else (\x.x))
	xhprttype t@sstr

and eqidstr s i = s=idtostr i
----------------------------------------------------
and prettypre :: (List(Id#(Ttype#(List Int)))) -> String
and prettypre gp = concmap (\ (i,(t,_)). hprid i @" :: "@hprttype (normtype t) @ "\n") gp

and xhprttype t = qhprttype curmod t
and fixid i = qfixid curmod i
and fixcontext k = qfixcontext curmod k
end
