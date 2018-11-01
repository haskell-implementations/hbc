module
-- Change method selection of known class vectors into the selected
-- component.
#include "../expr/id.t"
#include "../expr/id_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/constrfun.t"
#include "../expr/einfo_t.t"
#include "../expr/booltree.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../rename/renenv.t"
#include "../rename/renameutil.t" /* mkcompound */
#include "../main/topexprs.t"
#include "../transform/hexpr.t"
#include "../transform/cutil.t"
#include "predef.t"
#include "Expr_t.t"
#include "Eutil.t"
#include "Eprint.t"
#include "../funnos.h"
export classtrans, xflatsuper, id_callmethod;

rec classtrans e = if Curry then cltr e else e

and cltr (Emodule i expl dl) = 
        let dl' = map (mapsnd clt) dl in
    	Emodule i expl dl'
and
    clt (Eap e1 e2) = Eap (clt e1) (clt e2)
||  clt (Ecase e1 cl e2) = Ecase (clt e1) (mapthd clt cl) (clt e2)
||  clt (Elet r dl e) = Elet r (mapsnd clt dl) (clt e)
||  clt (Econstr c el) = Econstr c (map clt el)
||  clt (Efailmatch n) = Efailmatch n
||  clt (e as Ecfunction _ _) = e
||  clt (Eidapl i es) = chkmetxx i es (map clt es)
||  clt (Elaml is e) = Elaml is (clt e)
||  clt (Einfo f e) = Einfo f (clt e)

and chkmetxx i oes es =
	let r = chkmet i oes es in
--	trace ("chkmet "@prid i@" "@show_list pr es@" = "@pr r)
	r

and chkmet (mkid _  _ (idi_method [k] _ _) _) _ (Econstr _ [Eidapl i _; _].ys) & (eqid i genEq & length ys=2 & GenCmp) = 
	Eidapl (select (k+1) bigeqops) ys
||  chkmet (mkid _  _ (idi_method [k] _ _) _) _ (Econstr _ [Eidapl i _; _].ys) & (eqid i genOrd & length ys=2 & GenCmp & iscmpop k) =
	Eidapl (select (k+1) bigordops) ys

||  chkmet (mkid os _ (idi_method [k] _ (clsi _ c _ sis _ sm)) _) (Eidapl (mkid _ _ (idi_inst idcl mis der) _) xs._) (_.es) =
	if (length mis >= k) then
	    constconv (select (k+1) mis) (map clt xs@es)
	else
	    fail "bad method number in chkmet"
||  chkmet (i as mkid _ _ _ _) es' [e1; e as Econstr c []] & (FastConst & eqid i hifromInteger & isinteger c) =
    	chkmet hiPfromInteger es' [e1; Econstr ctriple [e; Emkint (stoi (cname c)); Econstr (mkdfloat (butlast (cname c))) []]]
||  chkmet (i as mkid _ _ _ _) es' [e1; e as Econstr c []] & (FastConst & eqid i hifromRational & isrational c) =
    	chkmet hiPfromRational es' [e1; Econstr cpair [e; Econstr (mkdfloat (cname c)) []]]
-- change fromInteger&fromRational on a constant into PfromInteger or PfromRational
-- transform method application into a call to the vector with the method number
||  chkmet (mkid _  _ (idi_method [k] a (clsi _ _ _ sis _ sm)) _) (Eidapl i xs . _) (_.ys) =
    	let [offset] = flatsuper i (xs @ [Emkint (k+sm)]) in
    	let e = Eidapl (id_callmethod (a+2)) (Eidapl i [] . offset . ys) in
	if length ys = a then
            e
        else
            Einfo (forcearity (a-length ys)) e
||  chkmet (i as mkid _  _ (idi_inst idcl _ true) _) _ es & (cmpclass idcl & all isder es & (~builtin idcl | ~null es) & GenCmp) =
--trace ("big "@prid i@show_list pr es) (
        Econstr (CTuple 2) [Eidapl (bigvec idcl) []; Econstr hcunit []]
--)
||  chkmet i _ (es as (_._)) & (id_isdict i) = let [offset] = flatsuper i es in Eidapl (id_callmethod 2) [Eidapl i []; offset]
||  chkmet i _ es & (id_isinst i) = mkDict i es
||  chkmet i _ es = Eidapl i es

and id_isdict (mkid _ _ (idi_var (var_dict ci) _ _) _) = true
||  id_isdict _ = false

and flatsuper i xs = xflatsuper Emkint (\ (Econstr c _).cno c) i xs
and fakeid (mkid _ _ (idi_var (var_dict ci) _ _) _) =
 	let bot = fail "flatsuper" in
	mkid bot bot (idi_class ci) bot
and xflatsuper imk iget i xs = 
    if FlatSuper then
	[imk (dosupsel (fakeid i) (map iget xs))]
    else
	xs

--Length of the vector (including flattened superclasses)
and veclen (mkid _ _ (idi_class (clsi _ _ ms sups _ nsup)) _) =
	nsup + length ms + Sum (map (veclen o fst) (head nsup sups))
and curveclen (mkid _ _ (idi_class (clsi _ _ ms sups _ nsup)) _) = nsup + length ms
and dosupsel _ [n] = n
||  dosupsel ci (n.ns) = supoffs ci n + dosupsel (fst (select (n+1) (getsups ci))) ns
and supoffs ci n = curveclen ci + Sum (map (veclen o fst) (head n (getsups ci)))
and getsups (c as mkid _ _ (idi_class (clsi _ _ _ sup _ _)) _) = sup
||  getsups c = fail ("No match in getsups "@prid c)

and iscmpop k = k < 4  -- the first 4 operations are the comparisons

and xxxxx i s = if id_no i = 0 & Curry then fail ("predef-ct undef"@s@"\n"@show_Renv preenv) else i --!!! --XXX
and mf s = xxxxx (rfind Kmeth  s preenv) s
and vf s = xxxxx (rfind Kvalue s preenv) s
and addq q s = if QualifiedNames then q@"."@s else s
and IfromI = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Integer"; "fromInteger"])
and ifromI = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Int"; "fromInteger"])
and FfromR = mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Float"; "fromRational"])
and DfromR = mf (mkcompound ["MM"; mkcompprel "Fractional"; mkcompprel "Double"; "fromRational"])
and CFfromR = mf (mkcompound ["MM"; mkcompprel "Fractional"; addq "Complex" "Complex~" @ mkcompprel "Float"; "fromRational"])
and CDfromR = mf (mkcompound ["MM"; mkcompprel "Fractional"; addq "Complex" "Complex~" @ mkcompprel "Double"; "fromRational"])
and ItoI   = mf (mkcompound ["MM"; mkcompprel "Integral"; mkcompprel "Integer"; "toInteger"])
and FfromI = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Float"; "fromInteger"])
and DfromI = mf (mkcompound ["MM"; mkcompprel "Num"; mkcompprel "Double"; "fromInteger"])
and CFfromI = mf (mkcompound ["MM"; mkcompprel "Num"; addq "Complex" "Complex~" @ mkcompprel "Float"; "fromInteger"])
and CDfromI = mf (mkcompound ["MM"; mkcompprel "Num"; addq "Complex" "Complex~" @ mkcompprel "Double"; "fromInteger"])
and hiPfromInteger = vf "PfromInteger"
and hiPfromRational = vf "PfromRational"
and constconv i [e as Econstr c []] =
             if eqid IfromI i & isinteger c then e
        else if eqid ItoI   i & isinteger c then e
	else if eqid ifromI i & isinteger c then Emkint (stoi (cname c))
        else if eqid FfromI i & isinteger c then Esfloat (butlast (cname c))
        else if eqid FfromR i & isrational c then Esfloat (cname c)
        else if eqid DfromI i & isinteger c then Edfloat (butlast (cname c))
        else if eqid DfromR i & isrational c then Edfloat (cname c)
--- XXX 1.3 complex
	else if ~H1_3 & eqid CFfromI i & isinteger c then Econstr ccmplx [Esfloat (butlast (cname c)); Esfloat "0"]
	else if ~H1_3 & eqid CFfromR i & isrational c then Econstr ccmplx [Esfloat (cname c); Esfloat "0"]
	else if ~H1_3 & eqid CDfromI i & isinteger c then Econstr ccmplx [Edfloat (butlast (cname c)); Edfloat "0"]
	else if ~H1_3 & eqid CDfromR i & isrational c then Econstr ccmplx [Edfloat (cname c); Edfloat "0"]
        else Eidapl i [e]
||  constconv i es = Eidapl i es

and Esfloat s = Econstr (mksfloat s) []
and Edfloat s = Econstr (mkdfloat s) []
and cpair   = nth_constr 0 (gettinfo (hituple 2))
and ctriple = nth_constr 0 (gettinfo (hituple 3))
and ccmplx  = nth_constr 0 (gettinfo (hiComplex))

-- Utils to convert to fast comparison
and genEq = vf "VV_Eq_a"
and genOrd = vf "VV_Ord_a"
and opno (mkidecl _ c _ _) k = if eqid c hiEq then k+1 else k+3
and cmpclass (mkidecl _ c _ _) = eqid c hiEq | eqid c hiOrd
and isder (e as Econstr _ [Eidapl i _; ex]) = 
--    let r =
	   case i in
	      (mkid _ _ (idi_inst c _ true) _) : cmpclass c & (nsup i=0 | nsup i=1 & isder ex | case ex in Econstr _ es : all isder es || _ : false end)
           || _ : eqid i genEq | eqid i genOrd
           end
--    in  trace ("isder "@pr e@" "@show_bool r) r
||  isder _ = false
and bigvec (mkidecl _ c _ _) = if eqid c hiEq then genEq else genOrd
and builtin (mkidecl _ _ [mktcons t _] _) = member eqid t [hiInt; hiInteger; hiSFloat; hiDFloat; hiChar; hiBool] -- ASSERT
||  builtin _ = false

and id_callmethod n = 
    let s = ("Pcallmethod"@itos n) in
    let lpfinfo = var_global /*var_pre*/ (finfo n [] (bttt,btff) n None)
    and lptype = Onotype --Ohastype (Tarr (Tvar 1) (Tarr (Tvar 2) (Tvar 1))) [1; 2] None
    and lporig = Orignames Vimported Nofixity (MI preludeBuiltin, s)
    in  mkid (Fcallmethod+n) s (idi_var lpfinfo lptype None) lporig

and nsup (mkid _  _ (idi_inst (mkidecl aas _ _ _) _ _) _) = length aas
and mkDict i es =
    if nsup i = length es then
        Econstr (DTuple 2) [Eidapl i []; 
			    case length es in
			       0 : Econstr hcunit []
			    || 1 : hd es
			    || k : Econstr (DTuple k) es
			    end]
    else
	--fail ("mkDict nsup="@itos (nsup i)@" "@pr (Eidapl i es))
	Eidapl i es
and DTuple n = CXTuple Interactive n
end
