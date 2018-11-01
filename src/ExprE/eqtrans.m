module -- eqtrans
-- Insert bigeq instead of eq where necessary.
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/types_t.t"
#include "../expr/ttype.t"
#include "../expr/tinfo.t"
#include "../expr/booltree.t"
#include "../expr/constr_t.t"
#include "../expr/einfo_t.t"
#include "../type/subst_t.t"
#include "../type/subst.t"
#include "../type/unify.t"
#include "Expr_t.t"
#include "../funnos.h"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../transform/hexpr.t"
export eqtrans;
rec bigt = [
(hieq, mkid Fbigeq "Pbigeq" idipre2f noorigname);
(hine, mkid Fbigne "Pbigne" idipre2f noorigname);
(hile, mkid Fbigle "Pbigle" idipre2f noorigname);
(hige, mkid Fbigge "Pbigge" idipre2f noorigname);
(hilt, mkid Fbiglt "Pbiglt" idipre2f noorigname);
(higt, mkid Fbiggt "Pbiggt" idipre2f noorigname)
]
and tagt = [
(hieq, mkid Feq "Peq" idipre2 noorigname);
(hine, mkid Fne "Pne" idipre2 noorigname);
(hile, mkid Fle "Ple" idipre2 noorigname);
(hige, mkid Fge "Pge" idipre2 noorigname);
(hilt, mkid Flt "Plt" idipre2 noorigname);
(higt, mkid Fgt "Pgt" idipre2 noorigname)
]
and dtagt = [
(hieq, mkid Fdfeq "Pdeq" idipre2 noorigname);
(hine, mkid Fdfne "Pdne" idipre2 noorigname);
(hile, mkid Fdfle "Pdle" idipre2 noorigname);
(hige, mkid Fdfge "Pdge" idipre2 noorigname);
(hilt, mkid Fdflt "Pdlt" idipre2 noorigname);
(higt, mkid Fdfgt "Pdgt" idipre2 noorigname)
]
and strt = [
(hieq, mkid Fstreq "Pstreq" idipre2f noorigname);
(hine, mkid Fstrne "Pstrne" idipre2f noorigname);
(hile, mkid Fstrle "Pstrle" idipre2f noorigname);
(hige, mkid Fstrge "Pstrge" idipre2f noorigname);
(hilt, mkid Fstrlt "Pstrlt" idipre2f noorigname);
(higt, mkid Fstrgt "Pstrgt" idipre2f noorigname)
]
and xxdocmp = hidocmp --mkid 111 "Pdocmp" idipre2f noorigname
and big i = assocdefeq eqid i bigt (fail "big")
and tag i = assocdefeq eqid i tagt (fail "big")
and dtag i= assocdefeq eqid i dtagt (fail "big")
and str i = assocdefeq eqid i strt (fail "big")
and v n = btors [btvar n]
and v01 = btands [v 0; v 1]
and idipre2f = idi_var (var_pre (finfo 2 [] (v01, btff) 2 None)) Onotype None
and idipre2  = idi_var (var_pre (finfo 2 [] (v01, v01) 2 None)) Onotype None
and cmpop i = member eqid i [hieq; hine; hile; hige; hilt; higt]
and czero = Cconstr "0" Tint ITint 0 (false,[],[]) []
and notarrow (t as mktcons ti _) = 
    if is_syn t then
        notarrow (synexpand t)
    else
	-- Even if it's not an arrow it may be a function 
	-- if it's an elided contructor, or an abstract type (which may be a synonym).
	let (inf as (mktinfo _ _ _ flat cs _ iso _)) = gettinfo ti in
	~ (eqid ti hiARROW | null cs & ~flat | iso)
||  notarrow _ = false
and isbas S (Einfo (trestr t) _) = flattype (TRtype S t)
||  isbas S _ = false
and isflt S (Einfo (trestr t) _) =
    let t' = TRtype S t in
    eqtype t' Tsfloat | eqtype t' Tdfloat
||  isflt S _ = false
and isstr S (Einfo (trestr t) _) = 
    let t' = TRtype S t in
    eqtype t' Tstring | eqtype t' (Tlist Tchar)
||  isstr S _ = false
and eqtrans S e = eqt true S e
and
    eqt b S (Emodule i expl dl) = Emodule i expl (map (mapsnd (eqt b S)) dl)
||  eqt b S (Ecase e pl dp) = 
   		Ecase (eqt false S e) (mapthd (eqt true S) pl) (eqt true S dp)
||  eqt b S (Elet r dl e) = Elet r (mapsnd (eqt false S) dl) (eqt b S e)
||  eqt b S (Econstr c el) = Econstr c (map (eqt false S) el)
||  eqt b S (e as Efailmatch n) = e
||  eqt b S (e as Ecfunction _ _) = e
||  eqt true S (Einfo (trestr t) (e as Eidapl _ [])) =
    let tx = synexpandall (TRtype S t) in
	if notarrow tx then
		Einfo (noarrow tx) e
	else
		e
||  eqt b S (Eidapl i (el as [_;_])) & (~Curry & cmpop i) =
        if exists (isbas S) el then
	    Eidapl (tag i) (map (eqt false S) el)
        else if exists (isflt S) el then
	    Eidapl (dtag i) (map (eqt false S) el)
        else if Strcmp & ~nuflag & exists (isstr S) el then
	    Eidapl (str i) (map (eqt false S) el)
	else
	    if nuflag then
		Eidapl (tag i) [Eidapl xxdocmp (map (eqt false S) el); Econstr czero [] ]
	    else
		Eidapl (big i) (map (eqt false S) el)
||  eqt b S (Eidapl i el) = Eidapl i (map (eqt false S) el)
||  eqt b S (Elaml il e) = Elaml il (eqt true S e)
||  eqt b S (Einfo (restr _ _) e) = eqt b S e
||  eqt b S (Einfo (trestr _) e) = eqt b S e
||  eqt b S (Einfo (srestr _) e) = eqt b S e
||  eqt b S (Einfo f e) = Einfo f (eqt b S e)
||  eqt b S _ = fail "eqtrans"
end
