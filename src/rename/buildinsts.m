module
#include "../misc/flags.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../misc/sort.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "renenv.t"
export buildinsts, addmets;
rec buildinsts sups env =
    let is0 = map (\(v as mkid _ _ (idi_inst t _ _) _).info v t) (filter okinst (filter id_isinst (rids Kmeth env))) in
    let iss = map (sort (\ (_,_,_,_,_,m1). \ (_,_,_,_,_,m2). length m1 > length m2))
	          (groupsort (\ (_,x1,y1,_,_,_).\ (_,x2,y2,_,_,_).id_no x1 < id_no x2 | id_no x1 = id_no x2 & id_no y1 < id_no y2) is0) in
    let is = concmap (seli o mkseteq xeq) iss in
    let t2i = map mkt (groupsort (\ (_, _, t1,_,_,_).\ (_, _, t2,_,_,_).id_no t1 < id_no t2) is)
    and c2i = map mkc (groupsort (\ (_, t1, _,_,_,_).\ (_, t2, _,_,_,_).id_no t1 < id_no t2) is) in
--trace (show_list (show_list (\ (_, c, ti, _, vs, _).prttype (mktcons c [mktcons ti vs]))) iss)
    ((c2i, t2i, sups), if AllowRedef then [] else map badmsg (filter ismultiinst iss))
and ismultiinst is =
    case mkseteq (\ (_,_,_,_,_,m1). \ (_,_,_,_,_,m2) .m1=m2) is in
       [_] : false
    || [_;(_,_,_,_,_,"_")] : false
    || _ : true
    end
and badmsg is = "[90] Multiple instance declarations "@show_list (\ (ts,c,t,v,vs,ms)."instance "@pridecl (mkidecl ts c [mktcons t vs] ms)) is

-- XXX okinst is a hack to get rid of instances that contain type/classes we do not know
and okinst (mkid _ _ (idi_inst (mkidecl _ c [mktcons ti _] _) _ _) _) = -- ASSERT
	not (id_isudef c) & not (id_isudef ti)

and info v (mkidecl ts c [mktcons ti vs] ms) = (ts, c, ti, v, vs, ms) -- ?inst -- ASSERT
and mkt (xs as (_,_,t,_,_,_)._) = (t, map (\(ts,c,t,v,vs,ms).(v,mkidecl ts c [mktcons t vs] ms)) xs) -- ASSERT
and mkc (xs as (_,i,_,_,_,_)._) = (i, map (\(ts,c,t,v,vs,ms).(v,mkidecl ts c [mktcons t vs] ms)) xs) -- ASSERT

and xeq (_,_,ti1,_,vs1,_) (_,_,ti2,_,vs2,_) = eqtype (normtype (mktcons ti1 vs1)) (normtype (mktcons ti2 vs2))
and seli is = if UseSpecInst then is else filter (\ (_,_,_,_,vs,_).all istvar vs) is
and istvar (mktvar _) = true
||  istvar _ = false

and metids (mkid _ _ (idi_inst _ is _) _) = is
||  metids _ = []
and addmets r =
    let mids = concmap metids (rids Kmeth r) in
    rjoin (rlist Kmeth mids) r

--------
and id_isudef (mkid _ _ idi_udef _) = true
||  id_isudef (mkid 0 ('['.'2'.'2'.']'._) _ _) = true
||  id_isudef _ = false
end
