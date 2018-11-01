module -- renenv
-- Handle environment for renaming.
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"	/* pridecl */

export rnil, rone, rmany, rjoin, rjoin1, rlist, rfind, rids, rperm, RKind, show_Renv, rstrs, envinfo,
       rmaptemp, rjoin1type,
       rfindid, rmkonametbl,
       rdbldefs, rfindselid,
       rsetct, rgetct,					-- class and instance tables
       rcone, rgetcenv,                                 -- conctypes
       rgetpragma, raddpragma,
       raddkt, rgetkind;

#define TEMP (List Id)
#define PERM (LArray TEMP)
#define HASH 11

#define GET(p,i) ((p) ? (i))

#define IDECLTAB (List (Id # (List (Id # IDecl))))

rec type Renv = renv (Option (LArray (List (String # Id))))	-- position info
    		     TEMP PERM TEMP PERM TEMP PERM TEMP PERM 
                  -- class2inst    type2inst    supers
                     (IDECLTAB  #  IDECLTAB  #  (List (Id # (List (Id # (List Int))))))
                     (List(Ttype#(List(IdOrConstr#Prod)))) -- conctype grammar
                     (List (Id#(List Pragma))) -- pragmas for ids
                     (List (Id#Kind)) -- kind table
and type RKind = Kvalue + Ktype + Kmodule + Kmeth + Kall

and np = array 0 (HASH-1) (\x.x) []
-- Hash string (and make it fast!)
-- skip first char since its mostly the same.
and hash [] = 0			-- Should never occur
||  hash [c1] = ord c1 % HASH
||  hash [c1;c2] = ord c2 % HASH
||  hash [c1;c2;c3] = (ord c2 + ord c3) % HASH
||  hash [c1;c2;c3;c4] = (ord c2 + ord c3 + ord c4) % HASH
||  hash [c1;c2;c3;c4;c5] = (ord c2 + ord c3 + ord c4 + ord c5) % HASH
||  hash (c1.c2.c3.c4.c5.c6.cs) = (ord c2 + ord c3 + ord c4 + ord c5 + ord c6 + hash cs) % HASH
and noct = ([],[],[]) --fail "ct not installed in renv"
and nonametbl = None
and rnil = renv nonametbl [] np [] np [] np [] np noct [] [] []
and rone k i = rinj k [i]
and rmany k is = rinj k is
and rinj k e =
	case k in
	    Kvalue : renv nonametbl e  np [] np [] np [] np noct [] [] []
	||  Ktype  : renv nonametbl [] np e  np [] np [] np noct [] [] []
	||  Kmodule: renv nonametbl [] np [] np  e np [] np noct [] [] []
	||  Kmeth  : renv nonametbl [] np [] np [] np  e np noct [] [] []
	end
and rmaptemp f (renv ont v pv t pt m pm n pn ct g ps kt) =
	(renv ont (map f v) pv (map f t) pt m pm n pn ct g ps kt)
and rsel Kvalue (renv _ e _ _ _ _ _ _ _ _ _ _ _) = e
||  rsel Ktype  (renv _ _ _ e _ _ _ _ _ _ _ _ _) = e
||  rsel Kmodule(renv _ _ _ _ _ e _ _ _ _ _ _ _) = e
||  rsel Kmeth  (renv _ _ _ _ _ _ _ e _ _ _ _ _) = e
and rselp Kvalue (renv _ _ e _ _ _ _ _ _ _ _ _ _) = e
||  rselp Ktype  (renv _ _ _ _ e _ _ _ _ _ _ _ _) = e
||  rselp Kmodule(renv _ _ _ _ _ _ e _ _ _ _ _ _) = e
||  rselp Kmeth  (renv _ _ _ _ _ _ _ _ e _ _ _ _) = e
and rjoin (renv None av _ at _ am _ an _ _ g1 ps1 kt1) (renv None bv pv bt pt bm pm bn pn ct g2 ps2 kt2) = renv nonametbl (av@bv) pv (at@bt) pt (am@bm) pm (an@bn) pn ct (g1@g2) (ps1@ps2) (kt1@kt2)
and rjoin1 (renv ont v pv t pt m pm n pn ct g ps kt) Kvalue  i = renv ont (i.v) pv t pt m pm n pn ct g ps kt
||  rjoin1 (renv None v pv t pt m pm n pn ct g ps kt) Ktype   i = renv None v pv (i.t) pt m pm n pn ct g ps kt
||  rjoin1 (renv ont v pv t pt m pm n pn ct g ps kt) Kmodule i = renv ont v pv t pt (i.m) pm n pn ct g ps kt
and rjoin1type (renv _ v pv t pt m pm n pn ct g ps kt) i = rmkonametbl (renv None v pv (i.t) pt m pm n pn ct g ps kt)
and rsetct (renv ont v pv t pt m pm n pn _  g ps kt) ct = renv ont v pv t pt m pm n pn ct g ps kt
and rgetct (renv _ v pv t pt m pm n pn ct g ps kt) = ct
and raddkt kt (renv ont v pv t pt m pm n pn ct g ps _) = renv ont v pv t pt m pm n pn ct g ps kt
and rgetkind i (renv _ v pv t pt m pm n pn ct g ps kt) = 
    assocdefeq eqid i kt (fail ("No kind for "@dprid i))
and rlist k r = rinj k r
and undefid = dummyid
and rmkonametbl (r as renv _ v pv t pt m pm n pn ct g ps kt) =
    let ont = array 0 (HASH-1) (\x.x) [let s = mstr@"."@estr in (hash s, (s, i)) ;; i <- t @ conc (aext pt); (mkid u ss ii (Orignames v f (MI mstr,estr))) <- [i] ]
    in  renv (Some ont) v pv t pt m pm n pn ct g ps kt
and rfindid Ktype (i as mkidi _ (Some (MI mstr, estr)) _ _) (r as renv (Some ont) _ _ _ _ _ _ _ _ _ _ _ _) = 
    let s = mstr@"."@estr in
    let rec look [] = undefid
	||  look ((si,i).l) = if s=si then i else look l in
    let ri = look (ont ? hash s)
    in  ri --trace ("rfindid "@dprid i@" "@dprid ri) ri
||  rfindid k i r = rfind k (idtostr i) r
and rfind Kall s (renv _ v _ t _ m _ n _ _ _ _ _) = lookfor s (t@v@m@n) undefid
||  rfind k s r = 
-- This should be fast since we don't build the second call to lookfor.
-- A lot of time is spent here in rename, so maybe it is worth it.
-- (The use of mkids "" to detect a non-hit is not accidental, using dummyid and checking if the
-- found id-no is 0 causes a (rare) data-dependency loop!)
--trace ("rfind "@s)
    case lookfor s (rsel k r) (mkids "") in
	mkids "" : lookfor s (GET(rselp k r, hash s)) undefid
    ||  i : i
    end
and lookfor s [] d = d
||  lookfor s ((i as mkid _ s1 _ _).is) d = if s=s1 then i else lookfor s is d
and rstrs Kall r = map idtostr (rsel Ktype r @ rsel Kvalue r) -- are Kmethod and Kmodule needed??
||  rstrs k r = map idtostr (rsel k r)
and aext a = [a ? i ;; i<-[lowerbound a..upperbound a]]
and rids Kall (renv _ v ov t ot m om n on _ _ _ _) = conc (conc [v.aext ov; t.aext ot; m.aext om; n.aext on])
||  rids k r = conc (rsel k r.aext (rselp k r))
and rperm (renv _ v ov t ot m om n on ct g ps kt) = renv nonametbl [] (hashl v ov) [] (hashl t ot) [] (hashl m om) [] (hashl n on) ct g ps kt
and hashl is old = array 0 (HASH-1) (\x.x) (map (\(i as mkid _ s _ _).(hash s, i)) (conc (is.aext old)))

and show_Renv (renv _ x xs y ys z zs w ws (ct,_,_) _ _ _) = 
    "Values:    "@show_list prid (conc (x.aext xs))@
  "\nTypes:     "@show_list prid (conc (y.aext ys))@
--  "\nMethods:   "@show_list prid (conc (w.aext ws))@
  "\nModules:   "@show_list prid (conc (z.aext zs))@
--  "\nInstances: "@show_list (show_pair(prid,show_list (show_pair(prid,pridecl)))) ct@
  "\n"


and rcone p = renv nonametbl [] np [] np [] np [] np noct [p] [] []
and rgetcenv (renv _ _ _ _ _ _ _ _ _ _ g _ _) = g

and raddpragma (renv ont a b c d e f g h i j k kt) ps = renv ont a b c d e f g h i j (k@ps) kt
and rgetpragma (renv _ _ _ _ _ _ _ _ _ _ _ ps _) = ps

and envinfo (renv _ vt vp tt tp mt mp nt np ct _ _ _) = 
    "\nEnvironment info:\n"@envi "Value " vt vp@envi "Type  " tt tp@envi "Module" mt mp@envi "Method" nt np@"\n"
and envi s t p = s@" "@itos (length t)@" trans. "@
                 let ls = [length (p ? i) ;; i<-[lowerbound p..upperbound p]] in 
                 itos (Sum ls)@" perm. ("@itos (Minl ls)@","@itos (Maxl ls)@")\n"


and strlt i1 i2 = idtostr i1 < idtostr i2

-- Check if the transient part of nenv overlaps the permanent part of env.
and rdbldefs k nenv env = 
    let nids = sort strlt (rsel k nenv) in
    let p = rselp k env in
    concmap (\i.ldbl nids (sort strlt (GET(p, i)))) (count 0 (HASH-1))
and ldbl (iis1 as (s1.is1)) (iis2 as (s2.is2)) =
    if strlt s1 s2 then
	ldbl is1 iis2
    else if strlt s2 s1 then
	ldbl iis1 is2
    else
#if 0
	if id_orignames s1 = id_orignames s2 then -- Allow same thing to reappear (unique numbers?)
	    ldbl is1 is2
	else
#endif
	    idtostr s1.ldbl is1 is2
||  ldbl _ _ = []


and rfindselid s r = 
    case lookforsel s (rsel Kvalue r) (mkids "") in
	mkids "" : lookforsel s (GET(rselp Kvalue r, hash s)) (mkids "")
    ||  i : i
    end
and lookforsel s [] d = d
||  lookforsel s ((i as mkid _ s1 (idi_var _ _ (Some _)) _).is) d & (s=s1) = i
||  lookforsel s (i.is) d = lookforsel s is d
end
