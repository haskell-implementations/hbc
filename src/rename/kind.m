module -- kind
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/types_t.t"
#include "../expr/einfo_t.t"
#include "../expr/pprint.t"
#include "../misc/triple.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"

export computekinds;
rec
    computekinds b =
        let uses = collectuses b in
	let useerrs = [ msg ;; (mkterror msg, ts) <- uses ] in
	let depgraph0 = [ (i, (mkseteq eqid o concmap gettcons) ts) ;; (mktcons i tttt, ts) <- uses] in
	let defids = map fst depgraph0 in
	let depgraph = mapsnd (intereq eqid defids) depgraph0 in
	let groups = scceq eqid depgraph in
	if ~null useerrs then (useerrs, []) else
        let r = addkinderrs b (reduce (dogroup uses) ([],[]) (map (map fst) (reverse groups))) in
	    tracex ("uses="@show_list (show_pair (prttype, show_list prttype)) uses @ "\n" @
                    "depgraph="@show_list (show_pair (prid, show_list prid)) depgraph   @ "\n" @
                    "groups="@show_list (show_list (show_pair (prid, show_list prid))) groups @ "\n" @
		    "r="@show_list (show_pair (prid, show_Kind)) (snd r)
		  )
	r

and dogroup uses is (errs,kt) =
	let its = filter (\ (mktcons i _, _). member eqid i is) uses in
        let l = length its in
	let kt' = combine ([ i ;; (mktcons i vs, t) <- its], map mkkvar (from 0)) @ kt in
	let do1type vs kt kv t (s, u) =
	        let nvs = difference (getTvars t) vs in
		let kv' = combine (nvs, map mkkvar (from u)) @ kv in
	        let (k, (s', u')) = kW kt kv' t (s, u+length nvs) in
		--tracex ("do1type "@prttype t@show_Kind k@show_subst s') 
-- XXX makes synonyms with non * kind impossible
		(unifys k mkkground s', u') 
	in
	let do1def kt (mktcons i vts, ts) (s, u) =
	        let vs = map (\ (mktvar v).v) vts in
		let kv = combine (vs, map mkkvar (from u)) in
		let st = reduce (do1type vs kt kv) (s, u+length vs) ts in
		tracex ("do1def: "@prttype (mktcons i vts)@show_list prttype ts@show_list(show_pair(show_int,show_Kind)) kv)
		(map snd kv, st)
	in
	let (kss, (s, _)) = Umap (do1def kt') its (Yes [], l) in
	let s' = reduce (\ (ks,(_,v)).\s.unifys (mkkarrows ks mkkground) v s) s (combine (kss, kt')) in
tracex ("dogroup kss&kt'="@show_list (\ (ks,(i,v)).prid i@" :: U("@show_Kind(mkkarrows ks mkkground)@","@show_Kind v@")") (combine (kss, kt'))@"\ns="@show_subst s@"\ns'="@show_subst s'@"\nkt''="@show_kt (mapsnd (subst s') kt'))
        (adderr s' its errs, mapsnd (makeground o subst s') (head l kt') @ kt)

and adderr (Yes _) _ errs = errs
||  adderr (No msg) xs errs = ("[103] Kind error ("@msg@") among definition of: "@mix [prid i;; (mktcons i vs, ts) <- xs] ",") . errs

and makeground (mkkvar _) = mkkground
||  makeground (mkkarrow k1 k2) = mkkarrow (makeground k1) (makeground k2)
||  makeground k = k

-- errors can make us fail to find the tyvar, so fake it
and getkv kv v = assocdef v kv mkkground -- (fail "kW: didn't find tyvar")

and kW kt kv (mktvar v) st = (getkv kv v, st)
||  kW kt kv (mktcons i ts) st = doap kt kv (getkind i kt) ts st
||  kW kt kv (mktap v ts) st = doap kt kv (getkv kv v) ts st
||  kW kt kv (mktcontype ctx t) st = kW kt kv t (reduce (assertW kt kv) st ctx)
--||  kW _ _ t _ = fail ("no match in kW: "@prttype t)
||  kW _  _  _ st = (mkkvar (-1), st)
and doap kt kv ki ts st =
    let (ks, (s', u')) = Umap (kW kt kv) ts st in
    let v = mkkvar u' in
    let s'' = unifys ki (mkkarrows ks v) s' in
    (subst s'' v, (s'', u'+1))
and assertW kt kv (a as mkassert c v) st =
    let (k, (s, u)) = kW kt kv (as2ty a) st
    in  (unifys k mkkground s, u)
||  assertW _ _ _ st = st

and unify (mkkvar v) (k as mkkvar v') s = if v = v' then s else add1 v k s
||  unify (mkkvar v) k                s = if occurs v k then occerr v k else add1 v k s
||  unify k          (mkkvar v)       s = if occurs v k then occerr v k else add1 v k s
||  unify mkkground  mkkground        s = s
||  unify (mkkarrow k1 k2) (mkkarrow k1' k2') s = unifys k2 k2' (unifys k1 k1' s)
||  unify k1         k2               _ = No (show_Kind k1@" vs. "@show_Kind k2)

and unifys k1 k2 s = unify (subst s k1) (subst s k2) s

and occerr v k = No (show_Kind (mkkvar v)@" vs. "@show_Kind k@", occurence")

and add1 v k (Yes s) = Yes ( (v,k).mapsnd (subst1 v k) s )
||  add1 _ _ s = s
and subst1 v k (k' as mkkvar v') = if v=v' then k else k'
||  subst1 v k (mkkarrow k1 k2) = mkkarrow (subst1 v k k1) (subst1 v k k2)
||  subst1 _ _ k = k
and subst (Yes s) k = subst' s k
||  subst _ k = k
and subst' s (k as mkkvar v) = assocdef v s k
||  subst' s (mkkarrow k1 k2) = mkkarrow (subst' s k1) (subst' s k2)
||  subst' _ k = k

and occurs v (mkkvar v') = v = v'
||  occurs v (mkkarrow k1 k2) = occurs v k1 | occurs v k2
||  occurs _ _ = false

and getkind i kt = tracex ("getkind "@prid i@" kt="@show_kt kt) (assocdefeq eqid i kt (tracex ("imported:"@prid i@"::"@show_Kind (get_id_kind i)) (get_id_kind i)))

and show_kt kt = show_list (show_pair (prid, show_Kind)) kt
and show_subst (Yes s) = show_list (show_pair ((\i."v"@show_int i),show_Kind)) s
||  show_subst (No m) = m

-- collect used types (with contexts)
and collectuses b = mapsnd (mkseteq eqtype) (collectuses' b)
and collectuses' (mkbtype t cs _ _) = 
        [(tpart t, t . concmap (\ (mkcons _ (_,_,xs) tbsi _) . [as2ty (mkassert c v) ;; (mkassert c v)<-xs]  @ map fst3 tbsi) cs )]
||  collectuses' (mkbview t ot cs b) = 
        [(tpart t, ot . t . concmap (\ (mkcons _ (_,_,xs) tbsi _) . [as2ty (mkassert c v) ;; (mkassert c v)<-xs]  @ map fst3 tbsi) cs )]
||  collectuses' (mkband b1 b2) = collectuses' b1 @ collectuses' b2
||  collectuses' (mkbrec b) = collectuses' b
||  collectuses' (mkbsyn t1 t2) = [(t1, [t2])]
||  collectuses' (mkbclass (mkcdecl ctx (a as mkassert c v)) b) = 
	[(as2ty a, xmkcontype ctx (as2ty a) . getsign b)]
||  collectuses' _ = []
and getsign (mkband b1 b2) = getsign b1 @ getsign b2
||  getsign (mkbrec b) = getsign b
||  getsign (mkbsign _ t) = [t]
||  getsign _ = []
and gettcons (mktcontype ctx t) = concmap getca ctx @ gettcons t
||  gettcons (mkterror _) = []
||  gettcons (mktvar _) = []
||  gettcons (mktap _ l) = concmap gettcons l
||  gettcons (mktcons c l) = c . concmap gettcons l
and getca (mkassert c _) = [c]
||  getca _ = []

and tracex s x = if X2 then if s=s then trace s x else fail "?" else x

---- find kind errors throughout the program
and addkinderrs b (errs, kt) = (errs @ gkb b, kt)
and gkb (mkbpat pes) = concmap (\ (p,e).gkp p@gke e) pes
||  gkb (mkband b1 b2) = gkb b1 @ gkb b2
||  gkb (mkbrec b) = gkb b
||  gkb (mkblocal b1 b2) = gkb b1 @ gkb b2
||  gkb (mkbclass _ b) = gkb b
||  gkb (mkbinstance (mkidecl ctx c ts _) b _) = gkt None (xmkcontype ctx (mktcons c ts)) @ gkb b
||  gkb (mkbsign is t) = gkt (Some is) t
||  gkb _ = []
and gke (mkap f a) = gke f @ gke a
||  gke (mklam p e) = gkp p @ gke e
||  gke (mkcase e pes) = gke e @ concmap (\ (p,e).gkp p@gke e) pes
||  gke (mkletv b e) = gkb b @ gke e
||  gke (mkident _) = []
||  gke (mkconst _) = []
||  gke (mkbrack _ _) = [] -- not really
||  gke (mkerror _) = []
||  gke (mkconstr _ es) = concmap gke es
||  gke (mkfailmatch _) = []
||  gke (mkinfo (restr _ t) e) = gke e @ gkt None t
||  gke (mkinfo _ e) = gke e
||  gke (mklistf _ es) = concmap gke es
||  gke (mklistg e qs) = gke e @ concmap gkq qs
||  gke (mkwhere pes b) = concmap (\ (p,e).gkp p@gke e) pes @ gkb b
||  gke (mkrecord _ ies _) = concmap (gke o snd) ies
||  gke (mkcfunction _ _) = []
||  gke e = fail ("no match in gke: "@ppr e)
and gkp (mkcondp _ e) = gke e
||  gkp _ = []
and gkq (mkqgen p e) = gkp p @ gke e
||  gkq (mkqfilter e) = gke e
||  gkq (mkqlet b) = gkb b
and gkt x t =
    let vs = getTvars t in
    let kv = combine (vs, map mkkvar (from 0)) in
    let (k, (s, _)) = kW [] kv t (Yes [], length kv) in
    case unifys k mkkground s in
       Yes _ : []
    || No msg : 
       case x in
	  None : 
	      case getterr t in
	         [] : ["[104] Kind error ("@msg@") in: "@hprttype t]
	      || r._ : [r]
	      end
       || Some is : ["[105] Kind error ("@msg@") in signature: "@mix (map prid is) "," @ " :: "@hprttype t]
       end
    end
and getterr (mktcons i _) & (head 4 (idtostr i) = "[22]") = [idtostr i]
||  getterr (mktcons _ ts) = concmap getterr ts
||  getterr (mktvar _) = []
||  getterr (mkterror s) = [s]
||  getterr (mktcontype _ t) = getterr t
||  getterr (mktap _ ts) = concmap getterr ts
end
