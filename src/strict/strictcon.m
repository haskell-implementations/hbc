module
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Esubst.t"
#include "../expr/einfo_t.t"
#include "../expr/id.t"
#include "../expr/tinfo.t"
#include "../misc/misc.t"
#include "../misc/util.t"
#include "../misc/flags.t"
#include "../ExprE/Eprint.t"
#include "../simpl/sutil.t" /* refc */

export strictcon;
rec
   strictcon e = S true e
and isisol [(c, _, _)] = isiso c
||  isisol _ = false
and isiso (Cconstr _ _ ti _ _ _) = get_isotype ti
||  isiso _ = false
and isid (Eidapl _ []) = true
||  isid _ = false
and isap (Eidapl _ _) = true
||  isap _ = false
and iscanon (Econstr c _) = ~isstring c
||  iscanon _ = false
and
   S b (e0 as Ecase e1 cs e2) = 
	if isisol cs then
		S b (case cs in
		   [(c, [i], e)] : 
		     let e' = 
		       case e2 in
			   Efailmatch _: e
                       ||  _ :
			       case e in
				   Ecase e11 cs1 (Efailmatch 1) : Ecase e11 cs1 e2
--			       ||  _ : fail ("Unexpected expression in strictcon "@pr e)
				   -- the e2 expression is unreachable
			       ||  _ : e
			       end
		       end
		     in 
		     if isap e1 & assocdefeq eqid i (refc (xgetid e')) 0 <= 1 then Esubst e1 i e' else Elet false [(i,e1)] e'
--		     simpl u [] (Elet false [(i,e1)] e')
		end)
	else
		Ecase (S true e1) (mapthd (S true) cs) (S true e2)
|| S b (Elet r ds e) = Elet r (mapsnd (S false) ds) (S b e)
|| S b (Emodule i expl ds) = Emodule i expl (map (mapsnd (S b)) ds)
|| S b (Econstr c es) =
	let tbs = cargs c in
	if isiso c then
	    S b (hd es)
	else if exists snd tbs then
		let e' = Econstr c (map2 (\e.\(_,b).if b then einfos (S true e) else S false e) es tbs)
		in if b | all isoke (combine (tbs,es)) then
			e'
		else
			Elaml [] e'
	else
		Econstr c (map (S false) es)
|| S b (Efailmatch n) = Efailmatch n
|| S b (e as Ecfunction _ _) = e
|| S b (Eidapl i es) = Eidapl i (map (S false) es)
|| S b (Elaml is e) = Elaml is (S true e)
|| S b (Einfo strict e) = Einfo strict (S true e)
|| S b (Einfo f e) = Einfo f (S b e)
|| S _ e = fail ("strictcon "@pr e)
-- Eap, Elam, Evar cannor occur 
and isoke ((_, false), _) = true
||  isoke (_, (Einfo noeval _)) = true
||  isoke (_, e) = iscanon e
and einfos (e as (Einfo noeval _)) = e
||  einfos e = Einfo strict e
end
