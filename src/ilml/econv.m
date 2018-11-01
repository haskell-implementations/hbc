module
#include "../misc/misc.t"
#include "../expr/id_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eprint.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/ttype.t"
#include "../expr/impexp_t.t"
#include "../expr/impexp.t"
#include "../transform/hexpr.t"
#include "cast.t"
#include "cons.t"
#include "cno.t"
#include "id.h"
#include "cexpr.t"
#include "mdep.t"
#include "../funnos.h"
export Econv;
rec Econv (eee as Emodule _ _ ds) = mapsnd econv (conc ds)
and econv (Ecase e ps d) = 
		if isflat ps then
			ecasef (econv e) (mapthd econv ps) (econv d)
		else
			ecase (econv e) (mapthd econv ps) (econv d)
||  econv (Elam i e) = Lam i (econv e)
||  econv (Eap e1 e2) = Ap (econv e1) (econv e2)
--||  econv (Evar i) & (iscallmethod i) = icallmet i
||  econv (Evar i) = Var i
||  econv (Econstr (Cconstr _ t _ n _ _) []) & (eqtype t Tint) = imkint n
||  econv (Econstr (Cconstr _ t _ n _ _) []) & (eqtype t Tchar) = Constant (CChar (chr n))
||  econv (Econstr (Cconstr s t _ _ _ _) []) & (eqtype t Tsfloat) = Constant (CSFloat s)
||  econv (Econstr (Cconstr s t _ _ _ _) []) & (eqtype t Tdfloat) = Constant (CDFloat s)
||  econv (Econstr (Cconstr s t _ _ _ _) []) & (eqtype t Tinteger) = Constant (CInteger s)
||  econv (Econstr (Cconstr s t _ _ _ _) []) & (eqtype t Trational) = Constant (CDFloat s)
||  econv (Econstr (Cconstr s _ _ (-1) _ _) []) = Constant (CString s)
#if 0
||  econv (Econstr (Cconstr _ t _ n _ _) []) & (eqtype t Tbool) = imkbool n
||  econv (Econstr (Cconstr _ t _ n _ _) es) & (eqid (tname t) hiList) = imklist n (map econv es)
||  econv (Econstr (Cconstr _ t _ _ _ _) es) & (member eqid (tname t) (for 2 5 hituple)) = imktuple (map econv es)
#endif
||  econv (Econstr c es) = ap (Konstr (cno c) (length es) (map snd (cargs c))) (map econv es)
||  econv (Efailmatch 0) = Fail
||  econv (Efailmatch _) = Var default_id
||  econv (Ecfunction b (mkids i)) = Var (mkid (-999) i idi_udef Noorigname) -- Try and find it somewhere later!
and ecasef e ((c,[],x).ps) Fail = ecasef e ps x
||  ecasef e ps d = 
    bind default_id d
	(Ap (reduce f (Lam dummyid (Var default_id)) ps) (Ap Vcno e)
	where f (c,is,e) a = ap Vtstf [imkint (cno c); e; a])
and ecase ex ((c,[],e).ps) Fail = ecase ex ps e
||  ecase ex [(c,is,e)] Fail = Ap (splt (cno c) (length is) (lam is e)) ex
||  ecase ex ps d = 
    bind default_id d
	(Ap (Ap (reduce f (Lam dummyid (Lam dummyid (Var default_id))) ps) (Ap Vcno ex)) ex
	where f (c,is,e) a = ap Vtst [imkint (cno c); splt (cno c) (length is) (lam is e); a])
and default_id = mkid 990 "_DEFAULT" (idi_var var_unknown Onotype None) noorigname
and bind i d e = Ap (Lam i e) d
and isflat' (Cconstr _ _ (mktinfo _ _ _ x _ _ _ _) _ _ _) = x
and isflat ((c,_,_)._) = isflat' c
||  isflat _ = false
and ap e xs = reduce (\x.\r.Ap r x) e (rev xs)
and lam xs e = reduce Lam e xs
and iscallmethod i = let n = id_no i in n > Fcallmethod & n < Fcallmethod + 50
end
