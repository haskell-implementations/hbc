module
#include "cexpr.t"
#include "imisc.t"
#include "cons.t"
#include "compilek.t"
/*import compk : Cexpr -> List Univ -> Univ {# ARITY _ = 2 #};*/
import cvectorwrap : Univ -> Univ {# ARITY _ = 1 #};
import {:"Pdftosf":} : *a {# ARITY _ = 1 #};
export compile;

/*#define UNIV(x) ((x){#NOTCHK#})*/

rec compile :: Cexpr -> Univ
and compile e = /*trace ("compile "@show_Cexpr e)*/ (xcompile e)
and xcompile (Constant k) = getc k
||  xcompile (Comb _ e) = e
||  xcompile (Konstr k 0 _) = ktag0 k		-- just to speed things up
||  xcompile (k as Konstr _ m _) = addlam m k []
||  xcompile (Ap (Comb "Pvectorwrap" _) x) = cvectorwrap (compile x)
||  xcompile (e as Ap _ _) = compap e []
||  xcompile e = ifail ("No match in compile "@show_Cexpr e)
and compap :: Cexpr -> List Univ -> Univ
and compap (Ap (Konstr 0 2 _) (Comb ('V'.'V'._) vv)) [t] = (seq t (seq vv (vv, t))){# NOTCHK #}	-- instance vector must have evaluated components
-- combinators do not have to have compilation postponed
||  compap (Ap f (Comb _ v))   l = compap f (v.l)
-- the general application case, check for constructor argument and eval it
||  compap (Ap f a)            l = let v = compile a in 
				   if isK 0 a then seq v compap f (v.l) 
				              else       compap f (v.l)
-- Lots of special cases for contructor encodings
||  compap (k as Konstr _ l _) es = 
    if length es ~= l then
	--fail "compap constr length"
	addlam (l - length es) k es
    else
	compk k es
#if 1
-- a few fast applications
||  compap (Comb _ e) [e1]       = (e e1){# NOTCHK #}
||  compap (Comb _ e) [e1;e2]    = (e e1 e2){# NOTCHK #}
||  compap (Comb _ e) [e1;e2;e3] = (e e1 e2 e3){# NOTCHK #}
#endif
-- and the general function case
||  compap (Comb _ e) l          = doap e l
||  compap e l = ifail ("No match in compap "@show_Cexpr e)
and doap :: Univ -> List Univ -> Univ
and doap e [] = e
||  doap f (x.xs) = doap ((f x){# NOTCHK #}) xs
and getc :: Tconst -> Univ
and getc (CInt i) = (i){# NOTCHK #}
||  getc (CChar c) = (c){# NOTCHK #}
||  getc (CString s) = (s){# NOTCHK #}
||  getc (CDFloat s) = (stof s){# NOTCHK #}
||  getc (CSFloat s) = (dftosf (stof s)){# NOTCHK #}
||  getc (CInteger s) = (stoiI s){# NOTCHK #}
and isK :: Int -> Cexpr -> Bool
and isK 0 (Constant _) = true
||  isK k (Konstr _ k' _) = k = k'
||  isK k (Ap f _) = isK (k+1) f
||  isK _ _ = false
and addlam :: Int -> Cexpr -> List Univ -> Univ
and addlam 0 e l = compk e l
||  addlam n e l = (\x.addlam (n-1) e (l@[x])){# NOTCHK #}

#if 0
and dftosf x = fail "dftosf"
#else
and dftosf x = {:"Pdftosf":} x
#endif
end
