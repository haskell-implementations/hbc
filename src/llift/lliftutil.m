module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../misc/setofid.t"
#include "../misc/util.t"
export numid, idexp, varsof, funsof, solve, strictprimitive, deltype;
rec
    numid i s = mknewids "Lam" i (idtostr s)
and idexp i = Eidapl i []
and varsof [] = []
||  varsof ((_, Elaml (_._) _).l) = varsof l
||  varsof ((i,e).l) = Iu [i] (varsof l)
and funsof [] = []
||  funsof ((i, Elaml (_._) _).l) = Iu [i] (funsof l)
||  funsof ((_,e).l) = funsof l
and solve      []      = []
||  solve [(f,s,e)]    = [(f,s)]
||  solve ((f,s,e).l)  = 
     let so = solve(map(\t.let (f1,s1,e1) = t
			  in if Imem f e1
			     then (f1, Iu s s1, Iu e e1)
			     else t)
		      l)
     in (f, itlist (\x.\p. Iu (assocdefeq eqid x so []) p) e s).so
     
and strictprimitive i = id_is_predef i
and deltype (xx, Elaml ois _) (i, e as Elaml is _) & (length ois ~= length is) = (adjtype (length is - length ois) i, e)
||  deltype _ ie = ie
and adjtype k (mkid n s (idi_var _ _ _) on) = mkid n s (idi_var var_unknown Onotype None) on
||  adjtype k i = i
end
