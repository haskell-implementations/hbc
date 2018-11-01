module -- asimpl
-- Arithmetic simplifications
#include "../funnos.h"
#include "../expr/id_t.t"
#include "../expr/constr_t.t"
#include "../expr/id.t"
#include "../expr/ttype.t"
#include "../ExprE/Expr_t.t"
#include "../ExprE/Eutil.t"
#include "../misc/misc.t"
#include "../misc/flags.t"
#include "sutil.t"
#include "../ExprE/predef.t"
export asimpl;
rec type sign *a = plus *a + minus *a + const Int
and neg (plus e) = minus e
||  neg (minus e) = plus e
||  neg (const i) = const (-i)
and constp (const _) = true
||  constp _ = false
and rreduce f z [] = z
||  rreduce f z (x.xs) = rreduce f (f x z) xs
and asimpl e =
	if NoAsimpl then e else
	let ops = collectops e in
	let (cs, es) = partition constp ops in
	let c = reduce (\(const x).\r. x + r) 0 cs in
	case es in
	   [] : Emkint c
	|| e.es :
		let r = rreduce smash (op e) es
		in if c = 0 then
			r
		else if c > 0 then
		        case r in
			    Eidapl n [e] & (eqid n pre_neg) : Esub (Emkint c) e
			|| _ : Eadd r (Emkint c)
			end
		else -- c < 0
			Esub r (Emkint (-c))
	end	
and op (plus e) = e
||  op (minus e) = Eneg e
and smash (plus e) r = Eadd r e
||  smash (minus e) r = Esub r e 
and collectops (Eidapl (mkid Fadd _ _ _) [e1; e2]) = collectops e1 @ collectops e2
||  collectops (Eidapl (mkid Fsub _ _ _) [e1; e2]) = collectops e1 @ map neg (collectops e2)
||  collectops (Eidapl (mkid Fneg _ _ _) [e1]) = map neg (collectops e1)
||  collectops (Econstr c []) = [const (cval c)]
||  collectops e = [plus e]

and Eadd e1 e2 = Eidapl pre_add [e1; e2]
and Esub e1 e2 = Eidapl pre_sub [e1; e2]
and Eneg e1    = Eidapl pre_neg [e1]
end
