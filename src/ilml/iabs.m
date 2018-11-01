module
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "cexpr.t"
#include "imisc.t"
#include "cast.t"
#include "id.h"
export abstract, idY;
rec VK  = Comb "K" (cast (\x.\y.x))
and VB  = Comb "B" (cast (\f.\g.\x.f (g x)))
and VC' = Comb "C1" (cast (\k.\f.\g.\x.k (f x) g))
and VC  = Comb "C" (cast (\f.\g.\x.f x g))
and VS' = Comb "S1" (cast (\k.\f.\g.\x.k (f x) (g x)))
and VS  = Comb "S" (cast (\f.\g.\x.f x (g x)))
and VI  = Comb "I" (cast (\x.x))
and VY  = Comb "Y" (cast (let rec Y f = let rec x = f x in x in Y))
and idY = mkid IDY "PY" idi_udef noorigname
and Ap2 f a1 a2 = Ap (Ap f a1) a2
and Ap3 f a1 a2 a3 = Ap (Ap (Ap f a1) a2) a3
and absap e1 e2 =
	case e1 in
	   Ap (Comb "K" _) k1 :
		case e2 in
		   Ap (Comb "K" _) k2 : Ap VK (Ap k1 k2)
		|| Comb "I" _ : k1
		|| _ : Ap2 VB k1 e2
		end
	|| Ap (Ap (Comb "B" _) b1) b2 :
		case e2 in
		   Ap (Comb "K" _) k2 : Ap3 VC' b1 b2 k2
		|| _ : Ap3 VS' b1 b2 e2
		end
	|| _ :
		case e2 in
		   Ap (Comb "K" _) k2 : Ap2 VC e1 k2
		|| _ : Ap2 VS e1 e2
		end
	end
and abs v (Ap e1 e2) = absap (abs v e1) (abs v e2)
||  abs v (Var i) & (eqid i idY) = VY
||  abs v (e as Var i) = if eqid i v then VI else Ap VK e
||  abs v (e as Comb _ _) = Ap VK e
||  abs v (e as Constant _) = Ap VK e
||  abs v (e as Konstr _ _ _) = Ap VK e
||  abs v (e as Fail) = e
||  abs v e = ifail ("Bad abstract " @ show_Cexpr e)
and abse (Lam i e) = abs i (abse e)
||  abse (Ap e1 e2) = Ap (abse e1) (abse e2)
||  abse (Var i) & (eqid i idY) = VY
||  abse e = e
and abstract e = /*trace ("abstract "@show_Cexpr e)*/ (abse e)
end
