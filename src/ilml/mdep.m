module
#include "../misc/misc.t"
#include "../expr/id_t.t"
#include "../expr/types_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../expr/ttype.t"
#include "../misc/misc.t"
#include "cast.t"
#include "cons.t"
#include "cno.t"
#include "id.h"
#include "cexpr.t"
#include "imisc.t"
#include "../funnos.h"
#define MAXPAIR 5
export con, imkint, Vtstf, Vtst, splt, Vcno; --, icallmet; --, imkbool, imklist, imktuple;
rec splt k 0 e = Lam dummyid e
||  splt k 1 e = Ap spltag e
||  splt k n e =
	if k < MAXPAIR then
		Ap splpair (splf (n-1) e)
	else
		Ap spltag (splf n e)
and splf 1 e = e
||  splf 2 e = Ap splpair e
||  splf _ e = Ap splvek e
and Vtstf = Comb "tstf" (cast (\k.\f.\r.\i.if k=(i:Int) then f else r i))
and Vtst = Comb "tst" (cast (\k.\f.\r.\i.\e.if k=(i:Int) then f e else r i e))
and Vcno = Comb "cno" (cast kno)
and spltag = Comb "spltag" (cast fspltag)
and splpair = Comb "splpair" (cast fsplpair)
and splvek = Comb "splvek" (cast ksplvek)
and fspltag = (f where rec type T = t Univ and f g (t x) = g x)
and fsplpair = (f where rec type T = t Univ Univ and f g (t x y) = g x y)
and imkint n = Constant (CInt n)
#if 0
and tag = Comb "@tag" (cast ktag)
and tag0 = Comb "@tag0" (cast ktag0)
and tvek = Comb "@tvek" (cast ktvek)
and pvek = Comb "@pvek" (cast kpvek)
and tpair = Comb "@tpair" (cast ktpair)
and ppair = Comb "@ppair" (cast kppair)
and pair 0 = Comb "@pair0" (cast kpair0)
||  pair 1 = Comb "@pair1" (cast kpair1)
||  pair 2 = Comb "@pair2" (cast kpair2)
||  pair 3 = Comb "@pair3" (cast kpair3)
||  pair 4 = Comb "@pair4" (cast kpair4)
and cpair k e1 e2 = Ap (Ap (pair k) e1) e2
and ctag k e = Ap (Ap tag (imkint k)) e
and con k [] = Ap tag0 (imkint k)
||  con k [e] = ctag k e
||  con k [e1;e2] =
    if k < MAXPAIR then
	cpair k e1 e2
    else
	Ap (Ap (Ap tpair (imkint k)) e1) e2
||  con k (l as [e1;e2;e3]) & (k < MAXPAIR) =
    ap ppair (imkint k.l)
||  con k l = 
    if k < MAXPAIR then
	ap pvek (imkint k. imkint (length l). l)
    else
	ap tvek (imkint k. imkint (length l). l)
and ap e xs = reduce (\x.\r.Ap r x) e (rev xs)

and imkbool 0 = Comb "@0" (cast false)
||  imkbool 1 = Comb "@1" (cast true)
and imklist 0 [] = Comb "@0" (cast [])
||  imklist 1 [e1;e2] = Ap (Ap (Comb "@." (cast (.))) e1) e2
and imktuple es = let l = length es in ap (Comb ("@#"@itos l) (rtup l)) es
and rtup 2 = cast (\x1.\x2.(x1, x2))
||  rtup 3 = cast (\x1.\x2.\x3.(x1, x2, x3))
||  rtup 4 = cast (\x1.\x2.\x3.\x4.(x1, x2, x3, x4))
||  rtup 5 = cast (\x1.\x2.\x3.\x4.\x5.(x1, x2, x3, x4, x5))
#else
and con = fail "con"
#endif
#if 0
and icallmet i = 
    let n = id_no i - Fcallmethod in
    if n > 15 then
	ifail "Sorry, Pcallmethod only partially implemented."
    else
	Comb (idtostr i) (select (n-1) callmettab)
#endif
end
