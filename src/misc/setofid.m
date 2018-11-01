module
#include "../expr/id_t.t"
export Ilt, Igt, Ieq, Imk, Imem, Iu, Is, Idi;

rec Ilt (mkid x _ _ _) (mkid y _ _ _) = x < y
and Igt (mkid x _ _ _) (mkid y _ _ _) = x > y
and Ieq (mkid x _ _ _) (mkid y _ _ _) = x = y

and Imk  []   = []
 || Imk (x.l) = Imk(filter(Igt x) l)@ [x] @
	        Imk(filter(Ilt x) l)
and Imem x  []   = false
 || Imem x (y.l) = Ieq x y | Igt x y & Imem x l
and Iu   A           []        = A
 || Iu   []          B         = B
 || Iu (aA as a.A) (bB as b.B) = if      Ieq a b then a.Iu A  B
				 else if Ilt a b then a.Iu A  bB
				 else /* a > b */     b.Iu aA B
and Is   A           []   = []
 || Is   []          B    = []
 || Is (aA as a.A) (bB as b.B) = if      Ieq a b then a.Is A  B
				 else if Ilt a b then   Is A  bB
				 else /* a > b */       Is aA B
and Idi  A    []   = A
 || Idi  []   B    = []
 || Idi (aA as a.A) (bB as b.B) = if      Ieq a b then  Idi A  B
				 else if Ilt a b then a.Idi A  bB
				 else /* a > b */       Idi aA B
end
