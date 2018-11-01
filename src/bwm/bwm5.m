module
#ifdef BWM
#include "bwm3.t"
#include "bwm4.t"
export bwmfix;
rec bwmfix bs = concmap fixb bs
and fixb (BUILD    bs) & (nlit bs > maxlit)   = let (bs1,bs2) = splitlit maxlit bs in [BUILDN bs1; BUILD  bs2]
||  fixb (UPDATE i bs) & (nlit bs > maxlit)   = let (bs1,bs2) = splitlit maxlit bs in [BUILDN bs1; UPDATE i bs2]
||  fixb (CASE  bs xs) & (nlit bs > maxlit-1) = let (bs1,bs2) = splitlit maxlit bs in [BUILDN bs1; CASE bs2 (map bwmfix xs)]
||  fixb (CASE bs xs) = [CASE bs (map bwmfix xs)]
||  fixb (FUN i a xs) = [FUN i a (bwmfix xs)]
||  fixb b = [b]
and nlit bs = length (filter islit bs)
and splitlit n bs =
	let (bs1, bs2) = splitlit' n bs
        in  (bs1, rept (length bs1) Bnoop @ bs2)
and splitlit' 0 bs = ([], bs)
||  splitlit' _ [] = ([], [])
||  splitlit' n (b.bs) = 
        let (bs1, bs2) = splitlit' (if islit b then n-1 else n) bs
        in  (b.bs1, bs2)
and islit (Bstack _) = false
||  islit (Bheap  _) = false
||  islit (Bnoop   ) = false
||  islit _          = true
#else
export ;
dummy=0
#endif
end
