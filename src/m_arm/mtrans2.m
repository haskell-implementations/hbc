module -- mtrans
--
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "tmp.h"
export mtrans2;
rec rc add (const n) = n>(-256) & n <256        --- Conservative
||  rc sub (const n) = n>(-256) & n <256        --- Conservative
||  rc _   (reg _)   = true
||  rc _   _         = false
and op31 op a1 a2 a3 & (rc op a1) = op32 op a1 a2 a3
||  op31 op a1 a2 a3 = Mmove a1 dtmp1 . op32 op dtmp1 a2 a3

and op32 (op as sub) a1 (a2 as const 0) a3 = op33 op a1 a2 a3
||  op32 op a1 (a2 as reg _) a3 = op33 op a1 a2 a3
||  op32 op a1 a2 a3 = Mmove a2 dtmp2 . op33 op a1 dtmp2 a3

and op33 op a1 a2 (a3 as reg _) = [Mop3 op a1 a2 a3]
||  op33 op a1 a2 a3 = [Mop3 op a1 a2 dtmp1; Mmove dtmp1 a3]

-- Insert temp regs.
and mr (Mmove (retaddr s) a2) = [Mmove (idlit s) (reg Ret); Mmove (reg Ret) a2]
||  mr (m as Mmove (reg _) _) = [m]
||  mr (m as Mmove _ (reg _)) = [m]
||  mr (Mmove a1 a2) = [Mmove a1 dtmp1; Mmove dtmp1 a2]
||  mr (m as Mcompare (reg _) (reg _)) = [m]
||  mr (Mcompare (reg r) (const n)) & (n>(-256) & n <256) = [Mcompare (reg r) (const n)]
||  mr (Mcompare a1 a2) & (rc add a1) = [Mmove a2 dtmp2; Mcompare a1 dtmp2]
||  mr (Mcompare a1 a2) & (rc add a2) = [Mmove a1 dtmp1; Mcompare dtmp1 a2]
||  mr (Mcompare a1 a2) = [Mmove a1 dtmp1; Mmove a2 dtmp2; Mcompare dtmp1 dtmp2]
||  mr (m as Mop2 op a1 (reg _)) & (rc op a1) = [m]
||  mr (Mop2 op a1 a2) & (rc op a1) =
	[Mmove a2 dtmp2; Mop2 op a1 dtmp2; Mmove dtmp2 a2]
||  mr (Mop2 op a1 (a2 as reg _)) = 
	[Mmove a1 dtmp1; Mop2 op dtmp1 a2]
||  mr (Mop2 op a1 a2) = 
	[Mmove a1 dtmp1; Mmove a2 dtmp2; Mop2 op dtmp1 dtmp2; Mmove dtmp2 a2]

||  mr (Mop3 add a1 a2 (const n)) & (n<0) = op31 sub a1 a2 (const (-n))
||  mr (Mop3 sub a1 a2 (const n)) & (n<0) = op31 add a1 a2 (const (-n))

||  mr (Mop3 op a1 a2 a3) = op31 op a1 a2 a3

||  mr (m as Mcase (reg _) x1 x2 x3 x4 x5) = [m]
||  mr (m as Mboolcc cc (reg _)) = [m]
||  mr (Mboolcc cc a) = [Mboolcc cc dtmp1; Mmove dtmp1 a]
||  mr m = [m]

and mt (Mmove (regrel r1 0) (reg r2)) & (r1=r2) = 
	[]
||  mt (Mmove (regrel r1 0) (reg r2)) = 
	[Mmove (reg r1) (reg r2)]
||  mt (Mmove (regrel r1 n) (reg r2)) = 
           if n > 0 then [Mop3 add (const (4*  n))  (reg r1) (reg r2)]
	   else          [Mop3 sub (const (4*(-n))) (reg r1) (reg r2)]
||  mt (Mop3 mul a1 a2 a3) & (a2=a3) = 
           if a1 ~=a3 then [Mop3 mul a2 a1 a3]
	   else [Mop3 mul a1 a2 (reg Ret); Mmove (reg Ret) a3]
||  mt (Mop2 mul a1 a2) = 
           if a1 ~=a2 then [Mop3 mul a2 a1 a2]
	   else [Mop3 mul a1 a2 (reg Ret); Mmove (reg Ret) a2]
||  mt m = [m]
and mm (Mcompare (glob "_ehp") a2) = Mcompare (reg Ehpr) a2
||  mm (Mcompare a1 (glob "_ehp")) = Mcompare a1 (reg Ehpr)
||  mm m = m
and mtrans2 ms = concmap mt (concmap mr (concmap mt (map mm ms)))

end
