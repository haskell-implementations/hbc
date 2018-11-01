module -- mtrans
--
-- Make constants second operand
-- Remove special regs
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "tmp.h"
export mtrans1;
-- Make constants second operand
rec mc (Mcompare (a1 as const _) a2.ms) = Mcompare a2 a1. invj ms
||  mc (m.ms) = m.mc ms
||  mc [] = []
and invj (Mjcond cc1 l1.Mjcond cc2 l2.ms) = Mjcond (mopcc (ccrev (mccop cc1))) l1. Mjcond (mopcc (ccrev (mccop cc2))) l2. mc ms
||  invj (Mjcond cc l.ms) = Mjcond (mopcc (ccrev (mccop cc))) l. mc ms
||  invj (Mboolcc cc l.ms) = Mboolcc (mopcc (ccrev (mccop cc))) l. mc ms
||  invj (m.ms) = m . invj ms
||  invj [] = []

and mv [] = []
||  mv (Mop3 op popV a2 a3.ms) =
     case a2 
     in dtmp1 : Mmove popV dtmp2.mv (Mop3 op dtmp2 a2 a3.ms)
     ||   _   : Mmove popV dtmp1.mv (Mop3 op dtmp1 a2 a3.ms)
     end
||  mv (Mop3 op a1 popV pushV.ms) = Mop3 op a1 (Vind 0) (Vind 0).mv ms
||  mv (Mop3 op a1 a2 pushV.ms) = Mmove (const 0) pushV.mv (Mop3 op a1 a2 (Vind 0).ms)
||  mv (Mop3 op a1 popV a3.ms) =
      case a1
      in dtmp1 : Mmove popV dtmp2.mv (Mop3 op a1 dtmp2 a3.ms)
      ||   _   : Mmove popV dtmp1.mv (Mop3 op a1 dtmp1 a3.ms)
      end
||  mv (Mop2 op popV a2.ms) =
      case a2 
      in dtmp1 : Mmove popV dtmp2.mv (Mop2 op dtmp2 a2.ms)
      ||   _   : Mmove popV dtmp1.mv (Mop2 op dtmp1 a2.ms)
      end
||  mv (Mcompare popV a2.ms) = 
      case a2 
      in dtmp1 : Mmove popV dtmp2.mv (Mcompare dtmp2 a2.ms)
      ||   _   : Mmove popV dtmp1.mv (Mcompare dtmp1 a2.ms)
      end
||  mv (Mcompare a1 popV.ms) =
      case a1 
      in dtmp1 : Mmove popV dtmp2.mv (Mcompare a1 dtmp2.ms)
      ||   _   : Mmove popV dtmp1.mv (Mcompare a1 dtmp1.ms)
      end
||  mv (m.ms) = m.mv ms

and mka Vp = (reg Vpr)
||  mka Sp = (reg Spr)
||  mka hp = (reg hpr)
||  mka (Vind n) = (regind Vpr n)
||  mka (Sind n) = (regind Spr n)
||  mka (hpind n) = (regind hpr n)
||  mka (Vrel n) = (regrel Vpr n)
||  mka (Srel n) = (regrel Spr n)
||  mka (hprel n) = (regrel hpr n)
||  mka a  = a

-- Remove special regs
and mk (Mmove a1 a2) = Mmove (mka a1) (mka a2)
||  mk (Mcompare a1 a2) = Mcompare (mka a1) (mka a2)
||  mk (Mop2 op a1 a2) = Mop2 op (mka a1) (mka a2)
||  mk (Mop3 op a1 a2 a3) = Mop3 op (mka a1) (mka a2) (mka a3)
||  mk (Mcase a x1 x2 x3 x4 x5) = Mcase (mka a) x1 x2 x3 x4 x5
||  mk (Mboolcc cc a) = Mboolcc cc (mka a)
||  mk (Masm s ass) = Masm s (map mka ass)
||  mk m = m
and mtrans1 ms = map mk (mc (mv ms))
end
