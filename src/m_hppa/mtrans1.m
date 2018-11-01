module -- mtrans
--
-- remove all popS, popV, tohp
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "reg.h"
export mtrans1;
rec
    srel 0 = Sp
||  srel n = Srel n
and hrel 0 = hp
||  hrel n = hprel n
and vrel 0 = Vp
||  vrel n = Vrel n
and adj s (_,_,V) Vp       = vrel V
||  adj s (_,_,V) (Vrel n) = vrel (V+n)
||  adj s (_,_,V) (Vind n) = Vind (V+n)
||  adj s (_,_,V) pushV    = Vind (V-s)
||  adj s (_,_,V) popV     = Vind V
||  adj s (_,S,_) Sp       = srel S
||  adj s (_,S,_) (Srel n) = srel (S+n)
||  adj s (_,S,_) (Sind n) = Sind (S+n)
||  adj s (_,S,_) pushS    = Sind (S-1)
||  adj s (_,S,_) popS     = Sind S
||  adj s (H,_,_) hp       = hrel H
||  adj s (H,_,_) (hprel n)= hrel (H+n)
||  adj s (H,_,_) (hpind n)= hpind (H+n)
||  adj s (H,_,_) tohp     = hpind H
||  adj s _     a        = a
and chg s (H,S,V) pushV    = (H,S,V-s)
||  chg s (H,S,V) popV     = (H,S,V+s)
||  chg s (H,S,V) pushS    = (H,S-1,V)
||  chg s (H,S,V) popS     = (H,S+1,V)
||  chg s (H,S,V) tohp     = (H+s,S,V)
||  chg s HSV     _        = HSV

and nonseq (Mcall _)	= true
||  nonseq Mreturn	= true
||  nonseq (Mcalltag _ _) = true
||  nonseq (Mjumptag _ _) = true
||  nonseq (Mjump _)	= true
||  nonseq (Mjumpf _)	= true
||  nonseq (Mjcond _ _) = true
||  nonseq (Mlabel _)	= true
||  nonseq (Mcase _ _ _ _ _ _) = true
||  nonseq (Masm _ _)	= true		-- safe assumption
||  nonseq _		= false

and flush (H,S,V) = (flh H @ fls S @ flv V
			where flh 0 = []
			   || flh n = [Mmove (hprel n) hp]
			and   fls 0 = []
			   || fls n = [Mmove (Srel n) Sp]
			and   flv 0 = []
			   || flv n = [Mmove (Vrel n) Vp])

and mskip HSV (Mtext.ms) = Mtext.mt HSV ms
||  mskip HSV     (m.ms) =     m.mskip HSV ms
||  mskip HSV     []     = []
-- Make constants second operand
and mc (Mcompare (a1 as const _) a2.ms) = Mcompare a2 a1. invj ms
||  mc (m.ms) = m.mc ms
||  mc [] = []
and invj (Mjcond cc l.ms) = Mjcond (invmop cc) l. mc ms
||  invj (Mboolcc cc l.ms) = Mboolcc (invmop cc) l. mc ms
||  invj (m.ms) = m . invj ms
||  invj [] = []
and ssize op = if dfsrc op then 2 else 1
and dsize op = if dfdst op then 2 else 1
and sfsrc op = mem op [sftoi; sftodf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and sfdst op = mem op [itosf; dftosf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and dfsrc op = mem op [dftoi; dftosf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and dfdst op = mem op [itodf; sftodf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and Mmove1 op x y =
    if dfsrc op then
	Mmovedf x y
    else if sfsrc op then
	Mmovesf x y
    else
	Mmove x y
and Mmove2 op x y =
    if dfdst op then
	Mmovedf x y
    else if sfdst op then
        Mmovesf x y
    else
	Mmove x y
and tmp4 op = if dfsrc op | sfsrc op then Ftmp4 else dtmp4
and
    mt _ [] = []
||  mt (H,S,V) (Mmove (Vrel n) Vp.ms) = mt (H, S,V+n) ms
||  mt (H,S,V) (Mmove (Srel n) Sp.ms) = mt (H, S+n,V) ms
||  mt (H,S,V) (Mmove (hprel n) hp.ms) = mt (H+n, S,V) ms
||  mt HSV (Mmove a1 Vp.ms) = 
	let (H',S',_) = chg 1 HSV a1 in
	Mmove (adj 1 HSV a1) Vp .mt (H',S',0) ms
||  mt HSV (Mmove a1 Sp.ms) = 
	let (H',_,V') = chg 1 HSV a1 in
	Mmove (adj 1 HSV a1) Sp .mt (H',0,V') ms
||  mt HSV (Mmove a1 a2.ms) =
	let HSV' = chg 1 HSV a1 in
	Mmove (adj 1 HSV a1) (adj 1 HSV' a2) .mt (chg 1 HSV' a2) ms
||  mt HSV (Mmovesf a1 a2.ms) =
	let HSV' = chg 1 HSV a1 in
	Mmovesf (adj 1 HSV a1) (adj 1 HSV' a2) .mt (chg 1 HSV' a2) ms
||  mt HSV (Mmovedf a1 a2.ms) =
	let HSV' = chg 2 HSV a1 in
	Mmovedf (adj 2 HSV a1) (adj 2 HSV' a2) . mt (chg 2 HSV' a2) ms -- dbl takes 2 words
||  mt HSV (Mcompare a1 a2.ms) =
	flush HSV @ let HSV = (0,0,0) in
	let HSV' = chg 1 HSV a1 in
	Mcompare (adj 1 HSV a1) (adj 1 HSV' a2) .mt (chg 1 HSV' a2) ms
||  mt HSV (Mcomparedf a1 a2.ms) =
	flush HSV @ let HSV = (0,0,0) in
	let HSV' = chg 2 HSV a1 in
	Mcomparedf (adj 2 HSV a1) (adj 2 HSV' a2) .mt (chg 2 HSV' a2) ms
||  mt HSV (Mcomparesf a1 a2.ms) =
	flush HSV @ let HSV = (0,0,0) in
	let HSV' = chg 1 HSV a1 in
	Mcomparedf (adj 1 HSV a1) (adj 1 HSV' a2) .mt (chg 1 HSV' a2) ms
||  mt HSV (Mop2 op a1 a2.ms) =
	let HSV'  = chg (ssize op) HSV  a1 in
	Mop2 op (adj (ssize op) HSV a1) (adj (dsize op) HSV' a2) .mt (chg (dsize op) HSV' a2) ms
||  mt HSV (Mop3 op a1 a2 a3.ms) =
	let HSV'  = chg (ssize op) HSV  a1 in
	let HSV'' = chg (ssize op) HSV' a2 in
	Mop3 op (adj (ssize op) HSV a1) (adj (ssize op) HSV' a2) (adj (dsize op) HSV'' a3) .mt (chg (dsize op) HSV'' a3) ms
||  mt HSV (Mboolcc cc a.ms) = 
	Mboolcc cc (adj 1 HSV a) .mt (chg 1 HSV a) ms
||  mt HSV (Mdata.ms) = Mdata .mskip HSV ms
||  mt HSV (m.ms) & (nonseq m) = flush (mx m HSV) @ m.mt (0,0,0) ms
||  mt HSV (m.ms) = m.mt HSV ms

-- extra word for call
and mx (Mcalltag _ _) (H,S,V) = (H,S,V-1)
||  mx (Mcall _) (H,S,V) = (H,S,V-1)
||  mx m HSV = HSV

and mv [] = []
||  mv (Mop3 op popV a2 a3.ms) = let tmp = tmp4 op in Mmove1 op popV tmp.mv (Mop3 op tmp a2 a3.ms)
||  mv (Mop3 op a1 popV pushV.ms) = Mop3 op a1 (Vind 0) (Vind 0).mv ms
||  mv (Mop3 op a1 a2 pushV.ms) = let tmp = tmp4 op in mv (Mop3 op a1 a2 tmp.Mmove2 op tmp pushV.ms)
||  mv (Mop3 op a1 popV a3.ms) = let tmp = tmp4 op in Mmove1 op popV tmp.mv (Mop3 op a1 tmp a3.ms)
||  mv (Mop2 op popV a2.ms) = let tmp = tmp4 op in Mmove1 op popV tmp.mv (Mop2 op tmp a2.ms)
||  mv (Mcompare popV a2.ms) = Mmove popV dtmp4.mv (Mcompare dtmp4 a2.ms)
||  mv (Mcompare a1 popV.ms) = Mmove popV dtmp4.mv (Mcompare a1 dtmp4.ms)
||  mv (Mcomparesf popV a2.ms) = Mmovesf popV Ftmp4.mv (Mcomparesf Ftmp4 a2.ms)
||  mv (Mcomparesf a1 popV.ms) = Mmovesf popV Ftmp4.mv (Mcomparesf a1 Ftmp4.ms)
||  mv (Mcomparedf popV a2.ms) = Mmovedf popV Ftmp4.mv (Mcomparedf Ftmp4 a2.ms)
||  mv (Mcomparedf a1 popV.ms) = Mmovedf popV Ftmp4.mv (Mcomparedf a1 Ftmp4.ms)
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
||  mk (Mmovedf a1 a2) = Mmovedf (mka a1) (mka a2)
||  mk (Mmovesf a1 a2) = Mmovesf (mka a1) (mka a2)
||  mk (Madda a1 a2) = Madda (mka a1) (mka a2)
||  mk (Mcompare a1 a2) = Mcompare (mka a1) (mka a2)
||  mk (Mcomparedf a1 a2) = Mcomparedf (mka a1) (mka a2)
||  mk (Mcomparesf a1 a2) = Mcomparesf (mka a1) (mka a2)
||  mk (Mop2 op a1 a2) = Mop2 op (mka a1) (mka a2)
||  mk (Mop3 op a1 a2 a3) = Mop3 op (mka a1) (mka a2) (mka a3)
||  mk (Mcase a x1 x2 x3 x4 x5) = Mcase (mka a) x1 x2 x3 x4 x5
||  mk (Mboolcc cc a) = Mboolcc cc (mka a)
||  mk (Masm s ass) = Masm s (map mka ass)
||  mk m = m

and mtrans1 = map mk o mt (0,0,0) o mc o mv
end
