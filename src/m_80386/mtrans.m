module -- mtrans
--
-- remove all popS, pushS, tohp because 80386 can't use them
-- remove popV, pushV from everything but moves
--
#include "../misc/flags.t"
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "reg.h"
export mtrans;
rec
    srel 0 = Sp
||  srel n = Srel n
and hrel 0 = hp
||  hrel n = hprel n
and adj (_,S) Sp       = srel S
||  adj (_,S) (Srel n) = srel (S+n)
||  adj (_,S) (Sind n) = Sind (S+n)
||  adj (_,S) pushS    = Sind (S-1)
||  adj (_,S) popS     = Sind S
||  adj (H,_) hp       = hrel H
||  adj (H,_) (hprel n)= hrel (H+n)
||  adj (H,_) (hpind n)= hpind (H+n)
||  adj (H,_) tohp     = hpind H
||  adj _     a        = a
and chg s (H,S) pushS    = (H,S-1)
||  chg s (H,S) popS     = (H,S+1)
||  chg s (H,S) tohp     = (H+s,S)
||  chg s HS    _        = HS

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

and flush (H,S) = (flh H @ fls S
			where flh 0 = []
			   || flh n = [Mmove (hprel n) hp]
			and   fls 0 = []
			   || fls n = [Mmove (Srel n) Sp])

and mskip HS (Mtext.ms) = Mtext.mt HS ms
||  mskip HS     (m.ms) =     m.mskip HS ms
||  mskip HS         [] = []
and sfsrc op = mem op [sftoi; sftodf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and sfdst op = mem op [itosf; dftosf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and dfsrc op = mem op [dftoi; dftosf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and dfdst op = mem op [itodf; sftodf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and ssize op = if dfsrc op then 2 else 1
and dsize op = if dfdst op then 2 else 1
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
and
    mt _ [] = []
||  mt (H,S) (Mmove (Srel n) Sp.ms) = mt (H, S+n) ms
||  mt (H,S) (Mmove (hprel n) hp.ms) = mt (H+n, S) ms
||  mt (H,S) (Mmove a1 Sp.ms) = Mmove (adj (H,S) a1) Sp .mt (H,0) ms
||  mt HS (Mmovedf a1 a2.ms) =
	let HS' = chg 2 HS a1 in
	Mmovedf (adj HS a1) (adj HS' a2) . mt (chg 2 HS' a2) ms -- dbl takes 2 words
||  mt HS (Mmovesf a1 a2.ms) =
	let HS' = chg 1 HS a1 in
	Mmovesf (adj HS a1) (adj HS' a2) .mt (chg 1 HS' a2) ms
||  mt HS (Mmove a1 a2.ms) =
	let HS' = chg 1 HS a1 in
	Mmove (adj HS a1) (adj HS' a2) .mt (chg 1 HS' a2) ms
||  mt HS (Mcomparesf a1 a2.ms) =
	let HS' = chg 1 HS a1 in
	Mcomparesf (adj HS a1) (adj HS' a2) .mt (chg 1 HS' a2) ms
||  mt HS (Mcompare a1 a2.ms) =
	let HS' = chg 1 HS a1 in
	Mcompare (adj HS a1) (adj HS' a2) .mt (chg 1 HS' a2) ms
||  mt HS (Mcomparedf a1 a2.ms) =
	let HS' = chg 2 HS a1 in
	Mcomparedf (adj HS a1) (adj HS' a2) .mt (chg 2 HS' a2) ms
||  mt HS (Mop2 op a1 a2.ms) =
	let HS'  = chg (ssize op) HS a1 in
	Mop2 op (adj HS a1) (adj HS' a2) .mt (chg (dsize op) HS' a2) ms
||  mt HS (Mop3 op a1 a2 a3.ms) =
	let HS'  = chg (ssize op) HS  a1 in
	let HS'' = chg (ssize op) HS' a2 in
	Mop3 op (adj HS a1) (adj HS' a2) (adj HS'' a3) .mt (chg (dsize op) HS'' a3) ms
||  mt HS (Mboolcc cc a.ms) = 
	Mboolcc cc (adj HS a) .mt (chg 1 HS a) ms
||  mt HS (Mdata.ms) = Mdata .mskip HS ms
||  mt HS (m.ms) & (nonseq m) = flush HS @ m.mt (0,0) ms
||  mt HS (m.ms) = m.mt HS ms
and dtmp1 = glob "dtmp1"
and dtmp2 = glob "dtmp2"
and Ftmp1 = dtmp1
and conflict (regind r1 _) (reg r2) = r1 = r2
||  conflict a1            a2       = a1 = a2
and mv [] = []
||  mv (Mop3 op popV a2 a3.ms) = let tmp = dtmp1 in Mmove1 op popV tmp.mv (Mop3 op tmp a2 a3.ms)
||  mv (Mop3 op a1 popV pushV.ms) = Mop3 op a1 (Vind 0) (Vind 0).mv ms
#if 0
||  mv (Mop3 op a1 a2 pushV.ms) = Mmove (const 0) pushV.mv (Mop3 op a1 a2 (Vind 0).ms)
||  mv (Mop3 op a1 popV a3.ms) =
	if ~ conflict a1 a3 then
		Mmove popV a3. Mop3 op a1 a3 a3. mv ms
	else
		Mmove popV dtmp2.mv (Mop3 op a1 dtmp2 a3.ms)
||  mv (Mop2 op popV a2.ms) = Mmove popV dtmp1.mv (Mop2 op dtmp1 a2.ms)
#else
||  mv (Mop3 op a1 popV a3.ms) = let tmp = dtmp1 in Mmove1 op popV tmp.mv (Mop3 op a1 tmp a3.ms)
||  mv (Mop2 op popV a2.ms) = let tmp = dtmp1 in Mmove1 op popV tmp.mv (Mop2 op tmp a2.ms)
||  mv (Mop3 op a1 a2 pushV.ms) = let tmp = dtmp1 in mv (Mop3 op a1 a2 tmp.Mmove2 op tmp pushV.ms)
#endif

||  mv (Mcompare popV a2.ms) = Mmove popV dtmp1.mv (Mcompare dtmp1 a2.ms)
||  mv (Mcompare a1 popV.ms) = Mmove popV dtmp1.mv (Mcompare a1 dtmp1.ms)
||  mv (Mcomparesf popV a2.ms) = Mmovesf popV Ftmp1.mv (Mcomparesf Ftmp1 a2.ms)
||  mv (Mcomparesf a1 popV.ms) = Mmovesf popV Ftmp1.mv (Mcomparesf a1 Ftmp1.ms)
||  mv (Mcomparedf popV a2.ms) = Mmovedf popV Ftmp1.mv (Mcomparedf Ftmp1 a2.ms)
||  mv (Mcomparedf a1 popV.ms) = Mmovedf popV Ftmp1.mv (Mcomparedf a1 Ftmp1.ms)
--||  mv (Mcompare popV a2.ms) = Mmove popV dtmp1.mv (Mcompare dtmp1 a2.ms)
--||  mv (Mcompare a1 popV.ms) = Mmove popV dtmp2.mv (Mcompare a1 dtmp2.ms)
||  mv (Mboolcc cc pushV.ms) = Mboolcc cc dtmp2.Mmove dtmp2 pushV.mv ms
||  mv (m.ms) = m.mv ms
-- Make constants first operand.
and mc (Mcompare (a1 as const _) a2.ms) = Mcompare a2 a1. invj ms
||  mc (m.ms) = m.mc ms
||  mc [] = []
and invj (Mjcond cc l.ms) = Mjcond (invmop cc) l. mc ms
||  invj (Mboolcc cc l.ms) = Mboolcc (invmop cc) l. mc ms
||  invj (m.ms) = m . invj ms
||  invj [] = []

-- Insert temp regs.
and mr ((m as Mmove a1 _).ms) & (okop a1) = m.mr ms
||  mr ((m as Mmove popV _).ms) = m.mr ms
||  mr ((m as Mmove a1 pushV).ms) & (~isrel a1) = m.mr ms
||  mr ((m as Mmove _ Sp).ms) = m.mr ms
||  mr ((m as Mmove _ Vp).ms) = m.mr ms
||  mr ((m as Mmove _ hp).ms) = m.mr ms
||  mr ((m as Mmove _ (reg _)).ms) = m.mr ms
||  mr ((m as Mmove a1 a2).ms) =
	let ms' = mr ms in
	case freeregs ms' (usedregs a2) in
	   [] : m.ms'
	|| r._ : Mmove a1 (reg r).Mmove (reg r) a2.ms'
	end
||  mr (m.ms) = m.mr ms
||  mr [] = []
and freeregs (Mmove a1 (a2 as reg r).ms) ur =
	let ur' = usedregs a1@ur in
	if mem r ur' then
		freeregs ms ur'
	else
		[r]
||  freeregs (Mmove a1 a2.ms) ur = freeregs ms (usedregs a1@usedregs a2@ur)
||  freeregs (Mop2 _ a1 a2.ms) ur = freeregs ms (usedregs a1@usedregs a2@ur)
||  freeregs (Mjumpf _.ms) ur = difference ALLREGS ur
||  freeregs (Mjcond _ _.Mcall "GARB".ms) ur = difference ALLREGS ur
||  freeregs (Mcall _.ms) ur = difference [3] ur		-- WHAT'S 3 ???
||  freeregs (Mcalltag _ r.ms) ur = difference [1;3] (r.ur)
||  freeregs (Mjumptag _ r.ms) ur = difference [1;3] (r.ur)
||  freeregs (Mreturn.ms) ur = difference [3] ur
||  freeregs (Mdata.ms) ur = freeregs (totext ms) ur
||  freeregs (Mcom _.ms) ur = freeregs ms ur
||  freeregs (Mnoop.ms) ur = freeregs ms ur
||  freeregs _ _ = []
and totext (Mtext.ms) = ms
||  totext (_.ms) = totext ms
||  totext [] = []
and usedregs (reg r) = [r]
||  usedregs (regind r _) = [r]
||  usedregs (regrel r _) = [r]
||  usedregs _ = []
and okop (Sp)	   = true
||  okop (Vp)	   = true
||  okop (hp)	   = true
||  okop (const _) = true
||  okop (idlit _) = true
||  okop (retaddr _) = true
||  okop (reg _)   = true
||  okop _         = false
and isrel (Vrel k) = k ~= 0
||  isrel (Srel k) = k ~= 0
||  isrel (hprel k) = k ~= 0
||  isrel (regrel _ k) = k ~= 0
||  isrel _ = false

#if 0
and msf ms = map msf1 ms
and msf1 (Mmovesf a1 a2) = Mmovedf a1 a2
||  msf1 (Mcomparesf a1 a2) = Mcomparedf a1 a2
||  msf1 (Msfloat s) = Mdfloat s
||  msf1 (Mop2 op a1 a2) = Mop2 (msfop op) a1 a2
||  msf1 (Mop3 op a1 a2 a3) = Mop3 (msfop op) a1 a2 a3
||  msf1 m = m
and msfop sfadd = dfadd
||  msfop sfsub = dfsub
||  msfop sfmul = dfmul
||  msfop sfdiv = dfdiv
||  msfop sfneg = dfneg
||  msfop sftoi = dftoi
||  msfop itosf = itodf
||  msfop op = op
#endif

and mtrans = mr o mc o mv o mt (0,0)
end
