module -- mtrans
--
-- remove all popS, pushS, tohp because 32016 can't use them
--
#include "../mcode/mcodedef_t.t"
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
and chg (H,S) pushS    = (H,S-1)
||  chg (H,S) popS     = (H,S+1)
||  chg (H,S) tohp     = (H+1,S)
||  chg HS    _        = HS

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
and
    mt _ [] = []
||  mt (H,S) (Mmove (Srel n) Sp.ms) = mt (H, S+n) ms
||  mt (H,S) (Mmove (hprel n) hp.ms) = mt (H+n, S) ms
||  mt (H,S) (Mmove a1 Sp.ms) = Mmove (adj (H,S) a1) Sp .mt (H,0) ms
||  mt HS (Mmove a1 a2.ms) =
	let HS' = chg HS a1 in
	Mmove (adj HS a1) (adj HS' a2) .mt (chg HS' a2) ms
||  mt HS (Mcompare a1 a2.ms) =
	let HS' = chg HS a1 in
	Mcompare (adj HS a1) (adj HS' a2) .mt (chg HS' a2) ms
||  mt HS (Mop2 op a1 a2.ms) =
	let HS'  = chg HS  a1 in
	Mop2 op (adj HS a1) (adj HS' a2) .mt (chg HS' a2) ms
||  mt HS (Mboolcc cc a.ms) = 
	Mboolcc cc (adj HS a) .mt (chg HS a) ms
||  mt HS (Mdata.ms) = Mdata .mskip HS ms
||  mt HS (m.ms) & (nonseq m) = flush HS @ m.mt (0,0) ms
||  mt HS (m.ms) = m.mt HS ms
and mtrans ms = mt (0,0) ms
end
