module -- mtrans
--
-- remove all popS, popV, tohp, etc because Cray can't use them
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "misc.t"

export mtrans;
rec
    srel 0 = Sp
||  srel n = Srel n
and hrel 0 = hp
||  hrel n = hprel n
and vrel 0 = Vp
||  vrel n = Vrel n
and adj (_,_,V) Vp       = vrel V
||  adj (_,_,V) (Vrel n) = vrel (V+n)
||  adj (_,_,V) (Vind n) = Vind (V+n)
||  adj (_,_,V) pushV    = Vind (V-1)
||  adj (_,_,V) popV     = Vind V
||  adj (_,S,_) Sp       = srel S
||  adj (_,S,_) (Srel n) = srel (S+n)
||  adj (_,S,_) (Sind n) = Sind (S+n)
||  adj (_,S,_) pushS    = Sind (S-1)
||  adj (_,S,_) popS     = Sind S
||  adj (H,_,_) hp       = hrel H
||  adj (H,_,_) (hprel n)= hrel (H+n)
||  adj (H,_,_) (hpind n)= hpind (H+n)
||  adj (H,_,_) tohp     = hpind H
||  adj _     a        = a
and chg (H,S,V) pushV    = (H,S,V-1)
||  chg (H,S,V) popV     = (H,S,V+1)
||  chg (H,S,V) pushS    = (H,S-1,V)
||  chg (H,S,V) popS     = (H,S+1,V)
||  chg (H,S,V) tohp     = (H+1,S,V)
||  chg HSV     _        = HSV

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
and -- Must know if label is in text or data-segment
   ml (Mlabel s.l) c = Mlabel (c.s). ml l c
|| ml (Mdata.l)    c = Mdata. ml l 'W'
|| ml (Mtext.l)    c = Mtext. ml l 'P'
|| ml (m.l)        c = m.ml l c
|| ml [] _           = []
and
    mt _ [] = []
||  mt (H,S,V) (Mmove (Vrel n) Vp.ms) = mt (H, S,V+n) ms
||  mt (H,S,V) (Mmove (Srel n) Sp.ms) = mt (H, S+n,V) ms
||  mt (H,S,V) (Mmove (hprel n) hp.ms) = mt (H+n, S,V) ms
||  mt HSV     (Mmove a1 Vp.ms) = 
	let (H',S',V') = chg HSV a1 in
	Mmove (adj HSV a1) Vp .mt (H',S',0) ms
||  mt HSV     (Mmove a1 Sp.ms) = 
	let (H',S',V') = chg HSV a1 in
	Mmove (adj HSV a1) Sp .mt (H',0,V') ms
||  mt HSV (Mmove a1 a2.ms) =
	let HSV' = chg HSV a1 in
	Mmove (adj HSV a1) (adj HSV' a2) .mt (chg HSV' a2) ms

-- This is ugly !!
|| mt HSV (Mcompare a1 a2 . Mjcond cc l . ms) =
	let HSV'  = chg (0,0,0) a1 in
        let HSV'' = chg HSV' a2 in
	let fl = flush HSV @
                 Mcompare (adj (0,0,0) a1) (adj HSV' a2) .
                 Mjcond cc l.
		 mt HSV'' ms
	in if HSV'' ~= (0,0,0) then fail "mtrans: Can't flush this !\n"
			       else fl

||  mt HSV (Mcompare a1 a2.ms) =
	let HSV' = chg HSV a1 in
	Mcompare (adj HSV a1) (adj HSV' a2) .mt (chg HSV' a2) ms
||  mt HSV (Mop2 op a1 a2.ms) =
	let HSV'  = chg HSV  a1 in
	Mop2 op (adj HSV a1) (adj HSV' a2) .mt (chg HSV' a2) ms
||  mt HSV (Mop3 op a1 a2 a3.ms) =
	let HSV'  = chg HSV  a1 in
	let HSV'' = chg HSV' a2 in
	Mop3 op (adj HSV a1) (adj HSV' a2) (adj HSV'' a3) .mt (chg HSV'' a3) ms
||  mt HSV (Mboolcc cc a.ms) = 
	Mboolcc cc (adj HSV a) .mt (chg HSV a) ms
||  mt HSV (Mdata.ms) = Mdata .mskip HSV ms
||  mt HSV (m.ms) & (nonseq m) = flush HSV @ m.mt (0,0,0) ms
||  mt HSV (m.ms) = m.mt HSV ms

-- insert a label after a calltag instruction so Cray can return from sub
and mct (Mcalltag t r.ms) (l.ls) = (Mcalltag t r).(Mlabel l).mct ms ls
 || mct [] l = []
 || mct (m.ms) l = m.mct ms l

-- Insert temp regs.
-- Needed for address computation (and compares on address registers).
    -- Flush return adress onto stack
and mr (Mmove (retaddr s) a2.ms) =
      [Mmove Rpreg a2; Mmove (idlit s) Rpreg] @ mr ms
||  mr ((m as Mmove a1 a2).ms) & (okop1 a1 | okop2 a2) = m.mr ms
||  mr ((m as Mmove a1 a2).ms) =
	let ms' = mr ms in
	case freeregs ms' (usedregs a2) in
	   [] : Mcom "no free regs".m.ms'
	|| r._ : Mmove a1 (reg r).Mmove (reg r) a2.ms'
	end
||  mr (m.ms) = m.mr ms
||  mr [] = []
-- Compute the set of free address registers after an instruction sequence.
-- As soon as a single free register is found the search ends.
and freeregs (Mmove a1 (a2 as reg r).ms) ur =
	let ur' = usedregs a1@ur in
	if mem r ur' then
		freeregs ms ur'
	else
		[r]
||  freeregs (Mmove a1 a2.ms) ur = freeregs ms (usedregs a1@usedregs a2@ur)
||  freeregs (Mop2 _ a1 a2.ms) ur = freeregs ms (usedregs a1@usedregs a2@ur)
||  freeregs (Mjumpf _.ms) ur = difference [0;1;2;3] ur
||  freeregs (Mjcond _ _.Mcall "GARB".ms) ur = difference [0;1;2;3] ur
||  freeregs (Mcall _.ms) ur = difference [3] ur
||  freeregs (Mcalltag _ r.ms) ur = difference [1;3] (r.ur)
||  freeregs (Mjumptag _ r.ms) ur = difference [1;3] (r.ur)
||  freeregs (Mreturn.ms) ur = difference [3] ur
||  freeregs (Mdata.ms) ur = freeregs (totext ms) ur
||  freeregs (Mnoop.ms) ur = freeregs ms ur
||  freeregs (Mcom _.ms) ur = freeregs ms ur
||  freeregs (Mlabel _.ms) ur = freeregs ms ur
||  freeregs (Mexport _.ms) ur = freeregs ms ur
||  freeregs (Mfunbegin _ _.ms) ur = freeregs ms ur
||  freeregs _ _ = []
and totext (Mtext.ms) = ms
||  totext (_.ms) = totext ms
||  totext [] = []
and usedregs (reg r) = [r]
||  usedregs (regind r _) = [r]
||  usedregs (regrel r _) = [r]
||  usedregs _ = []
and okop1 (regrel _ _) = false
||  okop1 (Vrel _)     = false
||  okop1 (Srel _)     = false
||  okop1 (hprel _)    = false
||  okop1 _            = true
and okop2 (Sp)	   = true
||  okop2 (Vp)	   = true
||  okop2 (hp)	   = true
||  okop2 (reg _)  = true
||  okop2 _        = false
-- ugly programming award finalist follows:
and getdata [] = []
 || getdata (Mtext.ms) = getdata (skiptodata ms)
 || getdata (m.ms)     = m.getdata ms
and skiptodata [] = []
 || skiptodata (l as Mdata.ms) = l
 || skiptodata (m.ms) = skiptodata ms
and onedata [] _ = []
 || onedata (Mdata.ms) 0 = Mdata. onedata ms 1
 || onedata (Mdata.ms) n = onedata ms n
 || onedata (m.ms) n = m.onedata ms n
and onetext [] _ = []
 || onetext (Mtext.ms) 0 = Mtext. onetext ms 1
 || onetext (Mtext.ms) n = onetext ms n
 || onetext (m.ms) n = m.onetext ms n
and gettext [] = []
 || gettext (Mdata.ms) = gettext (skiptotext ms)
 || gettext (m.ms)     = m.gettext ms
and skiptotext [] = []
 || skiptotext (l as Mtext.ms) = l
 || skiptotext (m.ms) = skiptotext ms
and md = \ms.onedata (onetext (getdata ms @ gettext ms) 0) 0
/*
and mstrconv s u = let t = "LSC"@itos u in conc (map2 (\c.\n. let tc = t @ '_'.itos n @ "c" in
		[ Mlabel (t@'_'.itos n);
		  Mword (glob "PAIR1");
		  Mword (glob tc);
		  Mword (glob (t@'_'.itos (n+1)));
		  Mlabel tc;
		  Mword (glob "CHAR");
		  Mword (const (ord c)) ] ) s (from 1)) @ [ Mlabel (t@'_'.itos (length s + 1)); Mword (glob "TAG0"); Mword (const 0) ]
and mstr [] _ = []
||  mstr (Mstring s.ms) (u.us) = mstrconv s u @ mstr ms us
||  mstr (m.ms) us = m . mstr ms us
*/
-- Implementation on Cray doesn't use string nodes, cause Cray
-- stuffs 4 chars into one word. Space efficient, but I will
-- not unpack them. Implementation uses list of chars insted
-- This routine transforms away the String Node declarations.
and msrm [] = []
||  msrm (Mword (glob "STRING"). Mword _. Mword _. Mlabel _.ms) = msrm ms
||  msrm (m.ms) = m.msrm ms

and mtrans ms = mct (md (ml (mr (mt (0,0,0) (msrm ms))) 'P'))
                    (map (\i."PCT%" @ itos i) (from 1))
end
