module -- mtransj
--
-- prepare for join of multiple tohp
--
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "tmp.h"
export mtransj,contfrom;
rec srcuse Vp       = [Vpr]
||  srcuse (Vrel n) = [Vpr]
||  srcuse (Vind n) = [Vpr]
||  srcuse pushV    = [Vpr]
||  srcuse popV     = [Vpr]
||  srcuse Sp       = [Spr]
||  srcuse (Srel n) = [Spr]
||  srcuse (Sind n) = [Spr]
||  srcuse pushS    = [Spr]
||  srcuse popS     = [Spr]
||  srcuse hp       = [hpr]
||  srcuse (hprel n)= [hpr]
||  srcuse (hpind n)= [hpr]
||  srcuse tohp     = [hpr]
||  srcuse (reg r)  = [r]
||  srcuse (regind r _) = [r]
||  srcuse (regrel r _) = [r]
||  srcuse _        = []

and dstuse Vp       = []       -- Not used but defined
||  dstuse (Vrel n) = fail "V relative as destination"
||  dstuse (Vind n) = [Vpr]
||  dstuse pushV    = [Vpr]
||  dstuse popV     = [Vpr]
||  dstuse Sp       = []
||  dstuse (Srel n) = fail "S relative as destination"
||  dstuse (Sind n) = [Spr]
||  dstuse pushS    = [Spr]
||  dstuse popS     = [Spr]
||  dstuse hp       = []
||  dstuse (hprel n)= fail "Hp relative as destination"
||  dstuse (hpind n)= [hpr]
||  dstuse tohp     = [hpr]
||  dstuse (reg r)  = []
||  dstuse (regrel r _) = fail "reg relative as destination"
||  dstuse (regind r _) = [r]
||  dstuse _        = [] -- Not a register at least

and dstdef Vp       = [Vpr]
||  dstdef (Vind n) = []
||  dstdef pushV    = []
||  dstdef popV     = []
||  dstdef Sp       = [Spr]
||  dstdef (Sind n) = []
||  dstdef pushS    = []
||  dstdef popS     = []
||  dstdef hp       = [hpr]
||  dstdef (hpind n)= []
||  dstdef tohp     = []
||  dstdef (reg r)  = [r]
||  dstdef (regind r _) = []
||  dstdef _        = []

and finduse []		          = []
||  finduse (Mmove a1 a2 .r)      = union (union (srcuse a1) (dstuse a2))
                                          (difference (finduse r) (dstdef a2))
||  finduse (Mop2 _ a1 a2 .r)     = union (union (srcuse a1) (dstuse a2))
                                          (difference (finduse r) (dstdef a2))
||  finduse (Mop3 _ a1 a2 a3 .r)  = union (union (union (srcuse a1) (srcuse a2))
					         (dstuse a3))
                                          (difference (finduse r) (dstdef a3))
||  finduse (Mcompare a1 a2 .r)   = union (union (srcuse a1) (srcuse a2))
                                          (finduse r)
||  finduse (Mboolcc _ a1.r)      = union (dstuse a1)
                                          (difference (finduse r) (dstdef a1))
||  finduse (Mdata .r)            = let _,r = splitat Mtext r in finduse r
||  finduse (Mtext .r)            = finduse r
||  finduse (Mword _ .r)          = finduse r
||  finduse (Mdfloat _ .r)        = finduse r
||  finduse (Msfloat _ .r)        = finduse r
||  finduse (Mstring _ .r)        = finduse r
||  finduse (Mexport _ .r)        = finduse r
||  finduse (Mnoop  .r)           = finduse r
||  finduse (Mcom _ .r)           = finduse r
||  finduse (Mfunbegin _ _.r)     = finduse r
||  finduse (Mfunend.r)           = [0]            --- I'm guessing from here !!!
||  finduse (Mcall _._)	          = [0]
||  finduse (Mreturn._)	          = []
||  finduse (Mcalltag _ _._)      = [0;1;3]
||  finduse (Mjumptag _ _._)      = [0;1;3]
||  finduse (Mjump _._)	          = [0;1;3]
||  finduse (Mjumpf _._)	  = [0;1;3]
||  finduse (Mjcond _ _._)        = [0;1]
||  finduse (Mlabel _.r)	  = finduse r
||  finduse (Mcase _ _ _ _ _ _._) = [0;1]
||  finduse (Masm _ _._)	  = count 1 7		-- safe assumption
||  finduse (m.r)                 = fail ("fail in finduse" @  mprint [m])
and findfree gl p = difference (count 1 7) ((finduse p) @ gl)

and fixsub b n ((regrel b' i).r) = regrel b' (if b = b' then (i+n) else i).fixsub b (n+1) r
||  fixsub b n (m.r) = m.fixsub b (n+1) r
||  fixsub b n [] = [] 

and fixcode base fr al =
   let lal = length al
   and lfr = length fr
   in 
     if lfr < 2
       then map (\(a,b).Mmove a b)  al
       else 
	 if lal > lfr
	   then fixcode base fr (head lfr al) @ fixcode base fr (tail lfr al)
	   else map2 (\r.\a.Mmove a (reg r)) fr (fixsub base 0 (map fst al)) @ 
	        map2 (\r.\b.Mmove (reg r) b) fr (map snd al)

and contfrom n ((_,regind _ n').r) = n = n' & contfrom (n+1) r
||  contfrom n   []   = true

and less (_,regind _ a) (_,regind _ b) = a < b

and mtransjp gl [] = []

||  mtransjp gl (pr as (Mmove a1 tohp._)) =
    let tH,r = take (\m.case m in Mmove _ tohp : true || _ : false end) pr
    in if length tH > 1 then
       let al = map (\(Mmove a b).(a,b)) tH
         and fr = findfree gl pr
         in if fr = []
            then tH @ mtransjp gl r   -- No register to play with
            else (fixcode hpr fr al @ mtransjp gl r)
    else
       tH @ mtransjp gl r   -- Only one

||  mtransjp gl ((m as (Mmove a1 (regind Spr n))).r) = m.mtransjp gl r  -- Dangerous is Sp

||  mtransjp gl (pr as (Mmove a1 (regind r1 n)._)) =
    let tH,r = take (\m.case m in Mmove _ (regind r2 _) : r1 = r2 || _ : false end) pr
    in if length tH > 1 then
     let al = sort less (map (\(Mmove a b).(a,b)) tH)
       and fr = findfree gl pr
       in if fr = [] | ~(contfrom 0 al)
          then tH @ mtransjp gl r   -- No register to play with or non-consecutive
          else (fixcode r1 fr al @ mtransjp gl r)
    else
       tH @ mtransjp gl r   -- Only one

||  mtransjp gl ((m as Mpragma "GC_ON").r) = m.mtransjp (union gl [GCSTART;GCEND;GCCUR]) r
||  mtransjp gl ((m as Mpragma "GC_OFF").r) = m.mtransjp (difference gl [GCSTART;GCEND;GCCUR]) r
||  mtransjp gl (m.r) = m.mtransjp gl r

and mtransj l = mtransjp [] l

end
