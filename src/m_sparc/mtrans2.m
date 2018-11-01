module -- mtrans
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "../misc/flags.t"
#include "regno.h"
#include "reg.h"
#include "machine.t"
#include "../runtime/tagtable.h"
#undef r2
export mtrans2;
rec

    ftmp1 0 = dtmp1
||  ftmp1 1 = dtmp2
||  ftmp1 2 = dtmp3
and ftmp2 0 = dtmp2
||  ftmp2 1 = dtmp3
||  ftmp2 2 = dtmp1
and ftmp3 0 = dtmp3
||  ftmp3 1 = dtmp1
||  ftmp3 2 = dtmp2
and fftmp1 0 = Ftmp1
||  fftmp1 1 = Ftmp2
||  fftmp1 2 = Ftmp3
and fftmp2 0 = Ftmp2
||  fftmp2 1 = Ftmp3
||  fftmp2 2 = Ftmp1
and fftmp3 0 = Ftmp3
||  fftmp3 1 = Ftmp1
||  fftmp3 2 = Ftmp2

and sfsrc op = mem op [sftoi; sftodf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and sfdst op = mem op [itosf; dftosf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and dfsrc op = mem op [dftoi; dftosf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and dfdst op = mem op [itodf; sftodf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and fsrc op = sfsrc op | dfsrc op
and fdst op = sfdst op | dfdst op
and dfop op = dfsrc op | dfdst op

and ftmpX1 op n = if fsrc op then fftmp1 n else ftmp1 n
and ftmpX2 op n = if fsrc op then fftmp2 n else ftmp2 n
and ftmpX3 op n = if fdst op then fftmp3 n else ftmp3 n
and MmoveX1 op a1 a2 = if sfsrc op then Mmovesf a1 a2 else if dfsrc op then Mmovedf a1 a2 else Mmove a1 a2
and MmoveX2 op a1 a2 = if sfdst op then Mmovesf a1 a2 else if dfdst op then Mmovedf a1 a2 else Mmove a1 a2

and small n = n < CLIM & n >= -CLIM
and rc add (const n) = small n
||  rc sub (const n) = small n
||  rc _   (reg _)   = true
||  rc _   _         = false
and op31 n add (a1 as reg _) (a2 as const _) a3 = op31 n add a2 a1 a3
||  op31 n op a1 a2 a3 & (rc op a1) = op32 n op a1 a2 a3
||  op31 n op a1 a2 a3 = let r = ftmpX1 op n in MmoveX1 op a1 r . op32 n op r a2 a3
and op32 n (op as sub) a1 (a2 as const 0) a3 = op33 n op a1 a2 a3
||  op32 n op a1 (a2 as reg _) a3 = op33 n op a1 a2 a3
||  op32 n op a1 a2 a3 = let r = ftmpX2 op n in MmoveX1 op a2 r . op33 n op a1 r a3
and op33 n op a1 a2 (a3 as reg _) = [Mop3 op a1 a2 a3]
||  op33 n op a1 a2 a3 = let r = ftmpX3 op n in [Mop3 op a1 a2 r; Mmove r a3]

and regzero = reg Zero
and saveret = Mmove (reg Ret) (regind Vpr 0)
and jtmp = dtmp4
and tagval oeval   = 0
 || tagval ounwind = 1
 || tagval ojfun   = 2
 || tagval ogettag = 3
 || tagval (onumtag i) = i
and tagtab = [
("INT", O_INT);
("SFLOAT", O_SFLOAT);
("DFLOAT", O_DFLOAT);
("BIGNUM", O_BIGNUM);
("CHAR", O_CHAR);
("PAIR", O_PAIR);
("PAIR0", O_PAIR0);
("PAIR1", O_PAIR1);
("PAIR2", O_PAIR2);
("PAIR3", O_PAIR3);
("PAIR4", O_PAIR4);
("TAG", O_TAG);
("TAG0", O_TAG0);
("VEK", O_VEK);
("DVEK", O_DVEK);
("CAP", O_CAP);
("FUN", O_FUN);
("INDIR", O_INDIR);
("ZAP", O_ZAP);
("HOLE", O_HOLE);
("STRING", O_STRING);
("STRINGN", O_STRINGN);
("INPUT", O_INPUT);
("INPUTD", O_INPUTD);
("STRING_F", O_STRING_F);
("STRINGN_F", O_STRINGN_F);
("INPUT_F", O_INPUT_F);
("INPUTD_F", O_INPUTD_F);
("AP", O_AP);
("APG", O_APG);
("VAP", O_VAP);
("VAPG", O_VAPG);
("MARKED", O_MARKED);
("MOVED", O_MOVED);
("GCRET", O_GCRET);
("GSRET", O_GSRET)
]

and tags = map fst tagtab
and tagno = mapsnd (\x.(x-O_CANON) * (TAGTABLESIZE * 4)) tagtab
and litopt n (Mmove (idlit t) a) & (~NoMOpt & mem t tags) = litopt' n t a
||  litopt n m = [m]
and litopt' n t a =
	case assoc t tagno in
	   0 : [Mmove (reg Canon) a]
        || k : mr n (Mop3 add (const k) (reg Canon) a)
        end

and twoop op = mem op [neg; btcompl; dfneg; dftoi; itodf; sfneg; sftoi; itosf; sftodf; dftosf]

and mdfconst b f s = 
	(glob (s@"b"), [Mdata ; Malign; Mlabel s ; Mdfloat (fmtf ".16e" f) ; Mtext ])
and msfconst b f s = 
	(glob (s@"b"), [Mdata ; Malign; Mlabel s ; Msfloat (fmtf ".8e" f) ; Mtext ])
and mXfconst op b f s = if sfsrc op then msfconst b f s else mdfconst b f s
and convloc = glob "cnvtmp"		-- Could be %fp-?? !!!
-- Insert temp regs.
and mr n (Mmove (retaddr s) a2) = [Mmove (reg Ret) a2; Mmove (idlit (s@"-8")) (reg Ret)]
||  mr n (Mmove (const 0) a2) = mr n (Mmove regzero a2)
||  mr n (Mmove (idlit t) a) & (~NoMOpt & mem t tags) = litopt' n t a
||  mr n (m as Mmove (reg _) _) = [m]
||  mr n (m as Mmove _ (reg _)) = mt m
||  mr n (Mmove a1 a2) = concmap mt [Mmove a1 (ftmp1 n); Mmove (ftmp1 n) a2]
||  mr n (m as Mmovedf (reg _) _) = [m]
||  mr n (m as Mmovedf _ (reg _)) = mt m
||  mr n (Mmovedf (fconst f) a2) = let (a1, ms) = mdfconst true f "1" in ms @ mr n (Mmovedf a1 a2)
||  mr n (Mmovedf a1 a2) = concmap mt [Mmovedf a1 (fftmp1 n); Mmovedf (fftmp1 n) a2]
||  mr n (m as Mmovesf (reg _) _) = [m]
||  mr n (m as Mmovesf _ (reg _)) = mt m
||  mr n (Mmovesf (fconst f) a2) = let (a1, ms) = msfconst true f "1" in ms @ mr n (Mmovesf a1 a2)
||  mr n (Mmovesf a1 a2) = concmap mt [Mmovesf a1 (fftmp1 n); Mmovesf (fftmp1 n) a2]
||  mr n (m as Mcompare (reg _) (reg _)) = [m]
||  mr n (m as Mcompare (reg _) (const k)) & (small k) = [m]
||  mr n (Mcompare a1 a2) & (rc add a1) = concmap mt (litopt (fail "mr-n") (Mmove a2 (ftmp2 n)) @ [Mcompare a1 (ftmp2 n)])
||  mr n (Mcompare a1 a2) & (rc add a2) = concmap mt (litopt (fail "mr-n") (Mmove a1 (ftmp1 n)) @ [Mcompare (ftmp1 n) a2])
||  mr n (Mcompare a1 a2) = concmap mt (litopt (fail "mr-n") (Mmove a1 (ftmp1 n)) @ litopt (fail "mr-n") (Mmove a2 (ftmp2 n)) @ [Mcompare (ftmp1 n) (ftmp2 n)])
||  mr n (m as Mcomparesf (reg _) (reg _)) = [m]
||  mr n (Mcomparesf (fconst f) a2) = let (a1, ms) = msfconst true f "1" in ms @ mr n (Mcomparesf a1 a2)
||  mr n (Mcomparesf a1 (fconst f)) = let (a2, ms) = msfconst true f "2" in ms @ mr n (Mcomparesf a1 a2)
||  mr n (Mcomparesf a1 a2) = concmap mt [Mmovesf a1 (fftmp1 n); Mmovesf a2 (fftmp2 n); Mcomparesf (fftmp1 n) (fftmp2 n)]
||  mr n (m as Mcomparedf (reg _) (reg _)) = [m]
||  mr n (Mcomparedf (fconst f) a2) = let (a1, ms) = mdfconst true f "1" in ms @ mr n (Mcomparedf a1 a2)
||  mr n (Mcomparedf a1 (fconst f)) = let (a2, ms) = mdfconst true f "2" in ms @ mr n (Mcomparedf a1 a2)
||  mr n (Mcomparedf a1 a2) = concmap mt [Mmovedf a1 (fftmp1 n); Mmovedf a2 (fftmp2 n); Mcomparedf (fftmp1 n) (fftmp2 n)]
||  mr n (m as Mop2 op _ _) & (~Sparc8 & (op = mul | op = div | op = mod)) = [m]
||  mr n (m as Mop2 op a1 (reg _)) & (rc op a1) = [m]
||  mr n (Mop2 op (fconst f) a2) = 
	let (a1, ms) = mXfconst op true f "1" in 
	ms @ mr n (Mop2 op a1 a2)
||  mr n (Mop2 op a1 a2) & (rc op a1) = 
    if twoop op then
        let t2 = ftmpX3 op n in
	concmap mt [Mop2 op a1 t2; MmoveX2 op t2 a2]
    else
        let t2 = ftmpX3 op n in
	concmap mt [MmoveX2 op a2 t2; Mop2 op a1 t2; MmoveX2 op t2 a2]
||  mr n (Mop2 op a1 (a2 as reg _)) = 
        let t1 = ftmpX1 op n in
	concmap mt [MmoveX1 op a1 t1; Mop2 op t1 a2]
||  mr n (Mop2 op a1 a2) = 
    if twoop op then
        let t1 = ftmpX1 op n
        and t2 = ftmpX3 op n in
	concmap mt [MmoveX1 op a1 t1; 
		    Mop2 op t1 t2;
		    MmoveX2 op t2 a2]
    else
        let t1 = ftmpX1 op n
        and t2 = ftmpX3 op n in
	concmap mt [MmoveX1 op a1 t1; 
		    MmoveX1 op a2 t2; 
		    Mop2 op t1 t2;
		    MmoveX2 op t2 a2]

||  mr n (Mop3 op (fconst f) a2 a3) = let (a1, ms) = mXfconst op true f "1" in ms @ mr n (Mop3 op a1 a2 a3)
||  mr n (Mop3 op a1 (fconst f) a3) = let (a2, ms) = mXfconst op true f "2" in ms @ mr n (Mop3 op a1 a2 a3)
||  mr n (m as Mop3 op _ _ _) & (~Sparc8 & (op = mul | op = div | op = mod)) = [m]
||  mr n (Mop3 op a1 a2 a3) = op31 n op a1 a2 a3

||  mr n (m as Mcase (reg _) x1 x2 x3 x4 x5) = [m]
||  mr n (m as Mboolcc cc (reg _)) = [m]
||  mr n (Mboolcc cc a) = [Mboolcc cc (ftmp1 n); Mmove (ftmp1 n) a]
||  mr n (Mcall s) = [saveret; Masm "\tcall\t^" [glob s]; Mnoop] @ retfixup
||  mr n (Mjumpf s) = 
    if FarJump then
	[Mmove (idlit s) jtmp; Masm "\tjmp\t^" [jtmp]; Mnoop]
    else
	[Masm "\tb\t^" [glob s]; Mnoop; Mword (const 0x12344321)]
||  mr n (Mjump s) = [Masm "\tb\t^" [glob s]; Mnoop]
||  mr n (Mreturn) =
	[Mop2 add (const 4) (reg Vpr);
	 Masm "\tjmpl\t^+8,^" [reg Ret; regzero];
         Mmove (regind Vpr (-1)) (reg Ret)]	-- restore return reg in the delay slot
||  mr n (Mcalltag t r) = 
	    [saveret; Mmove (regind r (tagval t)) jtmp; Masm "\tcall\t^" [jtmp]; Mnoop] @ retfixup
||  mr n (Mjumptag t r) =          [Mmove (regind r (tagval t)) jtmp; Masm "\tjmp\t^"  [jtmp]; Mnoop]
||  mr n (Mjcond cc l) = [Masm ("\t"@ccname cc@"\t^") [glob l]; Mnoop]
||  mr n m = [m]

and mt (Mmove (regrel r1 0) (reg r2)) & (r1=r2) = 
	[]
||  mt (Mmove (regrel r1 0) (reg r2)) = 
	[Mmove (reg r1) (reg r2)]
#if 0
||  mt (Mmove (regrel r1 n) (reg r2)) & (r1=r2) = 
	[Mop2 add (const (4*n)) (reg r1)]
||  mt (Mmove (regrel r1 n) (reg r2)) = 
	[Mop3 add (const (4*n)) (reg r1) (reg r2)]
#else
||  mt (Mmove (regrel r1 n) (reg r2)) = 
	op31 0 add (const (4*n)) (reg r1) (reg r2)
#endif
||  mt m = [m]
and mm (Mcompare (glob "_ehp") a2) = Mcompare (reg Ehpr) a2
||  mm (Mcompare a1 (glob "_ehp")) = Mcompare a1 (reg Ehpr)
||  mm (Mcompare a1 (idlit "CANON")) = Mcompare a1 (reg Canon)
||  mm m = m
-- Peephole over the transformed mcode to fill the delay slot
and mp [] = []
||  mp (Mop3 add a (r as reg _) (r' as reg _).ms) & (r=r') = mp (Mop2 add a r . ms) -- to enable next optim
||  mp (Mop2 add (const n) (r1 as reg _) . Mop2 add (const m) r2 . ms) & (r1=r2 & small (n+m)) = mp (Mop2 add (const (n+m)) r1 . ms)
||  mp ((ma as Mop2 add (const n) (reg Vpr)) . Mmove (reg Ret) (regind Vpr 0) . (mc as Masm "\tcall\t^" _) . Mnoop . ms) =
    Mmove (reg Ret) (regind Vpr (n/4)) . mc . ma . mp ms
||  mp ((ma as Mop2 add (const n) (reg Spr)) . (mr as Mmove (reg Ret) (regind Vpr 0)) . (mc as Masm "\tcall\t^" _) . Mnoop . ms) & (small n) =
    mr . mc . ma . mp ms
||  mp ((ma as Mop2 add (const n) (reg Vpr)) . Mmove (reg Ret) (regind Vpr 0) . (mi as Mmove (regind Indreg _) _) . (mc as Masm "\tcall\t^" _) . Mnoop . ms) & (small n) =
    Mmove (reg Ret) (regind Vpr (n/4)) . mi . mc . ma . mp ms
||  mp ((ma as Mop2 add (const n) (reg Vpr)) . (mr as Mmove (regind r _) j1) . (mc as Masm "\tjmp\t^" [j2]) . Mnoop . ms) & (j1=jtmp & j2=jtmp & r ~= Vpr & small n) =
    mr . mc . ma . mp ms
||  mp ((ma as Mop2 add (const n) _) . (mc as Masm "\tb\t^" _) . Mnoop . ms) & (small n) =
    mc . ma . mp ms
||  mp ((ma as Mop2 add (const n) (reg r)) . (mc as Mcompare _ (reg Ehpr)) . (mb as Masm "\tbgeu\t^" _) . Mnoop . ms) & (small n & r ~= hpr) =
    mc . mb . ma . mp ms
||  mp ((mc as Mcompare _ (reg Ehpr)) . (mb as Masm "\tbgeu\t^" _) . Mnoop . (ml as Mlabel s) . (mm as Mmove (regind _ k) (reg _)) . ms) & (small k) =
    mc . mb . mm . ml . mp (jumpnop s mm ms)
||  mp ((ma as Mop2 add (const n) (reg Spr)) . (mc as Mcompare _ (reg Canon)) . (mb as Masm "\tbge\t^" _) . Mnoop . ms) & (small n) =
    mc . mb . ma . mp ms
||  mp (Mop2 itodf (a1 as reg _) (a2 as reg _).ms) = [Mmove a1 convloc; Mmove convloc a2; Mop2 itodf a2 a2] @ mp ms
||  mp (Mop2 dftoi (a1 as reg _) (a2 as reg _).ms) = [Mop2 dftoi a1 Ftmp4; Mmove Ftmp4 convloc; Mmove convloc a2 ] @ mp ms
||  mp (Mop2 itosf (a1 as reg _) (a2 as reg _).ms) = [Mmove a1 convloc; Mmove convloc a2; Mop2 itosf a2 a2] @ mp ms
||  mp (Mop2 sftoi (a1 as reg _) (a2 as reg _).ms) = [Mop2 sftoi a1 Ftmp4; Mmove Ftmp4 convloc; Mmove convloc a2 ] @ mp ms
||  mp (m.ms) = m . mp ms

and jumpnop s m ((mb as Masm "\tb\t^" [glob d]) . Mnoop . ms) & (s = d) = mb . m . ms
||  jumpnop s m (m1.ms) = m1.jumpnop s m ms

#if 0
and notin (reg r) (reg r') = r ~= r'
||  notin (reg r) (regind r' _) = r ~= r'
||  notin _ _ = true
and okjmpopt r (Mop2 op a1 a2 ) = (op = add | op = sub) & notin r a1 & notin r a2
||  okjmpopt r (Mop3 op a1 a2 a3) = (op = add | op = sub) & notin r a1 & notin r a2 & notin r a3
||  okjmpopt _ _ = false
#endif

and mtrans2 ms = mp (concmap2 mr (let rec l = 0 . 1 . 2 . l in l) (concmap mt (map mm ms)))

and concmap2 f (x1.l1) (x2.l2) = f x1 x2 @ concmap2 f l1 l2
||  concmap2 f _ _ = []
end
