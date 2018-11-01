module -- mtrans
--
--
#include "../misc/flags.t"
#include "../mcode/mcodedef_t.t"
#include "../mcode/mutil1.t"
#include "../runtime/tagtable.h"
#include "tmp.h"

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
#if 0
and ssize op = if op >= dfadd & op <= dftoi then 2 else 1
and dsize op = if op >= dfadd & op < dftoi | op = itodf then 2 else 1

and fXtmp1 op n = if ssize op = 2 then fftmp1 n else ftmp1 n
and fXtmp2 op n = if dsize op = 2 then fftmp2 n else ftmp2 n
and MmoveX (a1 as reg n) a2 & (n >= 32) = Mmovedf a1 a2
||  MmoveX a1 (a2 as reg n) & (n >= 32) = Mmovedf a1 a2
||  MmoveX a1 a2                        = Mmove  a1 a2
#endif

and rc op  (const _) = mem op [add; sub; mul; btand; btor; btxor; btlsh; btrsh; btrsha]
||  rc _   (reg _)   = true
||  rc _   _         = false
and op31 n op (a1 as reg _ ) (a2 as const _) a3 & (mem op [add; mul; btand; btor; btxor]) = op31 n op a2 a1 a3
||  op31 n op a1 a2 a3 & (rc op a1) = op32 n op a1 a2 a3
||  op31 n op a1 a2 a3 = let r = ftmpX1 op n in MmoveX1 op a1 r . op32 n op r a2 a3
and op32 n op a1 (a2 as reg _) a3 = op33 n op a1 a2 a3
||  op32 n op a1 a2 a3 = let r = ftmpX2 op n in MmoveX1 op a2 r . op33 n op a1 r a3
and op33 n op a1 a2 (a3 as reg _) = [Mop3 op a1 a2 a3]
||  op33 n op a1 a2 a3 = let r = ftmpX3 op n in [Mop3 op a1 a2 r; Mmove r a3]

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
and tagno = mapsnd (\x.(x-O_CANON) * (TAGTABLESIZE * WSIZE)) tagtab
and litopt n (Mmove (idlit t) a) & (~NoMOpt & mem t tags) = litopt' n t a
||  litopt n m = [m]
and litopt' n t a =
	case assoc t tagno in
	   0 : [Mmove (reg Canon) a]
        || k : mr n (Mop3 add (const k) (reg Canon) a)
        end

and twoop op = mem op [neg; btcompl; dfneg; dftoi; itodf; sfneg; sftoi; itosf; sftodf; dftosf]

-- Insert temp regs.
and mr n (Mmove (retaddr s) a2) = [Mmove (reg Ret) a2; Mmove (idlit s) (reg Ret)]
||  mr n (Mmove (idlit t) a) & (~NoMOpt & mem t tags) = litopt' n t a
||  mr n (m as Mmove (reg _) _) = [m]
||  mr n (m as Mmove _ (reg _)) = [m]
||  mr n (Mmove a1 a2) = [Mmove a1 (ftmp1 n); Mmove (ftmp1 n) a2]
||  mr n (m as Mmovedf (reg _) _) = [m]
||  mr n (m as Mmovedf _ (reg _)) = mt m
||  mr n (Mmovedf a1 a2) = concmap mt [Mmovedf a1 (fftmp1 n); Mmovedf (fftmp1 n) a2]
||  mr n (m as Mmovesf (reg _) _) = [m]
||  mr n (m as Mmovesf _ (reg _)) = mt m
||  mr n (Mmovesf a1 a2) = concmap mt [Mmovesf a1 (fftmp1 n); Mmovesf (fftmp1 n) a2]
||  mr n (m as Mcompare (reg _) (reg _)) = [m]
||  mr n (m as Mcompare (reg _) (const _)) = [m]
||  mr n (Mcompare a1 a2) & (rc add a1) = [Mmove a2 (ftmp2 n); Mcompare a1 (ftmp2 n)]
||  mr n (Mcompare a1 a2) & (rc add a2) = [Mmove a1 (ftmp1 n); Mcompare (ftmp1 n) a2]
||  mr n (Mcompare a1 a2) = [Mmove a1 (ftmp1 n); Mmove a2 (ftmp2 n); Mcompare (ftmp1 n) (ftmp2 n)]
||  mr n (m as Mcomparedf (reg _) (reg _)) = [m]
||  mr n (Mcomparedf a1 a2) = concmap mt [Mmovedf a1 (fftmp1 n); Mmovedf a2 (fftmp2 n); Mcomparedf (fftmp1 n) (fftmp2 n)]
||  mr n (m as Mcomparesf (reg _) (reg _)) = [m]
||  mr n (Mcomparesf a1 a2) = concmap mt [Mmovesf a1 (fftmp1 n); Mmovesf a2 (fftmp2 n); Mcomparesf (fftmp1 n) (fftmp2 n)]
||  mr n (m as Mop2 op a1 (reg _)) & (rc op a1) = [m]
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
		    MmoveX2 op a2 t2; 
		    Mop2 op t1 t2;
		    MmoveX2 op t2 a2]
#if 0
||  mr n (Mop2 op a1 a2) & (rc op a1 & dsize op = 2) =
	concmap mt [Mmovedf a2 (fftmp2 n); Mop2 op a1 (fftmp2 n); Mmovedf (fftmp2 n) a2]
||  mr n (Mop2 op a1 a2) & (rc op a1) =
	[Mmove a2 (ftmp2 n); Mop2 op a1 (ftmp2 n); Mmove (ftmp2 n) a2]
||  mr n (Mop2 op a1 (a2 as reg _)) & (ssize op = 2) = 
	concmap mt [Mmovedf a1 (fftmp1 n); Mop2 op (fftmp1 n) a2]
||  mr n (Mop2 op a1 (a2 as reg _)) = 
	[Mmove a1 (ftmp1 n); Mop2 op (ftmp1 n) a2]
-- !!! MORE!!!
||  mr n (Mop2 op a1 a2) = 
	[Mmove a1 (ftmp1 n); Mmove a2 (ftmp2 n); Mop2 op (ftmp1 n) (ftmp2 n); Mmove (ftmp2 n) a2]
#endif

||  mr n (Mop3 op a1 a2 a3) = op31 n op a1 a2 a3

||  mr n (m as Mcase (reg _) x1 x2 x3 x4 x5) = [m]
||  mr n (m as Mboolcc cc (reg _)) = [m]
||  mr n (Mboolcc cc a) = [Mboolcc cc (ftmp1 n); Mmove (ftmp1 n) a]
||  mr n m = [m]

and wsize = WSIZE

and mt (Mmove (regrel r1 0) (reg r2)) & (r1=r2) = 
	[]
||  mt (Mmove (regrel r1 0) (reg r2)) = 
	[Mmove (reg r1) (reg r2)]
||  mt (Mmove (regrel r1 n) (reg r2)) =
	[Mop3 add (const (wsize*n)) (reg r1) (reg r2)]
||  mt m = [m]
and mm (Mcompare (glob "_ehp") a2) = Mcompare (reg Ehpr) a2
||  mm (Mcompare a1 (glob "_ehp")) = Mcompare a1 (reg Ehpr)
||  mm (Mcompare a1 (idlit "CANON")) = Mcompare a1 (reg Canon)
||  mm m = m

and mtrans2 ms = concmap mt (concmap2 mr (let rec l = 0 . 1 . 2 . l in l) (map mm ms))

and concmap2 f (x1.l1) (x2.l2) = f x1 x2 @ concmap2 f l1 l2
||  concmap2 f _ _ = []
end
