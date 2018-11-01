module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../mcode/mutil1.t"
#include "../misc/flags.t"
#include "mtrans1.t"
#include "mtrans2.t"
#include "reg.h"
#include "../mcode/limit.h"

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, farcase,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, retfixup;
rec
    Aregs = count 1 6 @ count 8 10 @ fail "Out of A-regs"
and Dregs = count 1 6 @ count 8 10 @ fail "Out of D-regs"
and Fregs = map (\x.x+FP+16) [0..15] @ fail "Out of F-regs"
and sfloatsize = 1
and dfloatsize = 2
and usecase max low high cnt = cnt>=4 & (high-low)<cnt*5
and farcase = true
and retfixup = []
and use3op = true
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg 11		-- (real %r19)
and regname n = if n < FP then "%r"@itos ((n+8)%32) else if n < FPL then "%fr"@itos (n-FP) else if n < FPR then "%fr"@itos (n-FPL)@"L" else "%fr"@itos (n-FPR)@"R"
and cputype = "HPPA"

and amode (reg i)      = regname i
 || amode (regind r i) = itos (i*4) @ "(0," @ regname r @ ")"
 || amode (regrel _ _) = fail "amode regrel\n"

 || amode (glob i)  = drop_ i
 || amode (idlit s) = drop_ s
 || amode (const n) = itos n
 || amode (fconst f) = fmtf ".17e" f
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am @ (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and isreg (reg _) = true
 || isreg _       = false
and drop_ ('_'.s) = s
||  drop_ s = s
and otag oeval   = "0"
 || otag ounwind = "4"
 || otag ojfun   = "8"
 || otag ogettag = "12"
 || otag (onumtag i) = itos (4*i)
and ccname eq = "="
 || ccname ne = "<>"
 || ccname lt = "<"
 || ccname gt = ">"
 || ccname le = "<="
 || ccname ge = ">="
 || ccname ltstack = "<<"
 || ccname ltheap = "<<"
 || ccname gtstack = ">>"
 || ccname geheap = ">>="
 || ccname dfeq = "dbl,="
 || ccname dfne = "dbl,!="
 || ccname dflt = "dbl,<"
 || ccname dfgt = "dbl,>"
 || ccname dfle = "dbl,<="
 || ccname dfge = "dbl,>="
 || ccname sfeq = "sgl,="
 || ccname sfne = "sgl,!="
 || ccname sflt = "sgl,<"
 || ccname sfgt = "sgl,>"
 || ccname sfle = "sgl,<="
 || ccname sfge = "sgl,>="
and opname add = "add"
||  opname sub = "sub"
||  opname mul = fail "opname mul"
||  opname div = fail "opname div"
||  opname mod = fail "opname mod"
||  opname neg = fail "opname neg"
||  opname btand = "and"
||  opname btor = "or"
||  opname btxor = "xor"
||  opname btcompl = fail "opname compl"
||  opname btlsh = fail "opname lsh"
||  opname btrsh = fail "opname rsh"
||  opname btrsha = fail "opname rsha"

||  opname dfadd = "fadd,dbl"
||  opname dfsub = "fsub,dbl"
||  opname dfmul = "fmpy,dbl"
||  opname dfdiv = "fdiv,dbl"
||  opname dfneg = fail "opname dfneg"
||  opname dftoi = fail "opname dftoi"
||  opname itodf = fail "opname itodf"
||  opname sfadd = "fadd,sgl"
||  opname sfsub = "fsub,sgl"
||  opname sfmul = "fmpy,sgl"
||  opname sfdiv = "fdiv,sgl"
||  opname sfneg = fail "opname sfneg"
||  opname sftoi = fail "opname sftoi"
||  opname itosf = fail "opname itosf"
||  opname sftodf = fail "opname sftodf"
||  opname dftosf = fail "opname dftosf"

and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and amode4 s a1 a2 a3 a4 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@","@amode a4@"\n"
and oneop s1 s2 = '\t'.s1@'\t'.s2@"\n"
-- extra space allocated by previous transformation
and saveret = ins true [] (Mmove (reg Ret) (regind Vpr 0))
and dtmp = dtmp4
and idtmp = (regind r 0 where (reg r) = dtmp)
and adtmp = amode dtmp
and theaddr = regind Rsp (-4)
and is14bit k = -8192 <= k & k <= 8191
and whatspace s =
	case s in
	   'C' . _ : ",data"
	|| 'V' . _ : ",data"
	|| 'U' . 'S' . 'E' . '_' . _ : ",data"
	|| 'J' . _ : ",code"
	|| 'S' . 'T' . 'R' . 'I' . 'N' . 'G' . _ : ",data"
	|| 'S' . 'Y' . _ : ",data"
	|| 'S' . _ : ",code"
	|| 'u' . 'n' . 'w' . _ : ",code"
	|| 'v' . 'u' . 'n' . 'w' . _ : ",code"
	|| _       : /*trace s*/ ",data" --fail ("whatspace "@s)
	end
and doimport ws ('*' . 'C' . '*' . '_' . s) = "\t.import\t"@s@",code\n"	-- hack for ccall
||  doimport ws ('*' . 'D' . '*' . '_' . s) = "\t.import\t"@s@",data\n"
||  doimport ws (s as c . _) = 
	if isalpha c & c ~= 'L' & ~(mem s ws) then "\t.import\t"@s@whatspace s@"\n" else ""
and splitld ws s r =
	doimport ws s @
        amode2 "ldil" (idlit ("L'"@s)) r @
        amode2 "ldo"  (idlit ("R'"@s@"("@amode r@")")) r 
-- XXX bug in HP assembler/linker.  Offset in branch instruction is sometimes miscomputed.
and brimport (s as c . _) =
	if c = 'L' then "" else "\t.import\t"@s@",code\n"
#if 0
and do_br br s = 
	brimport s
-- Buggy still
--	"\tldil\tL'"@s@","@adtmp @ "\n\t"@br@"\tR'"@s@"(4,"@adtmp@")\n"
	"\t"@br@"\tR'"@s@"(4,"@adtmp@")\n"
--and do_br br s = "\t.import\t"@s@",code\n\tldil\tL'"@s@","@adtmp@"\n\tldo\tR'"@s@"("@adtmp@"),"@adtmp@ "\n\t"@br@"\t0(4,"@adtmp@")\n"
#endif
and copy a1 a2 = amode2 "copy" a1 a2
and
    ins td ws (Mmove (r1 as reg _) (r2 as reg _)) = 
	copy r1 r2
||  ins td ws (Mmove (a1 as reg _) (a2 as regind _ _)) =
	amode2 "stw" a1 a2
||  ins td ws (Mmove (a1 as regind _ _) (a2 as reg _)) =
	amode2 "ldw" a1 a2
||  ins td ws (Mmove (a1 as reg _) pushV) =
	amode2 "stws,mb" a1 (regind Vpr (-1))
||  ins td ws (Mmove popV (a2 as reg _)) =
	amode2 "ldws,ma" (regind Vpr 1) a2
||  ins td ws (Mmove (a1 as const k) (a2 as reg _)) =
	if is14bit k then
	    amode2 "ldi" a1 a2
	else
	    splitld ws (itos k) a2
||  ins td ws (Mmove (idlit s) (a2 as reg _)) =
	splitld ws (drop_ s) a2
||  ins td ws (Mmove (a1 as reg _) (glob s)) =
        splitld ws (drop_ s) dtmp @
	amode2 "stw" a1 idtmp
||  ins td ws (Mmove (glob s) (a2 as reg _)) =
        splitld ws (drop_ s) dtmp @
	amode2 "ldw" idtmp a2

||  ins td ws (Mmovedf (r1 as reg _) (r2 as reg _)) = 
	amode2 "fcpy,dbl" r1 r2
||  ins td ws (Mmovedf (reg rf) (a2 as regind ri ii)) = 
	concmap (ins td ws) [Mmovesf (reg (rf-FP+FPL)) a2; Mmovesf (reg (rf-FP+FPR)) (regind ri (ii+1))]
--	amode2 "fstds" a1 a2
||  ins td ws (Mmovedf (a1 as regind ri ii) (reg rf)) =
	concmap (ins td ws) [Mmovesf a1 (reg (rf-FP+FPL)); Mmovesf (regind ri (ii+1)) (reg (rf-FP+FPR))]
--	amode2 "fldds" a1 a2
||  ins td ws (Mmovesf (r1 as reg _) (r2 as reg _)) = 
	amode2 "fcpy,sgl" r1 r2
||  ins td ws (Mmovesf (a1 as reg _) (a2 as regind r k)) =
	if -4 <= k & k <= 3 then
	    amode2 "fstws" a1 a2
	else
	    concmap (ins td ws) [Mop3 add (const (4*k)) (reg r) dtmp; 
				 Mmovesf a1 idtmp]
||  ins td ws (Mmovesf (a1 as regind r k) (a2 as reg _)) =
	if -4 <= k & k <= 3 then
	    amode2 "fldws" a1 a2
	else
	    concmap (ins td ws) [Mop3 add (const (4*k)) (reg r) dtmp; 
				 Mmovesf idtmp a2]
||  ins td ws (Mmovedf (a1 as reg _) (glob s)) =
	splitld ws s dtmp @
	ins td ws (Mmovedf a1 idtmp)
||  ins td ws (Mmovedf (glob s) (a2 as reg _)) =
	splitld ws s dtmp @
	ins td ws (Mmovedf idtmp a2)
||  ins td ws (Mmovesf (a1 as reg _) (glob s)) =
	splitld ws s dtmp @
	ins td ws (Mmovesf a1 idtmp)
||  ins td ws (Mmovesf (glob s) (a2 as reg _)) =
	splitld ws s dtmp @
	ins td ws (Mmovesf idtmp a2)
||  ins td ws (Madda (a1 as reg _) (a2 as reg _)) = amode3 "sh2addl" a1 a2 a2
||  ins td ws (Mop2 op a1 a2) & (op = itodf | op = itosf) =
	amode2 "stw" a1 theaddr @
	amode2 "fldws" theaddr Fmul1 @
	amode2 ("fcnvxf,sgl,"@if op = itodf then "dbl" else "sgl") Fmul1 a2
||  ins td ws (Mop2 op a1 a2) & (op = dftoi | op = sftoi) =
	amode2 ("fcnvfxt,"@(if op = dftoi then "dbl" else "sgl")@",sgl") a1 Fmul1 @
	amode2 "fstws" Fmul1 theaddr @
	amode2 "ldw" theaddr a2
||  ins td ws (Mop2 op a1 a2) & (op = sftodf | op = dftosf) =
	amode2 ("fcnvff,"@if op = sftodf then "sgl,dbl" else "dbl,sgl") a1 a2

||  ins td ws (Mop2 sfneg a1 a2) = ins td ws (Mop3 sfsub a1 (const 0) a2)
||  ins td ws (Mop2 dfneg a1 a2) = ins td ws (Mop3 dfsub a1 (const 0) a2)
||  ins td ws (Mop2 neg a1 a2) = ins td ws (Mop3 sub a1 (reg Rzero) a2)
||  ins td ws (Mop2 btcompl a1 a2) = amode3 "uaddcm" (const 0) a1 a2
||  ins td ws (Mop3 mul a1 a2 a3) =
	    amode2 "stw" a1 theaddr @
	    amode2 "fldws" theaddr Fmul1 @
	    amode2 "stw" a2 theaddr @
	    amode2 "fldws" theaddr Fmul2 @
	    amode3 "xmpyu" Fmul1 Fmul2 Fmul2d @
	    amode2 "fstws" Fmul2 theaddr @
	    amode2 "ldw" theaddr a3
||  ins td ws (Mop3 op a1 a2 a3) & (op = div | op = mod) =
	let s = if op = div then "div" else "rem" in
	copy a1 (reg Rarg1) @
	"\t.IMPORT\t$$"@s@"I,MILLICODE\n" @
	"\tbl\t$$"@s@"I,%r31\n" @
	copy a2 (reg Rarg0) @			-- copy second arg in the delay slot
	copy (reg Rres1) a3
||  ins td ws (Mop3 btlsh (const k) a1 a2) = amode4 "zdep" a1 (const (31-k)) (const (32-k)) a2
||  ins td ws (Mop3 btlsh a1 a2 a3) = 
	amode3 "subi" (const 31) a1 dtmp @
	amode1 "mtsar" dtmp @
	amode3 "zvdep" a2 (const 32) a3
||  ins td ws (Mop3 btrsh (const k) a1 a2) = amode4 "extru" a1 (const (31-k)) (const (32-k)) a2
||  ins td ws (Mop3 btrsh a1 a2 a3) = 
	amode1 "mtsar" a1 @
	amode3 "vshd" (const 0) a2 a3
||  ins td ws (Mop3 btrsha (const k) a1 a2) = amode4 "extrs" a1 (const (31-k)) (const (32-k)) a2
||  ins td ws (Mop3 btrsha a1 a2 a3) = 
	amode3 "subi" (const 31) a1 dtmp @
	amode1 "mtsar" dtmp @
	amode3 "vextrs" a2 (const 32) a3
||  ins td ws (Mop3 sub a1 (a2 as (const _)) a3) = amode3 "subi" a2 a1 a3
||  ins td ws (Mop3 add (const k) (reg r) a2) = "\tldo\t" @ itos k @ "(" @ regname r @ ")," @ amode a2 @ "\n"
-- XXX bitand 2^n-1 and can be better
||  ins td ws (Mop3 op a1 a2 a3) =
	amode3 (opname op) a2 a1 a3

#if 0
||  ins td ws (Mcall s) = saveret @ do_br "ble" s @ "\tcopy\t%r31,%r2\n"
||  ins td ws (Mjumpf s) = do_br "be,n" s
#else
||  ins td ws (Mcall s)  = saveret @ brimport s @ "\tbl,n\t" @ s @ ",%r2\n" @ "\tnop\n"
||  ins td ws (Mjumpf s) =           brimport s @ "\tb,n\t"  @ s @ "\n"
#endif
||  ins td ws (Mjump s)  = "\tb,n\t" @ s @ "\n"

||  ins td ws Mreturn = concmap (ins td ws)
	[Mmove (reg Ret) dtmp;
	 Masm "\tbv\t(^)" [dtmp];
	 Mmove popV (reg Ret)
	]

||  ins td ws (m as Mcompare _ _) = fail ("!! Bad "@mprint[m])
||  ins td ws (m as Mcomparesf _ _) = fail ("!! Bad "@mprint[m])
||  ins td ws (m as Mcomparedf _ _) = fail ("!! Bad "@mprint[m])
||  ins td ws (m as Mjcond _ _) = fail ("!! Bad "@mprint[m])
||  ins td ws (m as Mboolcc _ _) = fail ("!! Bad "@mprint[m])

||  ins td ws (m as Mcalltag t r) = saveret@"\tldw\t"@otag t@"("@regname r@"),"@adtmp@"\n\tble\t0(%sr4,"@adtmp@")\n\tcopy\t%r31,%r2\n"
||  ins td ws (m as Mjumptag t r) =         "\tldw\t"@otag t@"("@regname r@"),"@adtmp@"\n\tbv,n\t0("@adtmp@")\n"

||  ins td ws (Mcase a l h mx ls x) =
    concmap (ins td ws) (
 	let t = 'L'.itos x
	and s = h-l+1 in
 	[Mmove a dtmp] @
	(if l = 0 then [] else [Mop3 add (const (-l)) dtmp dtmp]) @
	[Masm "\taddi,uv\t^,^,%r0" [const (-s); dtmp];				-- addi may fail for VERY large intervals
	 Masm "\tblr,n\t^,%r0" [dtmp];
	 Masm "\tb,n\t^" [glob t] ] @
--	concmap (\l.[Masm "\tb\t^" [glob l]; Masm "\tnop" []]) ls @
	concmap (\l.[Mjumpf l; Masm "\tnop" [] ]) ls @
	[Mlabel t]
    )

 || ins td ws (Mnoop) = "\t.nop\n"
 || ins td ws (Mdata) = "\t.SPACE\t$PRIVATE$\n\t.SUBSPA\t$DATA$\n"
 || ins td ws (Mtext) = "\t.SPACE\t$TEXT$\n\t.SUBSPA\t$CODE$\n"
 || ins td ws (Mword (glob  a)) = let a' = drop_ a in doimport ws a' @ "\t.word\t" @ dropxx a' @"\n"
 || ins td ws (Mword (idlit a)) = let a' = drop_ a in doimport ws a' @ "\t.word\t" @ a' @"\n"
 || ins td ws (Mword (const i)) = "\t.word\t" @ itos i @"\n"
 || ins td ws (Mdfloat s) = "\t.double\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins td ws (Msfloat s) = "\t.float\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins td ws (Mstring s) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t4\n")
 				
 || ins td ws (Mexport a) = "\t.export\t" @ drop_ a @ (if td then ",code" else ",data") @ "\n"
 || ins td ws (Mcom s) = " # " @ s @ "\n"
 || ins td ws (Mpragma s) = ""
 || ins td ws (Mlabel l) = drop_ l @ "\n"
 || ins td ws (Masm s l) = aspr s l @ "\n"
 || ins td ws (Malign) = "\t.align\t8\n"
 || ins td ws (Mfunbegin _ n) = ""
--	"\t.import\tunw" @(if n <  MAXUNW then itos n else "N")@",code\n"@
--	"\t.import\tvunw"@(if n < MAXVUNW then itos n else "N")@",code\n"
 || ins td ws (Mfunend ) = ""
 || ins td ws m = fail ("ins: strange Mcode " @ mprint [m])

and dropxx ('*'.'C'.'*'.'_'.s) = 'P'.'%'.s
||  dropxx ('*'.'D'.'*'.'_'.s) = s
||  dropxx s = s
and incr a = Mop3 add (const 1) a a
and getcc ltstack = ">>="
||  getcc ltheap  = ">>="
||  getcc gtstack = "<<="
||  getcc geheap  = "<<"
||  getcc cc = ccname (negmop cc)
and ccjump instr cc a1 a2 s = 
	[Masm ("\t"@instr@","@getcc cc@"\t^,^,0") [a1; a2]; Masm "\tbl,n\t^,0" [glob s]]
and cmptr [] = []
||  cmptr (Mcompare a1 (a2 as (const k)).Mjcond cc s.ms) = cmptr (Mcompare a2 a1.Mjcond (invmop cc) s.ms)
||  cmptr (Mcompare (a1 as (const k)) a2.Mjcond cc s.ms) =
	ccjump "comiclr" cc a1 a2 s @ cmptr ms
||  cmptr (Mcompare a1 a2.Mjcond cc s.ms) =
	ccjump "comclr" cc a1 a2 s @ cmptr ms
--        Masm ("\tcomb,"@ccname cc@"\t^,^,^") [a1; a2; glob s].Masm "\tnop" [].cmptr ms

||  cmptr (Mcompare a1 (a2 as (const k)).Mboolcc cc a3.ms) = cmptr (Mcompare a2 a1.Mboolcc (invmop cc) a3.ms)
||  cmptr (Mcompare (a1 as (const k)) a2.Mboolcc cc a3.ms) =
        Masm ("\tcomiclr,"@ccname (negmop cc)@"\t^,^,^") [a1; a2; a3].incr a3.cmptr ms
||  cmptr (Mcompare a1 a2.Mboolcc cc a3.ms) =
        Masm ("\tcomclr,"@ccname (negmop cc)@"\t^,^,^") [a1; a2; a3].incr a3.cmptr ms

||  cmptr (Mcomparedf a1 a2.Mjcond cc s.ms) = 
	fpjump cc a1 a2 s @ cmptr ms
||  cmptr (Mcomparesf a1 a2.Mjcond cc s.ms) = 
	fpjump cc a1 a2 s @ cmptr ms

||  cmptr (Mcomparedf a1 a2.Mboolcc cc a3.ms) = 
	fpboolcc cc a1 a2 a3 @ cmptr ms
||  cmptr (Mcomparesf a1 a2.Mboolcc cc a3.ms) = 
	fpboolcc cc a1 a2 a3 @ cmptr ms

||  cmptr (m.ms) = m.cmptr ms

and fpjump cc a1 a2 s =
        Masm ("\tfcmp,"@ccname cc@"\t^,^") [a1; a2] .
	Masm "\tftest" [] .
	Masm "\tadd,tr\t0,0,0" [] .
	Masm "\tbl,n\t^,0" [glob s] .
	[]

and fpboolcc cc a1 a2 a3 =
	Masm "\tldi\t1,^" [a3] .
        Masm ("\tfcmp,"@ccname cc@"\t^,^") [a1; a2] .
	Masm "\tftest" [] .
	Masm "\tldi\t0,^" [a3] .
	[]

and tags = ["FUN"; "AP"; "VAPG"; "INT"; "CHAR"; "PAIR"; "PAIR0"; "PAIR1"; "PAIR2"; "PAIR3"; "PAIR4"; "TAG"; "TAG0"; "VEK"; "ZAP"; "SFLOAT"; "DFLOAT"; "VAP"; "HOLE"; "STRINGN"; "DVEK"; "CANON"; "INDIR"; "tp"; "etp"; "ehp"]

and hpux9 =
    case uname in
	(_,_,(_._._.'9'._),_,_) : true
    ||  _ : false
    end

and prol = 
	(if hpux9 then "" else "\t.LEVEL\t1.1\n") @
        "\t.SPACE\t$PRIVATE$\n\t.SUBSPA\t$DATA$,QUAD=1,ALIGN=8,ACCESS=31\n\t.SUBSPA\t$BSS$,QUAD=1,ALIGN=8,ACCESS=31,ZERO,SORT=82\n\t.SPACE\t$TEXT$\n\t.SUBSPA\t$LIT$,QUAD=0,ALIGN=8,ACCESS=44\n\t.SUBSPA\t$CODE$,QUAD=0,ALIGN=8,ACCESS=44,CODE_ONLY\n\t.IMPORT\t$global$,DATA\n\t.IMPORT\t$$dyncall,MILLICODE\n\n" @
	concmap (\s."\t.import\t"@s@",data\n") tags @ "\n"

and emit td ws [] = ""
||  emit td ws (Mpragma ('C'.'+'.s).ms) = 
	"\t.import\t"@s@",code\n" @ emit td (s.ws) ms
||  emit td ws (Mpragma ('D'.'+'.s).ms) = 
	"\t.import\t"@s@",data\n" @ emit td (s.ws) ms
||  emit _  ws (Mtext.ms) = ins true  ws Mtext @ emit true  ws ms
||  emit _  ws (Mdata.ms) = ins false ws Mdata @ emit false ws ms
||  emit td ws (m.ms) =
	ins td ws m @ emit td ws ms

and assemblercode m =
    let mt = cmptr (mtrans2 (mtrans1 m)) in
    (if PrMtrans then
	mprint mt
    else
	"") @
    prol @
    emit true tags ([Mdata; Malign; Mtext; Malign] @ mt)

end

