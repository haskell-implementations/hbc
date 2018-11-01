module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/flags.t"
#include "mtrans1.t"
#include "mtrans2.t"
#include "tmp.h"

-- To do:
-- negation via negu
-- avoid checking against upper limit in case if complete
-- use gp more (what constants should go into gp area?)

-- Registers:

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, farcase,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, retfixup;
rec
    Aregs = count 1 13 @ fail "Out of A-regs"
and Dregs = count 1 13 @ fail "Out of D-regs"
and Fregs = map (\x.x*2+32) [0..11] @ fail "Out of F-regs"
and sfloatsize = 1
and dfloatsize = 2
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*5
and farcase = true
and retfixup = []
and use3op = true
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg 31		-- (real r7)
and regname n = if n < 32 then "$"@itos ((n+8)%32) else "$f"@itos (n-32)
and cputype = "MIPS"

and double2ints d =
	let bs = doubleToBytes d "" in
	let! (i1, bs') = bytesToInt bs in
	let! (i2, _) = bytesToInt bs' in
	[i1;i2]
and amode (reg i)      = regname i
 || amode (regind r i) = itos (i*4) @ "(" @ regname r @ ")"
 || amode (regrel _ _) = fail "amode regrel\n"

 || amode (glob i)  = drop_ i
 || amode (idlit s) = drop_ s
 || amode (const n) = itos n
 || amode (fconst f) = fmtf ".17e" f
and drop_ ('_'.s) = s
||  drop_ s = s
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am @ (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and isreg (reg _) = true
 || isreg _       = false
and otag oeval   = "0"
 || otag ounwind = "4"
 || otag ojfun   = "8"
 || otag ogettag = "12"
 || otag (onumtag i) = itos (4*i)
and ccname eq = "eq"
 || ccname ne = "ne"
 || ccname lt = "lt"
 || ccname gt = "gt"
 || ccname le = "le"
 || ccname ge = "ge"
 || ccname ltstack = "ltu"
 || ccname ltheap = "ltu"
 || ccname gtstack = "gtu"
 || ccname geheap = "geu"
 || ccname dfeq = "teq"
 || ccname dfne = "feq"
 || ccname dflt = "tlt"
 || ccname dfgt = "fle"
 || ccname dfle = "tle"
 || ccname dfge = "flt"
 || ccname sfeq = "teq"
 || ccname sfne = "feq"
 || ccname sflt = "tlt"
 || ccname sfgt = "fle"
 || ccname sfle = "tle"
 || ccname sfge = "flt"
and opname add = "addu"
||  opname sub = "subu"
||  opname mul = "mul"
||  opname div = "div"
||  opname mod = "rem"
||  opname neg = "negu"
||  opname btand = "and"
||  opname btor = "or"
||  opname btxor = "xor"
||  opname btcompl = "not"
||  opname btlsh = "sll"
||  opname btrsh = "srl"
||  opname btrsha = "sra"
||  opname dfadd = "add.d"
||  opname dfsub = "sub.d"
||  opname dfmul = "mul.d"
||  opname dfdiv = "div.d"
||  opname dfneg = "neg.d"
||  opname dftoi = fail "dftoi"
||  opname itodf = fail "itodf"
||  opname sfadd = "add.s"
||  opname sfsub = "sub.s"
||  opname sfmul = "mul.s"
||  opname sfdiv = "div.s"
||  opname sfneg = "neg.s"
||  opname sftoi = fail "sftoi"
||  opname itosf = fail "itosf"
||  opname sftodf = "cvt.d.s"
||  opname dftosf = "cvt.s.d"

and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and oneop s1 s2 = '\t'.s1@'\t'.s2@"\n"
and saveret = ins /*extra space allocated by previous transformation */(Mmove (reg Ret) (regind Vpr 0))
and dtmp = dtmp4
and adtmp = amode dtmp
and
    ins (Mmove (r1 as reg _) (r2 as reg _)) = 
	amode2 "move" r2 r1
||  ins (Mmove (a1 as reg _) (a2 as regind _ _)) =
	amode2 "sw" a1 a2
||  ins (Mmove (a1 as regind _ _) (a2 as reg _)) =
	amode2 "lw" a2 a1
||  ins (Mmove (a1 as const _) (a2 as reg _)) =
	amode2 "li" a2 a1
||  ins (Mmove (a1 as idlit _) (a2 as reg _)) =
	amode2 "la" a2 a1
||  ins (Mmove (a1 as reg _) (a2 as glob _)) =
	amode2 "sw" a1 a2
||  ins (Mmove (a1 as glob _) (a2 as reg _)) =
	amode2 "lw" a2 a1
||  ins (Mmovedf (r1 as reg _) (r2 as reg _)) = 
	amode2 "mov.d" r2 r1
||  ins (Mmovedf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "s.d" a1 a2
||  ins (Mmovedf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "l.d" a2 a1
||  ins (Mmovedf (a1 as fconst _) (a2 as reg _)) =
	amode2 "li.d" a2 a1
||  ins (Mmovedf (a1 as reg _) (a2 as glob _)) =
	amode2 "s.d" a1 a2
||  ins (Mmovedf (a1 as glob _) (a2 as reg _)) =
	amode2 "l.d" a2 a1
||  ins (Mmovesf (r1 as reg _) (r2 as reg _)) = 
	amode2 "mov.s" r2 r1
||  ins (Mmovesf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "s.s" a1 a2
||  ins (Mmovesf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "l.s" a2 a1
||  ins (Mmovesf (a1 as fconst _) (a2 as reg _)) =
	amode2 "li.s" a2 a1
||  ins (Mmovesf (a1 as reg _) (a2 as glob _)) =
	amode2 "s.s" a1 a2
||  ins (Mmovesf (a1 as glob _) (a2 as reg _)) =
	amode2 "l.s" a2 a1
||  ins (Madda (a1 as reg _) (a2 as reg _)) = amode2 "move" dtmp a1 @ amode3 "mul" dtmp dtmp (const 4) @ amode3 "add" a2 dtmp a2
||  ins (Mop2 itodf a1 a2) = amode2 "mtc1" a1 a2 @ amode2 "cvt.d.w" a2 a2
||  ins (Mop2 dftoi a1 a2) = amode3 "trunc.w.d" Ftmp4 a1 a2 @ amode2 "mfc1" a2 Ftmp4
||  ins (Mop2 itosf a1 a2) = amode2 "mtc1" a1 a2 @ amode2 "cvt.s.w" a2 a2
||  ins (Mop2 sftoi a1 a2) = amode3 "trunc.w.s" Ftmp4 a1 a2 @ amode2 "mfc1" a2 Ftmp4
||  ins (Mop2 op a1 a2) & (mem op [neg; btcompl; dfneg; sfneg; sftodf; dftosf]) =
	amode2 (opname op) a2 a1
||  ins (Mop2 op (a1 as const _) (a2 as reg _)) =
	amode2 (opname op) a2 a1
||  ins (Mop2 op a1 a2) =
	amode3 (opname op) a2 a2 a1
||  ins (Mop3 op a1 a2 a3) =
	amode3 (opname op) a3 a2 a1

||  ins (Mcall s) = saveret @ oneop "jal" s
||  ins (Mjumpf s) = 
    if FarJump then
	ins (Mmove (idlit s) dtmp) @
	amode1 "j" dtmp
    else
	oneop "j" s
||  ins (Mjump s) = oneop "b" s

||  ins Mreturn = concmap ins 
	[Mmove (reg Ret) dtmp;
	 Mmove (regind Vpr 0) (reg Ret);
	 Mop2 add (const 4) (reg Vpr)] @ amode1 "j" dtmp

||  ins (m as Mcompare _ _) = "!! Bad "@mprint[m]
||  ins (m as Mcomparesf _ _) = "!! Bad "@mprint[m]
||  ins (m as Mcomparedf _ _) = "!! Bad "@mprint[m]
||  ins (m as Mjcond _ _) = "!! Bad "@mprint[m]
||  ins (m as Mboolcc _ _) = "!! Bad "@mprint[m]

||  ins (m as Mcalltag t r) = saveret@"\tlw\t"@adtmp@","@otag t@"("@regname r@")\n\tjal\t"@adtmp@"\n"
||  ins (m as Mjumptag t r) =         "\tlw\t"@adtmp@","@otag t@"("@regname r@")\n\tj\t"@adtmp@"\n"

 || ins (Mcase a l h mx ls x) =
 	let t = 'L'.itos x in
	let cb = "\tbgtu\t"@adtmp@","@itos(h-l)@","@t@"_2\n" in
	"\t.data\n"@t@"_1:\n"@
	concmap (\l."\t.word\t"@l@"\n") ls@
	"\t.text\n"@
 	ins (Mmove a dtmp) @
	(if l = 0 then "" else ins (Mop2 sub (const l) dtmp))@
        (if l = 0 & h = mx-1 then "" else cb) @
	"\tsll\t"@adtmp@","@adtmp@",2\n"@
	"\tlw\t"@adtmp@","@t@"_1("@adtmp@")\n"@
	"\tj\t"@adtmp@"\n"@
	t@"_2:\n"

 || ins (Mnoop) = ""
 || ins (Mdata) = "\t.data\n"
 || ins (Mtext) = "\t.text\n"
 || ins (Mword (glob  a)) = "\t.word\t" @ drop_ a @ " : 1\n"
 || ins (Mword (idlit a)) = "\t.word\t" @ drop_ a @ " : 1\n"
 || ins (Mword (const i)) = "\t.word\t" @ itos i @ " : 1\n"
 --|| ins (Mdfloat s) = "\t.double\t"@(if mem '.' s then s else s@".0")@" : 1\n"
 || ins (Mdfloat s) = 
	let is = double2ints (stof s) in
	"\t# .double\t"@(if mem '.' s then s else s@".0")@" : 1\n" @
	concmap (\i."\t.word\t" @ itos i @ " : 1\n") is
 || ins (Msfloat s) = "\t.float\t"@(if mem '.' s then s else s@".0")@" : 1\n"
 || ins (Mstring s) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t2\n")
 				
 || ins (Mexport a) = "\t.globl\t" @ drop_ a @ "\n"
 || ins (Mcom s) = " # " @ s @ "\n"
 || ins (Mpragma s) = ""
 || ins (Mlabel l) = drop_ l @ ":\n"
 || ins (Masm s l) = aspr s l @ "\n"
 || ins (Malign) = "\t.align\t3\n"
 || ins (Mfunbegin _ _) = ""
 || ins (Mfunend ) = ""
 || ins m = fail ("ins: strange Mcode " @ mprint [m])
and cmptr [] = []
||  cmptr (Mcompare a1 a2.Mjcond cc1 s1.Mjcond cc2 s2.ms) =
	Masm ("\tb"@ccname cc1@"\t^,^,^") [a1; a2; (glob s1)].Masm ("\tb"@ccname cc2@"\t^,^,^") [a1; a2; (glob s2)].cmptr ms
||  cmptr (Mcompare a1 a2.Mjcond cc s.ms) =
	Masm ("\tb"@ccname cc@"\t^,^,^") [a1; a2; (glob s)].cmptr ms
||  cmptr (Mcompare a1 a2.Mboolcc cc a3.ms) =
	Masm ("\ts"@ccname cc@"\t^,^,^") [a3; a1; a2].cmptr ms
||  cmptr (Mcomparedf a1 a2.Mjcond cc s.ms) =
	let (j.cmp) = ccname cc in
	Masm ("\tc."@cmp@".d\t^,^\n\tbc1"@j."\t^") [a1; a2; (glob s)].cmptr ms
||  cmptr (Mcomparedf a1 a2.Mboolcc cc a3.ms) = 
	cmptr (Mmove (const 1) a3 . Mcomparedf a1 a2 . Mjcond cc "1f" . Mmove (const 0) a3 . Mlabel "1" . ms)
||  cmptr (Mcomparesf a1 a2.Mjcond cc s.ms) =
	let (j.cmp) = ccname cc in
	Masm ("\tc."@cmp@".s\t^,^\n\tbc1"@j."\t^") [a1; a2; (glob s)].cmptr ms
||  cmptr (Mcomparesf a1 a2.Mboolcc cc a3.ms) = 
	cmptr (Mmove (const 1) a3 . Mcomparesf a1 a2 . Mjcond cc "1f" . Mmove (const 0) a3 . Mlabel "1" . ms)
||  cmptr (m.ms) = m.cmptr ms
and prol = concmap (\s."\t.extern\t"@s@" 4\n") [
"SFLOAT";"INT";"CHAR";"TAG";"TAG0";"VEK";"HOLE";"AP";"CAP";"VAP";"PAIR";"PAIR0";"PAIR1";"PAIR2";"PAIR3";"PAIR4";"DFLOAT";"BIGNUM";"DVEK";"CANON";"CANONDATA";"STRINGN"]

and assemblercode m =
--	let mt1 = mtrans1 m
--	in let mt = mtrans2 mt1 in
--	mprint mt1 @ mprint mt @
    let mt = cmptr (mtrans2 (mtrans1 m)) in
    (if PrMtrans then
	mprint mt
    else
	"") @
    prol @
    concmap ins mt

end
