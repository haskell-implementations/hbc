module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/flags.t"
#include "../misc/qstring.t"
#include "mtrans1.t"
#include "mtrans2.t"
#include "regno.h"
#include "reg.h"

-- Registers:
-- r0-r7	%l0-%l7
-- r8-r15	%i0-%i7	i6=fp
-- r16-r23	%o0-%o7	o6=sp o7=retaddr
-- r24-r31	%g0-%g7
-- r0-r10 Mcode, with r2 as tagreg etc
-- r11 indreg
-- r12 tmp (during mul)
-- r13 dtmp
-- r14 fp		FIXED
-- r15 old ra		FIXED
-- r16-r17 mul/div/rem	FIXED
-- r18-r21 dtmp
-- r22 sp		FIXED
-- r23 ra		FIXED
-- r24 0		FIXED
-- r25 machdep tmp	used by mul/div/rem
-- r26 -		used by mul/div/rem
-- r27 Canon		used by mul/div/rem
-- r28 Vp
-- r29 Sp
-- r30 Hp
-- r31 ehp
-- r32-r64		floating point regs
export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, farcase,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, ccname, retfixup;
rec
    Aregs = [1;2;3;4;5;6;7;8;9] @ fail "Out of A-regs"  -- used to have 10,11
and Dregs = [1;2;3;4;5;6;7;8;9] @ fail "Out of D-regs"  -- used to have 10,11
and Fregs = map (\x.x*2+32) [0..11] @ fail "Out of F-regs"
and sfloatsize = 1
and dfloatsize = 2
and usecase max low high cnt = cnt>=4 & (high-low)<cnt*5
and use3op = true
and argcreg = reg Argcreg
and tagreg  = reg Tagreg
and bigeqreg= reg Bigeqreg
and indreg  = reg Indreg
and cputype = "SPARC"
and farcase = ~ FarJump
and retfixup = if PIC then [Masm PICsetup []] else []

and GOTreg = 10

and drop_ ('_'.s) & (Solaris) = s
||  drop_ s = s
and regname n = 
	if n < 8 then
		"%l"@itos n
	else if n < 16 then
		"%i"@itos (n-8)
	else if n < 24 then
		"%o"@itos (n-16)
	else if n < 32 then
		"%g"@itos (n-24)
	else if n < 64 then
		"%f"@itos (n-32)
	else
		fail ("Bad reg "@itos n)
and regzero = reg 24
and amode (const 0) = "%g0"
||  amode x = amode0 x
and amode0 (reg i)      = regname i
 || amode0 (regind r i) = "[" @ regname r @ (if i >= 0 then "+" else "") @ itos (i*4) @ "]" 
 || amode0 (regrel _ _) = fail "amode regrel\n"

 || amode0 (glob i)  = drop_ i
 || amode0 (idlit s) = drop_ s
 || amode0 (const n) = itos n
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode0 am @ aspr cs ams
||  aspr ('~'.cs) (regind r i.ams) = amode0 (regind r (i+1)) @ aspr cs ams
||  aspr ('~'.cs) (am.ams) = amode0 am @ aspr cs ams
||  aspr (c.cs)   ams      = c.aspr cs ams

and isreg (reg _) = true
 || isreg _       = false
--and otag oeval   = "0"
-- || otag ounwind = "4"
-- || otag ojfun   = "8"
-- || otag ogettag = "12"
-- || otag (onumtag i) = itos (4*i)
and ccname eq = "be"
 || ccname ne = "bne"
 || ccname lt = "bl"
 || ccname gt = "bg"
 || ccname le = "ble"
 || ccname ge = "bge"
 || ccname ltstack = "blu"
 || ccname ltheap = "blu"
 || ccname gtstack = "bgu"
 || ccname geheap = "bgeu"
 || ccname dfeq = "fbe"
 || ccname dfne = "fbne"
 || ccname dflt = "fbul"
 || ccname dfgt = "fbug"
 || ccname dfle = "fbule"
 || ccname dfge = "fbuge"
 || ccname sfeq = "fbe"
 || ccname sfne = "fbne"
 || ccname sflt = "fbul"
 || ccname sfgt = "fbug"
 || ccname sfle = "fbule"
 || ccname sfge = "fbuge"
 || ccname x   = fail ("ccname "@mprint [Mjcond x ""])
and opname add = "add"
||  opname sub = "sub"
||  opname mul = "smul"
||  opname div = "sdiv"
||  opname mod = "_rem"
||  opname btand = "and"
||  opname btor = "or"
||  opname btxor = "xor"
||  opname btlsh = "sll"
||  opname btrsh = "srl"
||  opname btrsha = "sra"
||  opname dfadd = "faddd"
||  opname dfsub = "fsubd"
||  opname dfmul = "fmuld"
||  opname dfdiv = "fdivd"
||  opname dfneg = "fnegd"
||  opname dftoi = "fdtoi"
||  opname itodf = "fitod"
||  opname sfadd = "fadds"
||  opname sfsub = "fsubs"
||  opname sfmul = "fmuls"
||  opname sfdiv = "fdivs"
||  opname sfneg = "fnegs"
||  opname sftoi = "fstoi"
||  opname itosf = "fitos"
||  opname sftodf = "fstod"
||  opname dftosf = "fdtos"
||  opname x = fail ("opname "@mprint [Mop2 x (glob "X") (glob "Y")])
and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and oneop s1 s2 = '\t'.s1@'\t'.s2@"\n"
--and saveret = ins /*extra space allocated by previous transformation */(Mmove (reg Ret) (regind Vpr 0))
and dtmp = dtmp4
and gtmp = dtmp4
and (reg gtmpr) = gtmp
and adtmp = amode dtmp
and agtmp = amode gtmp
and adtmp1 = amode dtmp1
and aret = amode (reg Ret)
and mtmp0 = reg 16
and mtmp1 = reg 17
and mtmpr = reg 12
and mtmpc = reg 11		-- for saving Canon while mul/div/rem, a little risky if all registers are in use.
and nop = "\tnop\n"
and ldglob a1 (a2 as glob x) = 
    if PIC then
	ldaddr x gtmp @
	"\tld\t["@agtmp@"],"@amode a1@"\n"
    else
	"\tsethi\t%hi("@amode a2@"),"@agtmp@"\n\tld\t["@agtmp@"+%lo("@amode a2@")],"@amode a1@"\n"
and stglob a1 (a2 as glob x) = 
    if PIC then 
	ldaddr x gtmp @
	"\tst\t"@amode a1@",["@agtmp@"]\n"
    else
	"\tsethi\t%hi("@amode a2@"),"@agtmp@"\n\tst\t"@amode a1@",["@agtmp@"+%lo("@amode a2@")]\n"
and offchk (a as regind r i) = if i*4+4 <= (CLIM-1) then ("", a) else (amode2 "set" (const (i*4)) gtmp@amode3 "add" gtmp (reg r) gtmp, regind gtmpr 0)
and st a1 a2 = let (s, a2') = offchk a2 in s @ amode2 "st" a1  a2'
and ld a1 a2 = let (s, a1') = offchk a1 in s @ amode2 "ld" a1' a2
and std (a1 as reg r) a2 = let (s, (a2' as regind ri k)) = offchk a2 in s @ amode2 "st" a1 a2' @ amode2 "st" (reg (r+1)) (regind ri (k+1))
and ldd a1 (a2 as reg r) = let (s, (a1' as regind ri k)) = offchk a1 in s @ amode2 "ld" a1' a2 @ amode2 "ld" (regind ri (k+1)) (reg (r+1))
and ldaddr x r = 
    if PIC then
	amode2 "set" (idlit x) r @
	"\t\ld\t["@amode r@"+"@GOTregstr@"],"@amode r@"\n"
    else
	amode2 "set" (idlit x) r
#undef r2
and fltreg (reg n) = n >= 32
||  fltreg _ = false
and
    ins (Mmove a1 a2) & (fltreg a1 | fltreg a2) = ins (Mmovesf a1 a2)
||  ins (Mmove (r1 as reg _) (r2 as reg _)) = amode2 "mov" r1 r2
||  ins (Mmove (a1 as reg _) (a2 as regind _ _)) = st a1 a2
||  ins (Mmove (a1 as regind _ _) (a2 as reg _)) = ld a1 a2
||  ins (Mmove (a1 as const n) (a2 as reg _)) & (n < CLIM & n >=-CLIM) = amode2 "mov" a1 a2
||  ins (Mmove (a1 as const _) (a2 as reg _)) = amode2 "set" a1 a2
||  ins (Mmove (idlit x) (a2 as reg _)) = ldaddr x a2
||  ins (Mmove (a1 as reg _) (a2 as glob _)) = stglob a1 a2
||  ins (Mmove (a1 as glob _) (a2 as reg _)) = ldglob a2 a1

||  ins (Mmovedf (r1 as reg _) (r2 as reg _)) = amode2 "fmovd" r1 r2
||  ins (Mmovedf (a1 as reg _) (a2 as regind _ _)) = std a1 a2
||  ins (Mmovedf (a1 as regind _ _) (a2 as reg _)) = ldd a1 a2
||  ins (Mmovedf (a1 as reg _) (glob x)) = ldaddr x gtmp @ std a1 (regind gtmpr 0)
||  ins (Mmovedf (glob x) (a2 as reg _)) = ldaddr x gtmp @ ldd (regind gtmpr 0) a2
||  ins (Mmovedf a1 a2) = fail (amode2 "movedf" a1 a2)

||  ins (Mmovesf (r1 as reg _) (r2 as reg _)) = amode2 "fmovs" r1 r2
||  ins (Mmovesf (a1 as reg _) (a2 as regind _ _)) = st a1 a2
||  ins (Mmovesf (a1 as regind _ _) (a2 as reg _)) = ld a1 a2
||  ins (Mmovesf (a1 as reg _) (glob x)) = ldaddr x gtmp @ st a1 (regind gtmpr 0)
||  ins (Mmovesf (glob x) (a2 as reg _)) = ldaddr x gtmp @ ld (regind gtmpr 0) a2
||  ins (Mmovesf a1 a2) = fail (amode2 "movesf" a1 a2)

||  ins (Madda (a1 as reg _) (a2 as reg _)) = 
	amode2 "mov" a1 dtmp @ amode3 "sll" dtmp (const 2) dtmp @ amode3 "add" dtmp a2 a2

||  ins (Mcompare a1 a2) = amode2 "cmp" a1 a2
||  ins (Mcomparedf a1 a2) = amode2 "fcmped" a1 a2 @ nop
||  ins (Mcomparesf a1 a2) = amode2 "fcmpes" a1 a2 @ nop
||  ins (Mop2 neg a1 a2) = ins (Mop3 sub a1 regzero a2)
||  ins (Mop2 btcompl a1 a2) = ins (Mop3 btxor a1 (const (-1)) a2)
-- BRAIN DEAD SPARC! fnegd seems to turns into fnegs which only changes the MSW reg, thus the fmovs
||  ins (Mop2 dfneg (a1 as reg r1) (a2 as reg r2)) = amode2 "fnegs" a1 a2 @ (if r1 ~= r2 then amode2 "fmovs" (reg (r1+1)) (reg (r2+1)) else "")
||  ins (Mop2 op a1 a2) & (mem op [dftoi; itodf; sftoi; itosf; sfneg; sftodf; dftosf]) = amode2 (opname op) a1 a2
||  ins (Mop2 op a1 a2) = ins (Mop3 op a1 a2 a2)
||  ins (Mop3 op a1 a2 a3) & (op = div | op = mod | op = mul & ~Sparc8) =
    if Sparc8 then	-- must be mod or div
        ins (Masm "\tsra\t^,31,^" [a2; mtmp0]) @
        ins (Masm "\twr\t%g0,^,%y\n\tnop\n\tnop\n\tnop" [mtmp0]) @
        amode3 (opname div) a2 a1 a3 @
        (if op = div then "" else ins (Masm "\tsmul\t^,^,^" [a3;a1;a3]) @ ins (Masm "\tsub\t^,^,^" [a2;a3;a3]))
    else
	ins (Mmove a2 mtmp0) @
	ins (Mmove a1 mtmp1) @
	ins (Mmove (reg Ret) mtmpr) @
	"\tcall\t."@tl (opname op)@",2\n"@
        ins (Mmove (reg Canon) mtmpc) @		-- save Canon in delay slot
        ins (Mmove mtmpc (reg Canon)) @		-- and restore it first thing
	ins (Mmove mtmpr (reg Ret)) @
	ins (Mmove mtmp0 a3)
||  ins (Mop3 op a1 a2 a3) = amode3 (opname op) a2 a1 a3

||  ins (m as Mboolcc cc a) =
	"\t"@ccname cc@"\t1f\n"@
	"\tmov\t1,"@amode a@"\n"@
	"\tmov\t0,"@amode a@"\n"@
	"1:\n"

 || ins (Mcase a l h mx ls x) =
 	let t = 'L'.itos x in
        let cb = ins (Mcompare dtmp (const (h-l))) @
	         "\tbgu\t"@t@"_2\n"@nop
        in
 	ins (Mmove a dtmp) @
	(if l = 0 then "" else ins (Mop2 sub (const l) dtmp))@
        (if l = 0 & h = mx-1 then "" else cb) @
	"\tsll\t"@adtmp@",2,"@adtmp@"\n"@
	"\tset\t"@t@"_1,"@adtmp1@"\n"@
	"\tld\t["@adtmp@"+"@adtmp1@"],"@adtmp@"\n"@
	"\tjmp\t"@adtmp@"\n"@nop@
	t@"_1:\n"@
	concmap (\l."\t.word\t"@l@"\n") ls@
	t@"_2:\n"

 || ins (Mnoop) = nop
 || ins (Mdata) = if Solaris then "\t.section\t\".data\"\n" else "\t.data\n"
 || ins (Mtext) = if Solaris then "\t.section\t\".text\"\n" else "\t.text\n"
 || ins (Mword (glob  a)) = "\t.word\t" @ drop_ a @ "\n"
 || ins (Mword (idlit a)) = "\t.word\t" @ drop_ a @ "\n"
 || ins (Mword (const i)) = "\t.word\t" @ itos i @ "\n"
 || ins (Mdfloat s) = "\t.double\t0r"@fmtf ".17e" (stof s)@"\n"
 || ins (Msfloat s) = "\t.single\t0r"@fmtf ".8e" (stof s)@"\n"
#if 0
 || ins (Mstring s) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t4\n")
 				
#else
 || ins (Mstring s) = "\t.asciz\t\"" @ qstring s @ "\"\n\t.align\t4\n"
#endif
 || ins (Mexport a) = "\t.global\t" @ drop_ a @ "\n"
 || ins (Mcom s) = " ! " @ s @ "\n"
 || ins (Mpragma s) = ""
 || ins (Mlabel l) = drop_ l @ ":\n"
 || ins (Masm s l) = aspr s l @ "\n"
 || ins (Malign) = "\t.align\t8\n"
 || ins (Mfunbegin n _) = if PIC then PICsetup else ""
 || ins (Mfunend ) = ""
 || ins m = fail ("ins: strange Mcode " @ mprint [m])
and prol = "lml_compiled:\n" @
	if Solaris then "\t.section\t\".text\",#alloc,#execinstr\n\t.section\t\".data\",#alloc,#write\n"
	else ""

and PICsetup = 
#if 0
     let lbl1 = "LL1_"@n
     and lbl2 = "LL2_"@n in
     lbl1@":\n"@
     "\tcall\t"@lbl2@"\n"@
     "\tnop\n"@
     lbl2@":\n"@
     "\tsethi\t%hi(_GLOBAL_OFFSET_TABLE_-("@lbl1@"-.)),"@GOTregstr@"\n"@
     "\tor\t"@GOTregstr@",%lo(_GLOBAL_OFFSET_TABLE_-("@lbl1@"-.)),"@GOTregstr@"\n"@
     "\tadd\t"@GOTregstr@",%o7,"@GOTregstr@"\n"
#else
     "1:\n"@
     "\tmov\t%o7,%g1\n"@
     "\tcall\t2f\n"@
     "\tsethi\t%hi(_GLOBAL_OFFSET_TABLE_-(1b-.)-4),"@GOTregstr@"\n"@
     "2:\n"@
     "\tor\t"@GOTregstr@",%lo(_GLOBAL_OFFSET_TABLE_-(1b-.)-4),"@GOTregstr@"\n"@
     "\tadd\t"@GOTregstr@",%o7,"@GOTregstr@"\n"@
     "\tmov\t%g1,%o7\n"
#endif

and GOTregstr = regname GOTreg

and assemblercode m =
--	let mt1 = mtrans1 m
--	in let mt = mtrans2 mt1 in
--	mprint mt1 @ mprint mt @
    let mt = mtrans2 (mtrans1 m) in
    (if PrMtrans then
	mprint mt
    else
	"") @
    prol @
    concmap ins mt
end
