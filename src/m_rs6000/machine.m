module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../mcode/mutil1.t"
#include "../misc/flags.t"
#include "mtrans1.t"
#include "mtrans2.t"
#include "misc.t"
#include "tmp.h"

-- r0(8)		M-code magic
-- r1(9)-r13(21)	M-code alloc
-- r14(22)		Sp
-- r15(23)		Hp
-- r16(24)		Ehp
-- r17(25)		dtmp1
-- r18(26)		dtmp2
-- r19(27)		dtmp3
-- r20(28)		dtmp4
-- r21(29)		Argcr, Tagr, Bigeqr
-- r22(30)		Indr
-- r23(31)		C fp
-- r24(0)		unused, 0 when used as index reg
-- r25(1)		Vp, C sp
-- r26(2)		C toc pointer
-- r27(3)		C return value, C arg 1
-- r28(4)		C arg 2
-- r29(5)		-
-- r30(6)		-
-- r31(7)		-

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, farcase,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, retfixup;
rec
    Aregs = count 1 13 @ fail "Out of A-regs"
and Dregs = count 1 13 @ fail "Out of D-regs"
and Fregs = count 34 39 @ fail "Out of F-regs"
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*5
and farcase = false
and use3op = true
and sfloatsize = 1
and dfloatsize = 2
and retfixup = []
and argcreg = reg Argcr
and tagreg  = reg Tagr
and bigeqreg= reg Bigeqr
and indreg  = reg Indr
and cputype = if Power then "POWER" else if PowerPC then "PowerPC" else "POWER/PowerPC"

and artmp = amode (reg 13)		-- not kosher, but very unlikely to fail.

and regname n = if n > 32 then itos (n-32) else itos ((n+8)%32)
and zeroreg = 24
#define regk regrel
and amode (reg i)      = regname i
 || amode (regind r i) = itos (i*4) @ "(" @ regname r @ ")"
 || amode (regk r i) = itos i @ "(" @ regname r @ ")"		-- !! misuse!
 || amode (glob i)  = fixlabel i
-- || amode (idlit s) = fixlabel s
 || amode (const n) = itos n
 || amode _ = fail "amode\n"
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am @ (aspr cs ams)
||  aspr ('~'.cs) (am.ams) = '.'.amode am @ (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and isreg (reg _) = true
 || isreg _       = false
and otag oeval   = "0"
 || otag ounwind = "4"
 || otag ojfun   = "8"
 || otag ogettag = "12"
 || otag (onumtag i) = itos (4*i)
and ccc ltstack = false
 || ccc ltheap = false
 || ccc gtstack = false
 || ccc geheap = false
 || ccc cc = true
and ccname eq = "eq"
 || ccname ne = "ne"
 || ccname lt = "lt"
 || ccname gt = "gt"
 || ccname le = "le"
 || ccname ge = "ge"
 || ccname ltstack = "lt"
 || ccname ltheap = "lt"
 || ccname gtstack = "gt"
 || ccname geheap = "ge"
 || ccname dfeq = "eq"
 || ccname dfne = "ne"
 || ccname dflt = "lt"
 || ccname dfgt = "gt"
 || ccname dfle = "le"
 || ccname dfge = "ge"
 || ccname sfeq = "eq"
 || ccname sfne = "ne"
 || ccname sflt = "lt"
 || ccname sfgt = "gt"
 || ccname sfle = "le"
 || ccname sfge = "ge"
 || ccname x   = fail ("ccname "@mprint [Mjcond x ""])
and opname add = "a"
||  opname sub = "sf"
||  opname mul = "muls"
--||  opname div = "divs"
||  opname neg = "neg"
||  opname btand = "and"
||  opname btor = "or"
||  opname btxor = "xor"
||  opname btlsh = "sl"
||  opname btrsh = "sr"
||  opname btrsha = "sra"
||  opname dfadd = "fa"
||  opname dfsub = "fs"
||  opname dfmul = "fm"
||  opname dfdiv = "fd"
||  opname dfneg = "fneg"
||  opname sfadd = "fa"
||  opname sfsub = "fs"
||  opname sfmul = "fm"
||  opname sfdiv = "fd"
||  opname sfneg = "fneg"
||  opname x = fail ("opname "@mprint [Mop2 x (glob "X") (glob "Y")])

and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and oneop s1 s2 = '\t'.s1@'\t'.s2@"\n"
and saveret = /*extra space allocated by previous transformation */
	amode1 "mflr" dtmp @ ins (Mmove dtmp (regind Vpr 0))
and dtmp = dtmp4
and (reg rdtmp) = dtmp
and adtmp = amode dtmp
and move r1 r2 = amode3 "ai" r2 r1 (const 0)
and fmove r1 r2 = amode2 "fmr" r2 r1
and ccdestr = reg (-8)	-- turns into 0
and
    ins (Mmove (r1 as reg _) (r2 as reg _)) = move r1 r2
||  ins (Mmove (a1 as reg _) (a2 as regind _ _)) =
	amode2 "st" a1 a2
||  ins (Mmove (a1 as regind _ _) (a2 as reg _)) =
	amode2 "l" a2 a1
||  ins (Mmove (const k) (a2 as reg _)) & (smallh k) = amode2 "cal" a2 (regk zeroreg k)
||  ins (Mmove (const k) (a2 as reg _)) =
        let l = bitand k 65535
        and h = bitand (bitrsh k 16) 65535 in
        let (l', h') = if bitand l 32768 ~= 0 then (l-65536, bitand (h+1) 65535) else (l, h) in
	amode2 "cal" a2 (regk zeroreg l') @
        amode3 "cau" a2 a2 (const h')
||  ins (Mmove (a1 as idlit s) (a2 as reg _)) =
	amode2 "l" a2 (glob (globvar s))
||  ins (Mmove (a1 as reg _) (glob s)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "st" a1 (regind rdtmp 0)
||  ins (Mmove (glob s) (a2 as reg _)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "l" a2 (regind rdtmp 0)

||  ins (Mmovedf (r1 as reg _) (r2 as reg _)) = if r1=r2 then "" else fmove r1 r2
||  ins (Mmovedf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "stfd" a1 a2
||  ins (Mmovedf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "lfd" a2 a1
||  ins (Mmovedf (a1 as reg _) (glob s)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "stfd" a1 (regind rdtmp 0)
||  ins (Mmovedf (glob s) (a2 as reg _)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "lfd" a2 (regind rdtmp 0)

||  ins (Mmovesf (r1 as reg _) (r2 as reg _)) = if r1=r2 then "" else fmove r1 r2
||  ins (Mmovesf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "stfs" a1 a2
||  ins (Mmovesf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "lfs" a2 a1
||  ins (Mmovesf (a1 as reg _) (glob s)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "stfs" a1 (regind rdtmp 0)
||  ins (Mmovesf (glob s) (a2 as reg _)) =
        amode2 "l" dtmp (glob (globvar s)) @
	amode2 "lfs" a2 (regind rdtmp 0)

||  ins (Mop2 sftodf a1 a2) = ins (Mmovedf a1 a2)
||  ins (Mop2 dftosf a1 a2) = ins (Mmovedf a1 a2)

||  ins (Mop2 op a1 a2) & (op=dftoi | op=sftoi) =
	"\tmflr\t"@artmp@"\n"@
	fmove a1 (reg 33) @
	"\tbl\t.__itrunc\n" @
	"\tcror\t31,31,31\n" @
	move (reg 27) a2 @
	"\tmtlr\t"@artmp@"\n"

||  ins (Mop2 op a1 a2) & (op=itodf | op=itosf) =
	concmap ins [
	Masm "\txoriu\t^,^,^" [dtmp1; a1; const 0x8000];
	Mmove (idlit "convtmp") dtmp2;
	Mmove dtmp1 (regind rtmp2 1);
	Mmovedf (regind rtmp2 0) Ftmp4;
	Mmovedf (regind rtmp2 2) a2;
	Mop3 dfsub a2 Ftmp4 a2
	]

||  ins (Mop2 op a1 a2) & (op=neg | op=sfneg | op=dfneg) =
	amode2 (opname op) a2 a1
||  ins (Mop2 dfneg a1 a2) =
	amode2 (opname dfneg) a2 a1
||  ins (Mop2 btcompl a1 a2) =
	amode3 "sfi" a2 a1 (const (-1))
||  ins (Mop2 op a1 a2) = ins (Mop3 op a1 a2 a2)

-- The C compiler generates POWER instructions for portable programs,
-- so we do too.
||  ins (Mop3 div a1 a2 a3) =
	if Power then
	    amode3 "divs" a3 a2 a1
	else if PowerPC then
	    amode3 "divw" a3 a2 a1
	else
	    -- have to use millicode to be portable
	    "\tmflr\t"@artmp@"\n"@
	    move a2 (reg 27) @
	    move a1 (reg 28) @
	    "\tbla\t.__divss\n" @
	    move (reg 27) a3 @
	    "\tmtlr\t"@artmp@"\n"
||  ins (Mop3 mod a1 a2 a3) =
	if Power then
            ins (Mop3 div a1 a2 a3) @
            amode1 "mfmq" a3
	else if PowerPC then
            ins (Mop3 div a1 a2 a3) @
	    ins (Mop3 mul a3 a2 a3) @
	    ins (Mop3 sub a1 a3 a3)
	else
	    -- have to use millicode to be portable
	    "\tmflr\t"@artmp@"\n"@
	    move a2 (reg 27) @
	    move a1 (reg 28) @
	    "\tbla\t.__divss\n" @
	    move (reg 28) a3 @
	    "\tmtlr\t"@artmp@"\n"

||  ins (Mop3 sub (a1 as const k) a2 a3) = ins (Mop3 add (const (-k)) a2 a3)
||  ins (Mop3 sub a1 (a2 as const k) a3) = amode3 "sfi" a3 a1 a2
||  ins (Mop3 op (a1 as const k) a2 a3) =
        amode3 (opname op @ "i" @ (if op = btand then "l." else if op = btor then "l" else "")) a3 a2 a1
||  ins (Mop3 sub a1 a2 a3) =
	amode3 (opname sub) a3 a1 a2
||  ins (Mop3 op a1 a2 a3) =
	amode3 (opname op @ (if Power & (op = btlsh | op = btrsh) then "e" else "")) a3 a2 a1

||  ins (Mcall s) = saveret @ oneop "bl" (fixlabel s)
||  ins (Mjumpf s) = oneop "b" (fixlabel s)
||  ins (Mjump s) = oneop "b" s

||  ins Mreturn =
        "\tmflr\t"@adtmp@"\n"@
        "\tmtctr\t"@adtmp@"\n"@
        ins (Mmove (regind Vpr 0) dtmp)@
        ins (Mop2 add (const 4) (reg Vpr))@
        "\tmtlr\t"@adtmp@"\n"@
        "\tbctr\n"

||  ins (Madda (a1 as reg _) (a2 as reg _)) = amode2 "mr" dtmp a1 @ amode3 "sli" dtmp dtmp (const 2) @ amode3 "cax" a2 a2 dtmp

||  ins (Mcompare a1 (a2 as const k)) = amode3 "cmpi" ccdestr a1 a2
||  ins (Mcompare a1 a2) = amode3 "cmp" ccdestr a1 a2		-- !!! sometimes cmpl !!!
||  ins (Mcomparedf a1 a2) = amode3 "fcmpu" ccdestr a1 a2
||  ins (Mcomparesf a1 a2) = amode3 "fcmpu" ccdestr a1 a2
||  ins (Mjcond cc l) = 
	if FarJump & ccc cc then
	    "\tb"@ccname (negmop cc)@"\t$+8\n"@
	    "\tb\t"@l@"\n"
	else
	    "\tb"@ccname cc@"\t"@l@"\n"
||  ins (Mboolcc cc a) = concmap ins [Mmove (const 1) a; Mjcond cc "$+8"; Mmove (const 0) a]

||  ins (m as Mcalltag t r) = saveret@"\tl\t"@adtmp@","@otag t@"("@regname r@")\n\tmtctr\t"@adtmp@"\n\tbctrl\n"
||  ins (m as Mjumptag t r) = 
  "\tl\t"@adtmp@","@otag t@"("@regname r@")\n\tmtctr\t"@adtmp@"\n\tbctr\n"

 || ins (Mcase a l h _ ls x) =
 	let t = 'L'.itos x in
        let t1 = t@"_1"
        and t2 = t@"_2" in
 	ins (Mmove a dtmp) @
	(if l = 0 then "" else ins (Mop2 sub (const l) dtmp))@
        amode3 "cmpli" ccdestr dtmp (const (h-l)) @
        "\tbgt\t" @ t1 @ "\n" @
        amode3 "sli" dtmp dtmp (const 2) @
        amode2 "l" dtmp1 (glob (globvar t2)) @
        amode3 "a" dtmp dtmp dtmp1 @
        amode2 "l" dtmp (regind rdtmp 0) @
        amode3 "a" dtmp dtmp dtmp1 @
        amode1 "mtctr" dtmp @
        "\tbctr\n\t.align\t2\n" @
	t2 @ ":\n" @
        concmap (\l."\t.long\t"@l@"-"@t2@"\n") ls @
        t1 @ ":\n"

 || ins (Mnoop) = ""
 || ins (Mdata) = "\t.csect .data[RW]\n"
 || ins (Mtext) = "\t.csect .text[PR]\n"
 || ins (Mword (glob  a)) = "\t.long\t" @ fixlabel a @ "\n"
 || ins (Mword (idlit a)) = "\t.long\t" @ fixlabel a @ "\n"
 || ins (Mword (const i)) = "\t.long\t" @ itos i @ "\n"
 || ins (Msfloat s) = "\t.float\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins (Mdfloat s) = "\t.double\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins (Mstring s) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t2\n")
 				
 || ins (Mexport a) = "\t.globl\t" @ fixlabel a @ "\n"
 || ins (Mcom s) = " # " @ s @ "\n"
 || ins (Mpragma s) = ""
 || ins (Mlabel l) = fixlabel l @ ":\n"
 || ins (Masm s l) = aspr s l @ "\n"
 || ins (Malign) = "\t.align\t2\n"
 || ins (Mfunbegin _ _) = ""
 || ins (Mfunend ) = ""
 || ins m = fail ("ins: strange Mcode " @ mprint [m])

and prol = "\t.extern\t.__itrunc\n\t.extern\tconvtmp[RW]\n\t.toc\nLC..convtmp:\n\t.tc\tconvtmp[TC],convtmp[RW]\n"
	@ if PowerPC then "\t.machine\t\"ppc\"\n" else ""

and epil = "\t.csect .text[PR]\n_section_.text:\n\t.csect .data[RW]\n\t.long _section_.text\n"
	@ if Power | PowerPC then "" else "\t.extern\t.__divss\n"

and assemblercode m =
    let mt = mtrans2 (mtrans1 m) in
    (if PrMtrans then
	mprint mt
    else
	"") @
    prol @
    concmap ins (Mtext.extern_declarations mt)@
    epil
end
