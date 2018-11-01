module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/flags.t"
#include "../misc/qstring.t"
#include "mtrans1.t"
#include "mtrans2.t"
#include "tmp.h"

/*
   $f1			(nonsaved floating-point register)
   $f10-$f15		(likewise)
   $f22-$f30		(likewise)
   $f21-$f16		(likewise, but input args)
   $f0			(nonsaved, but return value)
   $f2-$f9		(saved floating-point registers)
   $1-$8		(nonsaved integer registers)
   $22-$25		(likewise)
   $28			(likewise)
   $0			(likewise, but return value)
   $21-$16		(likewise, but input args)
   $27			(procedure value)
   $9-$14		(saved integer registers)
   $26			(return PC)
   $15			(frame pointer)
   $29			(global pointer)
   $30			(stack pointer)
   $31, $f31		(always zero)
*/
-- Registers:
-- r0   - mcode r0
-- r1   - mcode r1
-- r2   - 
-- r3   - 
-- ...
-- r14  - 
-- r15  - fp
-- r16  - c-arg 0
-- r17  - addr tmp
-- r18  - dtmp4
-- r19  - Canon
-- r20  - Sp
-- r21  - hp
-- r22  - Ehpr
-- r23  - dtmp1
-- r24  - dtmp2
-- r25  - dtmp3
-- r26  - ret pc
-- r27  - proc value
-- r28  - at
-- r29  - gp
-- r30  - Vpr
-- r31  - zero

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, farcase,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, retfixup;
rec
    Aregs = count 1 lastreg @ fail "Out of A-regs"
and Dregs = count 1 lastreg @ fail "Out of D-regs"
and lastreg = 12
and Fregs = map (\x.x*2+32) [0..10] @ fail "Out of F-regs"
and sfloatsize = 1
and dfloatsize = 8/wsize
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*5
and farcase = false
and retfixup = [ldgp rdtmp]
and use3op = true
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg lastreg
and regname n = if n < 32 then "$"@itos ((n+ROFFS)%32) else "$f"@itos (n-32)
and cputype = "Alpha"

and amode (reg i)      = regname i
 || amode (regind r i) = itos (i*wsize) @ "(" @ regname r @ ")"
 || amode (regrel _ _) = fail "amode regrel\n"

 || amode (glob i)  = drop_ i
 || amode (idlit s) = drop_ s
 || amode (const n) = itos n
 || amode (fconst f) = fmtf ".17e" f
 || amode a = fail ("no match in amode: "@pamode a)
and drop_ ('_'.s) = s
||  drop_ s = s
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am @ (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and suffix = if wsize = 4 then "l" else "q"
and word = if wsize = 4 then "long" else "quad"
and wsize = WSIZE

and isreg (reg _) = true
 || isreg _       = false
and otag oeval   = itos (0*wsize)
 || otag ounwind = itos (1*wsize)
 || otag ojfun   = itos (2*wsize)
 || otag ogettag = itos (3*wsize)
 || otag (onumtag i) = itos (i*wsize)
and ccname eq = "Teq"
 || ccname ne = "Feq"
 || ccname lt = "Tlt"
 || ccname gt = "Fle"
 || ccname le = "Tle"
 || ccname ge = "Flt"
 || ccname ltstack = "Tult"
 || ccname ltheap = "Tult"
 || ccname gtstack = "Fule"
 || ccname geheap = "Fult"
---!!!
 || ccname dfeq = "Teq"
 || ccname dfne = "Feq"
 || ccname dflt = "Tlt"
 || ccname dfgt = "Fle"
 || ccname dfle = "Tle"
 || ccname dfge = "Flt"
 || ccname sfeq = ccname dfeq
 || ccname sfne = ccname dfne
 || ccname sflt = ccname dflt
 || ccname sfgt = ccname dfgt
 || ccname sfle = ccname dfle
 || ccname sfge = ccname dfge
and ccbop 'T' = "ne"
||  ccbop 'F' = "eq"
and opname add = "add"@suffix
||  opname sub = "sub"@suffix
||  opname mul = "mul"@suffix
||  opname div = "div"@suffix
||  opname mod = "rem"@suffix
||  opname neg = "neg"@suffix
||  opname btand = "and"
||  opname btor = "bis"
||  opname btxor = "xor"
--||  opname btcompl = "ornot $31"
||  opname btlsh = "sll"
||  opname btrsh = "srl" -- only works for 64 bit arith
||  opname btrsha = "sra"
||  opname dfadd = "addt"
||  opname dfsub = "subt"
||  opname dfmul = "mult"
||  opname dfdiv = "divt"
||  opname dfneg = "fneg"
||  opname dftoi = fail "dftoi"
||  opname itodf = fail "itodf"
||  opname sfadd = "adds"
||  opname sfsub = "subs"
||  opname sfmul = "muls"
||  opname sfdiv = "divs"
||  opname sfneg = "fneg"
||  opname sftoi = fail "sftoi"
||  opname itosf = fail "itosf"
--||  opname sftodf = "cvt.d.s"
--||  opname dftosf = "cvt.s.d"

and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and oneop s1 s2 = '\t'.s1@'\t'.s2@"\n"
and saveret = ins /*extra space allocated by previous transformation */(Mmove rret (regind Vpr 0))
and (reg rdtmp) = dtmp
and dtmp = dtmp4
and ftmp = Ftmp4
and adtmp = amode dtmp
and swtmp = reg lastreg
and raddr = reg Addr
and iaddr = regind Addr 0
and rret = reg Ret
and rproc = reg Proc
and iproc = regind Proc 0
and aproc = amode rproc
and rzero = reg Zero
and at = reg At
and gp = reg Gp
and st = "st"@suffix
and ld = "ld"@suffix
and cnvt = glob "cnvtmp"
and
    ins (Mmove (r1 as reg _) (r2 as reg _)) = 
	amode3 "bis" r1 r1 r2
||  ins (Mmove (a1 as reg _) (a2 as regind _ _)) =
	amode2 st a1 a2
||  ins (Mmove (a1 as regind _ _) (a2 as reg _)) =
	amode2 ld a2 a1
||  ins (Mmove (a1 as const k) (a2 as reg _)) =
        amode2 ("ldi"@suffix) a2 a1
||  ins (Mmove (a1 as idlit _) (a2 as reg _)) =
	amode2 "lda" a2 a1
||  ins (Mmove (a1 as reg _) (a2 as glob _)) =
        amode2 st a1 a2
||  ins (Mmove (a1 as glob _) (a2 as reg _)) =
	amode2 ld a2 a1
||  ins (Mmovedf (r1 as reg _) (r2 as reg _)) = 
	amode3 "cpys" r1 r1 r2
||  ins (Mmovedf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "stt" a1 a2
||  ins (Mmovedf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "ldt" a2 a1
||  ins (Mmovedf (a1 as fconst _) (a2 as reg _)) =
	amode2 "ldit" a2 a1
||  ins (Mmovedf (a1 as reg _) (a2 as glob _)) =
	amode2 "stt" a1 a2
||  ins (Mmovedf (a1 as glob _) (a2 as reg _)) =
	amode2 "ldt" a2 a1
||  ins (Mmovesf (r1 as reg _) (r2 as reg _)) = 
	amode3 "cpys" r1 r1 r2
||  ins (Mmovesf (a1 as reg _) (a2 as regind _ _)) =
	amode2 "sts" a1 a2
||  ins (Mmovesf (a1 as regind _ _) (a2 as reg _)) =
	amode2 "lds" a2 a1
||  ins (Mmovesf (a1 as fconst _) (a2 as reg _)) =
	amode2 "ldis" a2 a1
||  ins (Mmovesf (a1 as reg _) (a2 as glob _)) =
	amode2 "sts" a1 a2
||  ins (Mmovesf (a1 as glob _) (a2 as reg _)) =
	amode2 "lds" a2 a1
||  ins (Madda (a1 as reg _) (a2 as reg _)) = amode3 ("s"@itos wsize@"add"@suffix) a1 a2 a2
||  ins (Mop2 itodf a1 a2) = ins (Mmove a1 cnvt) @ ins (Mmovedf cnvt a2) @ amode2 "cvtqt" a2 a2
||  ins (Mop2 dftoi a1 a2) = amode2 "cvttqc" a1 Ftmp4 @ ins (Mmovedf Ftmp4 cnvt) @ ins (Mmove cnvt a2)
||  ins (Mop2 itosf a1 a2) = ins (Mop2 itodf a1 a2)
||  ins (Mop2 sftoi a1 a2) = ins (Mop2 dftoi a1 a2)

||  ins (Mop2 op a1 a2) & (mem op [sftodf; dftosf]) = if a1 = a2 then "" else ins (Mmovedf a1 a2)
||  ins (Mop2 btcompl a1 a2) =
	amode3 "ornot" rzero a1 a2
||  ins (Mop2 op a1 a2) & (mem op [neg; dfneg; sfneg]) =
	amode2 (opname op) a1 a2
||  ins (Mop2 op a1 a2) =
	amode3 (opname op) a2 a1 a2
||  ins (Mop3 op a1 a2 a3) =
	amode3 (opname op) a2 a1 a3

||  ins (Mcall s) = --saveret @ let s_ = drop_ s in amode2 "jsr" rret (glob s_)
	saveret @
	amode2 "lda" rproc (idlit s) @ 
	"\tjsr\t"@amode rret@",("@amode rproc@"),0\n"
||  ins (Mjumpf s) = 
--	let s_ = drop_ s in amode2 "jmp" rzero (glob s_)
	amode2 "lda" rproc (idlit s) @ 
	"\tjmp\t"@amode rzero@",("@amode rproc@"),0\n"
||  ins (Mjump s) = oneop "br" ("$31,"@s)

||  ins Mreturn = concmap ins 
	[Mmove (reg Ret) dtmp;
	 Mmove (regind Vpr 0) (reg Ret);
	 Mop2 add (const wsize) (reg Vpr)] @ "\tret\t" @ amode rzero @",("@amode dtmp@"),1\n"

||  ins (m as Mcompare _ _) = "!! Bad "@mprint[m]
||  ins (m as Mcomparesf _ _) = "!! Bad "@mprint[m]
||  ins (m as Mcomparedf _ _) = "!! Bad "@mprint[m]
||  ins (m as Mjcond _ _) = "!! Bad "@mprint[m]
||  ins (m as Mboolcc _ _) = "!! Bad "@mprint[m]

||  ins (m as Mcalltag t r) = saveret@"\t"@ld@"\t"@aproc@","@otag t@"("@regname r@")\n"@"\tjsr\t"@amode rret@",("@amode rproc@"),0\n"

||  ins (m as Mjumptag t r) = "\t"@ld@"\t"@aproc@","@otag t@"("@regname r@")\n"@"\tjmp\t"@amode rzero@",("@amode rproc@"),0\n"

 || ins (Mcase a l h mx ls x) =
 	let t = 'L'.itos x in
	let cb = amode3 "cmpule" dtmp (const (h-l)) swtmp @ amode2 "beq" swtmp (glob (t@"_2")) in
	"\t.rdata\n"@t@"_1:\n"@
	concmap (\l."\t.gprel32\t"@l@"\n") ls@
	"\t.text\n"@
 	ins (Mmove a dtmp) @
	(if l = 0 then "" else ins (Mop2 sub (const l) dtmp))@
        (if l = 0 & h = mx-1 then "" else cb) @
	"\t.set\tnoat\n"@
	"\tlda\t$28,"@t@"_1\n"@
	amode3 "s4addq" dtmp at dtmp@
        amode2 "ldl" dtmp (regind rdtmp 0)@
	ins (Mop3 add gp dtmp dtmp)@
	"\tjmp\t"@amode rzero@",("@amode dtmp@"),0\n"@
	"\t.set\tat\n"@
	t@"_2:\n"

 || ins (Mnoop) = "\tnoop\n"
 || ins (Mdata) = "\t.data\n"
 || ins (Mtext) = "\t.text\n"
 || ins (Mword (glob  a)) = tword (drop_ a)
 || ins (Mword (idlit a)) = tword (drop_ a)
 || ins (Mword (const i)) = tword (itos i)
 || ins (Mdfloat s) = "\t.t_floating\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins (Msfloat s) = "\t.s_floating\t"@(if mem '.' s then s else s@".0")@"\n"
 || ins (Mstring s) = "\t.ascii\t\""@qstring (s@[chr 0])@"\"\n"
 || ins (Mexport a) = "\t.globl\t" @ drop_ a @ "\n"
 || ins (Mcom s) = " # " @ s @ "\n"
 || ins (Mpragma s) = ""
 || ins (Mlabel l) = drop_ l @ ":\n"
 || ins (Masm s l) = aspr s l @ "\n"
 || ins (Malign) = "\t.align\t4\n"
 || ins (Mfunbegin _ _) = ""
 || ins (Mfunend ) = ""
 || ins m = fail ("ins: strange Mcode " @ mprint [m])
and tword s = "\t."@word@"\t"@s@"\n"
and cmptr [] = []
||  cmptr (Mcompare a1 a2.Mjcond cc1 s1.Mjcond cc2 s2.ms) = fail "cmptr: double jump"
||  cmptr (Mcompare a1 a2.Mjcond cc s.ms) =
        let (c.cs) = ccname cc in
	Masm ("\tcmp"@cs@"\t^,^,^") [a1; a2; dtmp].
	Masm ("\tb"@ccbop c@"\t^,^") [dtmp;glob s].
        cmptr ms
||  cmptr (Mcompare a1 a2.Mboolcc cc a3.ms) =
        let (c.cs) = ccname cc in
	Masm ("\tcmp"@cs@"\t^,^,^") [a1; a2; a3].
	(if c='F' then Mop3 btxor (const 1) a3 a3.cmptr ms else
	cmptr ms)
||  cmptr (Mcomparedf a1 a2.Mjcond cc s.ms) =
        let (c.cs) = ccname cc in
	Masm ("\tcmpt"@cs@"\t^,^,^") [a1; a2; ftmp].
	Masm ("\tfb"@ccbop c@"\t^,^") [ftmp;glob s].
        cmptr ms
||  cmptr (Mcomparedf a1 a2.Mboolcc cc a3.ms) = 
	cmptr (Mmove (const 1) a3 . Mcomparedf a1 a2 . Mjcond cc "1f" . Mmove (const 0) a3 . Mlabel "1" . ms)
||  cmptr (Mcomparesf a1 a2.Mjcond cc s.ms) =
        let (c.cs) = ccname cc in
	Masm ("\tcmpt"@cs@"\t^,^,^") [a1; a2; ftmp].
	Masm ("\tfb"@ccbop c@"\t^,^") [ftmp;glob s].
        cmptr ms
||  cmptr (Mcomparesf a1 a2.Mboolcc cc a3.ms) = 
	cmptr (Mmove (const 1) a3 . Mcomparesf a1 a2 . Mjcond cc "1f" . Mmove (const 0) a3 . Mlabel "1" . ms)
||  cmptr (m.ms) = m.cmptr ms
and prol = ""
#if 0
concmap (\s."\t.extern\t"@s@" 4\n") [
"SFLOAT";"INT";"CHAR";"TAG";"TAG0";"VEK";"HOLE";"AP";"CAP";"VAP";"PAIR";"PAIR0";"PAIR1";"PAIR2";"PAIR3";"PAIR4";"DFLOAT";"BIGNUM";"DVEK";"CANON";"CANONDATA";"STRINGN"]
#endif

and ldgp r = Masm "\tldgp\t$gp,^" [regind r 0]

and ldgptr ((m0 as Malign).(m1 as Mlabel _).(m2 as Mop3 add _ _ _).m3.(m4 as Mop3 add _ _ _).(m5 as Mlabel _).(m6 as Mfunbegin _ _).ms) =
-- insert code to load the gp register, assume Proc register points
-- to here.
    m0 .					-- align						-- alignment = 0
    m1 .					-- label						-- alignment = 0
    m2 .					-- add							-- alignment = 0
    m3 .					-- st							-- alignment = 4
    m4 .					-- add							-- alignment = 8
    Mop3 add (const 16) (reg Proc) (reg Proc) .	-- adjust for early entry point, 12 bytes back		-- alignment = 12
    m5 . 					-- label						-- alignment = 12
    m6 .					-- funbegin						-- alignment = 12
    ldgp Proc  .				-- load gp register					-- alignment = 16
    ldgptr ms
||  ldgptr (Mfunbegin s _._) = fail ("bad .funbegin in "@s)
||  ldgptr ((m as Mcall _) . ms) = m . ldgp rdtmp . ldgptr ms
||  ldgptr ((m as Mcalltag _ _) . ms) = m . ldgp rdtmp . ldgptr ms
||  ldgptr (m.ms) = m . ldgptr ms
||  ldgptr [] = []

-- Make sure everything is aliged and file boundary.
and alignsegs = [Mdata;Malign;Mword (const 0);Mtext;Malign;Mword (const 0)]

-- Insert an .ent/.end pair to silence the linker warning.
and pre = [--Masm "\t.verstamp 3 11 0 999 7" []; 
           Masm "\t.ent Dummy" []; 
           Mlabel "Dummy"]@
	  alignsegs
and post = alignsegs
	   @[Masm "\t.end Dummy" []]

and assemblercode m =
    let mt = pre@ldgptr (cmptr (mtrans2 (mtrans1 m)))@post in
    (if PrMtrans then
	mprint mt
    else
	"") @
    prol @
    concmap ins mt

end
