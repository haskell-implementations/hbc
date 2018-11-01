module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/flags.t"
#include "../misc/qstring.t"
#include "mtrans.t"
#include "reg.h"
--
-- Register allocation
-- sp	- value stack pointer
-- bp	- heap pointer
-- di	- pointer stack pointer
-- si,[abcd]x - mcode registers
-- cx	- some return values
-- bx	- scratch

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op,
	argcreg, tagreg, bigeqreg, indreg, cputype, sfloatsize, dfloatsize, farcase, retfixup;
rec
    Aregs = [RSI; RCX; RDX; RAX] @ fail "Out of A-regs"
and Dregs = [RCX; RDX; RSI; RAX] @ fail "Out of D-regs"
and Fregs = count FP0 FPlast
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*4
and use3op = false
and argcreg = reg RCX
and tagreg  = reg RCX
and bigeqreg= reg RCX
and indreg  = reg RSI
and cputype = if I586 then "Pentium" else if I486 then "I80486" else "I80386"
and dfloatsize = 2
and sfloatsize = 1
and farcase = true
and retfixup = []

and ELFformat = Solaris | Linux
and alignN = if ELFformat then "4" else "2"

and drop_ ('_'.s) & (ELFformat) = s
||  drop_ s = s

and fltpref s = if Solaris then "" else s

#ifdef USEDREG
and dreg    = reg RAX	-- Not the best choice?
#endif
and rax     = reg RAX
and rcx	    = reg RCX
and rdx     = reg RDX
and Spreg   = reg Spr
and Hpreg   = reg Hpr
and Vpreg   = reg Vpr
and Hpr = RDI
and Spr = RBP
and Vpr = RSP
and regname RBX = "%ebx"	-- make r0 an "address" reg
||  regname RAX = "%eax"
||  regname RCX = "%ecx"
||  regname RDX = "%edx"
||  regname RSI = "%esi"
||  regname RDI = "%edi"
||  regname RBP = "%ebp"
||  regname RSP = "%esp"
||  regname FP0 = fail "FP0 used"
||  regname r   = 
	if r >= FP0 then
	    "fpr" @ itos (r-FP0)
	else
	    "%st(" @ itos (r-ST0) @ ")"
and bytereg r =
	case regname r in
	   [_;_;c;_] : ['%';c;'l']
	end
and
    re r c = regname r @ c

and otag oeval c = c -- "oeval" @ c
 || otag ounwind c = "ounwind" @ c
 || otag ojfun c = "ojfun" @ c
 || otag ogettag c = "ogettag" @ c
 || otag (onumtag 0) c = c
 || otag (onumtag i) c = itos (4*i) @ c
 
and ojcc eq c = "je" @ c
 || ojcc ne c = "jne" @ c
 || ojcc lt c = "jl" @ c
 || ojcc gt c = "jg" @ c
 || ojcc le c = "jle" @ c
 || ojcc ge c = "jge" @ c
 || ojcc ltstack c = "jb" @ c
 || ojcc ltheap c = "jb" @ c
 || ojcc gtstack c = "ja" @ c
 || ojcc geheap c = "jae" @ c
 || ojcc dfeq c = "je" @ c
 || ojcc dfne c = "jne" @ c
 || ojcc dflt c = "jb" @ c
 || ojcc dfgt c = "ja" @ c
 || ojcc dfle c = "jbe" @ c
 || ojcc dfge c = "jae" @ c
 || ojcc sfeq c = "je" @ c
 || ojcc sfne c = "jne" @ c
 || ojcc sflt c = "jb" @ c
 || ojcc sfgt c = "ja" @ c
 || ojcc sfle c = "jbe" @ c
 || ojcc sfge c = "jae" @ c
 
and scc op c = "set" @ tl (ojcc op c)
 
and oop add c = "addl" @ c
 || oop sub c = "subl" @ c
 || oop mul c = "imull" @ c
 || oop neg c = "negl" @ c
 || oop btand c = "andl" @ c
 || oop btor  c = "orl" @ c
 || oop btxor c = "xorl" @ c
 || oop btcompl c = "notl" @ c
 || oop btlsh c = "shll" @ c
 || oop btrsh c = "shrl" @ c
 || oop btrsha c = "sarl" @ c

and 
    amode (Vp) c = "%esp" @ c
 || amode (Vind i) c = itos (4*i) @ "(%esp)" @ c
 || amode (Vrel _) c = fail "amode Srel\n"
 || amode (pushV) c = fail "amode pushV"
 || amode (popV) c = fail "amode popV"
 
 || amode (Sp) c = "%ebp" @ c
 || amode (Sind i) c = itos (4*i) @ "(%ebp)" @ c
 || amode (Srel _) c = fail "amode Srel\n"
 || amode (pushS) c = fail "amode pushS"
 || amode (popS) c = fail "amode popS"
 
 || amode (hp) c = "%edi" @ c
 || amode (hpind i) c = itos (4*i) @ "(%edi)" @ c
 || amode (hprel i) c = fail "amode hprel\n"
 || amode (tohp) c = fail "amode tohp"
 
 || amode (reg i) c = re i c
 || amode (regind r i) c = itos(4*i) @ "(" @ re r (')'.c)
 || amode (regrel _ _) c = fail "amode regrel\n"

 || amode (glob i) c = drop_ i @ c
 || amode (idlit s) c = '$'.drop_ s @ c
 || amode (retaddr s) c = '$'.drop_ s @ c
 || amode (const n) c = '$'.itos n @ c
 
and amode1 a1 c = amode a1 ('\n'.c)
and amode2 a1 a2 c = amode a1 (','.amode a2 ('\n'.c))
and amode3 a1 a2 a3 c = amode a1 (','.amode a2 (','.amode a3 ('\n'.c)))

and ok1 (Sp)	  = true
||  ok1 (Vp)	  = true
||  ok1 (hp)	  = true
||  ok1 (const _) = true
||  ok1 (idlit _) = true
||  ok1 (retaddr _) = true
||  ok1 (reg _)   = true
||  ok1 _         = false
and okop a1 a2 = ok1 a1 | ok1 a2
and /*lea  (a1 ar reg _) (a2 as reg _) c & (I486) =
||*/  lea  a1 (a2 as reg r) c = "\tleal\t" @ amode2 a1 a2 c
||  lea  (glob s) pushV c = push (idlit s) c
||  lea  (glob s) a2 c = move (idlit s) a2 c
||  lea  (regind r i) pushV c = push (reg r) (aop add (const (4*i)) (Vind 0) c)
||  lea  (regind r i) a2 c = move (reg r) a2 (aop add (const (4*i)) a2 c)
and move (const 0) (a2 as reg _) c = "\txorl\t" @ amode2 a2 a2 c
||  move (const 1) (a2 as reg _) c & (I486) = "\txorl\t" @ amode2 a2 a2 (aop add (const 1) a2 c)
||  move a1 a2 c & (ok1 a1) = "\tmovl\t" @ amode2 a1 a2 c
||  move a1 (a2 as reg _) c = "\tmovl\t" @ amode2 a1 a2 c
--||  move a1 a2 c = move a1 dreg (move dreg a2 c)
-- SLOW TRICK: do not destroy dreg
||  move a1 a2 c = push a1 (pop a2 c)
and push a c = "\tpushl\t"@amode1 a c
and pop a c = "\tpopl\t"@amode1 a c
and comp (a1 as reg _) (const 0) c = "\ttestl\t" @ amode2 a1 a1 c
||  comp a1 a2 c & (okop a1 a2) = "\tcmpl\t" @ amode2 a2 a1 c
#if USEDREG
||  comp a1 a2 c = move a1 dreg (comp dreg a2 c)
#else
-- Cant use xchgl, because a1 may not be writable
||  comp a1 a2 c =
	let dr = unusedreg [a1; a2] in
        let a1' = case a1 in Vind i : Vind (i+1) || _ : a1 end
        and a2' = case a2 in Vind i : Vind (i+1) || _ : a2 end in
	push dr (move a1' dr (comp dr a2' (pop dr c)))
#endif
and unusedreg ams = reg (hd (difference [1;2;3] (concmap usedreg ams)))
and usedreg (reg r) = [r]
||  usedreg (regind r _) = [r]
||  usedreg (regrel r _) = [r]
||  usedreg _ = []
and xchg a1 a2 c = "\txchgl\t" @ amode2 a1 a2 c
and aop add (const 1) a1 c = "\tincl\t" @ amode1 a1 c
||  aop sub (const 1) a1 c = "\tdecl\t" @ amode1 a1 c
||  aop add (const 0) a1 c = c
||  aop sub (const 0) a1 c = c
||  aop mul (a1 as const _) (a2 as reg _) c = '\t'.oop mul ('\t'.amode3 a1 a2 a2 c)
||  aop op a1 a2 c & (okop a1 a2) = '\t'.oop op ('\t'.amode2 a1 a2 c)
#ifdef USEDREG
||  aop op a1 a2 c = move a1 dreg (aop op dreg a2 c)
#else
||  aop op a1 a2 c = let dr = unusedreg [a1; a2] in xchg a2 dr (aop op a1 dr (xchg a2 dr c))
#endif

and aspr ""       ams      = ""
||  aspr ('^'.'^'.'^'.cs) (glob k.ams) = amode (glob (itos (stoi k*4+8))) (aspr cs ams)
||  aspr ('^'.'^'.cs) (glob k.ams) = amode (glob (itos (stoi k*4))) (aspr cs ams)
||  aspr ('#'.cs) (reg i.ams) = let [p;'e';r;'x'] = regname i in amode (glob [p;r;'l']) (aspr cs ams)
||  aspr ('#'.cs) (am.ams) = amode am (aspr cs ams)
||  aspr ('^'.cs) (am.ams) = amode am (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams
 
and isconst (const _) = true
 || isconst _ = false

and isrdx (reg RDX) = true
 || isrdx (regind RDX _) = true
 || isrdx _ = false

and isbad x = isconst x | isrdx x

and Mexch x y = Masm "\txchgl\t^,^" [x; y]

and streg = if Solaris then ",%st" else ""

and fopstr dfadd = "fadd"
||  fopstr dfsub = "fsub"
||  fopstr dfmul = "fmul"
||  fopstr dfdiv = "fdiv"
||  fopstr sfadd = "fadd"
||  fopstr sfsub = "fsub"
||  fopstr sfmul = "fmul"
||  fopstr sfdiv = "fdiv"
and opl op = if mem op [dfadd; dfsub; dfmul; dfdiv; dfneg] then "l" else "s"
and fop op (a as reg r) & (r < FP0) = Masm ("\t"@fopstr op@"\t^"@streg) [a]
||  fop op a = Masm ("\t"@fopstr op@opl op@"\t^") [a]
and mdfconst b f = 
    if b & f = 0.0 then
	(const 0, [])
    else if b & f = 1.0 then
	(const 1, [])
    else
	(glob "1b", [Mdata ; Mlabel "1" ; Mdfloat (fmtf ".16e" f) ; Mtext ])
and msfconst b f = 
    if b & f = 0.0 then
	(const 0, [])
    else if b & f = 1.0 then
	(const 1, [])
    else
	(glob "1b", [Mdata ; Mlabel "1" ; Msfloat (fmtf ".8e" f) ; Mtext ])
and rst0 = reg ST0
and rst1 = reg ST1
and cnvtmp = glob "cnvtmp"
and mdfld (const 0) = Masm "\tfldz" []
||  mdfld (const 1) = Masm "\tfld1" []
||  mdfld (a as reg s) & (s < FP0) = Masm "\tfld\t^" [a]
||  mdfld a = Masm "\tfldl\t^" [a]
and msfld (const 0) = Masm "\tfldz" []
||  msfld (const 1) = Masm "\tfld1" []
||  msfld (a as reg s) & (s < FP0) = Masm "\tfld\t^" [a]
||  msfld a = Masm "\tflds\t^" [a]
and mdfldn a = [mfpop; mdfld a]			--Masm "\tfstp\t^\n\tfldl\t^" [rst0; a]
and msfldn a = [mfpop; msfld a]			--Masm "\tfstp\t^\n\tfldl\t^" [rst0; a]
and mfpop = Masm "\tfstp\t^" [rst0]
and mfldi (a as const _) = [Mdata; Mlabel "2"; Mword a; Mtext; Masm "\tfildl\t^" [glob "2b"]]
||  mfldi (a as reg _) = [Mmove a cnvtmp; Masm "\tfildl\t^" [cnvtmp]]
||  mfldi a = [Masm "\tfildl\t^" [a]]
and mdfst a = Masm "\tfstpl\t^" [a]
and msfst a = Masm "\tfstps\t^" [a]
and mfsti (a as reg _) = [Masm "\tfistpl\t^" [cnvtmp]; Mmove cnvtmp a]
||  mfsti a = [Masm "\tfistpl\t^" [a]]
and mdfstn a = Masm "\tfstl\t^" [a]
and msfstn a = Masm "\tfsts\t^" [a]
and fccxf = [Masm "\tfstsw\t%ax" [];		-- uses ax !!! without check
	     Masm "\tsahf" [] ]
and sfsrc op = mem op [sftoi; sftodf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and sfdst op = mem op [itosf; dftosf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and dfsrc op = mem op [dftoi; dftosf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and dfdst op = mem op [itodf; sftodf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and mXfld op a = if sfsrc op then msfld a else mdfld a
and mXfst op a = if sfdst op then msfst a else mdfst a

and savemode = Masm "\tfnstcw\t^" [glob "ofmode"]
and trmode   = Masm "\tfldcw\t^"  [glob "tfmode"]
and resmode  = Masm "\tfldcw\t^"  [glob "ofmode"]

and
#define RFP0 (reg FP0)
/*#define RFP0 (reg 1000)*/
-- FP insns
    ins (Mmovedf (fconst f) a2.ms) = 
	let (a1, ns) = mdfconst true f in
	ins (ns @ Mmovedf a1 a2 . ms)
 || ins (Mmovedf a1 pushV.ms) =
	ins (Mmove (Vrel (-2)) Vp . Mmovedf a1 (Vind 0) . ms)
 || ins (Mmovedf popV a2.ms) =
	ins (Mmovedf (Vind 0) a2 . Mmove (Vrel 2) Vp . ms)
 || ins (Mmovedf a1 RFP0 .ms) = 
	ins (mdfldn a1 @ ms)
 || ins (Mmovedf RFP0 a2 .ms) = 
	ins (mdfstn a2 . ms)
 || ins (Mmovedf a1 a2.ms) = 
	ins (mdfld a1 . mdfst a2 . ms)

 || ins (Mmovesf (fconst f) a2.ms) = 
	let (a1, ns) = msfconst true f in
	ins (ns @ Mmovesf a1 a2 . ms)
 || ins (Mmovesf a1 pushV.ms) =
	ins (Mmove (Vrel (-1)) Vp . Mmovesf a1 (Vind 0) . ms)
 || ins (Mmovesf popV a2.ms) =
	ins (Mmovesf (Vind 0) a2 . Mmove (Vrel 1) Vp . ms)
 || ins (Mmovesf a1 RFP0 .ms) = 
	ins (msfldn a1 @ ms)
 || ins (Mmovesf RFP0 a2 .ms) = 
	ins (msfstn a2 . ms)
 || ins (Mmovesf a1 a2.ms) = 
	ins (msfld a1 . msfst a2 . ms)

 || ins (Mcomparedf a1 pushV.ms) =
	ins (Mmove (Vrel (-2)) Vp . Mcomparedf a1 (Vind 0) . ms)
 || ins (Mcomparedf popV a2.ms) =
	ins (Mcomparedf (Vind 0) a2 . Mmove (Vrel 2) Vp . ms)
 || ins (Mcomparedf (fconst f) a2.ms) = 
	let (a1, ns) = mdfconst false f in
	ins (ns @ Mcomparedf a1 a2 . ms)
 || ins (Mcomparedf a1 (fconst f).ms) = 
	let (a2, ns) = mdfconst false f in
	ins (ns @ Mcomparedf a1 a2 . ms)
 || ins (Mcomparedf RFP0 a2.ms) =
	ins (Masm "\tfcoml\t^" [a2] . 
	     fccxf @
	     ms )
 || ins (Mcomparedf a1 RFP0.ms) =
	ins (mdfld a1 .			-- load a1
--	     mdfld rst1 .		-- dup a2 = previous %st0
	     Masm "\tfcompp" [] . 	-- compare and pop 2
	     fccxf @
	     ms )
 || ins (Mcomparedf a1 a2.ms) = 
	ins (mdfld a1 .
	     Masm "\tfcompl\t^" [a2] . 
	     fccxf @
	     ms )

 || ins (Mcomparesf a1 pushV.ms) =
	ins (Mmove (Vrel (-1)) Vp . Mcomparesf a1 (Vind 0) . ms)
 || ins (Mcomparesf popV a2.ms) =
	ins (Mcomparesf (Vind 0) a2 . Mmove (Vrel 1) Vp . ms)
 || ins (Mcomparesf (fconst f) a2.ms) = 
	let (a1, ns) = msfconst false f in
	ins (ns @ Mcomparesf a1 a2 . ms)
 || ins (Mcomparesf a1 (fconst f).ms) = 
	let (a2, ns) = msfconst false f in
	ins (ns @ Mcomparesf a1 a2 . ms)
 || ins (Mcomparesf RFP0 a2.ms) =
	ins (Masm "\tfcoms\t^" [a2] . 
	     fccxf @
	     ms )
 || ins (Mcomparesf a1 RFP0.ms) =
	ins (msfld a1 .			-- load a1
--	     msfld rst1 .		-- dup a2 = previous %st0
	     Masm "\tfcompp" [] . 	-- compare and pop 2
	     fccxf @
	     ms )
 || ins (Mcomparesf a1 a2.ms) = 
	ins (msfld a1 .
	     Masm "\tfcomps\t^" [a2] . 
	     fccxf @
	     ms )

 || ins (Madda a1 hp . ms) = ins (Madda a1 Hpreg . ms)
 || ins (Madda (i as reg _) (a as reg _).ms) =
        "\tleal\t("@amode a (","@amode i (",4)," @ amode a ("\n" @ ins ms)))

 || ins (Mop2 op a1 pushV.ms) & (dfdst op) =
	ins (Mmove (Vrel (-2)) Vp . Mop2 op a1 (Vind 0) . ms)
 || ins (Mop2 op a1 pushV.ms) & (sfdst op | op = sftoi | op = dftoi) =
	ins (Mmove (Vrel (-1)) Vp . Mop2 op a1 (Vind 0) . ms)
 || ins (Mop2 op popV a2.ms) & (dfsrc op) =
	ins (Mop2 op (Vind 0) a2 . Mmove (Vrel 2) Vp . ms)
 || ins (Mop2 op popV a2.ms) & (sfsrc op | op = itosf | op = itodf) =
	ins (Mop2 op (Vind 0) a2 . Mmove (Vrel 1) Vp . ms)
 || ins (Mop2 op (fconst f) a2.ms) =
	let (a1, ns) = if sfsrc op then msfconst false f else mdfconst false f in
	ins (ns @ Mop2 op a1 a2 . ms)

 || ins (Mop2 dfneg RFP0 RFP0 . ms) =
	ins (Masm "\tfchs" [] . ms)
 || ins (Mop2 dfneg RFP0 a2.ms) =
	ins (mdfld rst0 . Masm "\tfchs" [] . mdfst a2 . ms)
 || ins (Mop2 dfneg a1 RFP0.ms) =
	ins (mdfldn a1 @ Masm "\tfchs" [] . ms)
 || ins (Mop2 dfneg a1 a2.ms) =
	ins (mdfld a1 . Masm "\tfchs" [] . mdfst a2 . ms)
 || ins (Mop2 itodf a1 RFP0 . ms) = ins (mfpop . mfldi a1 @ ms)
 || ins (Mop2 itodf a1 a2 . ms) = ins (mfldi a1 @ mdfst a2 . ms)
 || ins (Mop2 dftoi RFP0 a2 . ms) = ins (mdfld rst0 . savemode . trmode . mfsti a2 @ [resmode] @ ms)
 || ins (Mop2 dftoi a1 a2 . ms) = ins (mdfld a1 . savemode . trmode . mfsti a2 @ [resmode] @ ms)

 || ins (Mop2 sfneg RFP0 RFP0 . ms) =
	ins (Masm "\tfchs" [] . ms)
 || ins (Mop2 sfneg RFP0 a2.ms) =
	ins (msfld rst0 . Masm "\tfchs" [] . msfst a2 . ms)
 || ins (Mop2 sfneg a1 RFP0.ms) =
	ins (msfldn a1 @ Masm "\tfchs" [] . ms)
 || ins (Mop2 sfneg a1 a2.ms) =
	ins (msfld a1 . Masm "\tfchs" [] . msfst a2 . ms)

 || ins (Mop2 itosf a1 RFP0 . ms) = ins (mfpop . mfldi a1 @ ms)
 || ins (Mop2 itosf a1 a2 . ms) = ins (mfldi a1 @ msfst a2 . ms)
 || ins (Mop2 sftoi RFP0 a2 . ms) = ins (msfld rst0 . mfsti a2 @ ms)
 || ins (Mop2 sftoi a1 a2 . ms) = ins (msfld a1 . mfsti a2 @ ms)

 || ins (Mop2 sftodf RFP0 RFP0.ms) = ins ms
 || ins (Mop2 sftodf RFP0 a2.ms) = ins (mdfst a2 . ms)
 || ins (Mop2 sftodf a1 RFP0.ms) = ins (mfpop . msfld a1 .ms)
 || ins (Mop2 sftodf a1 a2.ms) = ins (msfld a1 . mdfst a2 . ms)

 || ins (Mop2 dftosf RFP0 RFP0.ms) = ins ms
 || ins (Mop2 dftosf RFP0 a2.ms) = ins (msfst a2 . ms)
 || ins (Mop2 dftosf a1 RFP0.ms) = ins (mfpop . mdfld a1 .ms)
 || ins (Mop2 dftosf a1 a2.ms) = ins (mdfld a1 . msfst a2 . ms)

 || ins (Mop2 op RFP0 RFP0.ms) =
	ins (fop op rst0 . ms)
 || ins (Mop2 op a1 RFP0.ms) =
	ins (fop op a1 . ms)
 || ins (Mop2 op RFP0 a2.ms) =
	ins (mXfld op a2 . fop op rst1 . mXfst op a2 . ms)
 || ins (Mop2 op a1 a2.ms) & (dfsrc op | sfsrc op | dfdst op | sfdst op) =
	ins (mXfld op a2 . fop op a1 . mXfst op a2 . ms)

-- Ordinary insns
 || ins (Mmove a Vp.c) = ins (Mmove a Vpreg.c)
 || ins (Mmove a Sp.c) = ins (Mmove a Spreg.c)
 || ins (Mmove a hp.c) = ins (Mmove a Hpreg.c)
 || ins (Mmove Vp a.c) = ins (Mmove Vpreg a.c)
 || ins (Mmove Sp a.c) = ins (Mmove Spreg a.c)
 || ins (Mmove hp a.c) = ins (Mmove Hpreg a.c)

 || ins (Mmove a pushS.c) = ins (Mmove (Srel(-1)) Sp. Mmove a (Sind 0). c)
 || ins (Mmove popS a.c) = ins (Mmove (Sind 0) a.Mmove (Srel 1) Sp.c)
#if 0
 || ins (Mmove (const 0) a2.Mop2 sub a1 a2'.c) & (a2=a2') =
	move a1 a2 ("\tnegl\t" @ amode1 a2 (ins c))
#endif

 || ins (Mmove (hprel i)    a.c) = lea (regind Hpr i) a (ins c)
 || ins (Mmove (Vrel  i)    a.c) = lea (regind Vpr i) a (ins c)
 || ins (Mmove (Srel  i)    a.c) = lea (regind Spr i) a (ins c)
 || ins (Mmove (regrel r i) a.c) = lea (regind r i) a (ins c)

 || ins (Mmove popV pushV.c) = ins c
 || ins (Mmove a pushV.c) = push a (ins c)
 || ins (Mmove popV a.c) =  pop  a (ins c)

 || ins (Mmove a1 a2.c) = move a1 a2 (ins c)
 || ins (Mcalltag t r.c) = "\tcall\t*"@otag t ("("@re r (")\n" @ ins c))
 || ins (Mjumptag t r.c) = "\tjmp\t*"@otag t ("("@re r (")\n" @ ins c))
 || ins (Mjump l.c) = "\tjmp\t" @ l @ '\n'.ins c
 || ins (Mjumpf l.c) = "\tjmp\t"@ l @ PLTtag @"\n"@ins c
 || ins (Mjumpind a.c) = "\tjmp\t*"@amode a (ins c)
 || ins (Mcall a.c) = "\tcall\t" @ a @ PLTtag @ '\n'.ins c
 || ins (Mcallind a.c) = "\tcall\t*"@amode a (ins c)
 || ins (Mreturn.c) = "\tret\n" @ ins c
 || ins (Mjcond cc l.c) = '\t'. ojcc cc ('\t'.l @ "\n" @ ins c)
 || ins (Mlabel l.c) = drop_ l @ ":\n" @ ins c
 || ins (Mcompare a1 a2.c) = comp a1 a2 (ins c)

 || ins (Mop3 _ _ _ _._) = fail "3-op arith"

 || ins (Mop2 op a1 pushV.c) & (op = neg | op = btcompl) = 
    push a1 ('\t'.oop op ('\t'.amode1 (Vind 0) (ins c)))
 || ins (Mop2 op a1 a2.c) & (op = neg | op = btcompl) = 
    move a1 a2 ('\t'.oop op ('\t'.amode1 a2 (ins c)))

 || ins (Mop2 op a1 a2.c) & (op = btlsh | op = btrsh | op = btrsha) =
    case a1 in
	const k : '\t'.oop op ("\t$"@itos k@","@amode a2 "\n") @ ins c
    ||  _ :
	if a2 = rcx then
	    push rcx (
	    ins [Mop2 op (case a1 in Vind i : Vind (i+1) || _ : a1 end) (Vind 0)] @
	    pop rcx 
	    (ins c))
	else
	    let (pu, po, a2') = if a1 = rcx then ("",                        "",         a2)
		                            else (push rcx (move a1 rcx ""), pop rcx "", case a2 in
						                                            Vind i : Vind (i+1) 
											 || regind RCX _ : fail "bitshift"
											 || _ : a2 
											 end)
	    in  pu @
		('\t'.oop op ("\t%cl,"@amode a2' "\n")) @
		po @
                (ins c)
    end

-- div&mod could be better
 || ins (Mop2 div a1 a2.c) =
		(if a2 = rax then "" else ins [Mmove a2 rax]) @
		(if a2 = rdx then "" else push rdx "") @
		(if isbad a1 then push rcx (move a1 rcx "") else "") @
		("\tcltd\n"@
		("\tidivl\t"@amode1 (if isbad a1 then rcx else a1)
		(if isbad a1 then pop rcx "" else "") @
		((if a2 = rax then "" else ins [Mmove rax a2]) @
		(if a2 = rdx then "" else pop rdx "") @
		(ins c))))
 || ins (Mop2 mod a1 a2.c) =
		(if a2 = rax then "" else ins [Mmove a2 rax]) @
		(if a2 = rdx then "" else push rdx "") @
		(if isbad a1 then push rcx (move a1 rcx "") else "") @
		("\tcltd\n"@
		("\tidivl\t"@amode1 (if isbad a1 then rcx else a1)
		(if isbad a1 then pop rcx "" else "") @
		(if a2 = rdx then "" else move rdx a2 (pop rdx "")) @
		(ins c)))

 || ins (Mop2 mul a1 (a2 as reg _).c) = aop mul a1 a2 (ins c)
#ifdef USEDREG
 || ins (Mop2 mul a1 a2.c) = move a2 dreg (aop mul a1 dreg (move dreg a2 (ins c)))
#else
 || ins (Mop2 mul a1 a2.c) = let dr = unusedreg [a1; a2] in xchg a2 dr (aop mul a1 dr (xchg dr a2 (ins c)))
#endif

 || ins (Mop2 op a1 a2.c) = aop op a1 a2 (ins c)

 || ins (Mboolcc cc (a as reg r).c) =
	'\t'.scc cc ('\t'.bytereg r@"\n"@
	"\tandl\t" @ amode2 (const 1) a (
	ins c))
 || ins (Mboolcc cc a.c) = let dr = unusedreg [a] in ins (Mexch dr a.Mboolcc cc dr. Mexch dr a.c)

 || ins (Mcase a l h _ ls x.c) =
 	let t = 'L'.itos x in
 	move a rax
	(let r = comp rax (const(h-l))
("\tja\t"@t@"_2\n\tjmp\t*"@t@"_1(,%eax,4)\n"
@t@"_1:\n")
	@ itlist (\l.\r."\t.long\t"@l@"\n"@r) ls (t@"_2:\n"@ins c)
	in
	if l = 0 then r else (aop sub (const l) rax r))

 || ins (Mnoop.c) = ins c
 || ins (Mdata.c) = "\t.data\n" @ ins c
 || ins (Mtext.c) = "\t.text\n" @ ins c
 || ins (Mword (glob  a).c) = "\t.long\t" @ drop_ a @ "\n" @ ins c
 || ins (Mword (idlit a).c) = "\t.long\t" @ drop_ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.long\t" @ itos i @ "\n" @ ins c
 || ins (Mdfloat s.c) = "\t.double\t"@fltpref "0D"@s@"\n" @ ins c
 || ins (Msfloat s.c) = "\t.float\t"@fltpref "0F"@s@"\n" @ ins c
#if 0
 || ins (Mstring s.c) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t2\n"@ins c)
#else
 || ins (Mstring s.c) =
        (if Solaris then "\t.string\t\""
        else "\t.asciz\t\"") @ qstring s @ "\"\n\t.align\t"@alignN@"\n" @ ins c
#endif
 || ins (Mexport a.c) = "\t.globl\t" @ drop_ a @ "\n" @ ins c
 || ins (Mcom s.c) = "/ " @ s @ "\n" @ ins c
 || ins (Mpragma s.c) = ins c
 || ins (Masm s l.c) = aspr s l @ "\n" @ ins c
 || ins (Malign.c) = "\t.align\t"@alignN@"\n" @ ins c
 || ins (Mfunbegin n _.c) = (if PICflag then PICprologue n else "")@ins c
 || ins (Mfunend .c) = ins c

 || ins [] = []
 || ins (m._) = fail ("ins: strange Mcode " @ mprint [m])

and PICprologue n = 
     let lbl = "LL_"@n in
     "\tcall\t"@n@"\n"@n@":\n\tpopl\t"@GOTreg@"\n\taddl\t$_GLOBAL_OFFSET_TABLE_+[.-"@lbl@"],"@GOTreg@"\n"
and GOTreg = "%%"
and PLTtag = if PICflag then "@PLT" else ""

and PICflag = false

and prel = ".file \""@realname@"\"\n.set oeval,0\n.set ounwind,4\n.set ojfun,8\n.set ogettag,12\n"
and assemblercode m = prel @ ins (mtrans m)

end
