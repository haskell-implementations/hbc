module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/qstring.t"

export assemblercode, Aregs, Dregs, Fregs, usecase, use3op, dfloatsize, sfloatsize,
	argcreg, tagreg, bigeqreg, indreg, cputype, farcase, retfixup;
rec
    Aregs = count 1 4 @ fail "Out of A-regs"
and Dregs = count 10 15 @ fail "Out of D-regs"
and Fregs = map (\x.x+16) [0..maxfreg-1] @ fail "Out of F-regs"
and maxfreg = 7
and dfloatsize = 2
and sfloatsize = 1
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*4
and farcase = false
and use3op = false
and retfixup = []
and argcreg = reg 10
and tagreg  = reg 10
and bigeqreg= reg 10
and indreg  = reg 4
and cputype = "MC680x0"

and rd0 = reg 8
and rd1 = reg 9

and re r c = (if r < 8 then 'a'.itos r 
              else if r < 16 then 'd'.itos(r-8)
	      else 'f'.'p'.itos(r-16)) @ c

and otag oeval c = "oeval" @ c
 || otag ounwind c = "ounwind" @ c
 || otag ojfun c = "ojfun" @ c
 || otag ogettag c = "ogettag" @ c
 || otag (onumtag i) c = itos (4*i) @ c
 
and scc eq c = "seq" @ c
 || scc ne c = "sne" @ c
 || scc lt c = "slt" @ c
 || scc gt c = "sgt" @ c
 || scc le c = "sle" @ c
 || scc ge c = "sge" @ c
 || scc dfeq c = "fseq" @ c
 || scc dfne c = "fsne" @ c
 || scc dflt c = "fslt" @ c
 || scc dfgt c = "fsgt" @ c
 || scc dfle c = "fsle" @ c
 || scc dfge c = "fsge" @ c
 || scc sfeq c = "fseq" @ c
 || scc sfne c = "fsne" @ c
 || scc sflt c = "fslt" @ c
 || scc sfgt c = "fsgt" @ c
 || scc sfle c = "fsle" @ c
 || scc sfge c = "fsge" @ c
 
and ojcc eq c = "jeq" @ c
 || ojcc ne c = "jne" @ c
 || ojcc lt c = "jlt" @ c
 || ojcc gt c = "jgt" @ c
 || ojcc le c = "jle" @ c
 || ojcc ge c = "jge" @ c
 || ojcc ltstack c = "jcs" @ c
 || ojcc ltheap c = "jcs" @ c
 || ojcc gtstack c = "jhi" @ c
 || ojcc geheap c = "jcc" @ c
 || ojcc dfeq c = "fjeq" @ c
 || ojcc dfne c = "fjne" @ c
 || ojcc dflt c = "fjlt" @ c
 || ojcc dfgt c = "fjgt" @ c
 || ojcc dfle c = "fjle" @ c
 || ojcc dfge c = "fjge" @ c
 || ojcc sfeq c = "fjeq" @ c
 || ojcc sfne c = "fjne" @ c
 || ojcc sflt c = "fjlt" @ c
 || ojcc sfgt c = "fjgt" @ c
 || ojcc sfle c = "fjle" @ c
 || ojcc sfge c = "fjge" @ c
 || ojcc x    c = fail ("ccname "@mprint [Mjcond x ""])
 
and oop add c = "addl" @ c
 || oop sub c = "subl" @ c
 || oop mul c = "mulsl" @ c
 || oop div c = "divsl" @ c
 || oop mod c = fail "mod"
 || oop neg c = "negl" @ c
 || oop btcompl c = "notl" @ c
 || oop btand c = "andl" @ c
 || oop btor c = "orl" @ c
 || oop btxor c = "eorl" @ c
 || oop btlsh c = "lsll" @ c
 || oop btrsh c = "lsrl" @ c
 || oop btrsha c = "asrl" @ c
and fopsuf _ (reg _) = "x"
||  fopsuf op _ = if sfsrc op then "s" else "d"
and fop dfadd = "fadd"
 || fop dfsub = "fsub"
 || fop dfmul = "fmul"
 || fop dfdiv = "fdiv"
 || fop dfneg = "fneg"
 || fop sfadd = "fadd"
 || fop sfsub = "fsub"
 || fop sfmul = "fsglmul"
 || fop sfdiv = "fsgldiv"
 || fop sfneg = "fneg"
and sfsrc op = mem op [sftoi; sftodf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and sfdst op = mem op [itosf; dftosf; sfneg; sfadd; sfsub; sfmul; sfdiv]
and dfsrc op = mem op [dftoi; dftosf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and dfdst op = mem op [itodf; sftodf; dfneg; dfadd; dfsub; dfmul; dfdiv]
and fsrc op = sfsrc op | dfsrc op
and fdst op = sfdst op | dfdst op
and dfop op = dfsrc op | dfdst op
and sfop op = sfsrc op | sfdst op
and xfop op = dfop op | sfop op
and 
    amode (Vp) c = "sp" @ c
 || amode (Vind 0) c = "sp@" @ c
 || amode (Vind i) c = "sp@(" @ itos (4*i) @ ")" @ c
 || amode (Vrel _) c = fail "amode Srel\n"
 || amode (pushV) c = "sp@-" @ c
 || amode (popV) c = "sp@+" @ c
 
 || amode (Sp) c = "a6" @ c
 || amode (Sind 0) c = "a6@" @ c
 || amode (Sind i) c = "a6@(" @ itos (4*i) @ ")" @ c
 || amode (Srel _) c = fail "amode Srel\n"
 || amode (pushS) c = "a6@-" @ c
 || amode (popS) c = "a6@+" @ c
 
 || amode (hp) c = "a5" @ c
 || amode (hpind 0) c = "a5@" @ c
 || amode (hpind i) c = "a5@(" @ itos (4*i) @ ")" @ c
 || amode (hprel i) c = fail "amode hprel\n"
 || amode (tohp) c = "a5@+" @ c
 
 || amode (reg i) c = re i c
 || amode (regind r 0) c = re r ('@' . c)
 || amode (regind r i) c = re r ("@(" @ itos(4*i) @ (')'.c))
 || amode (regrel _ _) c = fail "amode regrel\n"

 || amode (glob i) c = i @ c
 || amode (idlit s) c = '#'.s @ c
 || amode (retaddr s) c = '#'.s @ c
 || amode (const n) c = '#'.itos n @ c

 || amode (fconst f) c = "!!!"@ftos f@c

 || amode a c = fail ("amode: "@mprint [Mmove a a])

and unpush pushV = Vind 0
||  unpush a     = a

and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and bmode1 tohp  = hpind 0
||  bmode1 a     = a
and bmode2 pushS = Sind 0
||  bmode2 pushV = Vind 0
||  bmode2 a     = a
and movea m i a c = move m (bmode1 a) (aaop add (const (4*i)) (bmode2 a) (ins c))
-- check if an operand combination is permissible on the stupid 68000
and ok1 Sp        = true
||  ok1 Vp        = true
||  ok1 hp        = true
||  ok1 (const _) = true
||  ok1 (idlit _) = true
||  ok1 (retaddr _) = true
||  ok1 (reg _)   = true
||  ok1 _         = false
and cmpok1 (const _) = true
||  cmpok1 (idlit _) = true
||  cmpok1 (retaddr _) = true
||  cmpok1 _         = false
and cmpok2 Sp        = true
||  cmpok2 Vp        = true
||  cmpok2 hp        = true
||  cmpok2 (reg _)   = true
||  cmpok2 _         = false
and okop a1 a2 = ok1 a1 | ok1 a2
and amode1 a1 c = amode a1 ('\n'.c)
and amode2 a1 a2 c = amode a1 (','.amode a2 ('\n'.c))
and move (const 0) a2 c = "\tclrl\t"@amode1 a2 c
||  move (a1 as const k) (a2 as reg _) c & (-128 <= k & k <= 127) =
	"\tmoveq\t" @ amode2 a1 a2 c
||  move a1 a2 c = "\tmovl\t" @ amode2 a1 a2 c
and isreg (reg _) = true
||  isreg _ = false
and add4 (regind r k) = regind r (k+1)
||  add4 (Vind k) = Vind (k+1)
||  add4 (Sind k) = Sind (k+1)
||  add4 (glob s) = glob (s@"+4")
||  add4 popV = popV
||  add4 tohp = tohp
||  add4 a = fail ("add4 "@mprint [Mmove a a])
and fcon b (fconst f) l =
	((if b then
	    "\t.even\n\t.data\n"@l@":\t.single\t0r"@fmtf  ".8e" f@"\n\t.text\n"
	else
	    "\t.even\n\t.data\n"@l@":\t.double\t0r"@fmtf ".17e" f@"\n\t.text\n"), glob (l@"b"))
||  fcon _ a _ = ("", a)
and movedf a1 a2 c = 
    if isreg a1 | isreg a2 then
	"\tfmoved\t" @ amode2 a1 a2 c
    else if a2 = pushV then
	move (add4 a1) a2 (move a1 a2 c)
    else
	move a1 a2 (move (add4 a1) (add4 a2) c)
and movesf a1 a2 c = 
    if isreg a1 | isreg a2 then
	"\tfmoves\t" @ amode2 a1 a2 c
    else
	move a1 a2 c
and tmpf = reg (maxfreg+16)
and sf2df (reg _) (reg _) c = c
||  sf2df (a1 as reg _) a2 c = movedf a1 a2 c
||  sf2df a1 (a2 as reg _) c = movesf a1 a2 c
and df2sf (reg _) (reg _) c = c
||  df2sf (a1 as reg _) a2 c = movesf a1 a2 c
||  df2sf a1 (a2 as reg _) c = movedf a1 a2 c
and i2df a1 (a2 as reg _) c = "\tfmovel\t" @ amode2 a1 a2 c
||  i2df a1 a2 c = "\tfmovel\t" @ amode2 a1 tmpf (movedf tmpf a2 c)
and i2sf a1 (a2 as reg _) c = "\tfmovel\t" @ amode2 a1 a2 c
||  i2sf a1 a2 c = "\tfmovel\t" @ amode2 a1 tmpf (movesf tmpf a2 c)
and f2i op a1 a2 c = "\tfintrz"@fopsuf op a1@"\t" @ amode2 a1 tmpf ("\tfmovel\t"@amode2 tmpf a2 c)
-- could handle the case when a2 is a reg better by swapping
and compf b a1 a2 c =
	let (s1, a1') = fcon b a1 "1"
        and (s2, a2') = fcon b a2 "2" in
	let (s3, a1'') = if isreg a1' then ("", a1') else ((if b then movesf else movedf) a1' tmpf "", tmpf)
        in  
        s1@s2@s3@
        "\tfcmp"@(if isreg a1'' & isreg a2' then "x" else if b then "s" else "d")@"\t"@
        amode2 a2' a1'' (ins c)
and comp a1 a2 c = "\tcmpl\t" @ amode2 a1 a2 c
and lea a1 a2 c = "\tlea\t" @ amode2 a1 a2 c
and pea a1 c = "\tpea\t" @ amode1 a1 c
and aaop op (a1 as const n) a2 c & (1 <= n & n <= 8) =
	(if op = add then "\taddql\t" else "\tsubql\t") @ amode2 a1 a2 c
||  aaop op a1 a2 c = aop op a1 a2 c
and aop op a1 a2 c & (okop a1 a2) = '\t'.oop op ('\t'.amode2 a1 a2 c)
||  aop op a1 a2 c = move a1 rd0 ('\t'.oop op ('\t'.amode2 rd0 a2 c))
and
    ins (Mmove (hprel i) hp.c) = aaop add (const(4*i)) hp (ins c)
 || ins (Mmove (hprel i) (a as reg _).c) = lea (hpind i) a (ins c)
 || ins (Mmove (hprel i) pushV.c) = pea (hpind i) (ins c)
 || ins (Mmove (hprel i) a.c) = movea hp i a c

 || ins (Mmove (Vrel i) Vp.c) = aaop add (const(4*i)) Vp (ins c)
 || ins (Mmove (Vrel i) (a as reg _).c) = lea (Vrel i) a (ins c)
 || ins (Mmove (Vrel i) pushV.c) = pea (Vrel i) (ins c)
 || ins (Mmove (Vrel i) a.c) = movea Vp i a c

 || ins (Mmove (Srel i) Sp.c) = aaop add (const(4*i)) Sp (ins c)
 || ins (Mmove (Srel i) (a as reg _).c) = lea (Sind i) a (ins c)
 || ins (Mmove (Srel i) pushV.c) = pea (Sind i) (ins c)
 || ins (Mmove (Srel i) a.c) = movea Sp i a c

 || ins (Mmove (regrel r i) (a as reg _).c) = lea (regind r i) a (ins c)
 || ins (Mmove (regrel r i) pushV.c) = pea (regind r i) (ins c)
 || ins (Mmove (regrel r i) a.c) = movea (reg r) i a c

 || ins (Mmovedf a1 a2.c) = 
	let (s, a1') = fcon false a1 "1"
	in  s@movedf a1' a2 (ins c)
 || ins (Mmovesf a1 a2.c) = 
	let (s, a1') = fcon true a1 "1"
	in  s@movesf a1' a2 (ins c)
 || ins (Mop2 sftodf a1 a2.c) = sf2df a1 a2 (ins c)
 || ins (Mop2 dftosf a1 a2.c) = df2sf a1 a2 (ins c)
 || ins (Mop2 itodf a1 a2.c) = i2df a1 a2 (ins c)
 || ins (Mop2 itosf a1 a2.c) = i2sf a1 a2 (ins c)
 || ins (Mop2 dftoi a1 a2.c) = f2i dftoi a1 a2 (ins c)
 || ins (Mop2 sftoi a1 a2.c) = f2i sftoi a1 a2 (ins c)
 || ins (Mop2 op a1 a2.c) & (xfop op) = 
    case a2 in
       reg _ :
        let (s, a1') = fcon (sfsrc op) a1 "1"
	in  s@"\t"@fop op@fopsuf op a1@"\t"@amode2 a1' a2 (ins c)
    || _ :
        ins ((if sfdst op then Mmovesf else Mmovedf) a2 tmpf.
	     Mop2 op a1 tmpf.
	     (if sfdst op then Mmovesf else Mmovedf) tmpf a2.
             c)
    end
 || ins (Mcomparesf a1 a2.c) = compf true a1 a2 c
 || ins (Mcomparedf a1 a2.c) = compf false a1 a2 c

 || ins (Mcalltag t r.c) = "\tjsr\t"@re r ("@("@otag t (")@(0)\n" @ ins c))
 || ins (Mjumptag t r.c) = "\tjmp\t"@re r ("@("@otag t (")@(0)\n" @ ins c))
 || ins (Mjump l.c) = "\tjra\t" @ l @ '\n'.ins c
 || ins (Mjumpf l.c) = "\tjmp\t" @ l @ '\n'.ins c
 || ins (Mcall a.c) = "\tjsr\t" @ a @ '\n'.ins c
 || ins (Mreturn.c) = "\trts\n" @ ins c
 || ins (Mjcond cc l.c) = '\t'. ojcc cc ('\t'.l @ "\n" @ ins c)
 || ins (Mlabel l.c) = l @ ":\n" @ ins c
 || ins (Mcompare a1 a2.c) & (cmpok1 a2 | cmpok2 a1) =
	comp a2 a1 (ins c)
 || ins (Mcompare a1 a2.c) =
	move a1 rd0 (
	comp a2 rd0 (ins c))

 || ins (Mop2 mod a1 a2.c) =
	move a2 rd0
	("\tdivsll\t" @ amode a1 (",d1:d0\n" @
	move rd1 a2 (ins c)))
#if 0
 || ins (Mmove (const 0) pushV.Mop2 sub a1 (Vind 0).c) =
	move a1 pushV ("\tnegl\t"@amode1 (Vind 0) (ins c))
 || ins (Mmove (const 0) a2.Mop2 sub a1 a3.c) & (a2=a3) =
	move a1 a2 ("\tnegl\t"@amode1 a2 (ins c))
#endif
 || ins (Mop2 add a1 a2.c) = aaop add a1 a2 (ins c)
 || ins (Mop2 sub a1 a2.c) = aaop sub a1 a2 (ins c)
 || ins (Mop2 op a1 a2.c) & (op=div | op=mul) =
	case a2 in
	   reg _ : aop op a1 a2 (ins c)
	|| _ : move a2 rd1 (aop op a1 rd1 (move rd1 a2 (ins c)))
	end
 || ins (Mop2 op a1 a2.c) & (op = neg | op = btcompl) = move a1 a2 ('\t'.oop op ('\t'.amode1 (unpush a2) (ins c)))
 || ins (Mop2 bop a1 a2.c) & (bop = btxor | bop = btlsh | bop = btrsh | bop = btrsha) = 
        move a1 rd0 (
        aop bop rd0 a2 (ins c))
 || ins (Mop2 op a1 a2.c) = aop op a1 a2 (ins c)
 
 || ins (Mmove a1 a2.c) = move a1 a2 (ins c)

 || ins (Madda a1 a2.c) = move a1 rd0 (aop btlsh (const 2) rd0 (aop add rd0 a2 (ins c)))

-- It might be better to do a 'clr r; scc r; negb r'.
 || ins (Mboolcc cc (a as reg r).c) =
	'\t'.scc cc ('\t'.amode1 a (
	"\tandl\t" @ amode2 (const 1) a (
	ins c)))
 || ins (Mboolcc cc a.c) = ins (Mboolcc cc rd0.Mmove rd0 a.c)
 || ins (Mcase a l h _ ls x.c) =
 	let t = 'L'.itos x in
 	move a rd0
	(let r = comp (const(h-l)) rd0
("\tjhi\t"@t@"2\n\tmovw\tpc@(6,d0:l:2),d0\n\tjmp\tpc@(2,d0:w)\n"
@t@"1:\n")
	@ itlist (\l.\r."\t.word\t"@l@"-"@t@"1\n"@r) ls (t@"2:\n"@ins c)
	in
	if l = 0 then r else (aop sub (const l) rd0 r))
 || ins (Mnoop.c) = ins c
 || ins (Mdata.c) = "\t.data\n" @ ins c
 || ins (Mtext.c) = "\t.text\n" @ ins c
 || ins (Mword (glob  a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (idlit a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.long\t" @ itos i @ "\n" @ ins c
 || ins (Mdfloat s.c) = "\t.double\t0r"@fmtf ".17e" (stof s)@"\n"@ins c
 || ins (Msfloat s.c) = "\t.single\t0r"@fmtf ".8e" (stof s)@"\n"@ins c
#if 0
 || ins (Mstring s.c) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.even\n"@ins c)
#else
 || ins (Mstring s.c) = "\t.asciz\t\"" @ qstring s @ "\"\n\t.even\n" @ ins c
#endif
 || ins (Mexport a.c) = "\t.globl\t" @ a @ "\n" @ ins c
 || ins (Mcom s.c) = "| " @ s @ "\n" @ ins c
 || ins (Mpragma s.c) = ins c
 || ins (Malign.c) = "\t.even\n" @ ins c
 || ins (Masm s l.c) = aspr s l @ "\n" @ ins c
 || ins (Mfunbegin _ _.c) = ins c
 || ins (Mfunend .c) = ins c
 || ins [] = []
 || ins (m._) = fail ("ins: strange Mcode " @ mprint [m])

and prel = "oeval=0\nounwind=4\nojfun=8\nogettag=12\n"

and assemblercode m = prel @ ins m

end
