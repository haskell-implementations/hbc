module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
/* (at least) old 780 do addl3 faster than moval */
#define OLD780 1

export assemblercode, Aregs, Dregs, usecase, use3op,
	argcreg, tagreg, bigeqreg, indreg, cputype;
rec
    Aregs = count 1 7 @ fail "Out of A-regs"
and Dregs = count 1 7 @ fail "Out of D-regs"
-- cnt>4 saves time (on a VAX780)
-- (high-low)*2+5<5*cnt saves space if high<64
-- (high-low)*2+5<9*cnt saves space if high>64
and usecase max low high cnt = cnt>=4 & (high-low)*2<cnt*9
and use3op = true
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg 9
and cputype = "VAX"

and
    re r c = 'r' . itos r @ c

and otag oeval c = "oeval" @ c
 || otag ounwind c = "ounwind" @ c
 || otag ojfun c = "ojfun" @ c
 || otag ogettag c = "ogettag" @ c
 || otag (onumtag i) c = itos (4*i) @ c
 
and ojcc eq c = "jeql" @ c
 || ojcc ne c = "jneq" @ c
 || ojcc lt c = "jlss" @ c
 || ojcc gt c = "jgtr" @ c
 || ojcc le c = "jleq" @ c
 || ojcc ge c = "jgeq" @ c
 || ojcc ltstack c = "jlssu" @ c
 || ojcc ltheap c = "jlssu" @ c
 || ojcc gtstack c = "jgtru" @ c
 || ojcc geheap c = "jgequ" @ c
 
and oop add c = "addl3" @ c
 || oop sub c = "subl3" @ c
 || oop mul c = "mull3" @ c
 || oop div c = "divl3" @ c
 || oop neg c = "mnegl" @ c
 || oop btor c = "orl3" @ c
 || oop btxor c = "xorl3" @ c
 || oop btlsh c = "ashl" @ c
 || oop btcompl c = "mcoml" @ c

and oop2 add c = "addl2" @ c
 || oop2 sub c = "subl2" @ c
 || oop2 mul c = "mull2" @ c
 || oop2 div c = "divl2" @ c
 || oop2 btor c = "orl2" @ c
 || oop2 btxor c = "xorl2" @ c
 
and 
    amode (Vp) c = "sp" @ c
 || amode (Vind i) c = itos (4*i) @ "(sp)" @ c
 || amode (Vrel _) c = fail "amode Srel\n"
 || amode (pushV) c = "-(sp)" @ c
 || amode (popV) c = "(sp)+" @ c
 
 || amode (Sp) c = "%ep" @ c
 || amode (Sind i) c = itos (4*i) @ "(%ep)" @ c
 || amode (Srel _) c = fail "amode Srel\n"
 || amode (pushS) c = "-(%ep)" @ c
 || amode (popS) c = "(%ep)+" @ c
 
 || amode (hp) c = "%hp" @ c
 || amode (hpind i) c = itos (4*i) @ "(%hp)" @ c
 || amode (hprel i) c = fail "amode hprel\n"
 || amode (tohp) c = "(%hp)+" @ c
 
 || amode (reg i) c = re i c
 || amode (regind r i) c = itos(4*i) @ "(" @ re r (')'.c)
 || amode (regrel _ _) c = fail "amode regrel\n"

 || amode (glob i) c = i @ c
 || amode (idlit s) c = '$'.s @ c
 || amode (retaddr s) c = '$'.s @ c
 || amode (const n) c = '$'.itos n @ c
 
and amode1 a1 c = amode a1 ('\n'.c)
and amode2 a1 a2 c = amode a1 (','.amode a2 ('\n'.c))
and amode3 a1 a2 a3 c = amode a1 (','.amode a2 (','.amode a3 ('\n'.c)))

and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and tof s = if mem '.' s | mem 'e' s | mem 'E' s then s else s @ ".0"

and
#ifdef OLD780
    ins (Mmove (hprel i) hp.c) = "\taddl2\t" @ amode2 (const(4*i)) hp (ins c)
 || ins (Mmove (hprel i) a.c) = "\taddl3\t" @ amode3 (const(4*i)) hp a (ins c)
 || ins (Mmove (Vrel i) Vp.c) = "\taddl2\t" @ amode2 (const(4*i)) Vp (ins c)
 || ins (Mmove (Vrel i) a.c) = "\taddl3\t" @ amode3 (const(4*i)) Vp a (ins c)
 || ins (Mmove (Srel i) Sp.c) = "\taddl2\t" @ amode2 (const(4*i)) Sp (ins c)
 || ins (Mmove (Srel i) a.c) = "\taddl3\t" @ amode3 (const(4*i)) Sp a (ins c)
 || ins (Mmove (regrel r i) (reg r1).c) & (r = r1) =
	"\taddl2\t" @ amode2 (const(4*i)) (reg r) (ins c)
 || ins (Mmove (regrel r i) a.c) = "\taddl3\t"@amode3 (const(4*i)) (reg r) a (ins c)
#else
    ins (Mmove (hprel i) a.c) = "\tmoval\t" @ amode2 (hpind i) a (ins c)
 || ins (Mmove (Vrel i) a.c) = "\tmoval\t" @ amode2 (Vind i) a (ins c)
 || ins (Mmove (Srel i) a.c) = "\tmoval\t" @ amode2 (Sind i) a (ins c)
 || ins (Mmove (regrel r i) a.c) = "\tmoval\t"@ amode2 (regrel r i) a (ins c)
#endif
-- following not correct if a2=pop or push, but how could it be?
 || ins ((m as Mmove _ a2).Mcompare a3 (const 0).c) & (a2 = a3) = ins (m.c)
 || ins (Mmove a1 a2.c) = "\tmovl\t" @ amode2 a1 a2 (ins c)
 || ins (Mcalltag t r.c) = "\tjsb\t*" @ otag t ("("@re r (")\n"@ins c))
 || ins (Mjumptag t r.c) = "\tjmp\t*" @ otag t ("("@re r (")\n"@ins c))
 || ins (Mjump l.c) = "\tjbr\t" @ l @ '\n'.ins c
 || ins (Mjumpf l.c) = "\tjmp\t" @ l @ '\n'.ins c
 || ins (Mcall a.c) = "\tjsb\t" @ a @ '\n'.ins c
 || ins (Mreturn.c) = "\trsb\n" @ ins c
 || ins (Mjcond cc l.c) = '\t'. ojcc cc ('\t'.l @ "\n" @ ins c)
 || ins (Mlabel l.c) = l @ ":\n" @ ins c
 || ins (Mcompare a1 (const 0).c) = "\ttstl\t" @ amode1 a1 (ins c)
 || ins (Mcompare a1 a2.c) = "\tcmpl\t" @ amode2 a1 a2 (ins c)

 || ins (Mop2 mod a1 a2.c) = "\temul\t" @ amode a2 (",$1,$0,r8\n" @
 			     "\tediv\t" @ amode a1 (",r8,r8," @ amode1 a2
			     (ins c)))
 || ins (Mop2 op a1 a2.c) & (op = btand | op = btlsh | op = btrsh | op = btrsha) = ins (Mop3 op a1 a2 a2.c)
 || ins (Mop2 op a1 a2.c) = '\t'.oop2 op ('\t'.amode2 a1 a2 (ins c))
 || ins (Mop3 mod popV popV pushV.c) = "\temul\t4(sp),$1,$0,r8\n" @
 				       "\tediv\t(sp)+,r8,r8,(sp)\n" @ ins c
 || ins (Mop3 mod popV popV a3.c) = "\temul\t4(sp),$1,$0,r8\n" @
 				    "\tediv\t(sp)+,r8,(sp)+," @ amode1 a3 
				    (ins c)
 || ins (Mop3 mod a1 a2 a3.c) = "\temul\t" @ amode a2 (",$1,$0,r8\n" @
 				"\tediv\t" @ amode a1 (",r8,r8," @ amode1 a3
				(ins c)))
-- following not correct if a3=popV or pushV, but how could it be?
 || ins ((m as Mop3 _ _ _ a3).Mcompare a4 (const 0).c) & (a3=a4) = ins (m.c)
 || ins (Mop3 sub a1 (const 0) a2.c) = "\tmnegl\t" @ amode2 a1 a2 (ins c)
 || ins (Mop3 btand a1 a2 a3.c) = "\tmcoml\t" @ amode2 a1 (reg 8) ("\tbicl3\t" @ amode3 (reg 8) a2 a3 (ins c))
 || ins (Mop3 btrsh a1 a2 a3.c) = "\tsubl3\t" @ amode3 a1 (const 32) (reg 8) ("\textzv\t" @ amode a1 @ ("," @ amode3 (reg 8) a3 a3 (ins c)))
 || ins (Mop3 op a1 a2 a3.c) = '\t'.oop op ('\t'.amode3 a1 a2 a3 (ins c))
 || ins (Mboolcc cc a.c) =
	'\t'.ojcc cc ("\t1f\n\tmovl\t$0,"@amode a ("\n\tbrb\t2f\n1:\tmovl\t$1,"
	@amode a ("\n2:\n"@ins c)))
 || ins (Mcase a l h _ ls _.c) = "\tcasel\t"@amode3 a (const l) (const(h-l)) 
		("1:\n" @ itlist (\l.\r."\t.word\t"@l@"-1b\n"@r) ls (ins c))
 || ins (Mnoop.c) = ins c
 || ins (Mdata.c) = "\t.data\n" @ ins c
 || ins (Mtext.c) = "\t.text\n" @ ins c
 || ins (Mword (glob  a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (idlit a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.long\t" @ itos i @ "\n" @ ins c
 || ins (Mdfloat s.c) = "\t.double\t0d"@tof s@"\n" @ ins c
 || ins (Mstring s.c) = itlist (\x.\p.
 				  "\t.byte\t" @ itos(ord x)@"\n"@p) s 
				 ("\t.byte\t0\n\t.align\t2\n"@ins c)
 				
 || ins (Mexport a.c) = "\t.globl\t" @ a @ "\n" @ ins c
 || ins (Mcom s.c) = "# " @ s @ "\n" @ ins c
 || ins (Mpragma s.c) = ins c
 || ins (Masm s l.c) = aspr s l @ "\n" @ ins c
 || ins (Malign.c) = "\t.align\t2\n" @ ins c
 || ins (Mfunbegin _ _.c) = ins c
 || ins (Mfunend .c) = ins c
 || ins [] = []
 || ins (m._) = fail ("ins: strange Mcode " @ mprint [m])

and prel = ".set ep,10\n.set hp,11\n.set oeval,0\n.set ounwind,4\n.set ojfun,8\n.set ogettag,12\n"

and assemblercode m = prel @ ins m

end
