module
#include "../misc/flags.t"
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "mtrans.t"
--
-- RTPC 
--
-- Register allocation:
-- real mcod
-- r0	r10	(C: called function data area pointer)
-- r1	r11	Vp, (C: caller's frame pointer)
-- r2	r12	(C: arg0, return value0)
-- r3	r13	(C: arg1, return value1)
-- r4	r14	mytmpreg
-- r5	r15	mytmpreg2
-- r6	r0	mcode register
-- r7	r1	mcode register
-- r8	r2	mcode register
-- r9	r3	mcode register
-- r10	r4	mcode register
-- r11	r5	Sp
-- r12	r6	hp
-- r13	r7	(C: frame pointer)
-- r14	r8	(C: use data area pointer)
-- r15	r9	return adress
--
export assemblercode, Aregs, Dregs, usecase, use3op,
	argcreg, tagreg, bigeqreg, indreg, cputype;
rec
    Aregs = count 1 5 @ fail "Out of A-regs"
and Dregs = count 1 5 @ fail "Out of D-regs"
and usecase max low high cnt = if cnt = 0 then true else false
and use3op = false
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg 3
and cputype = "RT/PC"

and mytmpreg = reg 14
and mytmpreg2 = reg 15
and
    re r c = 'r' . itos ((6+r)%16) @ c

and otag oeval c = "oeval" @ c
 || otag ounwind c = "ounwind" @ c
 || otag ojfun c = "ojfun" @ c
 || otag ogettag c = "ogettag" @ c
 || otag (onumtag i) c = itos (4*i) @ c
 
and ccof eq c = "e" @ c
 || ccof ne c = "ne" @ c
 || ccof lt c = "l" @ c
 || ccof gt c = "h" @ c
 || ccof le c = "le" @ c
 || ccof ge c = "he" @ c
 || ccof ltstack c = "l" @ c
 || ccof ltheap c = "l" @ c
 || ccof gtstack c = "h" @ c
 || ccof geheap c = "he" @ c
 
and oop add c = "a" @ c
 || oop sub c = "s" @ c
 || oop mul c = fail "Multiplicate"
 || oop div c = fail "Divide"
 || oop mod c = fail "Modulo"
 
and 
    amode Vp c = "r1" @ c
 || amode (Vind i) c = itos (4*i) @ "(r1)" @ c
 || amode (Vrel _) c = fail "Vrel"
 || amode pushV c = fail "pushV"
 || amode popV c = fail "popV"
 
 || amode Sp c = "r11" @ c
 || amode (Sind i) c = itos (4*i) @ "(r11)" @ c
 || amode (Srel _) c = fail "Srel"
 || amode pushS c = fail "pushS" 
 || amode popS c = fail "popS"
 
 || amode hp c = "r12" @ c
 || amode (hpind i) c = itos (4*i) @ "(r12)" @ c
 || amode (hprel i) c = fail "hprel"
 || amode tohp c = fail "tohp"
 
 || amode (reg i) c = re i c
 || amode (regind r i) c = itos(4*i) @ "(" @ re r (')'.c)
 || amode (regrel _ _) c = fail "regrel"

 || amode (glob i) c = i @ c
 || amode (idlit s) c = s @ c
 || amode (retaddr s) c = s @ c
 || amode (const n) c = itos n @ c
 || amode x c = fail "no match in amode"
 
and amode1 a1 c = amode a1 ('\n'.c)
and amode2 a1 a2 c = amode a1 (','.amode a2 ('\n'.c))

and aspr ""       ams      = ""
 || aspr ('^'.cs) (am.ams) = amode am (aspr cs ams)
 || aspr ('~'.cs) (am.ams) = amode (asprfix am) (aspr cs ams)
 || aspr ('!'.cs) (am.ams) = amode (regfix am) (aspr cs ams)
 || aspr (c.cs)   ams      = c.aspr cs ams
and asprfix (glob i) = glob (labelfix i)
and labelfix ('_'.t) = "_." @ t
||  labelfix (h.t) = h.t
and clabel ('_'.t) = true
||  clabel (h.t) = false
and regfix (glob n) = reg ((stoi n) + 12)

-- check for conflicting addressing modes, i.e. will destroying a2 destroy
-- the possibility to access a1. (Only used for arithmetic operands.)
and
    conflict (regind r1 _) (reg r2) = r1 = r2
 || conflict a1            a2       = a1 = a2
and bit14 i = i >= -8192 & i <= 8191
and bit16 i = i >= -32768 & i <= 32767
and
    regmove a1 a2 c & (a1=a2) = c
 || regmove a1 a2 c = "\tcas\t" @ amode a2  (','.amode a1  (",r0\n" @ c))
and
    load (Srel i) dst c & (bit14 i) = "\tcal\t" @ amode dst
					("," @ amode (Sind i) ('\n'.c))
 || load (Srel i) dst c = "\tget\t" @ amode dst (",$" @ amode (Sind i) ('\n'.c))
 || load (Vrel i) dst c & (bit14 i) = "\tcal\t" @ amode dst
					("," @ amode (Vind i) ('\n'.c))
 || load (Vrel i) dst c = "\tget\t" @ amode dst (",$" @ amode (Vind i) ('\n'.c))
 || load (hprel i) dst c & (bit14 i) = "\tcal\t" @ amode dst
					("," @ amode (hpind i) ('\n'.c))
 || load (hprel i) dst c = "\tget\t"@ amode dst (",$"@ amode (hpind i) ('\n'.c))
 || load (popS) dst c = "\tls\t" @ amode2 dst (Sind 0)
			("\tinc\t" @ amode Sp (",4\n" @('\n'.c )))
 || load (popV) dst c = "\tls\t" @ amode2 dst (Vind 0)
			("\tinc\t" @ amode Vp (",4\n" @('\n'.c )))
 || load (regrel r i) dst c & (bit14 i) = "\tcal\t" @ amode dst
				("," @ amode (regind r i) ('\n'.c))
 || load (regrel r i) dst c = "\tget\t" @ amode dst
				(",$" @ amode (regind r i) ('\n'.c))
 || load (Vind i) dst c & (bit14 i) = "\tl\t" @ amode dst
				("," @ amode (Vind i) ('\n'.c))
 || load (Sind i) dst c & (bit14 i) = "\tl\t" @ amode dst
				("," @ amode (Sind i) ('\n'.c))
 || load (regind r i) dst c & (bit14 i) = "\tl\t" @ amode dst
				("," @ amode (regind r i) ('\n'.c))
 || load (const i) dst c & (bit16 i) = "\tcal\t" @ amode dst ("," @
			amode (const i) ("(r0)\n" @ c))
 || load (const i) dst c = "\tget\t" @ amode dst (",$" @
			amode (const i) ("\n" @ c))
 || load (idlit s) dst c = "\tget\t" @ amode dst (",$" @
			amode (idlit s) ("\n" @ c))
 || load (retaddr s) dst c = load (idlit s) dst c
 || load src dst c = "\tload\t" @ amode dst ( "," @ amode src ('\n'.c))
and
    store src (pushS) c = "\tdec\t" @ amode2 Sp (const 4)
			("\tsts\t" @ amode2 src (Sind 0) c)
 || store src (pushV) c = "\tdec\t" @ amode2 Vp (const 4)
			("\tsts\t" @ amode2 src (Vind 0) c)
 || store src (tohp) c = "\tsts\t" @ amode2 src (hpind 0)
				("\tinc\t" @ amode hp (",4\n" @ c))
 || store src (Sind i) c & (bit14 i) = "\tst\t" @ amode2 src (Sind i) c
 || store src (Vind i) c & (bit14 i) = "\tst\t" @ amode2 src (Vind i) c
 || store src (regind r i) c & (bit14 i) = "\tst\t" @ amode2 src (regind r i) c
 || store src dst c = "\tstore\t" @ amode src ("," @ amode dst
			("," @ amode (reg 13) ('\n'.c)))
and
    regok (reg i) = true
 || regok (Vp) = true
 || regok (Sp) = true
 || regok (hp) = true
 || regok x = false
and
    regiok (regind r i) = true
 || regiok (Vind i) = true
 || regiok (Sind i) = true
 || regiok (hpind i) = true
 || regiok x = false
and
    opok (add) = true
 || opok (sub) = true
 || opok x = false
and absreg r = ((r+10)%16)
and totmp1(a)&(regok a) = a
 || totmp1(a) = mytmpreg
and totmp2(a)&(regok a) = a
 || totmp2(a) = mytmpreg2
and wordnext (Mword d.c) = true
 || wordnext (Mlabel l.c) = wordnext c
 || wordnext _ = false
and
    ins (Mmove a1 a2.c)&(a1=a2) = "#OPTmove\t" @
				mprint (Mmove a1 a2.[]) @ '\n'.(ins c)
 || ins (Mmove a1 a2.c)&((regok a1)&(regok a2)) = regmove a1 a2 (ins c)
 || ins (Mmove a1 a2.c)&((regiok a1)&(regok a2)) = load a1 a2 (ins c)
 || ins (Mmove a1 a2.c)&((regok a1)&(regiok a2)) = store a1 a2 (ins c)
 || ins (Mmove a1 a2.c)&(regok a1) = store a1 a2 (ins c)
 || ins (Mmove a1 a2.c)&(regok a2) = load a1 a2 (ins c)
 || ins (Mmove a1 a2.c) = load a1 (mytmpreg) (store (mytmpreg) a2 (ins c))
 || ins (Mcall a.c) = "\tdec\t" @ amode2 Vp (const 4)
			("\tbalax\t" @ a @
			"\n\tst\t" @ amode2 (reg (absreg 15)) (Vind 0) (ins c))
 || ins (Mreturn.c) = "\tls\t"@amode2 (reg (absreg 15)) (Vind 0)
			("\tbrx\tr15\n\tinc\t" @ amode2 Vp (const 4) (ins c))
 || ins (Mcalltag t r.c) = "\tload\t" @ amode mytmpreg
					("," @ otag t ("(" @ re r
			(")\n\tdec\t" @ amode2 Vp (const 4)
			("\tbalrx\tr15," @ amode mytmpreg
			("\n\tst\t"@ amode2 (reg(absreg 15)) (Vind 0)
			(ins c))))))
 || ins (Mjumptag t r.c) = "\tload\t "@amode mytmpreg (","@otag t ("(" @ re r
				(")\n\tbr\t" @ amode mytmpreg ("\n" @ ins c))))
 || ins (Mjump a.c) = "\tbali\t" @ amode mytmpreg ( "," @ a @ '\n'.ins c)
 || ins (Mjumpf a.c) = "\tbali\t" @ amode mytmpreg ( "," @ a @ '\n'.ins c)
 || ins (Mjcond cc l.c) = "\tb" @ ccof cc ('\t'.l @ "\n" @ ins c)
 || ins (Mlabel l.c)&(wordnext c) = ins (Malign.[]) @ 
			if (clabel l) then
			    labelfix l @ ":\n" @ l @ ":\n" @ ins c
			else
			    l @ ":\n" @ ins c
 || ins (Mlabel l.c) =  if (clabel l) then
			    labelfix l @ ":\n" @ l @ ":\n" @ ins c
			else
			    l @ ":\n" @ ins c
 || ins (Mcompare a1 a2.c)&((regok a1)&(regok a2)) =
			"\tc\t" @ amode2 a1 a2 (ins c)
 || ins (Mcompare a1 a2.c) = ins (Mmove a1 (totmp1 a1).
					(Mmove a2 (totmp2 a2).
					(Mcompare (totmp1 a1) (totmp2 a2).c)))
 || ins (Mop2 op a1 a2.c)&((regok a1)&(regok a2)&(opok op)) =
	'\t'.oop op ('\t'.amode2 a2 a1 (ins c)) 
 || ins (Mop2 mod a1 a2.c) = ins (Mmove a2 (reg 12).
			(Mmove a1 (reg 13).[]))
			@ ("\t.globl\t_.ldiv$$\n\tbali\tr15,_.ldiv$$\n" @ ins
			(Mmove (reg 13) a2.c))
 || ins (Mop2 div a1 a2.c) = ins (Mmove a2 (reg 12).
			(Mmove a1 (reg 13).[]))
			@ ("\t.globl\t_.ldiv$$\n\tbali\tr15,_.ldiv$$\n" @ ins
			(Mmove (reg 12) a2.c))
 || ins (Mop2 mul a1 a2.c) = ins (Mmove a2 (reg 12).
			(Mmove a1 (reg 13).[]))
			@ ("\t.globl\t_.lmul$$\n\tbali\tr15,_.lmul$$\n" @ ins
			(Mmove (reg 12) a2.c))
 || ins (Mop2 op a1 a2.c) = ins (Mmove a1 (totmp1 a1).
			(Mmove a2 (totmp2 a2).
			(Mop2 op (totmp1 a1) (totmp2 a2).
			(Mmove (totmp2 a2) a2.c))))
 || ins (Mboolcc cc a.c) = ins (
	Mjcond cc "1f".
	Mmove (const 0) a.
	Mjump "2f".
	Masm "1:" [].
	Mmove (const 1) a.
	Masm "2:" [].
	c)
 || ins (Mnoop.c) = ins c
 || ins (Mdata.c) = "\t.data\t1\n" @ ins c
 || ins (Mtext.c) = "\t.text\n" @ ins c
 || ins (Mword (glob a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.long\t" @ itos i @ "\n" @ ins c
 || ins (Mword (idlit s).c) = "\t.long\t" @ s @ "\n" @ ins c
/*
 || ins (Mstring s.c) = "\t.asciz\t\"" @ s @ "\"\n\t.align\t2\n" @ ins c
*/
 || ins (Mstring s.c) = itlist(\x.\p.
				"\t.byte\t" @ itos(ord x) @ "\n" @ p)s
				("\t.byte\t0\n\t.align\t2\n" @ ins c)
 || ins (Mexport a.c) = if( clabel a) then
			    "\t.globl\t"@labelfix a@"\n\t.globl\t"@a@"\n"@ins c
			else
			    "\t.globl\t"@a@"\n"@ins c
 || ins (Mfunbegin _ _.c) = "# Funbegin\n" @ ins c
 || ins (Mfunend.c) = "# Funend\n" @ ins c
 || ins (Malign.c) = "\t.align\t2\n" @ ins c
 || ins (Mcom s.c) = "# " @ s @ "\n" @ ins c
 || ins (Mpragma s.c) = ins c
 || ins (Masm s l.c) = aspr s l @ "\n" @ ins c
 || ins [] = []
 || ins (m._) = fail ("ins: strange Mcode " @ mprint [m])
and prel1 = "\t.globl\t.oVncs\n\t.set\t.oVncs,0\n"
and prel2 = ".set oeval,0\n.set ounwind,4\n.set ojfun,8\n.set ogettag,12\n"

and assemblercode m = "# RT-PC assembler code:\n" @ prel1 @ prel2 @ ins m
end
