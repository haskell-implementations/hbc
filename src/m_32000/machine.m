module
#include "../misc/flags.t"
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "mtrans.t"
--
-- Register allocation
-- sp	- value stack pointer
-- fp	- unused
-- sb	- unused
-- r7 	- heap pointer
-- r6	- pointer stack pointer
-- r5-r1- mcode registers
-- r2	- some return values
-- r1	- scratch
-- r0	- cache of top of pointer stack
--
export assemblercode, Aregs, Dregs, usecase, use3op,
	argcreg, tagreg, bigeqreg, indreg, cputype;
rec
    Aregs = count 1 5 @ fail "Out of A-regs"
and Dregs = count 1 5 @ fail "Out of D-regs"
and usecase max low high cnt = cnt>=4 & (high-low)<cnt*4
and use3op = false
and argcreg = reg 2
and tagreg  = reg 2
and bigeqreg= reg 2
and indreg  = reg 3
and cputype = "NS32XXX"
and
    re r c = 'r' . itos r @ c

and otag oeval c = "oeval" @ c
 || otag ounwind c = "ounwind" @ c
 || otag ojfun c = "ojfun" @ c
 || otag ogettag c = "ogettag" @ c
 || otag (onumtag i) c = itos (4*i) @ c
 
and ccof eq c = "eq" @ c
 || ccof ne c = "ne" @ c
 || ccof lt c = "lt" @ c
 || ccof gt c = "gt" @ c
 || ccof le c = "le" @ c
 || ccof ge c = "ge" @ c
 || ccof ltstack c = "lo" @ c
 || ccof ltheap c = "lo" @ c
 || ccof gtstack c = "hi" @ c
 || ccof geheap c = "hs" @ c
 
and oop add c = "addd" @ c
 || oop sub c = "subd" @ c
 || oop mul c = "muld" @ c
 || oop div c = "quod" @ c
 || oop mod c = "remd" @ c
 
and 
    amode Vp c = "sp" @ c
 || amode (Vind i) c = itos (4*i) @ "(sp)" @ c
 || amode (Vrel _) c = fail "amode Srel\n"
 || amode pushV c = "tos" @ c
 || amode popV c = "tos" @ c
 
 || amode Sp c = "r6" @ c
 || amode (Sind i) c = itos (4*i) @ "(r6)" @ c
 || amode (Srel _) c = fail "amode Srel\n"
 || amode pushS c = fail "pushS" -- "-(%ep)" @ c
 || amode popS c = fail "popS" -- "(%ep)+" @ c
 
 || amode hp c = "r7" @ c
 || amode (hpind i) c = itos (4*i) @ "(r7)" @ c
 || amode (hprel i) c = fail "amode hprel\n"
 || amode tohp c = fail "tohp" -- "(%hp)+" @ c
 
 || amode (reg i) c = re i c
 || amode (regind r i) c = itos(4*i) @ "(" @ re r (')'.c)
 || amode (regrel _ _) c = fail "amode regrel\n"

 || amode (glob i) c = i @ c
 || amode (idlit s) c = '$'.s @ c
 || amode (retaddr s) c = '$'.s @ c
#ifdef SYSV
 || amode (const n) c = '$'.itos n @ c
#else
 || amode (const n) c = itos n @ c
#endif
 
and amode1 a1 c = amode a1 ('\n'.c)
and amode2 a1 a2 c = amode a1 (','.amode a2 ('\n'.c))

and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

-- check for conflicting addressing modes, i.e. will destroying a2 destroy
-- the possibility to access a1. (Only used for arithmetic operands.)
and conflict (regind r1 _) (reg r2) = r1 = r2
||  conflict a1            a2       = a1 = a2
and
    move (a1 as const n) a2 c & (-8 <= n & n <= 7) =
	"\tmovqd\t" @ itos n @ "," @ amode1 a2 c
||  move (idlit s) a2 c = "\taddr\t" @ amode2 (glob s) a2 c
||  move a1 a2 c = "\tmovd\t" @ amode2 a1 a2 c
and
    ins (Mcalltag t r.c) =
	"\tmovd\t"@otag t ("("@re r ("),r4\n\tjsr\t0(r4)\n" @ ins c))
 || ins (Mjumptag t r.c) =
	"\tmovd\t"@otag t ("("@re r ("),r4\n\tjump\t0(r4)\n" @ ins c))
 || ins (Mjump l.c) = "\tbr\t" @ l @ '\n'.ins c
 || ins (Mjumpf l.c) = "\tjump\t" @ l @ '\n'.ins c
 || ins (Mcall a.c) = "\tjsr\t" @ a @ '\n'.ins c
 || ins (Mreturn.c) = "\tret\t0\n" @ ins c
 || ins (Mjcond cc l.c) = "\tb"@ ccof cc ('\t'.l @ "\n" @ ins c)
 || ins (Mlabel l.c) = l @ ":\n" @ ins c
 || ins (Mcompare a1 a2.c) = "\tcmpd\t" @ amode2 a1 a2 (ins c)

 || ins (Mmove (const 0) pushV.Mop2 sub a1 (Vind 0).c) =
	"\tnegd\t" @ amode2 a1 pushV (ins c)
 || ins (Mmove (const 0) a2.Mop2 sub a1 a2'.c) & (a2=a2') =
	"\tnegd\t" @ amode2 a1 a2 (ins c)
 || ins (Mop2 op (a1 as const n) a2.c) & ((op=add | op=sub) & 0<=n & n<=7) =
	"\taddqd\t" @ itos (if op=add then n else -n) @ "," @ amode1 a2 (ins c)
 || ins (Mop2 op a1 a2.c) =
	'\t'.oop op ('\t'.amode2 a1 a2 (ins c))

 || ins (Mmove a pushS.c) = ins (Mmove (Srel(-1)) Sp. Mmove a (Sind 0). c)
 || ins (Mmove (hprel i) a.c) = "\taddr\t" @ amode2 (hpind i) a (ins c)
 || ins (Mmove (Srel i) a.c) = "\taddr\t" @ amode2 (Sind i) a (ins c)
 || ins (Mmove (Vrel i) Vp.c) = "\tadjspd\t" @ amode1 (const (-i*4)) (ins c)
 || ins (Mmove (Vrel i) a.c) = "\taddr\t" @ amode2 (Vind i) a (ins c)
 || ins (Mmove (regrel r i) a.c) = "\taddr\t" @ amode2 (regind r i) a (ins c)

 || ins (Mmove Vp a.c) = "\tsprd\tsp," @ amode1 a (ins c)
 || ins (Mmove a Vp.c) = "\tlprd\tsp," @ amode1 a (ins c)

 || ins (Mmove popS a.c) = ins (Mmove (Sind 0) a.Mmove (Srel 1) Sp.c)

 || ins (Mmove a1 a2.c) = move a1 a2 (ins c)

 || ins (Mcase a l h _ ls x.c) =
	let t = 'L'.itos x in
move a (reg 0) (
"\tcheckd\tr0,X" @ t @ ",r0\n\tbfs\tO" @ t @
"\nT" @ t @ ":\tcasew\t" @ "W" @ t @ "[r0:w]\nW" @ t @ ":\n" @
		(itlist (\l.\r."\t.word\t"@l@"-T"@t@"\n"@r) ls (
#ifdef SYSV
"X"@t@":\t.double\t"@itos h@","@itos l@"\nO"@t@":\n"@
#else
"X"@t@":\t.long\t"@itos h@","@itos l@"\nO"@t@":\n"@
#endif
ins c)))

 || ins (Mboolcc cc a.c) = "\ts" @ ccof cc ("d\t" @ (amode1 a (ins c)))
 || ins (Mnoop.c) = ins c
 || ins (Mdata.c) = "\t.data\n" @ ins c
 || ins (Mtext.c) = "\t.text\n" @ ins c
#ifdef SYSV
 || ins (Mword (glob a).c) = "\t.double\t" @ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.double\t" @ itos i @ "\n" @ ins c
 || ins (Mword (idlit s).c) = "\t.double\t" @ s @ "\n" @ ins c
#else
 || ins (Mword (glob a).c) = "\t.long\t" @ a @ "\n" @ ins c
 || ins (Mword (const i).c) = "\t.long\t" @ itos i @ "\n" @ ins c
 || ins (Mword (idlit s).c) = "\t.long\t" @ s @ "\n" @ ins c
#endif
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

and prol = ".set oeval,0\n.set ounwind,4\n.set ojfun,8\n.set ogettag,12\n"

#ifdef SYSV
and assemblercode ms = prol @ ins (mtrans ms)
#else
-- The Sequent assembler/linker is braindamaged, it cannot handle immediate
-- values that are non-constants.
-- We transform all such constants to IBM-like literals.
and literals [] s = s
||  literals (Mcompare a1 a2.ms) s = (
	literals ms (union (lit a1) (union (lit a2) s))
	where lit (idlit s) = [s]
	   || lit _ = [])
||  literals (m.ms) s = literals ms s
and assemblercode ms =
	let ms' = mtrans ms
	in prol @ ins (Mdata.concmap (\s.[Mlabel ('$'.s); Mword (glob s)]) (literals ms' [])@[Mtext]@ms')
#endif
end
