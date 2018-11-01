module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "../misc/flags.t"
#include "mtrans1.t"
#include "mtransj.t"
#include "mtrans2.t"
#include "mtrans3.t"
#include "tmp.h"

-- To do:


-- Registers:

export assemblercode, Aregs, Dregs, usecase, use3op,
	argcreg, tagreg, bigeqreg, indreg, cputype;
rec
    Aregs = count 2 7 @ fail "Out of A-regs"
and Dregs = count 2 7 @ fail "Out of D-regs"
and usecase max low high cnt =  cnt>=4 & (high-low)<cnt*5
and use3op = true
and argcreg = reg ArgCReg
and tagreg  = reg TagReg
and bigeqreg= reg BigEqReg
and indreg  = reg IndReg
and cputype = "ARM"

and regname Ehpr = "Ehpr"
 || regname hpr = "hpr"
 || regname Vpr = "Vpr"
 || regname Spr = "Spr"
 || regname Ret = "lr"
 || regname Pc = "pc"
 || regname n = 'r'.itos n

and reginc (reg a.(r as (reg b)._)) = a<b & reginc r
 || reginc    _ = true

and amode (reg i)      = regname i
 || amode (regind r i) =  "[" @ regname r @ ",#" @ itos (i*4) @"]"
 || amode (regrel _ _) = fail "amode regrel\n"
 || amode popV = "[" @ regname Vpr @"],#4" 
 || amode pushV = "[" @ regname Vpr @",#-4]!" 
 || amode popS = "[" @ regname Spr @"],#4" 
 || amode pushS = "[" @ regname Spr @",#-4]!" 
 || amode tohp = "[" @ regname hpr @"],#4" 
 || amode (glob i)  = drop_ i
 || amode (idlit s) = drop_ s
 || amode (const n) = '#'.itos n
and drop_ ('_'.s) = if mem ':' s then s else '|'.s@"|"
||  drop_ s =  if mem ':' s then s else '|'.s@"|"
and aspr ""       ams      = ""
||  aspr ('^'.cs) (am.ams) = amode am @ (aspr cs ams)
||  aspr (c.cs)   ams      = c.aspr cs ams

and isreg (reg _) = true
 || isreg _       = false
and otag oeval   = "#0"
 || otag ounwind = "#4"
 || otag ojfun   = "#8"
 || otag ogettag = "#12"
 || otag (onumtag k) = "#"@itos(4*k)
and ccname eq = "eq"
 || ccname ne = "ne"
 || ccname lt = "lt"
 || ccname gt = "gt"
 || ccname le = "le"
 || ccname ge = "ge"
 || ccname ltstack = "lo"
 || ccname ltheap = "lo"
 || ccname gtstack = "hi"
 || ccname geheap = "hs"
and ccnot eq = ne
 || ccnot ne = eq
 || ccnot lt = ge
 || ccnot gt = le
 || ccnot le = gt
 || ccnot ge = lt
 || ccnot ltstack = gtstack --- ?
 || ccnot ltheap = geheap
 || ccnot gtstack = ltstack --- ?
 || ccnot geheap = ltheap
and opname add = "add"
||  opname sub = "sub"
||  opname mul = "mul"
||  opname div = "div?"         --- ?
||  opname mod = "mod?"         --- ?
||  opname neg = "neg"
||  opname btand = "and"
||  opname btor = "orr"
||  opname btxor = "eor"
||  opname btcompl = "mvn"
||  opname btlsh = "lsl"
||  opname btrsh = "lsr"

and splitconst 0 s = []            -- splitconst works only on positive numbers !
 || splitconst n s = if n%4 = 0 then splitconst (n/4) (s+2) 
                                else (n%256,s).splitconst (n/256) (s+8)

and amode1 s a1 = '\t'.s@'\t'.amode a1@"\n"
and amode2 s a1 a2 = '\t'.s@'\t'.amode a1@","@amode a2@"\n"
and amode3 s a1 a2 a3 = '\t'.s@'\t'.amode a1@","@amode a2@","@amode a3@"\n"
and aconst r 0 = "\tmov\t"@amode r@",#0\n"
 || aconst r (-1) = "\tmvn\t"@amode r@",#0\n"
 || aconst r n & (n>0) =
       let cl = splitconst n 0
       and sr = amode r
       in let  n,s = hd cl
          in "\tmov\t"@sr@",#"@itos n@"<<"@itos s@"\n" @
	     concmap (\(n,s)."\torr\t"@sr@","@sr@",#"@itos n@"<<"@itos s@"\n") (tl cl)
 || aconst r n & (n<0) =
       let cl = splitconst ((-n)-1) 0           -- neg = inc o not
       and sr = amode r
       in let n,s = hd cl
          in "\tmvn\t"@sr@",#"@itos n@"<<"@itos s@"\n" @
	     concmap (\(n,s)."\tbic\t"@sr@","@sr@",#"@itos n@"<<"@itos s@"\n") (tl cl)

and aaddr r s =                                 -- only word addresses
    let sr = amode r
    and ss = amode s
    in
       if length ss > 2 & head 2 ss = "PC" then 
         let l,s = splitat ':' ss in
	  "\tldr\t"@sr@",|"@l@"| ; "@drop_ s@"\n"
       else
	  fail ("Does not know if label '"@ss@"' is within reach\n")

and oneop s1 s2 = '\t'.s1@'\t'.drop_ s2@"\n"
and saveret = "\tadd\tlr,pc,#4\n\tstr\tlr,[Vpr,#-4]!\n"
and adtmp1 = amode dtmp1
and
    ins (Mmove (r1 as reg _) (r2 as reg _).r) = 
	amode2 "mov" r2 r1 @ ins r
||  ins (Mmove popV (r2 as reg _).r) =
	amode2 "ldr" r2 popV @ ins r
||  ins (Mmove (r1 as reg _) pushV.r) =
	amode2 "str" r1 pushV @ ins r
||  ins (Mmove popS (r2 as reg _).r) =
	amode2 "ldr" r2 popS @ ins r
||  ins (Mmove (r1 as reg _) pushS.r) =
	amode2 "str" r1 pushS @ ins r
||  ins (Mmove (r1 as reg _) tohp.Mop3 add (const n) (reg hpr) (reg hpr).r) = -- Save 1 add
        "\tstr\t"@amode r1@",["@regname hpr@"],"@amode (const (4+n))@"\n" @ ins r
||  ins (Mmove (r1 as reg _) tohp.Mop2 add (const n) (reg hpr).r) = -- Save 1 add
        "\tstr\t"@amode r1@",["@regname hpr@"],"@amode (const (4+n))@" ; 2op\n" @ ins r

||  ins (pr as (Mmove (reg _) tohp._)) =
#ifdef JOIN
        let stm,rest = take (\m.case m in Mmove (reg _) tohp : true || _ : false end) pr
#else
        let stm,rest = [hd pr],tl pr
#endif
        in let reglist = map (\(Mmove r _).r) stm
        in (if length reglist > 1 & reginc reglist
	    then "\tstmia\t"@regname hpr@"!,{"@ tl (concmap (\r.','.amode r) reglist) @"}\n"
	    else concmap (\r.amode2 "str" r tohp) reglist)
	       @ ins rest
||  ins (pr as (Mmove (reg _) (regind d _)._)) =
#ifdef JOIN
        let stm,rest = take (\m.case m in Mmove (reg _) (regind d' _) : d = d' || _ : false end) pr
#else
        let stm,rest = [hd pr],tl pr
#endif
        in let oplist = map (\(Mmove a b).(a,b)) stm
        in (if length oplist > 1 & contfrom 0 oplist & reginc (map fst oplist)
	    then "\tstmia\t"@regname d@",{"@ tl (concmap (\(r,_).','.amode r) oplist) @"}\n"
	    else concmap (\(a,b).amode2 "str" a b) oplist)
	       @ ins rest

||  ins (pr as (Mmove (regind d _) (reg _)._)) =
#ifdef JOIN
        let ldm,rest = take (\m.case m in Mmove (regind d' _) (reg _): d = d' || _ : false end) pr
#else
        let ldm,rest = [hd pr],tl pr
#endif
        in let oplist = map (\(Mmove a b).(b,a)) ldm -- Note swap!
        in (if length oplist > 1 & contfrom 0 oplist & reginc (map fst oplist)
	    then "\tldmia\t"@regname d@",{"@ tl (concmap (\(r,_).','.amode r) oplist) @"}\n"
	    else concmap (\(a,b).amode2 "ldr" a b) oplist)
	       @ ins rest
||  ins (Mmove (a1 as const n) (a2 as reg _).r) =
	aconst a2 n @ ins r
||  ins (Mmove (a1 as idlit _) (a2 as reg _).r) =
	aaddr a2 a1 @ ins r
||  ins (Mmove (a1 as reg _) (a2 as glob _).r) =
        case a1
        in dtmp1 : aaddr dtmp2 a2 @ amode2 "str" a1 dtmp2ind
        ||   _   : aaddr dtmp1 a2 @ amode2 "str" a1 dtmp1ind
	end @ ins r
||  ins (Mmove (a1 as glob _) (a2 as reg _).r) =
        aaddr dtmp1 a1 @
	amode2 "ldr" a2 dtmp1ind  @ ins r
||  ins (Mop2 btcompl a1 a2.r) =
	amode2 "mvn" a2 a1 @ ins r
||  ins (Mop2 neg a1 a2.r) =
        "\trsb\t" @ amode a2 @ "," @ amode a1 @ ",#0\n"  @ ins r
||  ins (Mop2 op a1 a2.r)  & (op = btlsh | op = btrsh | op = btrsha) =
	"\tmov\t" @amode a2@ "," @amode a2@ ","@ opname op @" "@ amode a1  @ ins r
||  ins (Mop2 op a1 a2.r) =
	amode3 (opname op) a2 a2 a1 @ ins r
||  ins (Mop3 op a1 a2 a3.r)  & (op = btlsh | op = btrsh | op = btrsha) =
	"\tmov\t" @amode a3@ "," @amode a2@ ","@ opname op @" "@ amode a1 @ ins r
||  ins (Mop3 op a1 a2 a3.r) =
	amode3 (opname op) a3 a2 a1 @ ins r

||  ins (Mjcond ltheap l1. Mcall "GARB". Mlabel l2.r) & (l1 = l2) =
         "\tblhs\tGARB_LINK\n"  @ ins (Mlabel l2.r)

||  ins (Mcall s.r) = (if s = "GARB"
		       then oneop "bl" "GARB_LINK"
		       else saveret @ oneop "b" s) @ ins r
||  ins (Mjumpf s.r) = oneop "b" s  @ ins r
||  ins (Mjump s.r) = oneop "b" s  @ ins r

||  ins (Mreturn.r) = "\tldr\tpc,[Vpr],#4\n" @ ins r

||  ins (Mcompare a1 (const n).r) & (n<0) = amode2 "cmn" a1 (const (-n))  @ ins r
||  ins (Mcompare a1 a2.r) = amode2 "cmp" a1 a2  @ ins r
||  ins (Mjcond cc s.r) = "\tb" @ ccname cc @ "\t" @ amode (glob s) @"\n"  @ ins r
||  ins (Mboolcc cc a1.r) = "\tmov" @ ccname cc @ "\t" @ amode a1 @ ",#1\n" @
                          "\tmov" @ ccname (ccnot cc) @ "\t" @ amode a1 @ ",#0\n" @
			   ins r

||  ins (m as Mcalltag t r1.r) =
                 saveret @"\tldr\tpc,["@regname r1@","@otag t@"]\n" @ ins r
||  ins (m as Mjumptag t r1.r) = "\tldr\tpc,["@regname r1@","@otag t@"]\n" @ ins r

 || ins (Mcase a l h _ ls x.r) =
 	let t = 'L'.itos x in
 	ins [Mmove a dtmp1] @
	(if l = 0 then "" else ins [Mop2 sub (const l) dtmp1])@
	"\tcmp\t"@adtmp1@",#"@itos(h-l)@"\n"@
	"\tbhi\t"@t@"_2\n"@
	"\tadd\t"@adtmp1@",pc,"@adtmp1@",lsl #2 ; Piplining gives correct address if table 8 bytes ahead\n" @
	"\tldr\tpc,["@adtmp1@"]\n"@
	t@"_1:\n" @                                  --- Table after jump 
	concmap (\l."\tdcd\t"@l@"\n") ls@
	t@"_2:\n" @
	ins r

 || ins (Mnoop.r) = ins r
 || ins (Mdata.r) = "\tarea |lml$data|,DATA\n" @ ins r
 || ins (Mtext.r) = "\tarea |lml$code|,CODE,READONLY\n" @ ins r
 || ins (Mword (glob  a).r) = "\tdcd\t" @ drop_ a @ "\n" @ ins r
 || ins (Mword (idlit a).r) = "\tdcd\t" @ drop_ a @ "\n" @ ins r
 || ins (Mword (const i).r) = "\tdcd\t" @ itos i @ "\n" @ ins r
 || ins (Mdfloat s.r) = "\tdcfd\t"@(if mem '.' s then s else s@".0")@"\n" @ ins r
 || ins (Mstring s.r) = let str = concmap (\c.case c
                                            in '\n'  : "\\n"
                                            || '\t'  : "\\t"
                                            || '\b'  : "\\b"
                                            || '"'  : "\\\""
                                            || '\\'  : "\\\\"
                                            ||  c   : if isprint c then [c] else '\\'.itos (ord c)@"\\"
                                            end) s
                      in "\tdcb\t\"" @ str @"\",0\n\talign 4\n"  @ ins r
 				
 || ins (Mexport a.r) = "\tglobl\t"@drop_ a@"\n"@ ins r
 || ins (Mcom s.r) = " ; " @ s @ "\n" @ ins r
 || ins (Mpragma s.r) = " ;\tpragma\t" @ s @ "\n" @ ins r
 || ins (Mlabel l.r) = drop_ l @ ":\n" @ ins r
 || ins (Masm s l.r) = aspr s l @ "\n" @ ins r
 || ins (Malign.r) = "\talign 4\n" @ ins r
 || ins (Mfunbegin s _.r) = ";\tPROC\t"@s@"\n"  @ ins r
 || ins (Mfunend .r) = ";\tEND\n" @ ins r
 || ins (m.r) = " ;fail; "@mprint[m]@"\n"  @ ins r
--fail ("ins: strange Mcode " @ mprint [m])
 || ins [] = []

and regdecl = concmap (\n.regname n@"\trn\t"@itos n@"\n") (count 0 15) 
   @ "sl\trn\t10\nfp\trn\t11\nip\trn\t12\nsp\trn\t13\n"

and assemblercode m =
--	let mt1 = mtrans1 m
--	in let mt = mtrans2 mt1 in
--	mprint mt1 @ mprint mt @
#ifdef JOIN
    let mt = (mtrans3 o mtrans2 o mtransj o mtrans1) m in
#else
    let mt = (mtrans3 o mtrans2 o           mtrans1) m in
#endif
    (if PrMtrans then
	mprint mt
    else
	"") @
    regdecl @
    ins (Mtext.mt)
end
