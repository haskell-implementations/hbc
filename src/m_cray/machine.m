--- undvik data tmpreg utom f|r att ladda addresser
module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"
#include "mtrans.t"
#include "misc.t"
#include "misc2.t"

-- Address-register:
-- A7		value stack pointer (Vp)
-- A6		pointer stack pointer (Sp)
-- A5		heap pointer (Hp)
-- A1-A4	mcode registers (r0-r3)
-- A0		used with jumps and as temporary register (r4)
-- B00          contains top of return address,
-- B01          address temporary, also used with jumps
-- B00-B77      mcode registers (r100-r177)
-- Data-register:
-- S0		used with jumps and as temporary register
-- S1-S7	mcode-register (r8-r15)
-- T70-T77	used to save Sregs when doing aritmethics.
-- T00-T77      mcode registers (r200-r277)

export assemblercode, Aregs, Dregs, usecase, use3op,
       argcreg, tagreg, bigeqreg, indreg, cputype;
rec
    comm m = "* " @ mprint [m]
and Aregs = fromto 1 3  @ fail "Out of A-regs"
and Dregs = fromto 11 15 @ fail "Out of D-regs"
and fromto m n = if n=m then [m] else m.fromto (m+1) n
and usecase max low high cnt = cnt>0 & low > high -- this is false
and use3op = false
and argcreg = reg 11
and tagreg  = reg 11
and bigeqreg= reg 11
and indreg  = reg 3
and cputype = "CRAY/1"

and ins [] ul = []
 || ins (Mcompare a1 a2 .Mjcond cc l . c) ul & (isareg a1 | isareg a2) =
    comm (Mcompare a1 a2) @ comp a1 a2 cc @
    comm (Mjcond cc l)    @ oajcc cc l @ ins c ul
 || ins (Mcompare a1 a2 . (Mjcond cc l) . c) ul =
    comm (Mcompare a1 a2) @ comp a1 a2 cc @
    comm (Mjcond cc l)    @ ojcc cc l @ ins c ul
 || ins (Mcompare a1 a2 . Mboolcc cc a3 . c) ul =
    comm (Mcompare a1 a2) @ comp a1 a2 cc @ 
    comm (Mboolcc cc a3)  @ obcc cc a3 @ ins c ul
 || ins (Mcompare a1 a2 . _ . c) ul =
    fail "ins: comp not followed by jcond or boolcc\n"
       
-- Not true in runtime
 || ins (Mjcond cc l . c) ul =
      comm (Mjcond cc l) @ "* No compare before jcond. Asume data-compare\n" @
      ojcc cc l @ ins c ul
 || ins (_ . Mboolcc cc a . c) ul = fail "ins: No compare before boolcc\n"

 || ins (Mjump l.c) ul =
    comm (Mjump l) @
    emit "J" l @ ins c ul
 || ins (Mjumpf l.c) ul =
    comm (Mjumpf l) @
    emit "J" (fixclbl l) @ ins c ul
 -- Don't now if it is ok to use indirect addressmode
 || ins (Mjumptag t r.c) ul =
    comm (Mjumptag t r) @
    emit (amode atmp1) (otag t @ "," @ regname r) @
    emit "B01" (amode atmp1) @
    emit "J" "B01" @ ins c ul

 || ins (Mcall l.c) ul =
    comm (Mcall l) @
    emit (amode Vp) (amode Vp @ "-1")   @ -- push Vstack
    emit (amode atmp1) (amode Rpreg)    @ -- save B00 (previous return-address)
    emit (amode (Vind 0)) (amode atmp1) @
    emit "R" (fixclbl l) @ ins c ul       -- do the call

-- This is a tricky one because R only jumps to labels so must use J and
-- mtrans has made an unique label preceding every calltag.
 || ins (Mcalltag t r.Mlabel (cx.lbl).c) ul =
    comm (Mcalltag t r) @
    emit (amode Vp) (amode Vp @ "-1") @ -- push Vstack
    emit (amode atmp1) (amode Rpreg) @  -- save B00 (previous return-address)
    emit (amode (Vind 0)) (amode atmp1) @
    emit (amode atmp1) (otag t @ "," @ regname r) @  -- jump address
    emit "B01" (amode atmp1) @
    emit (amode atmp1) lbl @     -- compute return address
    emit (amode Rpreg) (amode atmp1) @
    emit "J" "B01" @ ins(Mlabel (cx.lbl).c) ul -- do the jump
 || ins (Mcalltag t r._.c) ul =
      fail "ins: No label after calltag (error is in mtrans).\n"
 || ins (Mreturn.c) ul =
    comm (Mreturn) @
    emit (amode atmp1) (amode Rpreg) @            -- save address to jump to
    emit "B01" (amode atmp1) @
    emit (amode atmp1) (amode (Vind 0)) @ -- restore B00 (prev. ret. addr.)
    emit (amode Vp) (amode Vp @ "+1") @   -- pop Vstack
    emit (amode Rpreg) (amode atmp1) @
    emit "J" "B01" @ ins c ul               -- do return

 || ins (Mmove a1 a2.c) ul & (a1 = a2) = ins c ul
 || ins (Mmove a Vp.c) ul = ins (Mmove a Vpreg.c) ul
 || ins (Mmove a Sp.c) ul = ins (Mmove a Spreg.c) ul
 || ins (Mmove a hp.c) ul = ins (Mmove a Hpreg.c) ul
 || ins (Mmove Vp a.c) ul = ins (Mmove Vpreg a.c) ul
 || ins (Mmove Sp a.c) ul = ins (Mmove Spreg a.c) ul
 || ins (Mmove hp a.c) ul = ins (Mmove Hpreg a.c) ul
 || ins (Mmove (Vind n) a.c)  ul = ins (Mmove (regind Vpr n) a.c) ul
 || ins (Mmove (Sind n) a.c)  ul = ins (Mmove (regind Spr n) a.c) ul
 || ins (Mmove (hpind n) a.c) ul = ins (Mmove (regind Hpr n) a.c) ul
 || ins (Mmove a (Vind n).c)  ul = ins (Mmove a (regind Vpr n).c) ul
 || ins (Mmove a (Sind n).c)  ul = ins (Mmove a (regind Spr n).c) ul
 || ins (Mmove a (hpind n).c) ul = ins (Mmove a (regind Hpr n).c) ul

 || ins (Mmove popV a.c) ul =
      ins (Mmove (Vind 0) a.Mmove (Vrel 1) Vp.c) ul

 || ins (Mmove (hprel i)    a.c) ul = ins (Mmove (regrel Hpr i) a.c) ul
 || ins (Mmove (Vrel  i)    a.c) ul = ins (Mmove (regrel Vpr i) a.c) ul
 || ins (Mmove (Srel  i)    a.c) ul = ins (Mmove (regrel Spr i) a.c) ul
 || ins (Mmove (regrel r i) a.c) ul =
    comm (Mmove (regrel r i) a) @ lea (regind r i)   a @ ins c ul

 || ins (Mmove (ai as idlit s) a.c) ul & (~isreg a) =
    comm (Mmove ai a) @
     emit (amode atmp1) (amode ai) @
     emit (amode a) (amode atmp1) @ ins c ul

 || ins (Mmove (const i) a.c) ul & (~isreg a) =
    comm (Mmove (const i) a) @
     emit (amode dtmp1) (amode (const i)) @
     emit (amode a) (amode dtmp1) @ ins c ul

 || ins (Mmove (a1 as regind r1 i1) (a2 as regind r2 i2).c) ul =
    comm (Mmove a1 a2) @
    emit (amode dtmp1) (amode a1) @
    emit (amode a2) (amode dtmp1) @ ins c ul

 || ins (Mmove a1 a2.c) ul =
    comm (Mmove a1 a2) @ move a1 a2 @ ins c ul

 || ins (Mop2 op (r1 as reg _) (r2 as reg _).c) ul =
    comm (Mop2 op r1 r2) @ aop op r1 r2 @ ins c ul
 || ins (Mop2 op (r1 as reg _) a2.c) ul =
    comm (Mop2 op r1 a2) @
    emit (amode dtmp1) (amode a2) @
    aop op r1 dtmp1 @
    emit (amode a2) (amode dtmp1) @
    ins c ul
 || ins (Mop2 op a1 (r2 as reg _).c) ul =
    comm (Mop2 op a1 r2) @
    emit (amode dtmp1) (amode a1) @
    aop op dtmp1 r2 @ ins c ul
 || ins (Mop2 op a1 a2.c) ul =
    comm (Mop2 op a1 a2) @
    emit (amode dtmp1) (amode a1) @
    emit (amode dtmp2) (amode a2) @
    aop op dtmp1 dtmp2 @
    emit (amode a2) (amode dtmp2) @
    ins c ul

 || ins (Mnoop.c) ul = ins c ul
 || ins (Mdata.c) ul =
    emit3 "@DATA" "SECTION" "DATA,CM" @
    emit3 "@DATA" "=" "W.*" @ ins c ul
 || ins (Mtext.c) ul =
    emit3 "@CODE" "SECTION" "CODE" @
    emit3 "@CODE" "=" "P.*" @ ins c ul


 || ins (Mword (glob  a).c) ul = mkglob a @ ins c ul
 || ins (Mword (idlit a).c) ul = mkglob a @ ins c ul
 || ins (Mword (const i).c) ul = mkconst i @ ins c ul
-- The non-standard C-interface make funny stuff to the runtime
 || ins (Mlabel (cx1.lbl1). Mlabel (cx2.lbl2). c) ul =
    let l1 = fixclbl lbl1 in
    let l2 = fixclbl lbl2 in
      if l1 = l2
        then ins (Mlabel (cx2.lbl2).c) ul
        else mklbl lbl1 cx1 @
             mklbl lbl2 cx2 @
             ins c ul
 || ins (Mlabel (cx.lbl).c) ul = mklbl lbl cx @ ins c ul
 || ins (Mexport a.c) ul = emit "ENTRY" (fixclbl a) @ ins c ul
 || ins (Malign.c) ul = ins c ul -- word-addressed machine
 || ins (Masm s l.c) ul =
      let (al,ul') = build_assoc s ul
      in aspr s l 0 al @ "\n" @ ins c ul'
 || ins (Mcom s.c) ul = "* " @ s @ "\n" @ ins c ul
 || ins (Mpragma s.c) = ins c
 || ins (Mfunbegin _ _.c) ul = ins c ul
 || ins (Mfunend .c) ul = ins c ul
-- || ins (m._) ul = fail ("ins: No match in ins " @ mprint [m])
 || ins (m.c) ul = "(*-- No match: " @ mprint [m] @ ins c ul
and prel = emit3 "oeval" "=" "0" @
           emit3 "ounwind" "=" "1" @
           emit3 "ojfun" "=" "2" @
           emit3 "ogettag" "=" "3"
and extern_declarations m = 
    difference (mkset (map fixclbl (get_id_used m)))
               (mkset (map fixclbl (get_id_defd m)))
and emit_ext "" = ""
 || emit_ext s  = emit "EXT" (fixclbl s)
and mstrconv s u = let t = "LSC"@itos u in conc (map2 (\c.\n. let tc = t @ '_'.itos n @ "c" in
		[ Mlabel (t@'_'.itos n);
		  Mword (glob "PAIR1");
		  Mword (glob tc);
		  Mword (glob (t@'_'.itos (n+1)));
		  Mlabel tc;
		  Mword (glob "CHAR");
		  Mword (const (ord c)) ] ) s (from 1)) @
                [Mlabel (t@'_'.itos (length s + 1));
                 Mword (glob "TAG0"); Mword (const 0)]
and mstr [] _ = []
||  mstr (Mstring s.ms) (u.us) = mstrconv s u @ mstr ms us
||  mstr (m.ms) us = m . mstr ms us
and 
    assemblercode m =
    emit "IDENT" "lmlc" @
    prel @
    let
      ms = mstr m (from 1)
    in(
      concmap emit_ext (extern_declarations ms) @
      ins (mtrans ms) (map (\i."CI%" @ itos i) (from 0))
    )
    @ expand "" 8 @ "END\n"
end
