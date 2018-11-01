module
#include "../mcode/mcodedef_t.t"
#include "../mcode/mprint.t"

-- Address-register:
-- A7		value stack pointer (Vp)
-- A6		pointer stack pointer (Sp)
-- A5		heap pointer (Hp)
-- A1-A4	mcode registers (r0-r3)
-- A0		used with jumps and as temporary register (r4)
-- B00          contains top of return address (Rp)
-- B01          address temporary, also used with jumps
-- B00-B77      mcode registers (r100-r177)
-- Data-register:
-- S0		used with jumps and as temporary register
-- S1-S7	mcode-register (r8-r15)
-- T70-T77	used to save Sregs when doing aritmethics.
-- T00-T77      mcode registers (r200-r277)

export  Hpreg, Spreg, Vpreg, Rpreg,
        amode, atmp1, comp, emit,
	ojcc, obcc, oajcc, otag, regname, isareg,
        Hpr, Spr, Vpr, Rpr, build_assoc,
        aop, aspr, dtmp1, dtmp2, emit3,
        emitlbl, expand, fixclbl, isreg, lea, move,
	get_id_used, get_id_defd;
rec
    expand1 s 0 = s
 || expand1 s n = expand1 (s @ " ") (n-1)
and expand s n = if length s >= n then s
                                  else expand1 s (n-length s)
and spaces n & (n>0) = ' '.spaces (n-1)
||  spaces _         = ""
and emit s1 s2 = expand "" 12 @ expand s1 16 @ s2 @ "\n"
and emit3 s0 s1 s2 = expand s0 12 @ expand s1 16 @ s2 @ "\n"
and emitlbl s0 s1 s2 = let s0' = fixclbl s0 in
                       let t1 = if length s0' < 12
                                  then expand s0' 12
                                  else s0' @ " "
                       in let t2 = if length (t1 @ s1) < 28
                                  then expand (t1 @ s1) 28
                                  else (t1 @ s1) @ " "
                       in t2 @ s2 @ "\n"
and Spreg   = reg Spr
and Hpreg   = reg Hpr
and Vpreg   = reg Vpr
and Rpreg   = reg Rpr
and Hpr = 5
and Spr = 6
and Vpr = 7
and Rpr = 100   -- B00
and dtmp1 = reg 10
and dtmp2 = reg 9
and atmp1 = reg 4
and atmpx = reg 2	-- USE WITH CARE!!
and savex = "B13"
and S1 = reg 9  -- used in modulus-operator
and S2 = reg 10
and S3 = reg 11
and S1_S7 = map reg [10;11;12;13;14;15;9]
and A1_A7 = map reg [0;1;2;3;5;6;7]
and regname 0  = "A1"
||  regname 1  = "A2"
||  regname 2  = "A3"
||  regname 3  = "A4"
||  regname 4  = "A0" -- tmp, jumps
||  regname 5  = "A5" -- Hp
||  regname 6  = "A6" -- Sp
||  regname 7  = "A7" -- Vp
||  regname 8  = "S0" -- tmp: used with jumps, to do aritmethic
||  regname 9  = "S1" -- tmp: to do aritmethic
||  regname 10 = "S2"
||  regname 11 = "S3"
||  regname 12 = "S4"
||  regname 13 = "S5"
||  regname 14 = "S6"
||  regname 15 = "S7"
||  regname n = -- regname doesn't check if it's argument is correct
    if n >= 100 & n <= 109 then 'B'.'0'. itos (n % 100) else
    if n >= 110 & n <= 177 then 'B'    . itos (n % 100) else
    if n >= 200 & n <= 209 then 'T'.'0'. itos (n % 200)
                           else 'T'    . itos (n % 200)
and datareg (reg r) & (r >= 8) = true
||  datareg _ = false
and aatmp1 = amode atmp1

-- and get_id (glob s as c._) & (isdigit c | c='-') = []
and get_id (glob i) = i
 || get_id (idlit i) = i
 || get_id (retaddr i) = i
 || get_id _ = []
and get_id_used [] = []
 || get_id_used (Mmove a1 a2.c)  = get_id a1 . get_id a2 . get_id_used c
 || get_id_used (Mcall s.c)  = s.get_id_used c
 || get_id_used (Mjumpf s.c) = s.get_id_used c
 || get_id_used (Mjump s.c)  = s.get_id_used c
 || get_id_used (Mjcond _ s.c)  = s.get_id_used c
 || get_id_used (Mcompare a1 a2.c) = get_id a1 . get_id a2 . get_id_used c
 || get_id_used (Mop2 mod a1 a2.c) = "MMOD" . get_id a1 . get_id a2 .
                                     get_id_used c
 || get_id_used (Mop2 _ a1 a2.c) = get_id a1 . get_id a2 . get_id_used c
 || get_id_used (Mop3 _ a1 a2 a3.c) = get_id a1 . get_id a2 . get_id a3
                                    . get_id_used c
 || get_id_used (Mcase a _ _ _ _ _.c) = get_id a . get_id_used c
 || get_id_used (Mboolcc _ a.c) = get_id a . get_id_used c
 || get_id_used (Mword a.c) = get_id a . get_id_used c
 || get_id_used (Masm s l.cs) = ((get_id_asm s l) @ get_id_used cs
      where rec
         get_id_asm "" _ = []
      || get_id_asm (c.cs) (am.ams) & (c='^' | c='&') = get_id_asm cs ams
      || get_id_asm ('~'.cs) (glob i.ams) = (fixclbl i)
                                        . get_id_asm cs ams
      || get_id_asm (c.cs) ams = get_id_asm cs ams)
 || get_id_used (m.ms) = get_id_used ms
and get_idlits ms = filter (\x.if x = "" then false else hd x = '$') (get_id_used ms)
and get_id_defd [] = []
 || get_id_defd (Mlabel s.c) = s.get_id_defd c
 || get_id_defd (m.ms) = get_id_defd ms
and fixclbl ('_'.s) = 	fixclbl1 s
||  fixclbl s = fixclbl1 s
and fixclbl1 ('_'.s) = '$'.fixclbl1 s
||  fixclbl1 (c.s)   = c.fixclbl1 s
||  fixclbl1 "" = ""
and amode (Vp)     = (regname Vpr)
 || amode (Vind i) = itos i @ "," @ (regname Vpr)
 || amode (Vrel _) = fail "amode Srel\n"
 || amode (pushV)  = fail "amode pushV"
 || amode (popV)   = fail "amode popV"
 
 || amode (Sp)     = (regname Spr)
 || amode (Sind i) = itos i @ "," @ (regname Spr)
 || amode (Srel _) = fail "amode Srel\n"
 || amode (pushS)  = fail "amode pushS"
 || amode (popS)   = fail "amode popS"
 
 || amode (hp)      = (regname Hpr)
 || amode (hpind i) = itos i @ "," @ (regname Hpr)
 || amode (hprel i) = fail "amode hprel\n"
 || amode (tohp)    = fail "amode tohp"
 
 || amode (reg i)      = regname i
 || amode (regind r i) = itos i @ "," @ regname r
 || amode (regrel _ _) = fail "amode regrel\n"

 || amode (glob i)  = fixclbl i @ ",0"
 || amode (idlit s) = fixclbl s
 || amode (const n) = itos n
and addone (const n) = const (n+1)
||  addone (glob i)  = glob (itos ((stoi i) + 1))
||  addone _ = fail "add1: wrong addressmode to add1\n"
-- To build the assoc-list used by aspr.
-- Need to to do this separately to resolve forward references.
-- Should perhaps rewrite this to use a fancy lazy thing in aspr
-- instead, but there is no god-looking-program-award yet.
and build_assoc "" ul = ([],ul)
||  build_assoc ('%'.c.cs) (u.ul) =
      let altl = build_assoc cs ul
      in  ((c,u).(fst altl), snd altl)
||  build_assoc (c.cs) ul = build_assoc cs ul
and aspr "" ams n al = ""
||  aspr ('^'.cs) (am.ams) n al =
      amode am @ aspr cs ams (n+length (amode am)) al
||  aspr ('&'.cs) (am.ams) n al =
      amode (addone am) @ aspr cs ams (n+length(amode (addone am))) al
||  aspr ('|'.cs) ams 0 al =
      spaces 12 @ aspr cs ams 12 al
||  aspr ('|'.cs) ams n al =
      spaces (28-n) @ aspr cs ams 28 al
||  aspr ('~'.cs) (glob i.ams) n al =
      let    fcl  = fixclbl i
      in let lfcl = length fcl
      in     fcl @ aspr cs ams (n+lfcl) al 
||  aspr ('%'.c.cs) ams n al =
      let u  = assoc c al
      in  u @ spaces (12-length u) @  "=" @ spaces 15 @ "P.*" @
          aspr cs ams n al
||  aspr ('#'.c.cs) ams n al =
      let    lbl = assoc c al
      in  lbl @ aspr cs ams (n + length lbl) al
||  aspr ('\n'.cs) ams n al =
      '\n'.aspr cs ams 0 al
||  aspr ('\\'.'&'.cs) ams n al =
      '&'.aspr cs ams (n+1) al
||  aspr (c.cs)   ams n al =
      c.aspr cs ams (n+1) al
and isreg (reg n) & ((n>=0 & n<=4) | (n>=8 & n<=15)) = true
 || isreg hp      = true
 || isreg Vp      = true
 || isreg Sp      = true
 || isreg _       = false
and isareg (reg r) = r < 8
 || isareg hp      = true
 || isareg Vp      = true
 || isareg Sp      = true
 || isareg _       = false
and isbreg (reg r) = r >= 100 & r <= 177
 || isbreg _       = false
and isaddress ltstack = true
 || isaddress ltheap  = true
 || isaddress gtstack = true
 || isaddress geheap  = true
 || isaddress _       = false
and compop eq = "\\"
 || compop ne = "\\"
 || compop _  = "-"
and comparg a1 a2 cop gt      = amode a2 @ cop @ amode a1
 || comparg a1 a2 cop le      = amode a2 @ cop @ amode a1
 || comparg a1 a2 cop gtstack = amode a2 @ cop @ amode a1
 || comparg a1 a2 cop _       = amode a1 @ cop @ amode a2
and comp a1 a2 cc & (isareg a1 & isareg a2) =
       emit "A0" (comparg a1 a2 "-" cc)
 || comp a1 a2 cc & (isareg a1 & a1 ~= atmpx) =
       emit savex (amode atmpx) @
       emit (amode atmpx) (amode a2) @
       emit "A0" (comparg a1 atmpx "-" cc) @
       emit (amode atmpx) savex
 || comp a1 a2 cc & (isareg a2 & a2 ~= atmpx) =
       emit savex (amode atmpx) @
       emit (amode atmpx) (amode a1) @
       emit "A0" (comparg atmpx a2 "-" cc) @
       emit (amode atmpx) savex
 || comp a1 a2 cc & (isaddress cc) = fail "comp: One operand must be Areg\n"
 || comp a1 a2 cc & (isreg a1 & isreg a2) =
       emit "S0" (comparg a1 a2 (compop cc) cc)
 || comp a1 a2 cc & (isreg a1) =
      emit (amode dtmp1) (amode a2) @
      emit "S0" (comparg a1 dtmp1 (compop cc) cc)
 || comp a1 a2 cc & (isreg a2) =
       emit (amode dtmp1) (amode a1) @
       emit "S0" (comparg dtmp1 a2 (compop cc) cc)
 || comp a1 a2 cc =
       emit (amode dtmp1) (amode a1) @
       emit (amode dtmp2) (amode a2) @
       emit "S0" (comparg dtmp1 dtmp2 (compop cc) cc)
-- These routines are vera redundant, and should be rewritten
-- this is a quick hack.
and ojcc lt      lbl = emit "JSM" lbl
 || ojcc gt      lbl = emit "JSM" lbl	-- swapped arguments
 || ojcc le      lbl = emit "JSP" lbl	-- swap
 || ojcc ge      lbl = emit "JSP" lbl
 || ojcc eq      lbl = emit "JSZ" lbl
 || ojcc ne      lbl = emit "JSN" lbl
 || ojcc ltstack lbl = emit "JAM" lbl
 || ojcc gtstack lbl = emit "JAM" lbl	-- swap
 || ojcc ltheap  lbl = emit "JAM" lbl
 || ojcc geheap  lbl = emit "JAP" lbl
and obcc cc a =
  emit (amode a) (amode (const 1)) @ -- a := true
  ojcc cc "P.*+3" @                  -- skip next instruktion, which
                                     -- is 3 parcels long.
  emit (amode a) (amode (const 0))   -- a := false
and oajcc lt      lbl = emit "JAM" lbl
 || oajcc gt      lbl = emit "JAM" lbl	-- swapped arguments
 || oajcc le      lbl = emit "JAP" lbl	-- swap
 || oajcc ge      lbl = emit "JAP" lbl
 || oajcc eq      lbl = emit "JAZ" lbl
 || oajcc ne      lbl = emit "JAN" lbl
 || oajcc ltstack lbl = emit "JAM" lbl
 || oajcc gtstack lbl = emit "JAM" lbl	-- swap
 || oajcc ltheap  lbl = emit "JAM" lbl
 || oajcc geheap  lbl = emit "JAP" lbl

and move a1 a2 & (isreg a1 | isreg a2) = emit (amode a2) (amode a1)
||  move a1 a2 & (isbreg a1 | isbreg a2) =
      emit (amode atmp1) (amode a1) @ emit (amode a2) (amode atmp1)
||  move a1 a2 = emit (amode dtmp1) (amode a1) @ emit (amode a2) (amode dtmp1)
and -- Simple regind to areg
    lea (regind r1 1) a2 & (isareg a2) =
    emit (amode a2) (amode (reg r1) @ "+1")
 || lea (regind r1 -1) a2 & (isareg a2) =
    emit (amode a2) (amode (reg r1) @ "-1")
 || lea (regind r1 2) a2 & (isareg a2 & a2 ~= atmp1) =
    emit (amode a2) (amode (reg r1) @ "+1") @
    emit (amode a2) (amode a2 @ "+1")
 || lea (regind r1 -2) a2 & (isareg a2 & a2 ~= atmp1) =
    emit (amode a2) (amode (reg r1) @ "-1") @
    emit (amode a2) (amode a2 @ "-1")
-- General regind to areg, except A0 and equal regs
 || lea (regind r i) (a2 as reg r2) & (isareg a2 & a2 ~= atmp1 & r ~= r2) =
    emit (amode a2) (itos i) @
    emit (amode a2) (amode a2 @ "+" @ (amode (reg r)))
-- General regind to reg
 || lea (regind r i) (a2 as reg r2) =
    let (rtmp._) = difference [0;1;2] [r;r2] in
    let atmp = reg rtmp
    and save = "B10" in
    emit save (amode atmp) @
    emit (amode atmp) (itos i) @
    (if isareg a2 then
	emit (amode a2) ((amode atmp) @ "+" @ (amode (reg r)))
    else
	emit (amode atmp) ((amode atmp) @ "+" @ (amode (reg r))) @
	emit (amode a2) (amode atmp)
    ) @
    emit (amode atmp) save
-- All others(?)
 || lea (a1 as regind _ _) a2 & (~isreg a2) =
    lea a1 atmp1 @
    emit (amode a2) aatmp1
 || lea a1 a2 = fail ("lea failed  " @ amode a1 @ "," @ amode a2 @ "\n")
and saveSreg (a as reg r) = emit ("T7" @ tl (regname r)) (amode a)
and restoreSreg (a as reg r) = emit (amode a) ("T7" @ tl (regname r))
and saveAreg (a as reg r) = emit ("B1" @ tl (regname r)) (amode a)
and restoreAreg (a as reg r) = emit (amode a) ("B1" @ tl (regname r))
and aop add r1 r2 =
    emit (amode r2) (amode r2 @ "+" @ amode r1)
 || aop sub r1 r2 =
    emit (amode r2) (amode r2 @ "-" @ amode r1)
 -- Now comes the tricky part
 || aop mul r1 r2 =
    let tmpregs = head 3 (difference S1_S7 [r1;r2])
    in let [t1;t2;t3] = map amode tmpregs
    in let x = amode r2
    and    y = amode r1
    in
      concmap saveSreg tmpregs @
      emit t1 "0.6" @
      emit t2 (t1 @ "-" @ x) @
      emit t2 (t1 @ "-" @ "F" @ t2) @
      emit t3 (t1 @ "-" @ y) @
      emit t3 (t1 @ "-" @ "F" @ t3) @
      emit t2 (t2 @ "*" @ "R" @ t3) @
      emit t2 (t1 @ "-" @ "F" @ t2) @
      emit x  (t1 @ "-" @ t2) @
      concmap restoreSreg tmpregs
 || aop div r1 r2 =
    let tmpregs = head 5 (difference S1_S7 [r1;r2])
    in let [t1;t2;t3;t4;t5] = map amode tmpregs
    in let x = amode r2
    and    y = amode r1
    in
      concmap saveSreg tmpregs @
      emit t1 "0.6" @
      emit t2 "0.4" @
      emit t3 (t1 @ "-" @ y) @
      emit t3 (t1 @ "-" @ "F" @ t3) @
      emit t4 (t1 @ "-" @ x) @
      emit t2 (x @ "!" @ t2 @ "&SB") @
      emit t5 (t1 @ "-F" @ t4) @
      emit t4 ("/H" @ t3) @
      emit t5 (t5 @ "+F" @ t2) @
      emit t3 (t3 @ "*I" @ t4) @
      emit t3 (t3 @ "*F" @ t4) @
      emit t3 (t5 @ "*R" @ t3) @
      emit t3 (t1 @ "-F" @ t3) @
      emit x  (t1 @ "-" @ t3)  @
      concmap restoreSreg tmpregs

 -- Mop2 mod r1 r2   =def=   r2 := r2 % r1
 || aop mod r1 r2 = -- assumes r1 and r2 is register
    let
      stmp = hd (difference S1_S7 [r1;r2;S1;S2])
    in
      concmap saveSreg (difference S1_S7 [r1;r2]) @
      concmap saveAreg (difference A1_A7 [Spreg;Hpreg;Vpreg]) @
      (
        if r2 ~= S1 then(
	  (if r1 = S1 then
	    emit (amode stmp) (amode r1)
	  else "") @
	  emit (amode S1) (amode r2)
	) else ""
      ) @
      (
	if r1 ~= S2 then(
	  if r1 = S1 then
	    emit (amode S2) (amode stmp)
	  else
	    emit (amode S2) (amode r1)
	) else ""
      ) @

      -- This is not nice, should use "ins" from machine.m instead
      "* call MMOD, manuel call, FIX THIS\n" @
      emit (amode Vp) (amode Vp @ "-1") @
      emit (amode atmp1) (amode Rpreg) @
      emit (amode (Vind 0)) (amode atmp1) @
      emit "R" "MMOD" @

      ( -- MMOD computes S2 := S2 % S1
	if r2 ~= S2 then
	  emit (amode r2) (amode S2)
	else ""
      ) @
      concmap restoreAreg (difference A1_A7 [Spreg;Hpreg;Vpreg]) @
      concmap restoreSreg (difference S1_S7 [r1;r2])

and otag oeval   = "oeval"
 || otag ounwind = "ounwind"
 || otag ojfun   = "ojfun"
 || otag ogettag = "ogettag"
 || otag (onumtag i) = itos (4*i)
#if 0
and to_octal i = (to22 (itoso i)
    where rec -- convert an integer to a string oktal repr
	itoso n = if n < 0 then '-'.itoso1 (-n)
			    else itoso1 n
      and itoso1 n = if n < 8 then [chr (n + ord '0')]
			      else itoso1 (n/8) @ [chr (n%8+ord '0')]
      and -- make oktal string have length 22
	to22 s =
	  let l = length s
	  in if l > 22 then fail "assembler: To large number\n"
	     else if l = 22 then s
	     else (to22_1 (22-l) '0') @ s
      and
	 to22_1 0 c = []
      || to22_1 n c = c.(to22_1 (n-1) c))
and sadd1 "" = "" -- this should not happen
||  sadd1 ('7'.cs) = '0'.sadd1 cs
||  sadd1 (c.cs)   = (chr ((ord c) + 1)).cs
and compl c & (isdigit c) = chr (7 - (ord c) + 2*(ord '0'))
#endif
end
