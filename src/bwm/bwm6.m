module
#ifdef BWM
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../misc/util.t"
#include "../funnos.h"
#include "bwm3.t"
#include "bwm4.t"
export bwmcoder, BwmC, show_BwmC, show_BwmCs, Word, show_Word, Pcinc, show_Pcop, Opspec, show_Opspec, prcode;
rec type BwmC = BC BwmS Int (List Bool) Bool (List Opspec) (List Word) + BC2 BwmS BwmS + BWord (List Word) + BComment String
and type BwmS = BCS Bool Bool Bool Bool Pcop Aluop (List Opspec) (List Word)
and type Pcop = Pcreg Int + Pcinc + Pcnoop
and type Aluop = Anoop + Aaddsh + Aadd + Asub + Amul + Adiv + Amod + Aand + Aor + Axor + Apri + Ahlt + Aprc
and type Opspec = Ostack Int + Oheap Int + Olit Int + Opcd + Opc + Oalu + Onoop + Ounused
and type Word = Wdata Int + Wnptr Bool Int + Wcptr Int Int + Wconstr Int Bool Bool Bool Int
and show_Pcop (Pcreg n) = "pc=%"@itos n@" "
||  show_Pcop Pcinc = "pc++ "
||  show_Pcop Pcnoop = ""
and show_Aluop Anoop = ""
||  show_Aluop Aadd = "op+ "
||  show_Aluop Asub = "op- "
||  show_Aluop Amul = "op* "
||  show_Aluop Adiv = "op/ "
||  show_Aluop Amod = "op% "
||  show_Aluop Apri = "pri "
||  show_Aluop Aprc = "prc "
||  show_Aluop Ahlt = "hlt "
and show_Opspec (Ostack n) = "%"@itos n@" "
||  show_Opspec (Oheap n) = "#"@itos n@" "
||  show_Opspec (Olit n) = "$"@itos n@" "
||  show_Opspec Opcd = "pcd "
||  show_Opspec Opc = "pc "
||  show_Opspec Oalu = "alu "
||  show_Opspec Onoop = "_ "
||  show_Opspec Ounused = ""
and show_Word (Wdata i) = "D"@itos i@" "
||  show_Word (Wnptr _ 0) = "U "
||  show_Word (Wnptr s i) = "N"@itos i@opt s "!"@" "
||  show_Word (Wcptr a i) = "I"@itos i@"("@itos a@") "
||  show_Word (Wconstr a n s d i) = "C"@itos i@"("@itos a@")"@opt n "n"@opt s "s"@opt d "d"@" "
and opt f s = if f then s else ""
and show_BwmS (BCS rd wr s alua npc aop ops lits) = 
        opt rd "RD "@opt wr "WR "@opt s "S "@opt alua "A"@show_Pcop npc@show_Aluop aop@concmap show_Opspec ops@concmap show_Word lits
and show_BwmC (BC bcs adj tos ap ops lits) = 
        show_BwmS bcs@"; "@
        (if adj ~= 0 then "sp+="@itos adj@" " else "")@(if Or tos then map (\s.if s then 'P' else '_') tos@" " else "")@opt ap "auto "@
        concmap show_Opspec ops@concmap show_Word lits@"\n"
||  show_BwmC (BC2 bcs1 bcs2) = show_BwmS bcs1 @ show_BwmS bcs2 @ "\n"
||  show_BwmC (BWord ws) = "DATA "@mix (map show_Word ws) ", "@"\n"
||  show_BwmC (BComment s) = "#"@s@"\n"
and show_BwmCs bs = concmap show_BwmC bs

and unused = Wnptr false 0
and nopush = rept maxnode false

and compact bs = bs
and compactfun (FUN i n bs) = FUN i n (compact bs)

and size1 (CASE _ bss) = Sum (map sizebs bss) + 1 + jtabsize bss
||  size1 _ = 1
and sizebs bs = Sum (map size1 bs)
and sizefun (FUN _ _ bs) = sizebs bs

and scanl f q xs = q . case xs in [] : [] || x.xs' : scanl f (f q x) xs' end
and butlast = reverse o tl o reverse

and fill m bs e = bs @ rept (m - length bs) e

and modes fs bs = let (ops, litss) = split (snd (mapstate (mode fs) 0 bs)) in (ops, conc litss)
and mode fs k (Bstack n)   = (k,   (Ostack n, []))
||  mode fs k (Bheap n)    = (k,   (Oheap  n, []))
||  mode fs k (Blit i)     =((k+1, (Olit   k, [Wcptr a c])) where (a,c) = look fs i)
||  mode fs k (Bcon i j m) = (k+1, (Olit   k, [Wconstr j (m=1) false false i]))
||  mode fs k (Bint i)     = (k+1, (Olit   k, [Wdata i]))
||  mode fs k (Bnoop)      = (k,   (Onoop   , []))
||  mode fs k (Bunused)    = (k,   (Ounused , []))
||  mode fs k (Balures)    = (k,   (Oalu    , []))
and look fs i = /*trace (show_list (show_pair(dprid,show_pair(show_int,show_int))) fs)*/ (assocdefeq eqid i fs (0,99999))

and pushmask n = rept n true @ rept (maxnode-n) false

and maxns = 3
and maxls = 1

and bc rd wr s alua npc aop adj tos ap ops lits =
    BC (BCS rd wr s alua npc aop (head maxns ops) (head maxls lits)) adj tos ap (tail maxns ops) (tail maxls lits)

and build n s (ops, lits) = 
    bc false n     s false Pcinc     Anoop 0    nopush                 false (fill maxnode ops Ounused) (fill maxlit lits unused)
and update n s (ops, lits) = 
    let k = length ops in 
    bc false false s false (Pcreg 0) Anoop n    (false.tl(pushmask k)) false (fill maxnode ops Ounused) (fill maxlit lits unused)
and push s (ops, lits) =
    let n = length ops in
    let pm = pushmask n in
    let nn = fill maxnode ops Ounused in
    bc false false s false Pcinc     Anoop (-n) (last pm.tl pm)        false (last nn.tl nn)            (fill maxlit lits unused)
and casec n s (ops', lits') =
    let ops = ops' @[Olit (length lits')]
    and lits= lits'@[Wcptr 0 n] in
    update (-length ops+1) s (ops, lits)
and jtab ss = 
    if length ss < maxnode then
	[BWord (fill maxnode (map (Wcptr 0) ss) unused)]
    else
	BWord (map (Wcptr 0) (head maxnode ss)) . jtab (tail maxnode ss)
and jtabsize xss = 
    let l = length xss in
    if l > 1 then
	(l + maxnode-1)/maxnode
    else
	0
and alu op (ops, lits) =
    bc false false false false Pcinc  (aluof op) 0    nopush            false (fill maxnode ops Ounused) (fill maxlit lits unused)
and aluof op = assocdef op [(Fadd, Aadd); (Fsub, Asub); (Fmul, Amul); (Fdiv, Adiv); (Fmod, Amod)] (fail ("aluop "@itos op))

and codefun fs (FUN i _ bs) = let a = snd (look fs i) in BComment ("Function "@prid i@" at "@itos a) . code fs a bs
and code fs n (BUILD bs.xs) = build true false (modes fs bs) . code fs (n+1) xs
||  code fs n (BUILDN bs.xs) = build false false (modes fs bs) . code fs (n+1) xs
||  code fs n (UPDATE k bs.xs) = update (k-length bs+1) false (modes fs bs) . code fs (n+1) xs
||  code fs n (PUSH bs.xs) = push false (modes fs bs) . code fs (n+1) xs
||  code fs n (CASE bs xss.[]) = 
    let t = jtabsize xss in
    let ss = butlast (scanl (+) t (map sizebs xss)) in
    casec (n+1) false (modes fs bs) . (if t=0 then [] else jtab (map (+ (n+1)) ss)) @ conc (map2 (code fs) ss xss)
||  code fs n (SMALL bs.xs) = fail "SMALL"
||  code fs n (CODE c.xs) = c . code fs (n+1) xs
||  code fs n (ALU op bs.xs) = alu op (modes fs bs) . code fs n xs
||  code fs n [] = []
||  code _  _ bs = fail ("code failed "@show_Bwms bs)

and bwmcoder bs = 
    let bs' = map compactfun bs in
    let bs'' = prel (map (\ (FUN i _ _).i) bs) @ bs' in
    let ss = map sizefun bs'' in
    let fs = map2 (\ (FUN i n _).\s.(i,(n,s))) bs'' (scanl (+) 0 ss) in
    concmap (codefun fs) bs''

and bi true = "1"
||  bi false= "0"
and prpcop (Pcreg n) = itos n
||  prpcop Pcinc = "4"
||  prpcop Pcnoop = "5"
and praluop a = itos (index [a] [Anoop; Aaddsh; Aadd; Asub; Amul; Adiv; Amod; Aand; Aor; Axor; 
				 Anoop; Anoop; Anoop; Anoop; Anoop; Anoop; Ahlt; Aprc; Apri ])
and prtos bs = itos (itlist (\b.\n.if b then n*2+1 else n*2) bs 0)
and prspec (Ostack n) = itos n
||  prspec (Oheap n)  = itos (n+40)
||  prspec (Olit n)   = itos (n+56)
||  prspec Opcd       = "59"
||  prspec Opc        = "60"
||  prspec Oalu       = "61"
||  prspec Onoop      = "62"
||  prspec Ounused    = "63"
and prword (Wdata i)   = itos (i+2147483648)
||  prword (Wnptr s i) = itos (i+if s then 536870912 else 0)
||  prword (Wcptr a i) = itos (i+1073741824+a*33554432)
||  prword (Wconstr a n s p i) = itos (i+1073741824+a*33554432+16777216+
				      (if n then 8388608 else 0)+(if s then 4194304 else 0)+(if p then 2097152 else 0))
and prcode1 (BC (BCS rd wr s alua npc aop ops1 lits1) adj tos ap ops2 lits2) = mix (["I";
        bi rd; bi wr; bi s; bi alua; prpcop npc; praluop aop]@map prspec ops1@map prword lits1@
        [itos adj; prtos tos; bi ap]@map prspec ops2@map prword lits2) " " @ "\n"
||  prcode1 (BC2 _ _) = fail "BC2"
||  prcode1 (BWord ws) = "N "@mix (map prword ws) " "@"\n"
||  prcode1 (BComment s) = "#"@s@"\n"
and prcode bs = "0\n" @ concmap prcode1 bs

and prel fs = -- Prelude to start execution at Pmain and print result as a string.
    [FUN startid 0 [BUILD [Blit (findmain fs); Bcon 0 0 2]; UPDATE 0 [Blit prsid; Bheap 1]];
     FUN prsid   1 [CASE [Bstack 0] [[bhalt]; [UPDATE 4 [Bstack 0; Blit prcid; Bstack 1]]]];
     FUN prcid   2 [bprint; UPDATE 2 [Blit prsid; Bstack 1]]
    ]
and prsid = mkid 400 "Pprs" idi_udef Noorigname
and prcid = mkid 402 "Pprc" idi_udef Noorigname
and startid = mkid 401 "Pstart" idi_udef Noorigname
and bprint = CODE (BC (BCS false false false false Pcinc Aprc [Ostack 0; Ounused; Ounused] [unused]) 0 (pushmask 0) false [Ounused] [unused])
and bhalt =  CODE (BC (BCS false false false false Pcinc Ahlt [Ounused;  Ounused; Ounused] [unused]) 0 (pushmask 0) false [Ounused] [unused])
and findmain fs = case filter (\i.idtostr i = "Pmain") fs in [i] : i || _ : fail "findmain failed" end
#else
export ;
dummy=0
#endif
end
