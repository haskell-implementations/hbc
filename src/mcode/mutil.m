module
#include "../misc/flags.t"
#include "../Gcode/Gcodedef_t.t"
#include "machine.tt"
#include "mcodedef_t.t"
#include "Wuse.t"
#include "mregs.t"
#include "mvectable.t"

export 	H, Ha, newheapS, newS, newV, newSF, newDF, allpopV, allpopS, allWpush,
	intoareg, intoreg0, prepWuse, indS, strbc, strop, isbinarithop, mgop,
	mvaptag, mvapgtag, mapgtag, maptag, mcaptag, mvektag, mtagtag, 
	mniltag, mpairtag, mindirtag, strbv, valbv, tostack,
        hpwords, hpmoves, hpmsize, constrbc, constrbv, mreturn;
rec
    H n (hprel x) = hprel(x-n)
 || H n a = a
and
    Ha n (hprel x.S) = (hprel (x-n)).Ha n S
 || Ha n (popS   .S) = popS.S
 || Ha n (  a    .S) = a.Ha n S
 || Ha n [] = fail "fail Ha: empty S"
and
    newheapS n (Wpush)  S c = (Mmove (hprel(-n)) pushS.c, popS.S)
 || newheapS n (Wreg r) S c = (Mmove (hprel(-n)) (reg r).c, (reg r).Ha n S)
 || newheapS n (Wuse)   S c = (c , hprel(-n).Ha n S)
and
    newS a (Wpush) S c = (Mmove a pushS.c , popS.S)
 || newS (reg q) (Wreg r) S c = ((if q=r then c else Mmove (reg q) (reg r).c), reg r.S)
 || newS a (Wreg r) S c = (Mmove a (reg r).c , reg r.S)
 || newS a (Wuse) S c = (c , a.S)
 || newS _ _ _ _ = fail "newS"
and
    newV a (Wpush) V c = (Mmove a pushV.c , popV.V)
 || newV (reg q) (Wreg r) V c = ((if q=r then c else Mmove (reg q) (reg r).c) 
		 	      ,reg r.V)
 || newV a (Wreg r) V c = (Mmove a (reg r).c , reg r.V)
 || newV a (Wuse) V c = (c , a.V)
 || newV _ _ _ _ = fail "newV"
and
    newDF a (Wpush) V c = (Mmovedf a pushV.c , popV.V)
 || newDF (reg q) (Wreg r) V c = ((if q=r then c else Mmovedf (reg q) (reg r).c)
		 	      ,reg r.V)
 || newDF a (Wreg r) V c = (Mmovedf a (reg r).c , reg r.V)
 || newDF a (Wuse) V c = (c , a.V)
 || newDF _ _ _ _ = fail "newDF"
and
    newSF a (Wpush) V c = (Mmovesf a pushV.c , popV.V)
 || newSF (reg q) (Wreg r) V c = ((if q=r then c else Mmovesf (reg q) (reg r).c)
		 	      ,reg r.V)
 || newSF a (Wreg r) V c = (Mmovesf a (reg r).c , reg r.V)
 || newSF a (Wuse) V c = (c , a.V)
 || newSF _ _ _ _ = fail "newSF"
and
    allpopV = popV.allpopV
and allpopS = popS.allpopS
and allWpush = Wpush.allWpush
and
    intoareg (reg r) V S = [], r
 || intoareg a V S = let r = gareg V S in
 			[ Mmove a (reg r) ] , r
and
    intoreg0 (reg 0) c = c
 || intoreg0 a c = Mmove a (reg 0).c
and
    prepWuse 0 l = l
 || prepWuse n l = Wuse. prepWuse (n-1) l
and
    indS n (popS.S) = Sind n
 || indS 0 (x.S) = x
 || indS n (x.S) = indS (n-1) S
 || indS _ _ = fail "indS"
and
    strbc Gbint  = "INT"
 || strbc Gbchar = "CHAR"
 || strbc Gbtag  = "TAG0"
 || strbc Gbsfloat= "SFLOAT"
 || strbc Gbdfloat= "DFLOAT"
and strbv (GvInt _)  = "INT"
 || strbv (GvChar _) = "CHAR"
 || strbv (GvSFloat _) = "SFLOAT"
 || strbv (GvDFloat _) = "DFLOAT"
and valbv (GvInt i) = i
||  valbv (GvChar c) = ord c
and constrbc Gbint  = "Int"
 || constrbc Gbchar = "Char"
 || constrbc Gbtag  = "Bool"
 || constrbc Gbsfloat= "SFloat"
 || constrbc Gbdfloat= "DFloat"
and constrbv (GvInt _)  = "Int"
 || constrbv (GvChar _) = "Char"
 || constrbv (GvSFloat _) = "SFloat"
 || constrbv (GvDFloat _) = "DFloat"
and
    strop ADD = "ADD"
 || strop SUB = "SUB"
 || strop MUL = "MUL"
 || strop DIV = "DIV"
 || strop MOD = "MOD"
 || strop NEG = "NEG"
 || strop EQ  = "EQ"
 || strop NE  = "NE"
 || strop GT  = "GT"
 || strop LT  = "LT"
 || strop LE  = "LE"
 || strop GE  = "GE"
 || strop CHR = "CHR"
 || strop ORD = "ORD"
 || strop TAG = "TAG"
 || strop AND = "AND"
 || strop OR  = "OR"
 || strop XOR = "XOR"
 || strop COMPL = "COMPL"
 || strop LSH = "LSH"
 || strop RSH = "RSH"
 || strop RSHA = "RSHA"
 || strop FTOI = "FTOI"
 || strop ITOF = "ITOF"
 || strop DFTOSF = "DFTOSF"
 || strop SFTODF = "SFTODF"
 || strop INDEX = "INDEX"
 || strop SQR = "SQR"
and
    isbinarithop ADD = true
 || isbinarithop SUB = true
 || isbinarithop MUL = true
 || isbinarithop DIV = true
 || isbinarithop MOD = true
 || isbinarithop AND = true
 || isbinarithop OR  = true
 || isbinarithop XOR = true
 || isbinarithop LSH = true
 || isbinarithop RSH = true
 || isbinarithop RSHA = true
 || isbinarithop  _  = false
and
    mgop Gbdfloat ADD = dfadd
 || mgop Gbdfloat SUB = dfsub
 || mgop Gbdfloat MUL = dfmul
 || mgop Gbdfloat DIV = dfdiv
 || mgop Gbdfloat NEG = dfneg
 || mgop Gbsfloat ADD = sfadd
 || mgop Gbsfloat SUB = sfsub
 || mgop Gbsfloat MUL = sfmul
 || mgop Gbsfloat DIV = sfdiv
 || mgop Gbsfloat NEG = sfneg
 || mgop Gbint   ADD = add
 || mgop Gbint   SUB = sub
 || mgop Gbint   MUL = mul
 || mgop Gbint   DIV = div
 || mgop Gbint   MOD = mod 
 || mgop Gbint   NEG = neg
 || mgop Gbint   AND = btand
 || mgop Gbint   OR  = btor
 || mgop Gbint   XOR = btxor
 || mgop Gbint   COMPL= btcompl
 || mgop Gbint   LSH = btlsh
 || mgop Gbint   RSH = btrsh
 || mgop Gbint   RSHA = btrsha

and maptag     = idlit "AP"
and mapgtag    = idlit "APG"
and mvaptag    = idlit "VAP"
and mvapgtag   = idlit "VAPG"
and mcaptag    = idlit "CAP"
and mtagtag    = idlit "TAG"
and mniltag    = idlit "TAG0"
and mvektag    = idlit "VEK"
and mindirtag  = idlit "INDIR"
and mpairtag n = if n<0 then idlit "PAIR" else idlit ("PAIR"@itos n)

/*
 *	The dynamic field is currently always zero. If we ever get serious 
 *	about using it, then it will have to be passed to these routines 
 *	as a parameter.	
 */

/*#define DYNAMIC	(const 0)*/

and hpwords vec =
	if ProfileHeap then
	    [ Mword (idlit (v2l vec))
--	    ; Mword DYNAMIC
	    ]
	else
	    []

and hpmoves vec tohp =
	if ProfileHeap then
            [ Mmove (idlit (v2l vec)) tohp
--	    ; Mmove DYNAMIC           tohp
            ]
	else
	    []
||  hpmoves vec (regind r n) =
	if ProfileHeap then
	    [ Mmove (idlit (v2l vec)) (regind r n)
--	    ; Mmove DYNAMIC           (regind r (n+1))
            ]
	else
	    []

and hpmsize n = if ProfileHeap then n+1 else n

and mreturn = if Trace then [Mcall "do_return"; Mreturn] else [Mreturn]

and tostack :: Int -> [Addrmode] -> [Mcode]
and tostack 0 S = []
||  tostack _ (popS._) = []
||  tostack n (s.ss) = tostack (n-1) ss @ [Mmove s pushS]
end
