module
#include "../expr/einfo_t.t"
#include "../expr/ttype_t.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../ExprE/Expr_t.t"
#include "../misc/flags.t"
#include "../funnos.h"
#include "Gseq.t"
#include "Genv.t"
#include "Gcodedef_t.t"
#include "Gmode_t.t"
#include "Gcode.t"
/*import G: Gmode->Expr->Genv->Int->Glabel->Int ->
		(Glabel->((List Gcode)#Glabel));*/

export 	Gjmp, Gpop, Gslide, Gcpushrev, pushvar, updstack, prebasicarg,
	prebasicconstr, prebasicop, gnoargs, typenamefromtinfo,
	bigop, lstrop, Gdoeval, Eisconst, cleanG;
rec
    Gjmp R l = Gi []
 || Gjmp _ l = Gi [ JMP l ]
and Gpop 0 = Gi []
 || Gpop n = Gi [ POP n ]
and Gslide 0 = Gi []
 || Gslide n = Gseq[ Gi [ MOVE n ]; Gpop (n-1) ]
and Gcpushrev el r n = Gseq (map (\(e,n).G (C n) e r n Notalabel 0)
		                  (combine (reverse el, from n)))

and prebasicarg Fdfeq   = Gbdfloat
 || prebasicarg Fdfne   = Gbdfloat
 || prebasicarg Fdflt   = Gbdfloat
 || prebasicarg Fdfle   = Gbdfloat
 || prebasicarg Fdfgt   = Gbdfloat
 || prebasicarg Fdfge   = Gbdfloat
 || prebasicarg Fdfadd  = Gbdfloat
 || prebasicarg Fdfsub  = Gbdfloat
 || prebasicarg Fdfmul  = Gbdfloat
 || prebasicarg Fdfdiv  = Gbdfloat
 || prebasicarg Fdfmod  = Gbdfloat
 || prebasicarg Fdfneg  = Gbdfloat
 || prebasicarg Fdftoi  = Gbdfloat
 || prebasicarg Fdftosf = Gbdfloat
 || prebasicarg Fsfeq   = Gbsfloat
 || prebasicarg Fsfne   = Gbsfloat
 || prebasicarg Fsflt   = Gbsfloat
 || prebasicarg Fsfle   = Gbsfloat
 || prebasicarg Fsfgt   = Gbsfloat
 || prebasicarg Fsfge   = Gbsfloat
 || prebasicarg Fsfadd  = Gbsfloat
 || prebasicarg Fsfsub  = Gbsfloat
 || prebasicarg Fsfmul  = Gbsfloat
 || prebasicarg Fsfdiv  = Gbsfloat
 || prebasicarg Fsfmod  = Gbsfloat
 || prebasicarg Fsfneg  = Gbsfloat
 || prebasicarg Fsftoi  = Gbsfloat
 || prebasicarg Fsftodf = Gbsfloat
 || prebasicarg Fsfsquare = Gbsfloat
 || prebasicarg Fdfsquare = Gbdfloat
 || prebasicarg _      = Gbint
and prebasicop Fadd   = ADD
 || prebasicop Fsub   = SUB
 || prebasicop Fmul   = MUL
 || prebasicop Fdiv   = DIV
 || prebasicop Fmod   = MOD
 || prebasicop Fneg   = NEG
 || prebasicop Feq    = EQ
 || prebasicop Fne    = NE
 || prebasicop Flt    = LT
 || prebasicop Fle    = LE
 || prebasicop Fgt    = GT
 || prebasicop Fge    = GE
 || prebasicop Fsfeq   = EQ
 || prebasicop Fsfne   = NE
 || prebasicop Fsflt   = LT
 || prebasicop Fsfle   = LE
 || prebasicop Fsfgt   = GT
 || prebasicop Fsfge   = GE
 || prebasicop Fdfeq   = EQ
 || prebasicop Fdfne   = NE
 || prebasicop Fdflt   = LT
 || prebasicop Fdfle   = LE
 || prebasicop Fdfgt   = GT
 || prebasicop Fdfge   = GE
 || prebasicop Fbigeq = EQ
 || prebasicop Fbigne = NE
 || prebasicop Fbiglt = LT
 || prebasicop Fbigle = LE
 || prebasicop Fbiggt = GT
 || prebasicop Fbigge = GE
 || prebasicop Fstreq = EQ
 || prebasicop Fstrne = NE
 || prebasicop Fstrlt = LT
 || prebasicop Fstrle = LE
 || prebasicop Fstrgt = GT
 || prebasicop Fstrge = GE
 || prebasicop Ford   = ORD
 || prebasicop Fchr   = CHR
 || prebasicop Ftag   = TAG
 || prebasicop Fand   = AND
 || prebasicop For    = OR
 || prebasicop Fxor   = XOR
 || prebasicop Fcompl = COMPL
 || prebasicop Frsh   = RSH
 || prebasicop Frsha  = RSHA
 || prebasicop Flsh   = LSH
 || prebasicop Fsfadd  = ADD
 || prebasicop Fsfsub  = SUB
 || prebasicop Fsfmul  = MUL
 || prebasicop Fsfdiv  = DIV
 || prebasicop Fsfmod  = MOD
 || prebasicop Fsfneg  = NEG
 || prebasicop Fdfadd  = ADD
 || prebasicop Fdfsub  = SUB
 || prebasicop Fdfmul  = MUL
 || prebasicop Fdfdiv  = DIV
 || prebasicop Fdfmod  = MOD
 || prebasicop Fdfneg  = NEG
 || prebasicop Fdftoi  = FTOI
 || prebasicop Fitodf  = ITOF
 || prebasicop Fsftoi  = FTOI
 || prebasicop Fitosf  = ITOF
 || prebasicop Fsftodf = SFTODF
 || prebasicop Fdftosf = DFTOSF
 || prebasicop Fsquare = SQR
 || prebasicop Fsfsquare = SQR
 || prebasicop Fdfsquare = SQR

and prebasicconstr Fadd = Gbint
||  prebasicconstr Fsub = Gbint
||  prebasicconstr Fmul = Gbint
||  prebasicconstr Fdiv = Gbint
||  prebasicconstr Fmod = Gbint
||  prebasicconstr Fneg = Gbint
||  prebasicconstr Fsfadd = Gbsfloat
||  prebasicconstr Fsfsub = Gbsfloat
||  prebasicconstr Fsfmul = Gbsfloat
||  prebasicconstr Fsfdiv = Gbsfloat
||  prebasicconstr Fsfmod = Gbsfloat
||  prebasicconstr Fsfneg = Gbsfloat
||  prebasicconstr Fsftoi = Gbint
||  prebasicconstr Fitosf = Gbsfloat
||  prebasicconstr Fdftosf= Gbsfloat
||  prebasicconstr Fdfadd = Gbdfloat
||  prebasicconstr Fdfsub = Gbdfloat
||  prebasicconstr Fdfmul = Gbdfloat
||  prebasicconstr Fdfdiv = Gbdfloat
||  prebasicconstr Fdfmod = Gbdfloat
||  prebasicconstr Fdfneg = Gbdfloat
||  prebasicconstr Fdftoi = Gbint
||  prebasicconstr Fitodf = Gbdfloat
||  prebasicconstr Fsftodf= Gbdfloat
||  prebasicconstr Ford = Gbint
||  prebasicconstr Fchr = Gbchar
||  prebasicconstr Feq = Gbtag
||  prebasicconstr Fne = Gbtag
||  prebasicconstr Flt = Gbtag
||  prebasicconstr Fle = Gbtag
||  prebasicconstr Fgt = Gbtag
||  prebasicconstr Fge = Gbtag
||  prebasicconstr Fsfeq = Gbtag
||  prebasicconstr Fsfne = Gbtag
||  prebasicconstr Fsflt = Gbtag
||  prebasicconstr Fsfle = Gbtag
||  prebasicconstr Fsfgt = Gbtag
||  prebasicconstr Fsfge = Gbtag
||  prebasicconstr Fdfeq = Gbtag
||  prebasicconstr Fdfne = Gbtag
||  prebasicconstr Fdflt = Gbtag
||  prebasicconstr Fdfle = Gbtag
||  prebasicconstr Fdfgt = Gbtag
||  prebasicconstr Fdfge = Gbtag
||  prebasicconstr Fbigeq = Gbtag
||  prebasicconstr Fbigne = Gbtag
||  prebasicconstr Fbiglt = Gbtag
||  prebasicconstr Fbigle = Gbtag
||  prebasicconstr Fbiggt = Gbtag
||  prebasicconstr Fbigge = Gbtag
||  prebasicconstr Fstreq = Gbtag
||  prebasicconstr Fstrne = Gbtag
||  prebasicconstr Fstrlt = Gbtag
||  prebasicconstr Fstrle = Gbtag
||  prebasicconstr Fstrgt = Gbtag
||  prebasicconstr Fstrge = Gbtag
||  prebasicconstr Ftag   = Gbtag
||  prebasicconstr Fcno   = Gbint
||  prebasicconstr Fand   = Gbint
||  prebasicconstr For    = Gbint
||  prebasicconstr Fxor   = Gbint
||  prebasicconstr Fcompl = Gbint
||  prebasicconstr Frsh   = Gbint
||  prebasicconstr Frsha  = Gbint
||  prebasicconstr Flsh   = Gbint
||  prebasicconstr Fsquare= Gbint
||  prebasicconstr Fdfsquare=Gbdfloat
||  prebasicconstr Fsfsquare=Gbsfloat

and gnoargs i = arity_of_id i
and push i r n =
	case lookenv r i in
	   0: Gi [ PUSHGLOBAL i ]
	|| m: Gi [ PUSH (n-m) ]
	end
and pushvar i r n s =
		Gseq [  push i r n;
			Gslide (n-s)
		]
and updstack i r n =
    if Indir then
	case lookenv r i in
	   0: Gi []
	|| m: Gi [PUSH 0; MOVE (n-m+2)]
	end
    else
	Gi []
and bigop n = mem n [Fbigeq; Fbigne; Fbigle; Fbigge; Fbiglt; Fbiggt]
and lstrop n = mem n [Fstreq; Fstrne; Fstrle; Fstrge; Fstrlt; Fstrgt]
and Gdoeval is r n =
	Gseq (map (\i.Gseq [push i r n; Gi [EVAL []]; updstack i r n; Gi [POP 1]]) is)
and
   Eisconst (Econstr _ el) = all Eisconst el
|| Eisconst (Eidapl i el) & (gnoargs i ~= -1) = all Eisconst el
|| Eisconst _ = false

and typenamefromtinfo (mktinfo tt _ _ _ _ _ _ _) =
	let rec f (mktcons id _) = idtostr id
	     || f (mktcontype _ t) = f t
	     || f _ = "UNKNOWN"
	in
	    f tt
and cleanG gs = all clean1 gs
and clean1 (PUSH _) = true
||  clean1 (PUSHGLOBAL _) = true
||  clean1 (CONSTR _ _ _ _) = true
||  clean1 (MKAP _) = true
||  clean1 (MKCAP _) = true
||  clean1 (MKAPLV _ _) = true
||  clean1 _ = false
end
