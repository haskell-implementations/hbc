module
#include "../misc/pri.t"
#include "../misc/text_t.t"
#include "../misc/Tflat.t"
#include "../expr/id_t.t"
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "Gcodedef_t.t"

export Gprint, Gprints;
rec
    Gprid i = Ts (idtostr i)

and Gprlabel (Label l) = Tl[Ts"L"; Ti l]
 || Gprlabel Notalabel = Ts"Lno"

and Gprbconstr(Gbint) = Ts"INT"
 || Gprbconstr(Gbchar) = Ts"CHAR"
 || Gprbconstr(Gbtag) = Ts"TAG0"
 || Gprbconstr(Gbdfloat) = Ts"DFLOAT"
 || Gprbconstr(Gbsfloat) = Ts"SFLOAT"
 || Gprbconstr(Gbinteger) = Ts"BIGNUM"
 || Gprbconstr(Gbstring) = Ts"STRING"
 || Gprbconstr(Gbother) = Ts"DATA"

and Gprconstr(Gint) = Ts"INT"
 || Gprconstr(Gchar) = Ts"CHAR"
 || Gprconstr(Gstring s) = Tl[Ts"STRING \""; Ts s; Ts"\""]
 || Gprconstr(Gtype) = Ts"TYPE"
 || Gprconstr(Gdfloat _) = Ts"DFLOAT"
 || Gprconstr(Gsfloat _) = Ts"SFLOAT"
 || Gprconstr(Ginteger _) = Ts"INTEGER"

and Gprcasel l = Tl (map (\(i,n,l).Tl[pri "(^," [i]; Gprlabel l; Ts")" ]) l)

and Gprbas ADD = Ts"ADD"
 || Gprbas SUB = Ts"SUB"
 || Gprbas MUL = Ts"MUL"
 || Gprbas DIV = Ts"DIV"
 || Gprbas MOD = Ts"MOD"
 || Gprbas NEG = Ts"NEG"
 || Gprbas EQ  = Ts"EQ"
 || Gprbas NE  = Ts"NE"
 || Gprbas LT  = Ts"LT"
 || Gprbas GT  = Ts"GT"
 || Gprbas LE  = Ts"LE"
 || Gprbas GE  = Ts"GE"
 || Gprbas CHR = Ts"CHR"
 || Gprbas ORD = Ts"ORD"
 || Gprbas TAG = Ts"TAG"
 || Gprbas AND = Ts"AND"
 || Gprbas OR = Ts"OR"
 || Gprbas XOR = Ts"XOR"
 || Gprbas COMPL = Ts"COMPL"
 || Gprbas LSH = Ts"LSH"
 || Gprbas RSH = Ts"RSH"
 || Gprbas RSHA = Ts"RSHA"
 || Gprbas ITOF = Ts"ITOF"
 || Gprbas FTOI = Ts"FTOI"
 || Gprbas INDEX = Ts"INDEX"
 || Gprbas SFTODF = Ts"FTODF"
 || Gprbas DFTOSF = Ts"FTOSF"
 || Gprbas SQR = Ts"SQR"

and Gprt Gbdfloat = Ts"D"
||  Gprt Gbsfloat = Ts"S"
||  Gprt _ = Ts""

and Gprall Aheap = Ts"HEAP"
 || Gprall Astack = Ts"STACK"

and prstubs ns = map (\ n . pri " ^" [n]) ns

and Gpr (PUSH n) 	= pri "PUSH ^" [n]
 || Gpr (PUSHV n) 	= pri "PUSHV ^" [n]
 || Gpr (PUSHGLOBAL i) 	= Tl [Ts"PUSHGLOBAL "; Gprid i]
 || Gpr (PUSHCFUNCTION _ i)= Tl [Ts"PUSHCFUNCTION "; Ts i]
 || Gpr (EVAL ns)	= Tl (Ts "EVAL" . prstubs ns)
 || Gpr (TEVAL)         = Ts "TEVAL"
 || Gpr (LABEL l) 	= Tl[ Gprlabel l; Ts":"]
 || Gpr (JMP l) 	= Tl[ Ts"JMP "; Gprlabel l]
 || Gpr (JFALSE l) 	= Tl[ Ts"JFALSE "; Gprlabel l]
 || Gpr (JTRUE l)       = Tl[ Ts"JTRUE "; Gprlabel l]
 || Gpr (JFUN i)	= pri "JFUN ^" [i]
 || Gpr (CALLFUN n ns)	= Tl (pri "CALLFUN ^" [n] . prstubs ns)
 || Gpr (UNWIND)	= Ts"UNWIND"
 || Gpr (ALLOC n)	= Tl[ pri "ALLOC ^" [n]]
 || Gpr (BCONSTR bc)	= Tl[ Ts"BCONSTR "; Gprbconstr bc]
 || Gpr (MKAP _)	= Ts"MKAP"
 || Gpr (MKCAP _)	= Ts"MKCAP"
 || Gpr (MKAPLV i n)	= Ts("MKAPLV "@prid i@" "@itos n)
 || Gpr (UPDATE tg n)	= Tl[Ts"UPDATE "; Gprbconstr tg; Ts" "; Ts(itos n)]
 || Gpr (UPDATEINDIR n)	= pri "UPDATEINDIR ^" [n]
 || Gpr (MOVE n)	= pri "MOVE ^" [n]
 || Gpr (CONSTR _ c n m)	= Tl[ Ts"CONSTR " ; Gprconstr c; pri " ^ ^"[n;m] ]
 || Gpr (GETTAG)	= Ts"GETTAG"
 || Gpr (CASE n l ld)	= Tl[ pri"CASE ^ "[n]; Gprcasel l; Gprlabel ld]
 || Gpr (SPLIT bs cno n)= Tl [pri "SPLIT ^ ^ " [cno;n]; Ts (map (\x.if x then '1' else '0') bs)]
 || Gpr (SPLITPAIR _ _) = Ts"SPLITPAIR"
 || Gpr (BASICOP ga gb bo)	= Tl[ Ts"BASICOP "; Gprt ga; Gprt gb; Gprbas bo]
 || Gpr (BIGOP Gbstring bo)	= Tl[ Ts"STR"; Gprbas bo]
 || Gpr (BIGOP _ bo)	= Tl[ Ts"BIG"; Gprbas bo]
 || Gpr (PUSHBASIC (GvInt x))	= pri "PUSHBASIC ^" [x]
 || Gpr (PUSHBASIC (GvDFloat x))	= Ts ("PUSHBASIC " @ fmtf ".16e" x)
 || Gpr (PUSHBASIC (GvSFloat x))	= Ts ("PUSHBASIC " @ fmtf ".9e" x)
 || Gpr (PUSHBASIC _)	= Ts "PUSHBASIC ?"
 || Gpr (POP n)		= pri "POP ^" [n]
 || Gpr (POPV n)	= pri "POPV ^" [n]
 || Gpr (GET gb)	= Tl [Ts"GET"; Gprt gb]
 || Gpr (GETMETHOD k)	= pri "GETMETHOD ^" [k]
 || Gpr (RECBLOCK gs)   = Tl [ Ts "  RECBLOCK\n"; Ts (mix (map Gprint gs) "\t  RECBLOCKNEXT\n"); Ts "\t  RECBLOCKEND"]
 || Gpr (CONSTBLOCK gs) = Tl [ Ts "  CONSTBLOCK\n"; Ts (Gprint gs); Ts "\t  CONSTBLOCKEND" ]
 || Gpr (UPDTRACE is)   = Tl ( Ts "UPDTRACE" . map (\x.pri " ^" [x]) is)
-- || Gpr (FUNSTART i n)	= Tl [Ts"FUNSTART "; Gprid i; pri " ^" [n] ]
-- || Gpr (SFUNSTART i n) = Tl [Ts"SFUNSTART "; Gprid i; pri " ^" [n] ]
 || Gpr (FUNEND)	= Ts"FUNEND\n"
 || Gpr (REST _)        = Ts"REST ???"
 || Gpr (RET)		= Ts"RET"
 || Gpr (JGLOBAL n i) 	= Tl [pri "JGLOBAL ^ " [n]; Gprid i]
 || Gpr (CALLGLOBAL n i ns)= Tl (pri "CALLGLOBAL ^ " [n] . Gprid i . prstubs ns)
 || Gpr (JMETHOD n i) 	= Tl [pri "JMETHOD ^ ^" [n; i]]
 || Gpr (CALLMETHOD n i ns)= Tl (pri "CALLMETHOD ^ ^" [n; i] . prstubs ns)
 || Gpr (SCALLGLOBAL n i)= Tl [pri "SCALLGLOBAL ^ " [n]; Gprid i]
 || Gpr (BUPDRET bc n)	= Tl[ Ts"BUPDRET "; Gprbconstr bc; pri " ^" [n]]
 || Gpr (CUPDRET c n m k)=Tl[ Ts"CUPDRET "; Gprconstr c; pri " ^ ^ ^"[n;m;k] ]
 || Gpr (AMODE a)	= Tl[ Ts"AMODE "; Gprall a ]
 || Gpr (ANNOT s)	= Tl[ Ts"ANNOT "; Ts s ]
-- || Gpr (CMVECTOR i is)	= Tl (Ts "CMVECTOR ".Gprid i.Ts "; ".map Gprid is)
 || Gpr (CNIL _ i)      = pri "CNIL ^" [i]
 || Gpr (CPAIR _ _)     = Ts "CPAIR"
 || Gpr (CTAG _ i)      = pri "CNIL ^" [i]
 || Gpr (CVEK _ i)      = pri "CNIL ^" [i]
 || Gpr (CBASIC _)      = Ts "CBASIC ..."
 || Gpr (CSTRING _)     = Ts "CSTRING ..."
and Gprint code =
	let f instr =
	   Tl [	
		let islabel (LABEL i) = true
		 || islabel _ = false
		in
		if islabel instr then Ts[] else Ts"\t";
		Gpr instr;
		Ts "\n"
	   ]
	in
	   Tflat (Tl (map f code)) []

and Gprints l = concmap gp l
and gp ((f, l), gs, None) = "\tFUNSTART "@idtostr f@" "@itos l@"\n" @ Gprint gs @ "\tFUNEND\n\n"
||  gp ((f, l), gs, Some gs') = "\tFUNSTART "@idtostr f@" "@itos l@"\n" @ Gprint gs @ "\tFUNEND\n\n" @
                                "\tSFUNSTART "@idtostr f@" "@itos l@"\n" @ Gprint gs' @ "\tFUNEND\n\n"
end
