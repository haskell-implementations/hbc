module
#include "../expr/constr_t.t"
#include "../expr/id.t"
#include "../ExprE/Expr_t.t"
#include "../Gcode/Gcodedef_t.t"
#include "../Gcode/Gprint.t"
#include "../misc/setofid.t"
#include "../misc/flags.t"
#include "../misc/misc.t"
#include "../transform/hexpr.t"
#include "mcodedef_t.t"
#include "movetext1.t"
#include "mutil.t"
#include "mutil1.t"
#include "mstrid.t"
#include "mpush.t"
#include "meval.t"
#include "mbigop.t"
#include "mbasicop.t"
#include "mpop.t"
#include "mconstr.t"
#include "mconst.t"
#include "mtype.t"
#include "mmkapl.t"
#include "mconstrret.t"
#include "mcbasic.t"
#include "msplit.t"
#include "mupdate.t"
#include "mconstrupd.t"
#include "mbconstr.t"
#include "mlabel.t"
#include "mjmp.t"
#include "mjfun.t"
#include "malloc.t"
#include "mmove.t"
#include "mcase.t"
#include "mget.t"
#include "mfunstart.t"
#include "mret.t"
#include "mjglobal.t"
#include "mjmethod.t"
#include "mmemcheck.t"
#include "mvectable.t"

export mmain, M;
rec 
   M svf t 0 (G as MKAPLV _ _._) V S = mmemcheck svf t G V S
|| M svf t 1 (MKAPLV i m.UPDATE tg n.G) V S =
	M svf t 1 (PUSHGLOBAL i. rept m (MKAP (idtopstr i)) @ UPDATE tg n. G) V S
|| M svf t 1 (MKAPLV i m.G) V S = mvap svf t G m i V S
|| M svf t g (ANNOT "VECTORDEF" . G) V S = mvectordef svf t g G 
|| M svf t g (ANNOT "REG2" . G) V S = mreg2 svf t g G V S
|| M svf t g (PUSHGLOBAL i.G) V S = mpushglobal svf t g G i V S
|| M svf t g (PUSHCFUNCTION b i.G) V S = mpushcfunction svf t g G b i V S
|| M svf t g (PUSH n.G) V S = mpush svf t g G n V S
|| M svf t g (PUSHV n.G) V S = fail "M: PUSHV not implemented" --mpushv svf t g G n V S
-- evalupdunw disturbs profiling
|| M svf t g (EVAL _.UPDATE _ 1.UNWIND.G) V S & (~(NoEvalupdunw | Profile)) = mevalupdunw svf t g G V S
|| M svf t g (EVAL ns.G) V S = meval svf t g G ns V S
|| M svf t g (TEVAL.G) V S = mteval svf t g G V S
|| M svf t g (BIGOP Gbstring op.JFALSE l.G) V S = mstrjrel svf t G op l V S
|| M svf t g (BIGOP Gbstring op.G) V S = mstrop svf t G op V S
|| M svf t g (BIGOP _ op.JFALSE l.G) V S = mbigjrel svf t G op l V S
|| M svf t g (BIGOP _ op.G) V S = mbigop svf t G op V S
|| M svf t g (BASICOP a p op.JFALSE l.G) V S & (mem op [EQ; NE; LT; GT; LE; GE]) = mjrel svf t G a p op l V
|| M svf t g (BASICOP a p op.G) V S = mbasicop svf t g G a p op V S
|| M svf t g (POP n.G) V S = mpop svf t g G n V S
|| M svf t g (POPV n.G) V S = mpopv svf t g G n V S
|| M svf t g (CONSTR _ (Gsfloat s) _ _.G) V S = mcfloat false svf t g G s V S
|| M svf t g (CONSTR _ (Gdfloat s) _ _.G) V S = mcfloat true svf t g G s V S
|| M svf t g (CONSTR _ (Ginteger s) _ _.G) V S = mcinteger svf t g G s V S
--|| M svf t g ((c as CONSTR _ _ _ _).G) V S = M svf t g (expconstr c @ G) V S
|| M svf t g (RECBLOCK gs.G) V S = mrecblock svf t g gs G V S
|| M svf t g (CONSTBLOCK gs.(G as UPDATE _ _._)) V S = M svf t g (CONSTBLOCK (butlast gs).last gs.G) V S
|| M svf t g (CONSTBLOCK gs.G) V S = mconstblock svf t g gs G V S
|| M svf t g (CNIL ct m.UPDATE _ 1.RET.G) V S = mcnilret svf ct t g G m 0 V S
|| M svf t g (CNIL ct m.UPDATE _ n.POP n1.RET.G) V S &(n-1=n1) = mcnilret svf ct t g G m n1 V S
|| M svf t g (CNIL ct m.UPDATE _ n.G) V S = mcnilupdate svf ct t g G m n V S
|| M svf t g (CNIL ct m.G) V S = mcnil svf ct t g G m V S
|| M svf t g (CPAIR ct m.UPDATE _ 1.RET.G) V S = mcpairret svf ct t g G m 0 V S
|| M svf t g (CPAIR ct m.UPDATE _ n.POP n1.RET.G) V S &(n-1=n1) = mcpairret svf ct t g G m n1 V S
|| M svf t g (CPAIR ct m.UPDATE _ n.G) V S = mcpairupdate svf ct t g G (mpairtag m) n V S
|| M svf t 1 (CPAIR ct m.G) V S = mcpair svf ct t (mpairtag m) G V S
|| M svf t g (CTAG ct m.UPDATE _ 1.RET.G) V S = mctagret svf ct t g G m 0 V S
|| M svf t g (CTAG ct m.UPDATE _ n.POP n1.RET.G) V S &(n-1=n1) = mctagret svf ct t g G m n1 V S
|| M svf t g (CTAG ct m.UPDATE _ n.G) V S = mctagupdate svf ct t g G m n V S
|| M svf t 1 (CTAG ct m.G) V S = mctag svf ct t G m V S 
|| M svf t 1 (CVEK ct m.G) V S = mcvek svf ct t G m mvektag (const m) V S
|| M svf t g (MKAP cs.UPDATE _ n.G) V S = mcpairupdate svf (cs,cs) t g G maptag n V S
|| M svf t g (MKCAP cs.UPDATE _ n.G) V S = mcpairupdate svf (cs,cs) t g G mcaptag n V S
|| M svf t 1 (MKAP cs.G) V S = mcpair svf (cs,cs) t maptag G V S
|| M svf t 1 (MKCAP cs.G) V S = mcpair svf (cs,cs) t mcaptag G V S
|| M svf t g (CBASIC bv.UPDATE _ 1.RET.G) V S = mcbasicret svf t g G bv 0 V S
|| M svf t g (CBASIC bv.UPDATE _ n.POP n1.RET.G) V S &(n-1=n1) = mcbasicret svf t g G bv n1 V S
|| M svf t g (CBASIC bv.UPDATE _ m.G) V S = mcbasicupdate svf t g G bv m V S
|| M svf t g (CBASIC bv.G) V S = mcbasic svf t g G bv V S
|| M svf t g (CSTRING s.G) V S = mcstring svf t g G s V S
--|| M svf t g (SPLIT bs v n.G) V S = M svf t g (msplit bs v n @ G) V S
|| M svf t g (SPLITTAG.G) V S = msplittag svf t g G V S
|| M svf t g (SPLITPAIR b1 b2.G) V S = msplitpair svf t g b1 b2 G V S
|| M svf t g (SPLITVEK bs m.G) V S = msplitvek svf t g bs G m V S
|| M svf t g (UPDATE tg m.G) V S = mupdate svf t g G tg m V S
|| M svf t g (UPDATEINDIR m.G) V S = mupdateindir svf t g G m V S
|| M svf t g (BCONSTR bc.UPDATE _ 1.RET.G) V S = mbconstrret svf t g G bc 0 V S
|| M svf t g (BCONSTR bc.UPDATE _ n.POP n1.RET.G) V S &(n-1=n1) = mbconstrret svf t g G bc n1 V S
|| M svf t g (BCONSTR bc.UPDATE _ m.G) V S = mbconstrupdate svf t g G bc m V S
|| M svf t 1 (BCONSTR bc.G) V S = mbconstr svf t G bc V S
|| M svf t g (LABEL l.G) V S = mlabel svf t G l
|| M svf t g (JMP l.G) V S = mjmp svf t G l
|| M svf t g (JFALSE l.G) V S = mjfalse svf t G V l
|| M svf t g (JTRUE l.G) V S = mjtrue svf t G V l
|| M svf t 1 (JFUN m.G) V S = mjfun svf t G m S
|| M svf t 1 (CALLFUN m ns.G) V S = mcallfun svf t G m ns V S
|| M svf t g (UNWIND.G) V S = munwind svf t G S
|| M svf t 1 (ALLOC m.G) V S = malloc svf t G m V S
|| M svf t g (MOVE m.G) V S = mmove svf t g G m V S
|| M svf t g (CASE n cl ldef.G) V S = mcase svf t G n cl ldef V 
|| M svf t g (GETTAG.CASE n cl ldef.G) V S = mgettagcase svf t g G n cl ldef V S
|| M svf t g (PUSHBASIC x.G) V S = mpushbasic svf t g G x V S
|| M svf t g (GET Gbint.G) V S = mget svf t g G V S
|| M svf t g (GET Gbsfloat.G) V S = mgetf false svf t g G V S
|| M svf t g (GET Gbdfloat.G) V S = mgetf true svf t g G V S
|| M svf t g (GETTAG.G) V S = mgettag svf t g G V S
|| M svf t g (GETMETHOD k.G) V S = mgetmethod svf k t g G V S
--|| M svf t g (FUNSTART i m.G) V S = mfunstart svf t G i m
|| M svf t g (SFUNSTART i m.G) V S = msfunstart svf t G i m
|| M svf t g (FUNEND.G) V S = mfunend svf t g G V S
--|| M svf t g (CMVECTOR i is.G) V S = mmvector svf i is t g G V S
|| M svf t g (RET.G) V S = mret svf t G S
|| M svf t g (JGLOBAL m i.G) V S = mjglobal svf t G i m S
|| M svf t g (CALLGLOBAL m i ns.G) V S = mcallglobal svf t G i m ns S
|| M svf t g (SCALLGLOBAL m i.G) V S = mscallglobal svf t G i m
|| M svf t g (JMETHOD m i.G) V S = mjmethod svf t G i m S
|| M svf t g (CALLMETHOD m i ns.G) V S = mcallmethod svf t G i m ns S
|| M svf t g (AMODE _.G) V S = M svf t g G V S
|| M svf t g (REST ((i, m), G, oG).[]) V S = mfunstart svf t G oG i m
|| M svf t 0 (i.G) V S = mmemcheck svf t (i.G) V S
|| M svf t g [] V S = (vectable svf, [], [], [], 0)
|| M svf _ g (i._) _ _ = fail ("M:g="@itos g@"i="@Gprint [i])

and ctnil = (idtostr hinil, idtostr hiList)
and ctcons = (idtostr hicons, idtostr hiList)

and flagref = [Mdata;
               Mword (glob (     if ProfileHeap then "_USE_heap"
			    else if Profile     then "_USE_time"
			    else                     "_USE_none"));
               Mtext]

and mmain gcode =
	let (c, _, _, _, _) = M ([],[],"?") 0 0 (collapse (expc gcode)) [] []
	in flagref @ movetext1 c

and collapse [] = []
||  collapse ((n, gs, ogs).xs) = [REST (n, gs @ ogc n ogs @ FUNEND . collapse xs, ogs)]
and ogc _      None      = []
||  ogc (i, m) (Some gs) = SFUNSTART i m . gs

and expc g = map (\ (n,gs,ogs).(n, concmap expconstr gs, oapply (concmap expconstr) ogs)) g
and expconstr (CONSTR ct Gtype v n) = mconstr ct v n
||  expconstr (CONSTR _ (Gstring[]) _ _) = [CNIL ctnil 0]
||  expconstr (CONSTR _ (Gstring[c]) _ _) = [CNIL ctnil 0; CBASIC (GvChar c); CPAIR ctcons 1]
||  expconstr (CONSTR _ (Gstring s) _ _) = [CSTRING s]
||  expconstr (CONSTR _ Gint  x _) = [CBASIC (GvInt x)]
||  expconstr (CONSTR _ Gchar x _) = [CBASIC (GvChar (chr x))]
||  expconstr (RECBLOCK gss) = [RECBLOCK (map (concmap expconstr) gss)]
||  expconstr (CONSTBLOCK gs) = [CONSTBLOCK (concmap expconstr gs)]
||  expconstr (SPLIT bs v n) = msplit bs v n
||  expconstr g = [g]
end
