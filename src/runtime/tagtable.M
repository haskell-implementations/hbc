#include "machdep.M"
#include "tagtable.h"

#include "gc.h"


#ifdef m_hppa
#define E(x)	.pragma	C+x
#define A(x)	.pragma D+x

E(Ecmp)
E(Egcret)
E(Ehole)
E(Ejbig)
E(Ejchr)
E(Ejdvk)
E(Ejflt)
E(Ejgcr)
E(Ejhol)
E(Ejind)
E(Ejinp)
E(Ejint)
E(Ejmkd)
E(Ejmvd)
E(Ejpa0)
E(Ejpa1)
E(Ejpa2)
E(Ejpa3)
E(Ejpa4)
E(Ejpai)
E(Ejstr)
E(Ejta0)
E(Ejtag)
E(Ejvek)
E(Ejzap)
E(Emkd)
E(Emvd)
E(Eprt)
E(Errg)
E(Errgs_moved)
E(Erru)
E(Ezap)
E(JM2)
E(JM3)
E(JM3u)
E(JMap)
E(JMfun)
E(JMindF)
E(JMini)
E(JMinpF)
E(JMmrk)
E(JMmvd)
E(JMret)
E(JMstrF)
E(JMstrnF)
E(JMvap)
E(JMvapG)
E(JMvek)
E(Poprsb)
E(Rsb)
E(apev)
E(apevG)
E(apunw)
E(apunwG)
E(bcmpb)
E(bprt)
E(cprt)
E(dcmp)
E(dprt)
E(f1p)
E(f2p)
E(ferr)
E(fev)
E(fevi)
E(fret)
E(funj)
E(fununw)
E(fvek)
E(g2cfun)
E(g2cgcret)
E(g2cgsret)
E(g2cindir)
E(g2cmkd)
E(g2cmvd)
E(g2d11)
E(g2d12)
E(g2d20)
E(g2d21)
E(g2d30)
E(g2dap)
E(g2dapG)
E(g2dchr)
E(g2ddvek)
E(g2dfun)
E(g2dgcret)
E(g2dgsret)
E(g2dhole)
E(g2dind)
E(g2dindir)
E(g2dinp)
E(g2dint)
E(g2dmkd)
E(g2dmvd)
E(g2dtag0)
E(g2dvap)
E(g2dvapG)
E(g2dvek)
E(g2dzap)
E(g2l11)
E(g2l12)
E(g2l20)
E(g2l21)
E(g2l30)
E(g2lE)
E(g2ldvek)
E(g2lfun)
E(g2lindir)
E(g2lmvd)
E(g2lvap)
E(g2lvek)
E(g2m11)
E(g2m12)
E(g2m20)
E(g2m21)
E(g2m30)
E(g2mE)
E(g2mOut)
E(g2mdvek)
E(g2mgcret)
E(g2mgsret)
E(g2mmvd)
E(g2mvap)
E(g2mvek)
E(g2n11)
E(g2n12)
E(g2n20)
E(g2n21)
E(g2n30)
E(g2nchr)
E(g2ndvek)
E(g2nind)
E(g2ninp)
E(g2nint)
E(g2ntag0)
E(g2nvap)
E(g2nvek)
E(g2nzap)
E(g2o11)
E(g2o12)
E(g2o20)
E(g2o21)
E(g2o30)
E(g2ochr)
E(g2odvek)
E(g2oind)
E(g2oinp)
E(g2oint)
E(g2otag0)
E(g2ovap)
E(g2ovek)
E(g2ozap)
E(g2p11)
E(g2p12)
E(g2p20)
E(g2p21)
E(g2p30)
E(g2pE)
E(g2pN)
E(g2pap)
E(g2pdvek)
E(g2pgcret)
E(g2phole)
E(g2pvap)
E(g2pvapG)
E(g2pvek)
E(g2s11)
E(g2s12)
E(g2s20)
E(g2s21)
E(g2s30)
E(g2sE)
E(g2sdvek)
E(g2sfun)
E(g2shole)
E(g2sind)
E(g2sindir)
E(g2sinp)
E(g2smkd)
E(g2svap)
E(g2svek)
E(g2szap)
E(gc11)
E(gc12)
E(gc20)
E(gc21)
E(gc30)
E(gcap)
E(gccap)
E(gcchr)
E(gcdvek)
E(gcfun)
E(gcind)
E(gcindi)
E(gcinp)
E(gcint)
E(gcmkd)
E(gcmvd)
E(gcret)
E(gctag0)
E(gcvap)
E(gcvek)
E(gczap)
E(genc11)
E(genc12)
E(genc20)
E(genc21)
E(genc30)
E(gencap)
E(gencapG)
E(genccap)
E(gencchr)
E(gencdvek)
E(gencfun)
E(genchole)
E(gencind)
E(gencindf)
E(gencindi)
E(gencinp)
E(gencinpf)
E(gencint)
E(gencmkd)
E(gencmvd)
E(gencret)
E(gencstr)
E(gencstrn)
E(genctag0)
E(gencvap)
E(gencvapG)
E(gencvek)
E(genczap)
E(gens11)
E(gens12)
E(gens20)
E(gens21)
E(gens30)
E(gensap)
E(gensapG)
E(genscap)
E(gensdvek)
E(gensfun)
E(genshole)
E(gensind)
E(gensindf)
E(gensindi)
E(gensinp)
E(gensinpf)
E(gensmkd)
E(gensret)
E(gensstr)
E(gensstrn)
E(gensvap)
E(gensvapG)
E(gensvek)
E(genszap)
E(gett)
E(gtte)
E(icmp)
E(ieval)
E(ijfun)
E(indev)
E(indevF)
E(inpev)
E(inpevF)
E(iprt)
E(iunw)
E(jfAP)
E(jfVAP)
E(majorc11)
E(majorc12)
E(majorc20)
E(majorc21)
E(majorc30)
E(majorcap)
E(majorcapG)
E(majorccap)
E(majorcchr)
E(majorcdvek)
E(majorcfun)
E(majorchole)
E(majorcind)
E(majorcindi)
E(majorcinp)
E(majorcint)
E(majorcmkd)
E(majorcmvd)
E(majorcret)
E(majorctag0)
E(majorcvap)
E(majorcvapG)
E(majorcvek)
E(majorczap)
E(pcmp)
E(pget0)
E(pget1)
E(pget2)
E(pget3)
E(pget4)
E(pprt)
E(scmp)
E(sgc11)
E(sgc12)
E(sgc20)
E(sgc21)
E(sgc30)
E(sgcdvek)
E(sgcfun)
E(sgcind)
E(sgcindir)
E(sgcinp)
E(sgcmkd)
E(sgcmvd)
E(sgcret)
E(sgcvap)
E(sgcvek)
E(sgczap)
E(strev)
E(strevF)
E(strnev)
E(strnevF)
E(t0cmp)
E(t0prt)
E(tcmp)
E(tprt)
E(vapev)
E(vapevG)
E(vapunw)
E(vapunwG)
E(vcmp)
E(vprt)

A(SFLOAT)
A(SFLOAT_1)
A(SFLOAT_2)
A(STRING)
A(STRING_1)
A(STRING_2)
A(STRING_F)
A(STRINGN)
A(STRINGN_1)
A(STRINGN_2)
A(STRINGN_F)
A(hp)
A(ep)
A(vp)

#undef E
#undef A
#endif

#ifdef NOUNDERSCORE
#define L(l) .export l && l: .word 
#else
#define L(l) .export l && .export _/**/l && l: && _/**/l: .word 
#endif
	TAGTABLE
; The number of words per tag and the tag order is defined in the file tagtable.h, change it if necessary.
; All tag table entries must have the same size!
;  	EVAL,  UNWIND, JFUN, GTAG, GC     CMP,  PRINT,FORCE, GCT,     GENC,     GENS,     MAJORC,     SAMPLE,	BMOVE,	SCAN,	SCANO,	JMOVE, OGEN2S, OGEN2C, OGEN2T, OGEN2P,	CONSTR,	OGEN2L,	OGEN2M,	extra4
;	  0      1       2     3     4      5     6     7      8        9,        10,       11,         12,	13,	14,	15,	16,    17,     18,     19,     20
;	  0      4       8     12    16     20    24,   28,    32,      36,       40,       44,         48,	52,	54,	60,	64,    68,     72,     76,     80	 21	22	23	24
L(TAGFIRST)

L(AP_2)   apev,   apunw, jfAP, gtte, gcap,  Ecmp, Eprt, fev,   tgcap,	gencap,   gensap,    majorcap,	sgc12,	APG,	0,	0,	JMap,  g2sE,   g2o12,      APG,g2pE,	O_AP,	g2l12,	g2m12,	g2d12
L(VAP_2)  vapev,  vapunw,jfVAP,gtte, gcvap, Ecmp, Eprt, fev,   tgcvap,	gencvap,  gensvap,   majorcvap,	sgcvap,	VAPG,	0,	0,	JMvap, g2sE,   g2ovap,    VAPG,g2pE,	O_VAP,	g2lvap,	g2mvap,	g2dvap
L(ZAP_2)  Ezap,   Erru,  Ejzap,gtte, gczap, Ecmp, Eprt, ferr,  tgczap,	genczap,  genszap,   majorczap,	sgczap,	ZAP,	0,	0,	JM3u,  g2sE,   g2ozap,     ZAP,g2pE,	O_ZAP,	g2l30,	g2m30,	g2dzap
L(HOLE_2) Ehole,  Erru,  Ejhol,gtte, gc30,  Ecmp, Eprt, ferr,  tgchole,genchole,  genshole,  majorchole,sgc30,	HOLE,	0,	0,	JM3u,  g2sE,   g2o30,     HOLE,g2pE,	O_HOLE,	g2l30,	g2m30,	g2d30
L(STRING_2) strevF,Erru,  Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr,	gencstr,   gensstr,  majorc30,	sgc30,	STRING,	0,	0,	JM3u,  g2sE,   g2o30,   STRING,g2pE,	O_STRING,  g2l30,g2m30,	g2d30
L(STRINGN_2) strnevF,Erru,Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr, gencstrn,  gensstrn, majorc30,	sgc30,	STRINGN,0,	0,	JM3u,  g2sE,   g2o30,  STRINGN,g2pE,	O_STRINGN, g2l30,g2m30,	g2d30
L(INPUTD_2) indevF,Erru,  Ejind,gtte, gcind, Ecmp, Eprt, fev,   tgcind,	gencind,   gensind,  majorcind,	sgcind,	INPUTD,	0,	0,	JM3u,  g2sE,   g2oind,  INPUTD,g2pE,	O_INPUTD,  g2l30,g2m30,	g2dind
L(INPUT_2) inpevF,Erru,   Ejinp,gtte, gcinp, Ecmp, Eprt, fev,   tgcinp,	gencinp,   gensinp,  majorcinp,	sgcinp,	INPUT,	0,	0,	JM3u,  g2sE,   g2oinp,   INPUT,g2pE,	O_INPUT,   g2l30,g2m30,	g2dinp

L(AP_1)   apev,   apunw, jfAP, gtte, gcap,  Ecmp, Eprt, fev,   tgcap,	gencap,   gensap,    majorcap,	sgc12,	APG,	0,	0,	JMap,  g2sE,   g2n12,      AP_2,g2pE,	O_AP,	g2l12,	g2m12,	g2d12
L(VAP_1)  vapev,  vapunw,jfVAP,gtte, gcvap, Ecmp, Eprt, fev,   tgcvap,	gencvap,  gensvap,   majorcvap,	sgcvap,	VAPG,	0,	0,	JMvap, g2sE,   g2nvap,    VAP_2,g2pE,	O_VAP,	g2lvap,	g2mvap,	g2dvap
L(ZAP_1)  Ezap,   Erru,  Ejzap,gtte, gczap, Ecmp, Eprt, ferr,  tgczap,	genczap,  genszap,   majorczap,	sgczap,	ZAP,	0,	0,	JM3u,  g2sE,   g2nzap,     ZAP_2,g2pE,	O_ZAP,	g2l30,	g2m30,	g2dzap
L(HOLE_1) Ehole,  Erru,  Ejhol,gtte, gc30,  Ecmp, Eprt, ferr,  tgchole,genchole,  genshole,  majorchole,sgc30,	HOLE,	0,	0,	JM3u,  g2sE,   g2n30,     HOLE_2,g2pE,	O_HOLE,	g2l30,	g2m30,	g2d30

L(STRING_1) strevF,Erru,  Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr,	gencstr,   gensstr,  majorc30,	sgc30,	STRING,	0,	0,	JM3u,  g2sE,   g2o30,   STRING,g2pE,	O_STRING,  g2l30,g2m30,	g2d30
L(STRINGN_1) strnevF,Erru,Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr, gencstrn,  gensstrn, majorc30,	sgc30,	STRINGN,0,	0,	JM3u,  g2sE,   g2o30,  STRINGN,g2pE,	O_STRINGN, g2l30,g2m30,	g2d30
L(INPUTD_1) indevF,Erru,  Ejind,gtte, gcind, Ecmp, Eprt, fev,   tgcind,	gencind,   gensind,  majorcind,	sgcind,	INPUTD,	0,	0,	JM3u,  g2sE,   g2oind,  INPUTD,g2pE,	O_INPUTD,  g2l30,g2m30,	g2dind
L(INPUT_1) inpevF,Erru,   Ejinp,gtte, gcinp, Ecmp, Eprt, fev,   tgcinp,	gencinp,   gensinp,  majorcinp,	sgcinp,	INPUT,	0,	0,	JM3u,  g2sE,   g2oinp,   INPUT,g2pE,	O_INPUT,   g2l30,g2m30,	g2dinp

;  Nodes before this line are only generated by the garbage collector

L(STRING_F) strevF,Erru,  Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr,	gencstr,   gensstr,  majorc30,	sgc30,	STRING,	0,	0,	JMstrF, g2sE,  g2n30, STRING_1,g2pE,    O_STRING,  g2l30,g2m30,	g2d30
L(STRINGN_F) strnevF,Erru,Ejstr,gtte, gc30,  Ecmp, Eprt, fev,   tgcstr, gencstrn,  gensstrn, majorc30,	sgc30,	STRINGN,0,	0,	JMstrnF,g2sE,  g2n30,STRINGN_1,g2pE,    O_STRINGN, g2l30,g2m30,	g2d30
L(INPUTD_F) indevF,Erru,  Ejind,gtte, gcind, Ecmp, Eprt, fev,   tgcind,	gencindf,  gensindf, majorcind,	sgcind,	INPUTD,	0,	0,	JMindF, g2sE,  g2nind,INPUTD_1,g2pE,    O_INPUTD,  g2l30,g2m30,	g2dind
L(INPUT_F) inpevF,Erru,   Ejinp,gtte, gcinp, Ecmp, Eprt, fev,   tgcinp,	gencinpf,  gensinpf, majorcinp,	sgcinp,	INPUT,	0,	0,	JMinpF, g2sE,  g2ninp, INPUT_1,g2pE,    O_INPUT,   g2l30,g2m30,	g2dinp

;  Nodes before this line are generated by the garbage collector or the runtime


L(AP)     apev,  apunw,  jfAP, gtte, gcap,  Ecmp, Eprt, fev,   tgcap,	gencap,   gensap,    majorcap,	sgc12,	APG,	0,	0,	JMap,  g2s12,  g2n12,     AP_1,g2pap,	O_AP,	g2l12,	g2m12,	g2dap
L(APG)    apevG, apunwG, jfAP, gtte, gcap,  Ecmp, Eprt, fev,   tgcapG,	gencapG,  gensapG,   majorcapG,	sgc12,	APG,	0,	0,	JM3,   g2s12,  g2n12,     AP_1,g2p12,	O_AP,	g2l12,	g2m12,	g2dapG
L(VAP)	  vapev, vapunw, jfVAP,gtte, gcvap, Ecmp, Eprt, fev,   tgcvap,	gencvap,  gensvap,   majorcvap,	sgcvap,	VAPG,	0,	0,	JMvap, g2svap, g2nvap,   VAP_1,g2pvap,	O_VAP,	g2lvap,	g2mvap,	g2dvap
L(VAPG)	  vapevG,vapunwG,jfVAP,gtte, gcvap, Ecmp, Eprt, fev,   tgcvapG,	gencvapG, gensvapG,  majorcvapG,sgcvap,	VAPG,	0,	0,	JMvapG,g2svap, g2nvap,   VAP_1,g2pvapG,	O_VAP,	g2lvap,	g2mvap,	g2dvapG
L(ZAP)	  Ezap,  Erru,   Ejzap, gtte, gczap, Ecmp, Eprt, ferr,  tgczap,	genczap,  genszap,   majorczap,	sgczap,	ZAP,	0,	0,	JM3u,  g2szap, g2nzap,   ZAP_1,g2p30,	O_ZAP,	g2l30,	g2m30,	g2dzap
L(HOLE)	  Ehole, Erru,   Ejhol, gtte, gc30,  Ecmp, Eprt, ferr,  tgchole,genchole, genshole,  majorchole,sgc30,	HOLE,	0,	0,	JM3u,  g2shole,g2n30,   HOLE_1,g2phole,	O_HOLE,	g2l30,	g2m30,	g2dhole
L(STRING) strev, Erru,   Ejstr, gtte, gc30,  Ecmp, Eprt, fev,   tgcstr,	genc30,   gens30,    majorc30,	sgc30,	STRING,	0,	0,	JM3u,  g2s30,  g2n30, STRING_1,g2p30,	O_STRING,g2l30,	g2m30,	g2d30
L(STRINGN) strnev,Erru,  Ejstr, gtte, gc30,  Ecmp, Eprt, fev,   tgcstr, genc30,   gens30,    majorc30,	sgc30,	STRINGN,0,	0,	JM3u,  g2s30,  g2n30,STRINGN_1,g2p30,	O_STRINGN,g2l30,g2m30,	g2d30
L(INPUT)  inpev, Erru,   Ejinp, gtte, gcinp, Ecmp, Eprt, fev,   tgcinp,	gencinp,  gensinp,   majorcinp,	sgcinp,	INPUT,	0,	0,	JM3u,  g2sinp, g2ninp, INPUT_1,g2p30,	O_INPUT,g2l30,	g2m30,	g2dinp
L(INPUTD) indev, Erru,   Ejind, gtte, gcind, Ecmp, Eprt, fev,   tgcind,	gencind,  gensind,   majorcind,	sgcind,	INPUTD,	0,	0,	JM3u,  g2sind, g2nind,INPUTD_1,g2p30,	O_INPUTD,g2l30,	g2m30,	g2dind

L(INDIR)  ieval, iunw,   ijfun, gtte, gcindi,Ecmp, Eprt, fevi,  tgcindi,gencindi, gensindi,  majorcindi,sgcindir,INDIR,	0,	0,	JMini, g2sindir,g2cindir,    0,g2p11,	O_INDIR, g2lindir,g2mOut,g2dindir

L(CANON)
L(CAP)    Rsb,   apunw,  jfAP, Errg, gccap, Ecmp, Eprt, fret,  tgccap,	genccap,  genscap,   majorccap,	sgc12,	CAP,	0,	0,	JM3,   g2s12,  g2n12,   CAP_1,g2p12,	O_CAP,	g2l12,	g2m12,	g2d12
/* The next two tags are only generated by gc, tagtable si updated now ! */
L(CAP_1)  Rsb,   apunw,  jfAP, Errg, gccap, Ecmp, Eprt, fret,  tgccap,	genccap,  genscap,   majorccap,	sgc12,	CAP,	0,	0,	JM3,   g2s12,   g2n12,  CAP_2,g2p12,	O_CAP,	g2l12,	g2m12,	g2d12
L(CAP_2)  Rsb,   apunw,  jfAP, Errg, gccap, Ecmp, Eprt, fret,  tgccap,	genccap,  genscap,   majorccap,	sgc12,	CAP,	0,	0,	JM3,   g2s12,   g2o12,    CAP,g2p12,	O_CAP,	g2l12,	g2m12,	g2d12
/* The previous two tags are only generated by gc */
L(FUN)	  Rsb,   fununw, funj, Errg, gcfun, Ecmp, Eprt, fret,  tgcfun,	gencfun,  gensfun,   majorcfun,	sgcfun,	FUN,	0,	0,	JMfun, g2sfun, g2cfun,      0,g2pE,	O_FUN,	g2lfun,	g2mOut,	g2dfun
L(CANONDATA)

L(INT)	  Rsb,   Poprsb, Ejint, gett, gcint, icmp, iprt, fret,  tgcint,	gencint,  gens20,   majorcint,	sgc20,	INT,	0,	0,	JM2,   g2s20,  g2nint,  INT_1,g2p20,	O_INT,	g2l20,	g2m20,	g2dint
L(SFLOAT) Rsb,   Poprsb, Ejflt, Errg, gc20,  scmp, Eprt, fret,  tgc20,	genc20,   gens20,   majorc20,	sgc20,	SFLOAT,	0,	0,	JM2,   g2s20,  g2n20,SFLOAT_1,g2p20,	O_SFLOAT,g2l20,	g2m20,	g2d20
L(DFLOAT) Rsb,   Poprsb, Ejflt, Errg, gc30,  dcmp, dprt, fret,  tgc30,	genc30,   gens30,   majorc30,	sgc30,	DFLOAT,	0,	0,	JM3,   g2s30,  g2n30,DFLOAT_1,g2p30,	O_DFLOAT,g2l30,	g2m30,	g2d30
L(BIGNUM) Rsb,   Poprsb, Ejbig, Errg, gc11,  bcmpb,bprt, fret,  tgcbig,	genc11,   gens11,   majorc11,	sgc11,	BIGNUM,	0,	0,	JM2,   g2s11,  g2n11,BIGNUM_1,g2p11,	O_BIGNUM,g2l11,	g2m11,	g2d11
L(CHAR)	  Rsb,   Poprsb, Ejchr, gett, gcchr, icmp, cprt, fret,  tgcchr,	gencchr,  gens20,   majorcchr,	sgc20,	CHAR,	0,	0,	JM2,   g2s20,  g2nchr, CHAR_1,g2p20,	O_CHAR,	g2l20,	g2m20,	g2dchr
L(PAIR)   Rsb,   Poprsb, Ejpai, Errg, gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR,	0,	0,	JM3,   g2s12,  g2n12,  PAIR_1,g2p12,    O_PAIR,	g2l12,	g2m12,	g2d12
L(PAIR0)  Rsb,   Poprsb, Ejpa0, pget0,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR0,	0,	0,	JM3,   g2s12,  g2n12, PAIR0_1,g2p12,	O_PAIR0,g2l12,	g2m12,	g2d12
L(PAIR1)  Rsb,   Poprsb, Ejpa1, pget1,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR1,	0,	0,	JM3,   g2s12,  g2n12, PAIR1_1,g2p12,	O_PAIR1,g2l12,	g2m12,	g2d12
L(PAIR2)  Rsb,   Poprsb, Ejpa2, pget2,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR2,	0,	0,	JM3,   g2s12,  g2n12, PAIR2_1,g2p12,	O_PAIR2,g2l12,	g2m12,	g2d12
L(PAIR3)  Rsb,   Poprsb, Ejpa3, pget3,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR3,	0,	0,	JM3,   g2s12,  g2n12, PAIR3_1,g2p12,	O_PAIR3,g2l12,	g2m12,	g2d12
L(PAIR4)  Rsb,   Poprsb, Ejpa4, pget4,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR4,	0,	0,	JM3,   g2s12,  g2n12, PAIR4_1,g2p12,	O_PAIR4,g2l12,	g2m12,	g2d12
L(TAG)	  Rsb,   Poprsb, Ejtag, gett, gc21,  tcmp, tprt, f1p,   tgc21,	genc21,   gens21,   majorc21,	sgc21,	TAG,	0,	0,	JM3,   g2s21,  g2n21,   TAG_1,g2p21,    O_TAG,	g2l21,	g2m21,	g2d21
L(TAG0)	  Rsb,   Poprsb, Ejta0, gett, gctag0,t0cmp,t0prt,fret,  tgctag0,genctag0, gens20,   majorctag0,	sgc20,	TAG0,	0,	0,	JM2,   g2s20,  g2ntag0,TAG0_1,g2p20,    O_TAG0,	g2l20,	g2m20,	g2dtag0
L(VEK)	  Rsb,   Erru,   Ejvek, Errg, gcvek, vcmp, vprt, fvek,  tgcvek,	gencvek,  gensvek,  majorcvek,	sgcvek,	VEK,	0,	0,	JMvek, g2svek, g2nvek,  VEK_1,g2pvek,	O_VEK,	g2lvek,	g2mvek,	g2dvek
L(DVEK)   Rsb,   Erru,   Ejdvk, Errg, gcdvek,Ecmp, Eprt, fret,  tgcdvek,gencdvek, gensdvek, majorcdvek,	sgcdvek,DVEK,	0,	0,	JMvek, g2sdvek,g2ndvek,DVEK_1,g2pdvek,	O_DVEK,	g2ldvek, g2mdvek,g2ddvek

;  Nodes after this line are only generated by the garbage collector

L(MARKED) Emkd,   Erru,  Ejmkd,Errg, gcmkd, Ecmp, Eprt, ferr,  tgcmkd,	gencmkd,  gensmkd,   majorcmkd,	sgcmkd,	0,	0,	0,	JMmrk, g2smkd, g2cmkd,      0,g2pN,	O_MARKED,g2lE,	g2mE,	g2dmkd
L(MOVED)  Emvd,   Erru,  Ejmvd,Errg, gcmvd, Ecmp, Eprt, ferr,  tgcmvd,	gencmvd,Errgs_moved, majorcmvd,	sgcmvd,	0,	0,	0,	JMmvd, g2sE,   g2cmvd,      0,g2pN,	O_MOVED, g2lmvd,g2mmvd,	g2dmvd
L(GCRET)  Egcret, Erru,  Ejgcr,Errg, gcret, Ecmp, Eprt, ferr,  tgcret,	gencret,  gensret,   majorcret,	sgcret,	0,	0,	0,	JMret, g2sE,   g2cgcret,    0,g2pgcret,O_GCRET,	 g2lE,	g2mgcret,g2dgcret
L(GSRET)  Egcret, Erru,  Ejgcr,Errg, gcret, Ecmp, Eprt, ferr,  tgcret,	gencret,  gensret,   majorcret,	sgcret,	0,	0,	0,	JMret, g2sE,   g2cgsret,    0,g2pN,	O_GSRET, g2lE,	g2mgsret,g2dgsret

L(INT_1)  Rsb,   Poprsb, Ejint, gett, gcint, icmp, iprt, fret,  tgcint,	gencint,  gens20,   majorcint,	sgc20,	INT,	0,	0,	JM2,   g2s20,   g2oint, INT,  g2p20,	O_INT,	g2l20,	g2m20,	g2dint
L(SFLOAT_1) Rsb, Poprsb, Ejflt, Errg, gc20,  scmp, Eprt, fret,  tgc20,	genc20,   gens20,   majorc20,	sgc20,	SFLOAT,	0,	0,	JM2,   g2s20,   g2o20,  SFLOAT,g2p20,	O_SFLOAT,g2l20,	g2m20,	g2d20
L(DFLOAT_1) Rsb, Poprsb, Ejflt, Errg, gc30,  dcmp, dprt, fret,  tgc30,	genc30,   gens30,   majorc30,	sgc30,	DFLOAT,	0,	0,	JM3,   g2s30,   g2o30,  DFLOAT,g2p30,	O_DFLOAT,g2l30,	g2m30,	g2d30
L(BIGNUM_1) Rsb, Poprsb, Ejbig, Errg, gc11,  bcmpb,bprt, fret,  tgcbig,	genc11,   gens11,   majorc11,	sgc11,	BIGNUM,	0,	0,	JM2,   g2s11,   g2n11,  BIGNUM_2,g2p11,	O_BIGNUM,g2l11,	g2m11,	g2d11
L(CHAR_1) Rsb,   Poprsb, Ejchr, gett, gcchr, icmp, cprt, fret,  tgcchr,	gencchr,  gens20,   majorcchr,	sgc20,	CHAR,	0,	0,	JM2,   g2s20,   g2ochr,    CHAR,g2p20,	O_CHAR,	g2l20,	g2m20,	g2dchr
L(PAIR_1) Rsb,   Poprsb, Ejpai, Errg, gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR,	0,	0,	JM3,   g2s12,   g2n12,    PAIR_2,g2p12,    O_PAIR,g2l12,g2m12,	g2d12
L(PAIR0_1) Rsb,  Poprsb, Ejpa0, pget0,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR0,	0,	0,	JM3,   g2s12,   g2n12,   PAIR0_2,g2p12,	O_PAIR0,g2l12,	g2m12,	g2d12
L(PAIR1_1) Rsb,  Poprsb, Ejpa1, pget1,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR1,	0,	0,	JM3,   g2s12,   g2n12,   PAIR1_2,g2p12,	O_PAIR1,g2l12,	g2m12,	g2d12
L(PAIR2_1) Rsb,  Poprsb, Ejpa2, pget2,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR2,	0,	0,	JM3,   g2s12,   g2n12,   PAIR2_2,g2p12,	O_PAIR2,g2l12,	g2m12,	g2d12
L(PAIR3_1) Rsb,  Poprsb, Ejpa3, pget3,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR3,	0,	0,	JM3,   g2s12,   g2n12,   PAIR3_2,g2p12,	O_PAIR3,g2l12,	g2m12,	g2d12
L(PAIR4_1) Rsb,  Poprsb, Ejpa4, pget4,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR4,	0,	0,	JM3,   g2s12,   g2n12,   PAIR4_2,g2p12,	O_PAIR4,g2l12,	g2m12,	g2d12
L(TAG_1)  Rsb,   Poprsb, Ejtag, gett, gc21,  tcmp, tprt, f1p,   tgc21,	genc21,   gens21,   majorc21,	sgc21,	TAG,	0,	0,	JM3,   g2s21,   g2n21,     TAG_2,g2p21, O_TAG,g2l21,	g2m21,	g2d21
L(TAG0_1) Rsb,   Poprsb, Ejta0, gett, gctag0,t0cmp,t0prt,fret,  tgctag0,genctag0, gens20,   majorctag0,	sgc20,	TAG0,	0,	0,	JM2,   g2s20,   g2otag0,    TAG0,g2p20,   O_TAG0,g2l20,	g2m20,	g2dtag0
L(VEK_1)  Rsb,   Erru,   Ejvek, Errg, gcvek, vcmp, vprt, fvek,  tgcvek,	gencvek,  gensvek,  majorcvek,	sgcvek,	VEK,	0,	0,	JMvek, g2svap,   g2nvek,  VEK_2,g2pvek,	O_VEK,	g2lvek,	g2mvek,	g2dvek
L(DVEK_1)  Rsb,  Erru,   Ejdvk, Errg, gcdvek,Ecmp, Eprt, fret,  tgcdvek,gencdvek, gensdvek, majorcdvek,	sgcdvek,DVEK,	0,	0,	JMvek,g2sdvek,   g2odvek,  DVEK,g2pdvek, O_DVEK,g2ldvek,g2mdvek,g2ddvek

L(INT_2)  Rsb,   Poprsb, Ejint, gett, gcint, icmp, iprt, fret,  tgcint,	gencint,  gens20,   majorcint,	sgc20,	INT,	0,	0,	JM2,   g2s20,   g2oint,     INT,g2p20,	O_INT,	g2l20,	g2m20,	g2dint
L(SFLOAT_2) Rsb, Poprsb, Ejflt, Errg, gc20,  scmp, Eprt, fret,  tgc20,	genc20,   gens20,   majorc20,	sgc20,	SFLOAT,	0,	0,	JM2,   g2s20,   g2o20,  SFLOAT,g2p20,	O_SFLOAT,g2l20,	g2m20,	g2d20
L(DFLOAT_2) Rsb, Poprsb, Ejflt, Errg, gc30,  dcmp, dprt, fret,  tgc30,	genc30,   gens30,   majorc30,	sgc30,	DFLOAT,	0,	0,	JM3,   g2s30,   g2o30,  DFLOAT,g2p30,	O_DFLOAT,g2l30,	g2m30,	g2d30
L(BIGNUM_2) Rsb, Poprsb, Ejbig, Errg, gc11,  bcmpb,bprt, fret,  tgcbig,	genc11,   gens11,   majorc11,	sgc11,	BIGNUM,	0,	0,	JM2,   g2s11,   g2o11,  BIGNUM,g2p11,	O_BIGNUM,g2l11,	g2m11,	g2d11
L(CHAR_2) Rsb,   Poprsb, Ejchr, gett, gcchr, icmp, cprt, fret,  tgcchr,	gencchr,  gens20,   majorcchr,	sgc20,	CHAR,	0,	0,	JM2,   g2s20,   g2ochr,    CHAR,g2p20,	O_CHAR,	g2l20,	g2m20,	g2dchr
L(PAIR_2) Rsb,   Poprsb, Ejpai, Errg, gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR,	0,	0,	JM3,   g2s12,   g2o12,    PAIR,g2p12,    O_PAIR,g2l12,	g2m12,	g2d12
L(PAIR0_2) Rsb,  Poprsb, Ejpa0, pget0,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR0,	0,	0,	JM3,   g2s12,   g2o12,   PAIR0,g2p12,	O_PAIR0,g2l12,	g2m12,	g2d12
L(PAIR1_2) Rsb,  Poprsb, Ejpa1, pget1,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR1,	0,	0,	JM3,   g2s12,   g2o12,   PAIR1,g2p12,	O_PAIR1,g2l12,	g2m12,	g2d12
L(PAIR2_2) Rsb,  Poprsb, Ejpa2, pget2,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR2,	0,	0,	JM3,   g2s12,   g2o12,   PAIR2,g2p12,	O_PAIR2,g2l12,	g2m12,	g2d12
L(PAIR3_2) Rsb,  Poprsb, Ejpa3, pget3,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR3,	0,	0,	JM3,   g2s12,   g2o12,   PAIR3,g2p12,	O_PAIR3,g2l12,	g2m12,	g2d12
L(PAIR4_2) Rsb,  Poprsb, Ejpa4, pget4,gc12,  pcmp, pprt, f2p,   tgc12,	genc12,   gens12,   majorc12,	sgc12,	PAIR4,	0,	0,	JM3,   g2s12,   g2o12,   PAIR4,g2p12,	O_PAIR4,g2l12,	g2m12,	g2d12
L(TAG_2)  Rsb,   Poprsb, Ejtag, gett, gc21,  tcmp, tprt, f1p,   tgc21,	genc21,   gens21,   majorc21,	sgc21,	TAG,	0,	0,	JM3,   g2s21,   g2o21,     TAG,g2p21,   O_TAG,	g2l21,	g2m21,	g2d21
L(TAG0_2) Rsb,   Poprsb, Ejta0, gett, gctag0,t0cmp,t0prt,fret,  tgctag0,genctag0, gens20,   majorctag0,	sgc20,	TAG0,	0,	0,	JM2,   g2s20,   g2otag0,    TAG0,g2p20,   O_TAG0,g2l20,	g2m20,	g2dtag0
L(VEK_2)  Rsb,   Erru,   Ejvek, Errg, gcvek, vcmp, vprt, fvek,  tgcvek,	gencvek,  gensvek,  majorcvek,	sgcvek,	VEK,	0,	0,	JMvek, g2svap,  g2ovek,    VEK,g2pvek,	O_VEK,	g2lvek,	g2mvek,	g2dvek
L(DVEK_2)  Rsb,  Erru,   Ejdvk, Errg, gcdvek,Ecmp, Eprt, fret,  tgcdvek,gencdvek, gensdvek, majorcdvek,	sgcdvek,DVEK,	0,	0,	JMvek,g2sdvek,   g2odvek,  DVEK,g2pdvek, O_DVEK,g2ldvek,g2mdvek,g2ddvek

L(TAGLAST) 0

#ifdef TAGAREA
tagarea:
	.word	INT, DFLOAT, BIGNUM, CHAR, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4, TAG, TAG0, VEK, BVEK, DVEK, ZAP, HOLE, STRING, INPUT, INPUTD, AP, APG, CAP, FUN, VAP, VAPG, MARKED, MOVED, GCRET
#endif

