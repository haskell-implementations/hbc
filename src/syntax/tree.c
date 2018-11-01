

#include "include.h"
#include "tree.h"
struct Smodule {
	Ttree tag;
	list Xgimplist;
	list Xgexplist;
	list Xgmodlist;
};

struct Sident {
	Ttree tag;
	id Xgident;
};

struct Sinteger {
	Ttree tag;
	int Xginteger;
};

struct Scharr {
	Ttree tag;
	id Xgchar;
};

struct Sstring {
	Ttree tag;
	id Xgstring;
};

struct Sfloatt {
	Ttree tag;
	double Xgfloat;
};

struct Sbignum {
	Ttree tag;
	id Xgbignum;
};

struct Sratnum {
	Ttree tag;
	id Xgratnum;
};

struct Stuple {
	Ttree tag;
	list Xgtuplelist;
};

struct Sap {
	Ttree tag;
	tree Xgfun;
	tree Xgarg;
};

struct Slam {
	Ttree tag;
	tree Xglamid;
	tree Xglamexpr;
};

struct Scexpr {
	Ttree tag;
	list Xgsymlist;
};

struct Sletv {
	Ttree tag;
	list Xgletvdeflist;
	tree Xgletvexpr;
};

struct Scasee {
	Ttree tag;
	tree Xgcaseexpr;
	list Xgcasebody;
};

struct Spar {
	Ttree tag;
	tree Xgpare;
};

struct Sas {
	Ttree tag;
	id Xgasid;
	tree Xgase;
};

struct Scondp {
	Ttree tag;
	tree Xgcondpp;
	tree Xgcondpe;
};

struct Slazyp {
	Ttree tag;
	tree Xglazyp;
};

struct Srestr {
	Ttree tag;
	tree Xgrestre;
	ttype Xgrestrt;
};

struct Seannot {
	Ttree tag;
	tree Xgeannote;
	id Xgeannota;
};

struct Slistf {
	Ttree tag;
	int Xglistt;
	list Xglistf;
};

struct Slistg {
	Ttree tag;
	tree Xglgg;
	list Xglgq;
};

struct Shmodule {
	Ttree tag;
	id Xghmodid;
	list Xghexp;
	list Xghimp;
	list Xghfix;
	list Xghbind;
};

struct Swherev {
	Ttree tag;
	list Xgwges;
	list Xgwdefs;
};

struct Sdoexp {
	Ttree tag;
	list Xgstmt;
};

struct Srecord {
	Ttree tag;
	tree Xgrecid;
	list Xgrecfields;
};

Ttree ttree(t)
 tree t;
{
	return(t -> tag);
}


/************** module ******************/

tree mkmodule(PPgimplist, PPgexplist, PPgmodlist)
 list PPgimplist;
 list PPgexplist;
 list PPgmodlist;
{
	register struct Smodule *pp =
		(struct Smodule *) malloc(sizeof(struct Smodule));
	pp -> tag = module;
	pp -> Xgimplist = PPgimplist;
	pp -> Xgexplist = PPgexplist;
	pp -> Xgmodlist = PPgmodlist;
	return((tree)pp);
}

list *Rgimplist(t)
 struct Smodule *t;
{
	if(t -> tag != module)
		printf("gimplist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimplist);
}

list *Rgexplist(t)
 struct Smodule *t;
{
	if(t -> tag != module)
		printf("gexplist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexplist);
}

list *Rgmodlist(t)
 struct Smodule *t;
{
	if(t -> tag != module)
		printf("gmodlist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgmodlist);
}

/************** ident ******************/

tree mkident(PPgident)
 id PPgident;
{
	register struct Sident *pp =
		(struct Sident *) malloc(sizeof(struct Sident));
	pp -> tag = ident;
	pp -> Xgident = PPgident;
	return((tree)pp);
}

id *Rgident(t)
 struct Sident *t;
{
	if(t -> tag != ident)
		printf("gident: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgident);
}

/************** integer ******************/

tree mkinteger(PPginteger)
 int PPginteger;
{
	register struct Sinteger *pp =
		(struct Sinteger *) malloc(sizeof(struct Sinteger));
	pp -> tag = integer;
	pp -> Xginteger = PPginteger;
	return((tree)pp);
}

int *Rginteger(t)
 struct Sinteger *t;
{
	if(t -> tag != integer)
		printf("ginteger: illegal selection; was %d\n", t -> tag);
	return(& t -> Xginteger);
}

/************** charr ******************/

tree mkcharr(PPgchar)
 id PPgchar;
{
	register struct Scharr *pp =
		(struct Scharr *) malloc(sizeof(struct Scharr));
	pp -> tag = charr;
	pp -> Xgchar = PPgchar;
	return((tree)pp);
}

id *Rgchar(t)
 struct Scharr *t;
{
	if(t -> tag != charr)
		printf("gchar: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgchar);
}

/************** string ******************/

tree mkstring(PPgstring)
 id PPgstring;
{
	register struct Sstring *pp =
		(struct Sstring *) malloc(sizeof(struct Sstring));
	pp -> tag = string;
	pp -> Xgstring = PPgstring;
	return((tree)pp);
}

id *Rgstring(t)
 struct Sstring *t;
{
	if(t -> tag != string)
		printf("gstring: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgstring);
}

/************** floatt ******************/

tree mkfloatt(PPgfloat)
 double PPgfloat;
{
	register struct Sfloatt *pp =
		(struct Sfloatt *) malloc(sizeof(struct Sfloatt));
	pp -> tag = floatt;
	pp -> Xgfloat = PPgfloat;
	return((tree)pp);
}

double *Rgfloat(t)
 struct Sfloatt *t;
{
	if(t -> tag != floatt)
		printf("gfloat: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgfloat);
}

/************** bignum ******************/

tree mkbignum(PPgbignum)
 id PPgbignum;
{
	register struct Sbignum *pp =
		(struct Sbignum *) malloc(sizeof(struct Sbignum));
	pp -> tag = bignum;
	pp -> Xgbignum = PPgbignum;
	return((tree)pp);
}

id *Rgbignum(t)
 struct Sbignum *t;
{
	if(t -> tag != bignum)
		printf("gbignum: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgbignum);
}

/************** ratnum ******************/

tree mkratnum(PPgratnum)
 id PPgratnum;
{
	register struct Sratnum *pp =
		(struct Sratnum *) malloc(sizeof(struct Sratnum));
	pp -> tag = ratnum;
	pp -> Xgratnum = PPgratnum;
	return((tree)pp);
}

id *Rgratnum(t)
 struct Sratnum *t;
{
	if(t -> tag != ratnum)
		printf("gratnum: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgratnum);
}

/************** tuple ******************/

tree mktuple(PPgtuplelist)
 list PPgtuplelist;
{
	register struct Stuple *pp =
		(struct Stuple *) malloc(sizeof(struct Stuple));
	pp -> tag = tuple;
	pp -> Xgtuplelist = PPgtuplelist;
	return((tree)pp);
}

list *Rgtuplelist(t)
 struct Stuple *t;
{
	if(t -> tag != tuple)
		printf("gtuplelist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtuplelist);
}

/************** ap ******************/

tree mkap(PPgfun, PPgarg)
 tree PPgfun;
 tree PPgarg;
{
	register struct Sap *pp =
		(struct Sap *) malloc(sizeof(struct Sap));
	pp -> tag = ap;
	pp -> Xgfun = PPgfun;
	pp -> Xgarg = PPgarg;
	return((tree)pp);
}

tree *Rgfun(t)
 struct Sap *t;
{
	if(t -> tag != ap)
		printf("gfun: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgfun);
}

tree *Rgarg(t)
 struct Sap *t;
{
	if(t -> tag != ap)
		printf("garg: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgarg);
}

/************** lam ******************/

tree mklam(PPglamid, PPglamexpr)
 tree PPglamid;
 tree PPglamexpr;
{
	register struct Slam *pp =
		(struct Slam *) malloc(sizeof(struct Slam));
	pp -> tag = lam;
	pp -> Xglamid = PPglamid;
	pp -> Xglamexpr = PPglamexpr;
	return((tree)pp);
}

tree *Rglamid(t)
 struct Slam *t;
{
	if(t -> tag != lam)
		printf("glamid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglamid);
}

tree *Rglamexpr(t)
 struct Slam *t;
{
	if(t -> tag != lam)
		printf("glamexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglamexpr);
}

/************** cexpr ******************/

tree mkcexpr(PPgsymlist)
 list PPgsymlist;
{
	register struct Scexpr *pp =
		(struct Scexpr *) malloc(sizeof(struct Scexpr));
	pp -> tag = cexpr;
	pp -> Xgsymlist = PPgsymlist;
	return((tree)pp);
}

list *Rgsymlist(t)
 struct Scexpr *t;
{
	if(t -> tag != cexpr)
		printf("gsymlist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsymlist);
}

/************** letv ******************/

tree mkletv(PPgletvdeflist, PPgletvexpr)
 list PPgletvdeflist;
 tree PPgletvexpr;
{
	register struct Sletv *pp =
		(struct Sletv *) malloc(sizeof(struct Sletv));
	pp -> tag = letv;
	pp -> Xgletvdeflist = PPgletvdeflist;
	pp -> Xgletvexpr = PPgletvexpr;
	return((tree)pp);
}

list *Rgletvdeflist(t)
 struct Sletv *t;
{
	if(t -> tag != letv)
		printf("gletvdeflist: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgletvdeflist);
}

tree *Rgletvexpr(t)
 struct Sletv *t;
{
	if(t -> tag != letv)
		printf("gletvexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgletvexpr);
}

/************** casee ******************/

tree mkcasee(PPgcaseexpr, PPgcasebody)
 tree PPgcaseexpr;
 list PPgcasebody;
{
	register struct Scasee *pp =
		(struct Scasee *) malloc(sizeof(struct Scasee));
	pp -> tag = casee;
	pp -> Xgcaseexpr = PPgcaseexpr;
	pp -> Xgcasebody = PPgcasebody;
	return((tree)pp);
}

tree *Rgcaseexpr(t)
 struct Scasee *t;
{
	if(t -> tag != casee)
		printf("gcaseexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcaseexpr);
}

list *Rgcasebody(t)
 struct Scasee *t;
{
	if(t -> tag != casee)
		printf("gcasebody: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcasebody);
}

/************** par ******************/

tree mkpar(PPgpare)
 tree PPgpare;
{
	register struct Spar *pp =
		(struct Spar *) malloc(sizeof(struct Spar));
	pp -> tag = par;
	pp -> Xgpare = PPgpare;
	return((tree)pp);
}

tree *Rgpare(t)
 struct Spar *t;
{
	if(t -> tag != par)
		printf("gpare: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgpare);
}

/************** as ******************/

tree mkas(PPgasid, PPgase)
 id PPgasid;
 tree PPgase;
{
	register struct Sas *pp =
		(struct Sas *) malloc(sizeof(struct Sas));
	pp -> tag = as;
	pp -> Xgasid = PPgasid;
	pp -> Xgase = PPgase;
	return((tree)pp);
}

id *Rgasid(t)
 struct Sas *t;
{
	if(t -> tag != as)
		printf("gasid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgasid);
}

tree *Rgase(t)
 struct Sas *t;
{
	if(t -> tag != as)
		printf("gase: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgase);
}

/************** condp ******************/

tree mkcondp(PPgcondpp, PPgcondpe)
 tree PPgcondpp;
 tree PPgcondpe;
{
	register struct Scondp *pp =
		(struct Scondp *) malloc(sizeof(struct Scondp));
	pp -> tag = condp;
	pp -> Xgcondpp = PPgcondpp;
	pp -> Xgcondpe = PPgcondpe;
	return((tree)pp);
}

tree *Rgcondpp(t)
 struct Scondp *t;
{
	if(t -> tag != condp)
		printf("gcondpp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcondpp);
}

tree *Rgcondpe(t)
 struct Scondp *t;
{
	if(t -> tag != condp)
		printf("gcondpe: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcondpe);
}

/************** lazyp ******************/

tree mklazyp(PPglazyp)
 tree PPglazyp;
{
	register struct Slazyp *pp =
		(struct Slazyp *) malloc(sizeof(struct Slazyp));
	pp -> tag = lazyp;
	pp -> Xglazyp = PPglazyp;
	return((tree)pp);
}

tree *Rglazyp(t)
 struct Slazyp *t;
{
	if(t -> tag != lazyp)
		printf("glazyp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglazyp);
}

/************** restr ******************/

tree mkrestr(PPgrestre, PPgrestrt)
 tree PPgrestre;
 ttype PPgrestrt;
{
	register struct Srestr *pp =
		(struct Srestr *) malloc(sizeof(struct Srestr));
	pp -> tag = restr;
	pp -> Xgrestre = PPgrestre;
	pp -> Xgrestrt = PPgrestrt;
	return((tree)pp);
}

tree *Rgrestre(t)
 struct Srestr *t;
{
	if(t -> tag != restr)
		printf("grestre: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgrestre);
}

ttype *Rgrestrt(t)
 struct Srestr *t;
{
	if(t -> tag != restr)
		printf("grestrt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgrestrt);
}

/************** eannot ******************/

tree mkeannot(PPgeannote, PPgeannota)
 tree PPgeannote;
 id PPgeannota;
{
	register struct Seannot *pp =
		(struct Seannot *) malloc(sizeof(struct Seannot));
	pp -> tag = eannot;
	pp -> Xgeannote = PPgeannote;
	pp -> Xgeannota = PPgeannota;
	return((tree)pp);
}

tree *Rgeannote(t)
 struct Seannot *t;
{
	if(t -> tag != eannot)
		printf("geannote: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgeannote);
}

id *Rgeannota(t)
 struct Seannot *t;
{
	if(t -> tag != eannot)
		printf("geannota: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgeannota);
}

/************** listf ******************/

tree mklistf(PPglistt, PPglistf)
 int PPglistt;
 list PPglistf;
{
	register struct Slistf *pp =
		(struct Slistf *) malloc(sizeof(struct Slistf));
	pp -> tag = listf;
	pp -> Xglistt = PPglistt;
	pp -> Xglistf = PPglistf;
	return((tree)pp);
}

int *Rglistt(t)
 struct Slistf *t;
{
	if(t -> tag != listf)
		printf("glistt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglistt);
}

list *Rglistf(t)
 struct Slistf *t;
{
	if(t -> tag != listf)
		printf("glistf: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglistf);
}

/************** listg ******************/

tree mklistg(PPglgg, PPglgq)
 tree PPglgg;
 list PPglgq;
{
	register struct Slistg *pp =
		(struct Slistg *) malloc(sizeof(struct Slistg));
	pp -> tag = listg;
	pp -> Xglgg = PPglgg;
	pp -> Xglgq = PPglgq;
	return((tree)pp);
}

tree *Rglgg(t)
 struct Slistg *t;
{
	if(t -> tag != listg)
		printf("glgg: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglgg);
}

list *Rglgq(t)
 struct Slistg *t;
{
	if(t -> tag != listg)
		printf("glgq: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglgq);
}

/************** hmodule ******************/

tree mkhmodule(PPghmodid, PPghexp, PPghimp, PPghfix, PPghbind)
 id PPghmodid;
 list PPghexp;
 list PPghimp;
 list PPghfix;
 list PPghbind;
{
	register struct Shmodule *pp =
		(struct Shmodule *) malloc(sizeof(struct Shmodule));
	pp -> tag = hmodule;
	pp -> Xghmodid = PPghmodid;
	pp -> Xghexp = PPghexp;
	pp -> Xghimp = PPghimp;
	pp -> Xghfix = PPghfix;
	pp -> Xghbind = PPghbind;
	return((tree)pp);
}

id *Rghmodid(t)
 struct Shmodule *t;
{
	if(t -> tag != hmodule)
		printf("ghmodid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xghmodid);
}

list *Rghexp(t)
 struct Shmodule *t;
{
	if(t -> tag != hmodule)
		printf("ghexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xghexp);
}

list *Rghimp(t)
 struct Shmodule *t;
{
	if(t -> tag != hmodule)
		printf("ghimp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xghimp);
}

list *Rghfix(t)
 struct Shmodule *t;
{
	if(t -> tag != hmodule)
		printf("ghfix: illegal selection; was %d\n", t -> tag);
	return(& t -> Xghfix);
}

list *Rghbind(t)
 struct Shmodule *t;
{
	if(t -> tag != hmodule)
		printf("ghbind: illegal selection; was %d\n", t -> tag);
	return(& t -> Xghbind);
}

/************** wherev ******************/

tree mkwherev(PPgwges, PPgwdefs)
 list PPgwges;
 list PPgwdefs;
{
	register struct Swherev *pp =
		(struct Swherev *) malloc(sizeof(struct Swherev));
	pp -> tag = wherev;
	pp -> Xgwges = PPgwges;
	pp -> Xgwdefs = PPgwdefs;
	return((tree)pp);
}

list *Rgwges(t)
 struct Swherev *t;
{
	if(t -> tag != wherev)
		printf("gwges: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgwges);
}

list *Rgwdefs(t)
 struct Swherev *t;
{
	if(t -> tag != wherev)
		printf("gwdefs: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgwdefs);
}

/************** doexp ******************/

tree mkdoexp(PPgstmt)
 list PPgstmt;
{
	register struct Sdoexp *pp =
		(struct Sdoexp *) malloc(sizeof(struct Sdoexp));
	pp -> tag = doexp;
	pp -> Xgstmt = PPgstmt;
	return((tree)pp);
}

list *Rgstmt(t)
 struct Sdoexp *t;
{
	if(t -> tag != doexp)
		printf("gstmt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgstmt);
}

/************** record ******************/

tree mkrecord(PPgrecid, PPgrecfields)
 tree PPgrecid;
 list PPgrecfields;
{
	register struct Srecord *pp =
		(struct Srecord *) malloc(sizeof(struct Srecord));
	pp -> tag = record;
	pp -> Xgrecid = PPgrecid;
	pp -> Xgrecfields = PPgrecfields;
	return((tree)pp);
}

tree *Rgrecid(t)
 struct Srecord *t;
{
	if(t -> tag != record)
		printf("grecid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgrecid);
}

list *Rgrecfields(t)
 struct Srecord *t;
{
	if(t -> tag != record)
		printf("grecfields: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgrecfields);
}
