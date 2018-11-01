

#include "include.h"
#include "impid.h"
struct Simpid {
	Timpidt tag;
	id Xgimpid;
	ttype Xgimptype;
	finfot Xgimpfinfo;
};

struct Simptype {
	Timpidt tag;
	ttype Xgimptypet;
	list Xgimpder;
	list Xgimptypi;
};

struct Simpeqtype {
	Timpidt tag;
	ttype Xgimpeqtype;
	list Xgimpeqcon;
	list Xgimpeqder;
};

struct Simpisotype {
	Timpidt tag;
	ttype Xgimpisotype;
	list Xgimpisocon;
	list Xgimpisoder;
};

struct Simpview {
	Timpidt tag;
	ttype Xgimpviewtype;
	ttype Xgimpviewof;
	list Xgimpviewcon;
};

struct Simpimport {
	Timpidt tag;
	id Xgimpimpmodid;
	list Xgimpimpexp;
	list Xgimpimpren;
};

struct Simpsyn {
	Timpidt tag;
	ttype Xgimpsynsrc;
	ttype Xgimpsyndst;
};

struct Simpclass {
	Timpidt tag;
	ttype Xgimpclasst;
	list Xgimpclassd;
	list Xgimpclasss;
};

struct Simpinst {
	Timpidt tag;
	ttype Xgimpinstt;
	int Xgimpinstd;
	id Xgimpinstm;
	list Xgimpinsts;
};

struct Simpids {
	Timpidt tag;
	list Xgimpids;
	ttype Xgimptypes;
	finfot Xgimpfinfos;
};

struct Simpctype {
	Timpidt tag;
	ttype Xgimpcttype;
	list Xgimpctprod;
};

Timpidt timpidt(t)
 impidt t;
{
	return(t -> tag);
}


/************** impid ******************/

impidt mkimpid(PPgimpid, PPgimptype, PPgimpfinfo)
 id PPgimpid;
 ttype PPgimptype;
 finfot PPgimpfinfo;
{
	register struct Simpid *pp =
		(struct Simpid *) malloc(sizeof(struct Simpid));
	pp -> tag = impid;
	pp -> Xgimpid = PPgimpid;
	pp -> Xgimptype = PPgimptype;
	pp -> Xgimpfinfo = PPgimpfinfo;
	return((impidt)pp);
}

id *Rgimpid(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		printf("gimpid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpid);
}

ttype *Rgimptype(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		printf("gimptype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptype);
}

finfot *Rgimpfinfo(t)
 struct Simpid *t;
{
	if(t -> tag != impid)
		printf("gimpfinfo: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpfinfo);
}

/************** imptype ******************/

impidt mkimptype(PPgimptypet, PPgimpder, PPgimptypi)
 ttype PPgimptypet;
 list PPgimpder;
 list PPgimptypi;
{
	register struct Simptype *pp =
		(struct Simptype *) malloc(sizeof(struct Simptype));
	pp -> tag = imptype;
	pp -> Xgimptypet = PPgimptypet;
	pp -> Xgimpder = PPgimpder;
	pp -> Xgimptypi = PPgimptypi;
	return((impidt)pp);
}

ttype *Rgimptypet(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		printf("gimptypet: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptypet);
}

list *Rgimpder(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		printf("gimpder: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpder);
}

list *Rgimptypi(t)
 struct Simptype *t;
{
	if(t -> tag != imptype)
		printf("gimptypi: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptypi);
}

/************** impeqtype ******************/

impidt mkimpeqtype(PPgimpeqtype, PPgimpeqcon, PPgimpeqder)
 ttype PPgimpeqtype;
 list PPgimpeqcon;
 list PPgimpeqder;
{
	register struct Simpeqtype *pp =
		(struct Simpeqtype *) malloc(sizeof(struct Simpeqtype));
	pp -> tag = impeqtype;
	pp -> Xgimpeqtype = PPgimpeqtype;
	pp -> Xgimpeqcon = PPgimpeqcon;
	pp -> Xgimpeqder = PPgimpeqder;
	return((impidt)pp);
}

ttype *Rgimpeqtype(t)
 struct Simpeqtype *t;
{
	if(t -> tag != impeqtype)
		printf("gimpeqtype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpeqtype);
}

list *Rgimpeqcon(t)
 struct Simpeqtype *t;
{
	if(t -> tag != impeqtype)
		printf("gimpeqcon: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpeqcon);
}

list *Rgimpeqder(t)
 struct Simpeqtype *t;
{
	if(t -> tag != impeqtype)
		printf("gimpeqder: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpeqder);
}

/************** impisotype ******************/

impidt mkimpisotype(PPgimpisotype, PPgimpisocon, PPgimpisoder)
 ttype PPgimpisotype;
 list PPgimpisocon;
 list PPgimpisoder;
{
	register struct Simpisotype *pp =
		(struct Simpisotype *) malloc(sizeof(struct Simpisotype));
	pp -> tag = impisotype;
	pp -> Xgimpisotype = PPgimpisotype;
	pp -> Xgimpisocon = PPgimpisocon;
	pp -> Xgimpisoder = PPgimpisoder;
	return((impidt)pp);
}

ttype *Rgimpisotype(t)
 struct Simpisotype *t;
{
	if(t -> tag != impisotype)
		printf("gimpisotype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpisotype);
}

list *Rgimpisocon(t)
 struct Simpisotype *t;
{
	if(t -> tag != impisotype)
		printf("gimpisocon: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpisocon);
}

list *Rgimpisoder(t)
 struct Simpisotype *t;
{
	if(t -> tag != impisotype)
		printf("gimpisoder: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpisoder);
}

/************** impview ******************/

impidt mkimpview(PPgimpviewtype, PPgimpviewof, PPgimpviewcon)
 ttype PPgimpviewtype;
 ttype PPgimpviewof;
 list PPgimpviewcon;
{
	register struct Simpview *pp =
		(struct Simpview *) malloc(sizeof(struct Simpview));
	pp -> tag = impview;
	pp -> Xgimpviewtype = PPgimpviewtype;
	pp -> Xgimpviewof = PPgimpviewof;
	pp -> Xgimpviewcon = PPgimpviewcon;
	return((impidt)pp);
}

ttype *Rgimpviewtype(t)
 struct Simpview *t;
{
	if(t -> tag != impview)
		printf("gimpviewtype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpviewtype);
}

ttype *Rgimpviewof(t)
 struct Simpview *t;
{
	if(t -> tag != impview)
		printf("gimpviewof: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpviewof);
}

list *Rgimpviewcon(t)
 struct Simpview *t;
{
	if(t -> tag != impview)
		printf("gimpviewcon: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpviewcon);
}

/************** impimport ******************/

impidt mkimpimport(PPgimpimpmodid, PPgimpimpexp, PPgimpimpren)
 id PPgimpimpmodid;
 list PPgimpimpexp;
 list PPgimpimpren;
{
	register struct Simpimport *pp =
		(struct Simpimport *) malloc(sizeof(struct Simpimport));
	pp -> tag = impimport;
	pp -> Xgimpimpmodid = PPgimpimpmodid;
	pp -> Xgimpimpexp = PPgimpimpexp;
	pp -> Xgimpimpren = PPgimpimpren;
	return((impidt)pp);
}

id *Rgimpimpmodid(t)
 struct Simpimport *t;
{
	if(t -> tag != impimport)
		printf("gimpimpmodid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpimpmodid);
}

list *Rgimpimpexp(t)
 struct Simpimport *t;
{
	if(t -> tag != impimport)
		printf("gimpimpexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpimpexp);
}

list *Rgimpimpren(t)
 struct Simpimport *t;
{
	if(t -> tag != impimport)
		printf("gimpimpren: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpimpren);
}

/************** impsyn ******************/

impidt mkimpsyn(PPgimpsynsrc, PPgimpsyndst)
 ttype PPgimpsynsrc;
 ttype PPgimpsyndst;
{
	register struct Simpsyn *pp =
		(struct Simpsyn *) malloc(sizeof(struct Simpsyn));
	pp -> tag = impsyn;
	pp -> Xgimpsynsrc = PPgimpsynsrc;
	pp -> Xgimpsyndst = PPgimpsyndst;
	return((impidt)pp);
}

ttype *Rgimpsynsrc(t)
 struct Simpsyn *t;
{
	if(t -> tag != impsyn)
		printf("gimpsynsrc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpsynsrc);
}

ttype *Rgimpsyndst(t)
 struct Simpsyn *t;
{
	if(t -> tag != impsyn)
		printf("gimpsyndst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpsyndst);
}

/************** impclass ******************/

impidt mkimpclass(PPgimpclasst, PPgimpclassd, PPgimpclasss)
 ttype PPgimpclasst;
 list PPgimpclassd;
 list PPgimpclasss;
{
	register struct Simpclass *pp =
		(struct Simpclass *) malloc(sizeof(struct Simpclass));
	pp -> tag = impclass;
	pp -> Xgimpclasst = PPgimpclasst;
	pp -> Xgimpclassd = PPgimpclassd;
	pp -> Xgimpclasss = PPgimpclasss;
	return((impidt)pp);
}

ttype *Rgimpclasst(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		printf("gimpclasst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclasst);
}

list *Rgimpclassd(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		printf("gimpclassd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclassd);
}

list *Rgimpclasss(t)
 struct Simpclass *t;
{
	if(t -> tag != impclass)
		printf("gimpclasss: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpclasss);
}

/************** impinst ******************/

impidt mkimpinst(PPgimpinstt, PPgimpinstd, PPgimpinstm, PPgimpinsts)
 ttype PPgimpinstt;
 int PPgimpinstd;
 id PPgimpinstm;
 list PPgimpinsts;
{
	register struct Simpinst *pp =
		(struct Simpinst *) malloc(sizeof(struct Simpinst));
	pp -> tag = impinst;
	pp -> Xgimpinstt = PPgimpinstt;
	pp -> Xgimpinstd = PPgimpinstd;
	pp -> Xgimpinstm = PPgimpinstm;
	pp -> Xgimpinsts = PPgimpinsts;
	return((impidt)pp);
}

ttype *Rgimpinstt(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		printf("gimpinstt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstt);
}

int *Rgimpinstd(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		printf("gimpinstd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstd);
}

id *Rgimpinstm(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		printf("gimpinstm: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinstm);
}

list *Rgimpinsts(t)
 struct Simpinst *t;
{
	if(t -> tag != impinst)
		printf("gimpinsts: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpinsts);
}

/************** impids ******************/

impidt mkimpids(PPgimpids, PPgimptypes, PPgimpfinfos)
 list PPgimpids;
 ttype PPgimptypes;
 finfot PPgimpfinfos;
{
	register struct Simpids *pp =
		(struct Simpids *) malloc(sizeof(struct Simpids));
	pp -> tag = impids;
	pp -> Xgimpids = PPgimpids;
	pp -> Xgimptypes = PPgimptypes;
	pp -> Xgimpfinfos = PPgimpfinfos;
	return((impidt)pp);
}

list *Rgimpids(t)
 struct Simpids *t;
{
	if(t -> tag != impids)
		printf("gimpids: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpids);
}

ttype *Rgimptypes(t)
 struct Simpids *t;
{
	if(t -> tag != impids)
		printf("gimptypes: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimptypes);
}

finfot *Rgimpfinfos(t)
 struct Simpids *t;
{
	if(t -> tag != impids)
		printf("gimpfinfos: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpfinfos);
}

/************** impctype ******************/

impidt mkimpctype(PPgimpcttype, PPgimpctprod)
 ttype PPgimpcttype;
 list PPgimpctprod;
{
	register struct Simpctype *pp =
		(struct Simpctype *) malloc(sizeof(struct Simpctype));
	pp -> tag = impctype;
	pp -> Xgimpcttype = PPgimpcttype;
	pp -> Xgimpctprod = PPgimpctprod;
	return((impidt)pp);
}

ttype *Rgimpcttype(t)
 struct Simpctype *t;
{
	if(t -> tag != impctype)
		printf("gimpcttype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpcttype);
}

list *Rgimpctprod(t)
 struct Simpctype *t;
{
	if(t -> tag != impctype)
		printf("gimpctprod: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimpctprod);
}
