

#include "include.h"
#include "bind.h"
struct Stbind {
	Tbinding tag;
	ttype Xgtbindid;
	list Xgtbindc;
	list Xgtbindd;
};

struct Spbind {
	Tbinding tag;
	list Xgpbindl;
};

struct Sgbind {
	Tbinding tag;
	ttype Xgtcbindid;
	list Xgtcbindc;
};

struct Sabind {
	Tbinding tag;
	binding Xgabindfst;
	binding Xgabindsnd;
};

struct Srbind {
	Tbinding tag;
	binding Xgrbind;
};

struct Slbind {
	Tbinding tag;
	binding Xglbindfst;
	binding Xglbindsnd;
};

struct Sebind {
	Tbinding tag;
	ttype Xgebindid;
	ttype Xgebindt;
};

struct Sibind {
	Tbinding tag;
	ttype Xgitype;
	binding Xgibindb;
};

struct Scbind {
	Tbinding tag;
	ttype Xgcbindt;
	binding Xgcbindb;
};

struct Snbind {
	Tbinding tag;
};

struct Subind {
	Tbinding tag;
	list Xguids;
};

struct Ssbind {
	Tbinding tag;
	list Xgbsids;
	ttype Xgbstype;
};

struct Sspecbind {
	Tbinding tag;
	id Xgspecid;
	list Xgspectypes;
};

struct Sspecinstbind {
	Tbinding tag;
	ttype Xgspecinst;
};

struct Sxbind {
	Tbinding tag;
	ttype Xgxbindid;
	list Xgxbindc;
	list Xgxbindd;
};

struct Svbind {
	Tbinding tag;
	ttype Xgvbindid;
	ttype Xgvbindt;
	list Xgvbindc;
	binding Xgvbindb;
};

Tbinding tbinding(t)
 binding t;
{
	return(t -> tag);
}


/************** tbind ******************/

binding mktbind(PPgtbindid, PPgtbindc, PPgtbindd)
 ttype PPgtbindid;
 list PPgtbindc;
 list PPgtbindd;
{
	register struct Stbind *pp =
		(struct Stbind *) malloc(sizeof(struct Stbind));
	pp -> tag = tbind;
	pp -> Xgtbindid = PPgtbindid;
	pp -> Xgtbindc = PPgtbindc;
	pp -> Xgtbindd = PPgtbindd;
	return((binding)pp);
}

ttype *Rgtbindid(t)
 struct Stbind *t;
{
	if(t -> tag != tbind)
		printf("gtbindid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtbindid);
}

list *Rgtbindc(t)
 struct Stbind *t;
{
	if(t -> tag != tbind)
		printf("gtbindc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtbindc);
}

list *Rgtbindd(t)
 struct Stbind *t;
{
	if(t -> tag != tbind)
		printf("gtbindd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtbindd);
}

/************** pbind ******************/

binding mkpbind(PPgpbindl)
 list PPgpbindl;
{
	register struct Spbind *pp =
		(struct Spbind *) malloc(sizeof(struct Spbind));
	pp -> tag = pbind;
	pp -> Xgpbindl = PPgpbindl;
	return((binding)pp);
}

list *Rgpbindl(t)
 struct Spbind *t;
{
	if(t -> tag != pbind)
		printf("gpbindl: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgpbindl);
}

/************** gbind ******************/

binding mkgbind(PPgtcbindid, PPgtcbindc)
 ttype PPgtcbindid;
 list PPgtcbindc;
{
	register struct Sgbind *pp =
		(struct Sgbind *) malloc(sizeof(struct Sgbind));
	pp -> tag = gbind;
	pp -> Xgtcbindid = PPgtcbindid;
	pp -> Xgtcbindc = PPgtcbindc;
	return((binding)pp);
}

ttype *Rgtcbindid(t)
 struct Sgbind *t;
{
	if(t -> tag != gbind)
		printf("gtcbindid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtcbindid);
}

list *Rgtcbindc(t)
 struct Sgbind *t;
{
	if(t -> tag != gbind)
		printf("gtcbindc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtcbindc);
}

/************** abind ******************/

binding mkabind(PPgabindfst, PPgabindsnd)
 binding PPgabindfst;
 binding PPgabindsnd;
{
	register struct Sabind *pp =
		(struct Sabind *) malloc(sizeof(struct Sabind));
	pp -> tag = abind;
	pp -> Xgabindfst = PPgabindfst;
	pp -> Xgabindsnd = PPgabindsnd;
	return((binding)pp);
}

binding *Rgabindfst(t)
 struct Sabind *t;
{
	if(t -> tag != abind)
		printf("gabindfst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgabindfst);
}

binding *Rgabindsnd(t)
 struct Sabind *t;
{
	if(t -> tag != abind)
		printf("gabindsnd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgabindsnd);
}

/************** rbind ******************/

binding mkrbind(PPgrbind)
 binding PPgrbind;
{
	register struct Srbind *pp =
		(struct Srbind *) malloc(sizeof(struct Srbind));
	pp -> tag = rbind;
	pp -> Xgrbind = PPgrbind;
	return((binding)pp);
}

binding *Rgrbind(t)
 struct Srbind *t;
{
	if(t -> tag != rbind)
		printf("grbind: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgrbind);
}

/************** lbind ******************/

binding mklbind(PPglbindfst, PPglbindsnd)
 binding PPglbindfst;
 binding PPglbindsnd;
{
	register struct Slbind *pp =
		(struct Slbind *) malloc(sizeof(struct Slbind));
	pp -> tag = lbind;
	pp -> Xglbindfst = PPglbindfst;
	pp -> Xglbindsnd = PPglbindsnd;
	return((binding)pp);
}

binding *Rglbindfst(t)
 struct Slbind *t;
{
	if(t -> tag != lbind)
		printf("glbindfst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglbindfst);
}

binding *Rglbindsnd(t)
 struct Slbind *t;
{
	if(t -> tag != lbind)
		printf("glbindsnd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xglbindsnd);
}

/************** ebind ******************/

binding mkebind(PPgebindid, PPgebindt)
 ttype PPgebindid;
 ttype PPgebindt;
{
	register struct Sebind *pp =
		(struct Sebind *) malloc(sizeof(struct Sebind));
	pp -> tag = ebind;
	pp -> Xgebindid = PPgebindid;
	pp -> Xgebindt = PPgebindt;
	return((binding)pp);
}

ttype *Rgebindid(t)
 struct Sebind *t;
{
	if(t -> tag != ebind)
		printf("gebindid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgebindid);
}

ttype *Rgebindt(t)
 struct Sebind *t;
{
	if(t -> tag != ebind)
		printf("gebindt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgebindt);
}

/************** ibind ******************/

binding mkibind(PPgitype, PPgibindb)
 ttype PPgitype;
 binding PPgibindb;
{
	register struct Sibind *pp =
		(struct Sibind *) malloc(sizeof(struct Sibind));
	pp -> tag = ibind;
	pp -> Xgitype = PPgitype;
	pp -> Xgibindb = PPgibindb;
	return((binding)pp);
}

ttype *Rgitype(t)
 struct Sibind *t;
{
	if(t -> tag != ibind)
		printf("gitype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgitype);
}

binding *Rgibindb(t)
 struct Sibind *t;
{
	if(t -> tag != ibind)
		printf("gibindb: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgibindb);
}

/************** cbind ******************/

binding mkcbind(PPgcbindt, PPgcbindb)
 ttype PPgcbindt;
 binding PPgcbindb;
{
	register struct Scbind *pp =
		(struct Scbind *) malloc(sizeof(struct Scbind));
	pp -> tag = cbind;
	pp -> Xgcbindt = PPgcbindt;
	pp -> Xgcbindb = PPgcbindb;
	return((binding)pp);
}

ttype *Rgcbindt(t)
 struct Scbind *t;
{
	if(t -> tag != cbind)
		printf("gcbindt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcbindt);
}

binding *Rgcbindb(t)
 struct Scbind *t;
{
	if(t -> tag != cbind)
		printf("gcbindb: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcbindb);
}

/************** nbind ******************/

binding mknbind()
{
	register struct Snbind *pp =
		(struct Snbind *) malloc(sizeof(struct Snbind));
	pp -> tag = nbind;
	return((binding)pp);
}

/************** ubind ******************/

binding mkubind(PPguids)
 list PPguids;
{
	register struct Subind *pp =
		(struct Subind *) malloc(sizeof(struct Subind));
	pp -> tag = ubind;
	pp -> Xguids = PPguids;
	return((binding)pp);
}

list *Rguids(t)
 struct Subind *t;
{
	if(t -> tag != ubind)
		printf("guids: illegal selection; was %d\n", t -> tag);
	return(& t -> Xguids);
}

/************** sbind ******************/

binding mksbind(PPgbsids, PPgbstype)
 list PPgbsids;
 ttype PPgbstype;
{
	register struct Ssbind *pp =
		(struct Ssbind *) malloc(sizeof(struct Ssbind));
	pp -> tag = sbind;
	pp -> Xgbsids = PPgbsids;
	pp -> Xgbstype = PPgbstype;
	return((binding)pp);
}

list *Rgbsids(t)
 struct Ssbind *t;
{
	if(t -> tag != sbind)
		printf("gbsids: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgbsids);
}

ttype *Rgbstype(t)
 struct Ssbind *t;
{
	if(t -> tag != sbind)
		printf("gbstype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgbstype);
}

/************** specbind ******************/

binding mkspecbind(PPgspecid, PPgspectypes)
 id PPgspecid;
 list PPgspectypes;
{
	register struct Sspecbind *pp =
		(struct Sspecbind *) malloc(sizeof(struct Sspecbind));
	pp -> tag = specbind;
	pp -> Xgspecid = PPgspecid;
	pp -> Xgspectypes = PPgspectypes;
	return((binding)pp);
}

id *Rgspecid(t)
 struct Sspecbind *t;
{
	if(t -> tag != specbind)
		printf("gspecid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgspecid);
}

list *Rgspectypes(t)
 struct Sspecbind *t;
{
	if(t -> tag != specbind)
		printf("gspectypes: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgspectypes);
}

/************** specinstbind ******************/

binding mkspecinstbind(PPgspecinst)
 ttype PPgspecinst;
{
	register struct Sspecinstbind *pp =
		(struct Sspecinstbind *) malloc(sizeof(struct Sspecinstbind));
	pp -> tag = specinstbind;
	pp -> Xgspecinst = PPgspecinst;
	return((binding)pp);
}

ttype *Rgspecinst(t)
 struct Sspecinstbind *t;
{
	if(t -> tag != specinstbind)
		printf("gspecinst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgspecinst);
}

/************** xbind ******************/

binding mkxbind(PPgxbindid, PPgxbindc, PPgxbindd)
 ttype PPgxbindid;
 list PPgxbindc;
 list PPgxbindd;
{
	register struct Sxbind *pp =
		(struct Sxbind *) malloc(sizeof(struct Sxbind));
	pp -> tag = xbind;
	pp -> Xgxbindid = PPgxbindid;
	pp -> Xgxbindc = PPgxbindc;
	pp -> Xgxbindd = PPgxbindd;
	return((binding)pp);
}

ttype *Rgxbindid(t)
 struct Sxbind *t;
{
	if(t -> tag != xbind)
		printf("gxbindid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgxbindid);
}

list *Rgxbindc(t)
 struct Sxbind *t;
{
	if(t -> tag != xbind)
		printf("gxbindc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgxbindc);
}

list *Rgxbindd(t)
 struct Sxbind *t;
{
	if(t -> tag != xbind)
		printf("gxbindd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgxbindd);
}

/************** vbind ******************/

binding mkvbind(PPgvbindid, PPgvbindt, PPgvbindc, PPgvbindb)
 ttype PPgvbindid;
 ttype PPgvbindt;
 list PPgvbindc;
 binding PPgvbindb;
{
	register struct Svbind *pp =
		(struct Svbind *) malloc(sizeof(struct Svbind));
	pp -> tag = vbind;
	pp -> Xgvbindid = PPgvbindid;
	pp -> Xgvbindt = PPgvbindt;
	pp -> Xgvbindc = PPgvbindc;
	pp -> Xgvbindb = PPgvbindb;
	return((binding)pp);
}

ttype *Rgvbindid(t)
 struct Svbind *t;
{
	if(t -> tag != vbind)
		printf("gvbindid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgvbindid);
}

ttype *Rgvbindt(t)
 struct Svbind *t;
{
	if(t -> tag != vbind)
		printf("gvbindt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgvbindt);
}

list *Rgvbindc(t)
 struct Svbind *t;
{
	if(t -> tag != vbind)
		printf("gvbindc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgvbindc);
}

binding *Rgvbindb(t)
 struct Svbind *t;
{
	if(t -> tag != vbind)
		printf("gvbindb: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgvbindb);
}
