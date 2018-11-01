

#include "include.h"
#include "ttype.h"
struct Stname {
	Tttype tag;
	id Xgtypeid;
	list Xgtypel;
};

struct Stvar {
	Tttype tag;
	int Xgtvar;
};

struct Ststrict {
	Tttype tag;
	ttype Xgtstrict;
};

struct Stcontext {
	Tttype tag;
	list Xgcontexts;
	ttype Xgctype;
};

struct Stsels {
	Tttype tag;
	list Xgselidstys;
};

struct Stap {
	Tttype tag;
	ttype Xgtapvar;
	list Xgtaptypel;
};

struct Stnamek {
	Tttype tag;
	id Xgtypeidk;
	kind Xgkindk;
	list Xgtypelk;
};

Tttype tttype(t)
 ttype t;
{
	return(t -> tag);
}


/************** tname ******************/

ttype mktname(PPgtypeid, PPgtypel)
 id PPgtypeid;
 list PPgtypel;
{
	register struct Stname *pp =
		(struct Stname *) malloc(sizeof(struct Stname));
	pp -> tag = tname;
	pp -> Xgtypeid = PPgtypeid;
	pp -> Xgtypel = PPgtypel;
	return((ttype)pp);
}

id *Rgtypeid(t)
 struct Stname *t;
{
	if(t -> tag != tname)
		printf("gtypeid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypeid);
}

list *Rgtypel(t)
 struct Stname *t;
{
	if(t -> tag != tname)
		printf("gtypel: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypel);
}

/************** tvar ******************/

ttype mktvar(PPgtvar)
 int PPgtvar;
{
	register struct Stvar *pp =
		(struct Stvar *) malloc(sizeof(struct Stvar));
	pp -> tag = tvar;
	pp -> Xgtvar = PPgtvar;
	return((ttype)pp);
}

int *Rgtvar(t)
 struct Stvar *t;
{
	if(t -> tag != tvar)
		printf("gtvar: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtvar);
}

/************** tstrict ******************/

ttype mktstrict(PPgtstrict)
 ttype PPgtstrict;
{
	register struct Ststrict *pp =
		(struct Ststrict *) malloc(sizeof(struct Ststrict));
	pp -> tag = tstrict;
	pp -> Xgtstrict = PPgtstrict;
	return((ttype)pp);
}

ttype *Rgtstrict(t)
 struct Ststrict *t;
{
	if(t -> tag != tstrict)
		printf("gtstrict: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtstrict);
}

/************** tcontext ******************/

ttype mktcontext(PPgcontexts, PPgctype)
 list PPgcontexts;
 ttype PPgctype;
{
	register struct Stcontext *pp =
		(struct Stcontext *) malloc(sizeof(struct Stcontext));
	pp -> tag = tcontext;
	pp -> Xgcontexts = PPgcontexts;
	pp -> Xgctype = PPgctype;
	return((ttype)pp);
}

list *Rgcontexts(t)
 struct Stcontext *t;
{
	if(t -> tag != tcontext)
		printf("gcontexts: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcontexts);
}

ttype *Rgctype(t)
 struct Stcontext *t;
{
	if(t -> tag != tcontext)
		printf("gctype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgctype);
}

/************** tsels ******************/

ttype mktsels(PPgselidstys)
 list PPgselidstys;
{
	register struct Stsels *pp =
		(struct Stsels *) malloc(sizeof(struct Stsels));
	pp -> tag = tsels;
	pp -> Xgselidstys = PPgselidstys;
	return((ttype)pp);
}

list *Rgselidstys(t)
 struct Stsels *t;
{
	if(t -> tag != tsels)
		printf("gselidstys: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgselidstys);
}

/************** tap ******************/

ttype mktap(PPgtapvar, PPgtaptypel)
 ttype PPgtapvar;
 list PPgtaptypel;
{
	register struct Stap *pp =
		(struct Stap *) malloc(sizeof(struct Stap));
	pp -> tag = tap;
	pp -> Xgtapvar = PPgtapvar;
	pp -> Xgtaptypel = PPgtaptypel;
	return((ttype)pp);
}

ttype *Rgtapvar(t)
 struct Stap *t;
{
	if(t -> tag != tap)
		printf("gtapvar: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtapvar);
}

list *Rgtaptypel(t)
 struct Stap *t;
{
	if(t -> tag != tap)
		printf("gtaptypel: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtaptypel);
}

/************** tnamek ******************/

ttype mktnamek(PPgtypeidk, PPgkindk, PPgtypelk)
 id PPgtypeidk;
 kind PPgkindk;
 list PPgtypelk;
{
	register struct Stnamek *pp =
		(struct Stnamek *) malloc(sizeof(struct Stnamek));
	pp -> tag = tnamek;
	pp -> Xgtypeidk = PPgtypeidk;
	pp -> Xgkindk = PPgkindk;
	pp -> Xgtypelk = PPgtypelk;
	return((ttype)pp);
}

id *Rgtypeidk(t)
 struct Stnamek *t;
{
	if(t -> tag != tnamek)
		printf("gtypeidk: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypeidk);
}

kind *Rgkindk(t)
 struct Stnamek *t;
{
	if(t -> tag != tnamek)
		printf("gkindk: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgkindk);
}

list *Rgtypelk(t)
 struct Stnamek *t;
{
	if(t -> tag != tnamek)
		printf("gtypelk: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypelk);
}
