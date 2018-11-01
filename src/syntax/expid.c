

#include "include.h"
#include "expid.h"
struct Sexpid {
	Texpidt tag;
	id Xgexpid;
};

struct Sexpdd {
	Texpidt tag;
	id Xgexpdd;
};

struct Sexppdd {
	Texpidt tag;
	id Xgexppdd;
};

struct Sexpl {
	Texpidt tag;
	id Xgexplid;
	list Xgexpll;
};

Texpidt texpidt(t)
 expidt t;
{
	return(t -> tag);
}


/************** expid ******************/

expidt mkexpid(PPgexpid)
 id PPgexpid;
{
	register struct Sexpid *pp =
		(struct Sexpid *) malloc(sizeof(struct Sexpid));
	pp -> tag = expid;
	pp -> Xgexpid = PPgexpid;
	return((expidt)pp);
}

id *Rgexpid(t)
 struct Sexpid *t;
{
	if(t -> tag != expid)
		printf("gexpid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexpid);
}

/************** expdd ******************/

expidt mkexpdd(PPgexpdd)
 id PPgexpdd;
{
	register struct Sexpdd *pp =
		(struct Sexpdd *) malloc(sizeof(struct Sexpdd));
	pp -> tag = expdd;
	pp -> Xgexpdd = PPgexpdd;
	return((expidt)pp);
}

id *Rgexpdd(t)
 struct Sexpdd *t;
{
	if(t -> tag != expdd)
		printf("gexpdd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexpdd);
}

/************** exppdd ******************/

expidt mkexppdd(PPgexppdd)
 id PPgexppdd;
{
	register struct Sexppdd *pp =
		(struct Sexppdd *) malloc(sizeof(struct Sexppdd));
	pp -> tag = exppdd;
	pp -> Xgexppdd = PPgexppdd;
	return((expidt)pp);
}

id *Rgexppdd(t)
 struct Sexppdd *t;
{
	if(t -> tag != exppdd)
		printf("gexppdd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexppdd);
}

/************** expl ******************/

expidt mkexpl(PPgexplid, PPgexpll)
 id PPgexplid;
 list PPgexpll;
{
	register struct Sexpl *pp =
		(struct Sexpl *) malloc(sizeof(struct Sexpl));
	pp -> tag = expl;
	pp -> Xgexplid = PPgexplid;
	pp -> Xgexpll = PPgexpll;
	return((expidt)pp);
}

id *Rgexplid(t)
 struct Sexpl *t;
{
	if(t -> tag != expl)
		printf("gexplid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexplid);
}

list *Rgexpll(t)
 struct Sexpl *t;
{
	if(t -> tag != expl)
		printf("gexpll: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexpll);
}
