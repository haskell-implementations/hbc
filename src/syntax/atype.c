

#include "include.h"
#include "atype.h"
struct Satc {
	Tatype tag;
	id Xgatcid;
	list Xgatcctx;
	list Xgatctypel;
};

Tatype tatype(t)
 atype t;
{
	return(t -> tag);
}


/************** atc ******************/

atype mkatc(PPgatcid, PPgatcctx, PPgatctypel)
 id PPgatcid;
 list PPgatcctx;
 list PPgatctypel;
{
	register struct Satc *pp =
		(struct Satc *) malloc(sizeof(struct Satc));
	pp -> tag = atc;
	pp -> Xgatcid = PPgatcid;
	pp -> Xgatcctx = PPgatcctx;
	pp -> Xgatctypel = PPgatctypel;
	return((atype)pp);
}

id *Rgatcid(t)
 struct Satc *t;
{
	if(t -> tag != atc)
		printf("gatcid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgatcid);
}

list *Rgatcctx(t)
 struct Satc *t;
{
	if(t -> tag != atc)
		printf("gatcctx: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgatcctx);
}

list *Rgatctypel(t)
 struct Satc *t;
{
	if(t -> tag != atc)
		printf("gatctypel: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgatctypel);
}
