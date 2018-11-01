

#include "include.h"
#include "pdef.h"
struct Sppat {
	Tpbinding tag;
	tree Xgppat;
	tree Xgpexpr;
};

Tpbinding tpbinding(t)
 pbinding t;
{
	return(t -> tag);
}


/************** ppat ******************/

pbinding mkppat(PPgppat, PPgpexpr)
 tree PPgppat;
 tree PPgpexpr;
{
	register struct Sppat *pp =
		(struct Sppat *) malloc(sizeof(struct Sppat));
	pp -> tag = ppat;
	pp -> Xgppat = PPgppat;
	pp -> Xgpexpr = PPgpexpr;
	return((pbinding)pp);
}

tree *Rgppat(t)
 struct Sppat *t;
{
	if(t -> tag != ppat)
		printf("gppat: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgppat);
}

tree *Rgpexpr(t)
 struct Sppat *t;
{
	if(t -> tag != ppat)
		printf("gpexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgpexpr);
}
