

#include "include.h"
#include "pair.h"
struct Sppair {
	Tpair tag;
	voidptr Xpfst;
	voidptr Xpsnd;
};

Tpair tpair(t)
 pair t;
{
	return(t -> tag);
}


/************** ppair ******************/

pair mkppair(PPpfst, PPpsnd)
 voidptr PPpfst;
 voidptr PPpsnd;
{
	register struct Sppair *pp =
		(struct Sppair *) malloc(sizeof(struct Sppair));
	pp -> tag = ppair;
	pp -> Xpfst = PPpfst;
	pp -> Xpsnd = PPpsnd;
	return((pair)pp);
}

voidptr *Rpfst(t)
 struct Sppair *t;
{
	if(t -> tag != ppair)
		printf("pfst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfst);
}

voidptr *Rpsnd(t)
 struct Sppair *t;
{
	if(t -> tag != ppair)
		printf("psnd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpsnd);
}
