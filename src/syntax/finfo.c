

#include "include.h"
#include "finfo.h"
struct Snofinfo {
	Tfinfot tag;
};

struct Sfinfo {
	Tfinfot tag;
	id Xfi1;
	id Xfi2;
	int Xfi3;
};

struct Shfinfo {
	Tfinfot tag;
	list Xpfinline;
	id Xpfstrict;
	list Xpfentry;
	int Xpfarity;
	int Xpffrsize;
	list Xpfinsts;
	int Xpfevaled;
};

Tfinfot tfinfot(t)
 finfot t;
{
	return(t -> tag);
}


/************** nofinfo ******************/

finfot mknofinfo()
{
	register struct Snofinfo *pp =
		(struct Snofinfo *) malloc(sizeof(struct Snofinfo));
	pp -> tag = nofinfo;
	return((finfot)pp);
}

/************** finfo ******************/

finfot mkfinfo(PPfi1, PPfi2, PPfi3)
 id PPfi1;
 id PPfi2;
 int PPfi3;
{
	register struct Sfinfo *pp =
		(struct Sfinfo *) malloc(sizeof(struct Sfinfo));
	pp -> tag = finfo;
	pp -> Xfi1 = PPfi1;
	pp -> Xfi2 = PPfi2;
	pp -> Xfi3 = PPfi3;
	return((finfot)pp);
}

id *Rfi1(t)
 struct Sfinfo *t;
{
	if(t -> tag != finfo)
		printf("fi1: illegal selection; was %d\n", t -> tag);
	return(& t -> Xfi1);
}

id *Rfi2(t)
 struct Sfinfo *t;
{
	if(t -> tag != finfo)
		printf("fi2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xfi2);
}

int *Rfi3(t)
 struct Sfinfo *t;
{
	if(t -> tag != finfo)
		printf("fi3: illegal selection; was %d\n", t -> tag);
	return(& t -> Xfi3);
}

/************** hfinfo ******************/

finfot mkhfinfo(PPpfinline, PPpfstrict, PPpfentry, PPpfarity, PPpffrsize, PPpfinsts, PPpfevaled)
 list PPpfinline;
 id PPpfstrict;
 list PPpfentry;
 int PPpfarity;
 int PPpffrsize;
 list PPpfinsts;
 int PPpfevaled;
{
	register struct Shfinfo *pp =
		(struct Shfinfo *) malloc(sizeof(struct Shfinfo));
	pp -> tag = hfinfo;
	pp -> Xpfinline = PPpfinline;
	pp -> Xpfstrict = PPpfstrict;
	pp -> Xpfentry = PPpfentry;
	pp -> Xpfarity = PPpfarity;
	pp -> Xpffrsize = PPpffrsize;
	pp -> Xpfinsts = PPpfinsts;
	pp -> Xpfevaled = PPpfevaled;
	return((finfot)pp);
}

list *Rpfinline(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfinline: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfinline);
}

id *Rpfstrict(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfstrict: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfstrict);
}

list *Rpfentry(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfentry: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfentry);
}

int *Rpfarity(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfarity: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfarity);
}

int *Rpffrsize(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pffrsize: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpffrsize);
}

list *Rpfinsts(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfinsts: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfinsts);
}

int *Rpfevaled(t)
 struct Shfinfo *t;
{
	if(t -> tag != hfinfo)
		printf("pfevaled: illegal selection; was %d\n", t -> tag);
	return(& t -> Xpfevaled);
}
