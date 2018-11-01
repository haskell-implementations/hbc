

#include "include.h"
#include "list.h"
struct Slcons {
	Tlist tag;
	list Xlhd;
	list Xltl;
};

struct Slnil {
	Tlist tag;
};

Tlist tlist(t)
 list t;
{
	return(t -> tag);
}


/************** lcons ******************/

list mklcons(PPlhd, PPltl)
 list PPlhd;
 list PPltl;
{
	register struct Slcons *pp =
		(struct Slcons *) malloc(sizeof(struct Slcons));
	pp -> tag = lcons;
	pp -> Xlhd = PPlhd;
	pp -> Xltl = PPltl;
	return((list)pp);
}

list *Rlhd(t)
 struct Slcons *t;
{
	if(t -> tag != lcons)
		printf("lhd: illegal selection; was %d\n", t -> tag);
	return(& t -> Xlhd);
}

list *Rltl(t)
 struct Slcons *t;
{
	if(t -> tag != lcons)
		printf("ltl: illegal selection; was %d\n", t -> tag);
	return(& t -> Xltl);
}

/************** lnil ******************/

list mklnil()
{
	register struct Slnil *pp =
		(struct Slnil *) malloc(sizeof(struct Slnil));
	pp -> tag = lnil;
	return((list)pp);
}
