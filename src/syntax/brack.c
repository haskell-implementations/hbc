

#include "include.h"
#include "brack.h"
struct Snormal {
	Tbrack tag;
	list Xgconstrn;
	int Xgprecn;
	assoc Xgassn;
};

struct Sforget {
	Tbrack tag;
	list Xgconstrf;
	int Xgprecf;
	assoc Xgassf;
};

Tbrack tbrack(t)
 brack t;
{
	return(t -> tag);
}


/************** normal ******************/

brack mknormal(PPgconstrn, PPgprecn, PPgassn)
 list PPgconstrn;
 int PPgprecn;
 assoc PPgassn;
{
	register struct Snormal *pp =
		(struct Snormal *) malloc(sizeof(struct Snormal));
	pp -> tag = normal;
	pp -> Xgconstrn = PPgconstrn;
	pp -> Xgprecn = PPgprecn;
	pp -> Xgassn = PPgassn;
	return((brack)pp);
}

list *Rgconstrn(t)
 struct Snormal *t;
{
	if(t -> tag != normal)
		printf("gconstrn: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgconstrn);
}

int *Rgprecn(t)
 struct Snormal *t;
{
	if(t -> tag != normal)
		printf("gprecn: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgprecn);
}

assoc *Rgassn(t)
 struct Snormal *t;
{
	if(t -> tag != normal)
		printf("gassn: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgassn);
}

/************** forget ******************/

brack mkforget(PPgconstrf, PPgprecf, PPgassf)
 list PPgconstrf;
 int PPgprecf;
 assoc PPgassf;
{
	register struct Sforget *pp =
		(struct Sforget *) malloc(sizeof(struct Sforget));
	pp -> tag = forget;
	pp -> Xgconstrf = PPgconstrf;
	pp -> Xgprecf = PPgprecf;
	pp -> Xgassf = PPgassf;
	return((brack)pp);
}

list *Rgconstrf(t)
 struct Sforget *t;
{
	if(t -> tag != forget)
		printf("gconstrf: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgconstrf);
}

int *Rgprecf(t)
 struct Sforget *t;
{
	if(t -> tag != forget)
		printf("gprecf: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgprecf);
}

assoc *Rgassf(t)
 struct Sforget *t;
{
	if(t -> tag != forget)
		printf("gassf: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgassf);
}
