

#include "include.h"
#include "qual.h"
struct Sqgen {
	Tqual tag;
	tree Xgqgenpat;
	tree Xgqgenexp;
};

struct Sqfilter {
	Tqual tag;
	tree Xgqfilter;
};

struct Sqlet {
	Tqual tag;
	binding Xgqbinding;
};

Tqual tqual(t)
 qual t;
{
	return(t -> tag);
}


/************** qgen ******************/

qual mkqgen(PPgqgenpat, PPgqgenexp)
 tree PPgqgenpat;
 tree PPgqgenexp;
{
	register struct Sqgen *pp =
		(struct Sqgen *) malloc(sizeof(struct Sqgen));
	pp -> tag = qgen;
	pp -> Xgqgenpat = PPgqgenpat;
	pp -> Xgqgenexp = PPgqgenexp;
	return((qual)pp);
}

tree *Rgqgenpat(t)
 struct Sqgen *t;
{
	if(t -> tag != qgen)
		printf("gqgenpat: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgqgenpat);
}

tree *Rgqgenexp(t)
 struct Sqgen *t;
{
	if(t -> tag != qgen)
		printf("gqgenexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgqgenexp);
}

/************** qfilter ******************/

qual mkqfilter(PPgqfilter)
 tree PPgqfilter;
{
	register struct Sqfilter *pp =
		(struct Sqfilter *) malloc(sizeof(struct Sqfilter));
	pp -> tag = qfilter;
	pp -> Xgqfilter = PPgqfilter;
	return((qual)pp);
}

tree *Rgqfilter(t)
 struct Sqfilter *t;
{
	if(t -> tag != qfilter)
		printf("gqfilter: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgqfilter);
}

/************** qlet ******************/

qual mkqlet(PPgqbinding)
 binding PPgqbinding;
{
	register struct Sqlet *pp =
		(struct Sqlet *) malloc(sizeof(struct Sqlet));
	pp -> tag = qlet;
	pp -> Xgqbinding = PPgqbinding;
	return((qual)pp);
}

binding *Rgqbinding(t)
 struct Sqlet *t;
{
	if(t -> tag != qlet)
		printf("gqbinding: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgqbinding);
}
