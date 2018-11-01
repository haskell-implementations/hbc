

#include "include.h"
#include "sym.h"
struct Santiquote {
	Tsym tag;
	tree Xgexpr;
};

struct Sgramterm {
	Tsym tag;
	char Xgcharacter;
};

struct Sgramintterm {
	Tsym tag;
	int Xgcint;
};

struct Sgramidterm {
	Tsym tag;
	id Xgcid;
};

struct Sgramsymterm {
	Tsym tag;
	id Xgcsym;
};

struct Sexpterm {
	Tsym tag;
	char Xgcharacter2;
};

struct Sexpintterm {
	Tsym tag;
	int Xgcint2;
};

struct Sexpidterm {
	Tsym tag;
	id Xgcid2;
};

struct Sexpsymterm {
	Tsym tag;
	id Xgcsym2;
};

struct Snonterminal {
	Tsym tag;
	ttype Xgtype;
};

struct Slist1 {
	Tsym tag;
	ttype Xgtypel1;
	list Xgterms1;
};

struct Slist0 {
	Tsym tag;
	ttype Xgtypel0;
	list Xgterms0;
};

Tsym tsym(t)
 sym t;
{
	return(t -> tag);
}


/************** antiquote ******************/

sym mkantiquote(PPgexpr)
 tree PPgexpr;
{
	register struct Santiquote *pp =
		(struct Santiquote *) malloc(sizeof(struct Santiquote));
	pp -> tag = antiquote;
	pp -> Xgexpr = PPgexpr;
	return((sym)pp);
}

tree *Rgexpr(t)
 struct Santiquote *t;
{
	if(t -> tag != antiquote)
		printf("gexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgexpr);
}

/************** gramterm ******************/

sym mkgramterm(PPgcharacter)
 char PPgcharacter;
{
	register struct Sgramterm *pp =
		(struct Sgramterm *) malloc(sizeof(struct Sgramterm));
	pp -> tag = gramterm;
	pp -> Xgcharacter = PPgcharacter;
	return((sym)pp);
}

char *Rgcharacter(t)
 struct Sgramterm *t;
{
	if(t -> tag != gramterm)
		printf("gcharacter: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcharacter);
}

/************** gramintterm ******************/

sym mkgramintterm(PPgcint)
 int PPgcint;
{
	register struct Sgramintterm *pp =
		(struct Sgramintterm *) malloc(sizeof(struct Sgramintterm));
	pp -> tag = gramintterm;
	pp -> Xgcint = PPgcint;
	return((sym)pp);
}

int *Rgcint(t)
 struct Sgramintterm *t;
{
	if(t -> tag != gramintterm)
		printf("gcint: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcint);
}

/************** gramidterm ******************/

sym mkgramidterm(PPgcid)
 id PPgcid;
{
	register struct Sgramidterm *pp =
		(struct Sgramidterm *) malloc(sizeof(struct Sgramidterm));
	pp -> tag = gramidterm;
	pp -> Xgcid = PPgcid;
	return((sym)pp);
}

id *Rgcid(t)
 struct Sgramidterm *t;
{
	if(t -> tag != gramidterm)
		printf("gcid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcid);
}

/************** gramsymterm ******************/

sym mkgramsymterm(PPgcsym)
 id PPgcsym;
{
	register struct Sgramsymterm *pp =
		(struct Sgramsymterm *) malloc(sizeof(struct Sgramsymterm));
	pp -> tag = gramsymterm;
	pp -> Xgcsym = PPgcsym;
	return((sym)pp);
}

id *Rgcsym(t)
 struct Sgramsymterm *t;
{
	if(t -> tag != gramsymterm)
		printf("gcsym: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcsym);
}

/************** expterm ******************/

sym mkexpterm(PPgcharacter2)
 char PPgcharacter2;
{
	register struct Sexpterm *pp =
		(struct Sexpterm *) malloc(sizeof(struct Sexpterm));
	pp -> tag = expterm;
	pp -> Xgcharacter2 = PPgcharacter2;
	return((sym)pp);
}

char *Rgcharacter2(t)
 struct Sexpterm *t;
{
	if(t -> tag != expterm)
		printf("gcharacter2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcharacter2);
}

/************** expintterm ******************/

sym mkexpintterm(PPgcint2)
 int PPgcint2;
{
	register struct Sexpintterm *pp =
		(struct Sexpintterm *) malloc(sizeof(struct Sexpintterm));
	pp -> tag = expintterm;
	pp -> Xgcint2 = PPgcint2;
	return((sym)pp);
}

int *Rgcint2(t)
 struct Sexpintterm *t;
{
	if(t -> tag != expintterm)
		printf("gcint2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcint2);
}

/************** expidterm ******************/

sym mkexpidterm(PPgcid2)
 id PPgcid2;
{
	register struct Sexpidterm *pp =
		(struct Sexpidterm *) malloc(sizeof(struct Sexpidterm));
	pp -> tag = expidterm;
	pp -> Xgcid2 = PPgcid2;
	return((sym)pp);
}

id *Rgcid2(t)
 struct Sexpidterm *t;
{
	if(t -> tag != expidterm)
		printf("gcid2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcid2);
}

/************** expsymterm ******************/

sym mkexpsymterm(PPgcsym2)
 id PPgcsym2;
{
	register struct Sexpsymterm *pp =
		(struct Sexpsymterm *) malloc(sizeof(struct Sexpsymterm));
	pp -> tag = expsymterm;
	pp -> Xgcsym2 = PPgcsym2;
	return((sym)pp);
}

id *Rgcsym2(t)
 struct Sexpsymterm *t;
{
	if(t -> tag != expsymterm)
		printf("gcsym2: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgcsym2);
}

/************** nonterminal ******************/

sym mknonterminal(PPgtype)
 ttype PPgtype;
{
	register struct Snonterminal *pp =
		(struct Snonterminal *) malloc(sizeof(struct Snonterminal));
	pp -> tag = nonterminal;
	pp -> Xgtype = PPgtype;
	return((sym)pp);
}

ttype *Rgtype(t)
 struct Snonterminal *t;
{
	if(t -> tag != nonterminal)
		printf("gtype: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtype);
}

/************** list1 ******************/

sym mklist1(PPgtypel1, PPgterms1)
 ttype PPgtypel1;
 list PPgterms1;
{
	register struct Slist1 *pp =
		(struct Slist1 *) malloc(sizeof(struct Slist1));
	pp -> tag = list1;
	pp -> Xgtypel1 = PPgtypel1;
	pp -> Xgterms1 = PPgterms1;
	return((sym)pp);
}

ttype *Rgtypel1(t)
 struct Slist1 *t;
{
	if(t -> tag != list1)
		printf("gtypel1: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypel1);
}

list *Rgterms1(t)
 struct Slist1 *t;
{
	if(t -> tag != list1)
		printf("gterms1: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgterms1);
}

/************** list0 ******************/

sym mklist0(PPgtypel0, PPgterms0)
 ttype PPgtypel0;
 list PPgterms0;
{
	register struct Slist0 *pp =
		(struct Slist0 *) malloc(sizeof(struct Slist0));
	pp -> tag = list0;
	pp -> Xgtypel0 = PPgtypel0;
	pp -> Xgterms0 = PPgterms0;
	return((sym)pp);
}

ttype *Rgtypel0(t)
 struct Slist0 *t;
{
	if(t -> tag != list0)
		printf("gtypel0: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgtypel0);
}

list *Rgterms0(t)
 struct Slist0 *t;
{
	if(t -> tag != list0)
		printf("gterms0: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgterms0);
}
