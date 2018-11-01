

#include "include.h"
#include "import.h"
struct Simport {
	Timpstuff tag;
	id Xgimodid;
	list Xgiimps;
	list Xgifixes;
	list Xgients;
	impstuff Xgispec;
	list Xgirename;
	int Xg3qual;
	list Xg3as;
};

struct Sispec {
	Timpstuff tag;
	int Xgiexpose;
	list Xgiids;
};

struct Sirename {
	Timpstuff tag;
	id Xgirensrc;
	id Xgirendst;
};

struct Sifix {
	Timpstuff tag;
	list Xgifixids;
	int Xgifixass;
	int Xgifixprec;
};

struct Sinone {
	Timpstuff tag;
};

struct Sisome {
	Timpstuff tag;
	list Xgisome;
};

struct Sinterface {
	Timpstuff tag;
	id Xgiimodid;
	list Xgiiimps;
	list Xgiifixes;
	list Xgiients;
};

struct Sitypinfo {
	Timpstuff tag;
	int Xgincon;
	int Xgiflat;
};

Timpstuff timpstuff(t)
 impstuff t;
{
	return(t -> tag);
}


/************** import ******************/

impstuff mkimport(PPgimodid, PPgiimps, PPgifixes, PPgients, PPgispec, PPgirename, PPg3qual, PPg3as)
 id PPgimodid;
 list PPgiimps;
 list PPgifixes;
 list PPgients;
 impstuff PPgispec;
 list PPgirename;
 int PPg3qual;
 list PPg3as;
{
	register struct Simport *pp =
		(struct Simport *) malloc(sizeof(struct Simport));
	pp -> tag = import;
	pp -> Xgimodid = PPgimodid;
	pp -> Xgiimps = PPgiimps;
	pp -> Xgifixes = PPgifixes;
	pp -> Xgients = PPgients;
	pp -> Xgispec = PPgispec;
	pp -> Xgirename = PPgirename;
	pp -> Xg3qual = PPg3qual;
	pp -> Xg3as = PPg3as;
	return((impstuff)pp);
}

id *Rgimodid(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("gimodid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgimodid);
}

list *Rgiimps(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("giimps: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiimps);
}

list *Rgifixes(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("gifixes: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgifixes);
}

list *Rgients(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("gients: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgients);
}

impstuff *Rgispec(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("gispec: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgispec);
}

list *Rgirename(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("girename: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgirename);
}

int *Rg3qual(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("g3qual: illegal selection; was %d\n", t -> tag);
	return(& t -> Xg3qual);
}

list *Rg3as(t)
 struct Simport *t;
{
	if(t -> tag != import)
		printf("g3as: illegal selection; was %d\n", t -> tag);
	return(& t -> Xg3as);
}

/************** ispec ******************/

impstuff mkispec(PPgiexpose, PPgiids)
 int PPgiexpose;
 list PPgiids;
{
	register struct Sispec *pp =
		(struct Sispec *) malloc(sizeof(struct Sispec));
	pp -> tag = ispec;
	pp -> Xgiexpose = PPgiexpose;
	pp -> Xgiids = PPgiids;
	return((impstuff)pp);
}

int *Rgiexpose(t)
 struct Sispec *t;
{
	if(t -> tag != ispec)
		printf("giexpose: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiexpose);
}

list *Rgiids(t)
 struct Sispec *t;
{
	if(t -> tag != ispec)
		printf("giids: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiids);
}

/************** irename ******************/

impstuff mkirename(PPgirensrc, PPgirendst)
 id PPgirensrc;
 id PPgirendst;
{
	register struct Sirename *pp =
		(struct Sirename *) malloc(sizeof(struct Sirename));
	pp -> tag = irename;
	pp -> Xgirensrc = PPgirensrc;
	pp -> Xgirendst = PPgirendst;
	return((impstuff)pp);
}

id *Rgirensrc(t)
 struct Sirename *t;
{
	if(t -> tag != irename)
		printf("girensrc: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgirensrc);
}

id *Rgirendst(t)
 struct Sirename *t;
{
	if(t -> tag != irename)
		printf("girendst: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgirendst);
}

/************** ifix ******************/

impstuff mkifix(PPgifixids, PPgifixass, PPgifixprec)
 list PPgifixids;
 int PPgifixass;
 int PPgifixprec;
{
	register struct Sifix *pp =
		(struct Sifix *) malloc(sizeof(struct Sifix));
	pp -> tag = ifix;
	pp -> Xgifixids = PPgifixids;
	pp -> Xgifixass = PPgifixass;
	pp -> Xgifixprec = PPgifixprec;
	return((impstuff)pp);
}

list *Rgifixids(t)
 struct Sifix *t;
{
	if(t -> tag != ifix)
		printf("gifixids: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgifixids);
}

int *Rgifixass(t)
 struct Sifix *t;
{
	if(t -> tag != ifix)
		printf("gifixass: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgifixass);
}

int *Rgifixprec(t)
 struct Sifix *t;
{
	if(t -> tag != ifix)
		printf("gifixprec: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgifixprec);
}

/************** inone ******************/

impstuff mkinone()
{
	register struct Sinone *pp =
		(struct Sinone *) malloc(sizeof(struct Sinone));
	pp -> tag = inone;
	return((impstuff)pp);
}

/************** isome ******************/

impstuff mkisome(PPgisome)
 list PPgisome;
{
	register struct Sisome *pp =
		(struct Sisome *) malloc(sizeof(struct Sisome));
	pp -> tag = isome;
	pp -> Xgisome = PPgisome;
	return((impstuff)pp);
}

list *Rgisome(t)
 struct Sisome *t;
{
	if(t -> tag != isome)
		printf("gisome: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgisome);
}

/************** interface ******************/

impstuff mkinterface(PPgiimodid, PPgiiimps, PPgiifixes, PPgiients)
 id PPgiimodid;
 list PPgiiimps;
 list PPgiifixes;
 list PPgiients;
{
	register struct Sinterface *pp =
		(struct Sinterface *) malloc(sizeof(struct Sinterface));
	pp -> tag = interface;
	pp -> Xgiimodid = PPgiimodid;
	pp -> Xgiiimps = PPgiiimps;
	pp -> Xgiifixes = PPgiifixes;
	pp -> Xgiients = PPgiients;
	return((impstuff)pp);
}

id *Rgiimodid(t)
 struct Sinterface *t;
{
	if(t -> tag != interface)
		printf("giimodid: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiimodid);
}

list *Rgiiimps(t)
 struct Sinterface *t;
{
	if(t -> tag != interface)
		printf("giiimps: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiiimps);
}

list *Rgiifixes(t)
 struct Sinterface *t;
{
	if(t -> tag != interface)
		printf("giifixes: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiifixes);
}

list *Rgiients(t)
 struct Sinterface *t;
{
	if(t -> tag != interface)
		printf("giients: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiients);
}

/************** itypinfo ******************/

impstuff mkitypinfo(PPgincon, PPgiflat)
 int PPgincon;
 int PPgiflat;
{
	register struct Sitypinfo *pp =
		(struct Sitypinfo *) malloc(sizeof(struct Sitypinfo));
	pp -> tag = itypinfo;
	pp -> Xgincon = PPgincon;
	pp -> Xgiflat = PPgiflat;
	return((impstuff)pp);
}

int *Rgincon(t)
 struct Sitypinfo *t;
{
	if(t -> tag != itypinfo)
		printf("gincon: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgincon);
}

int *Rgiflat(t)
 struct Sitypinfo *t;
{
	if(t -> tag != itypinfo)
		printf("giflat: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgiflat);
}
