

#include "include.h"
#include "stmt.h"
struct Sstmtexp {
	Tstmt tag;
	tree Xgseexp;
};

struct Sstmtexpstmt {
	Tstmt tag;
	tree Xgsesexp;
	stmt Xgsesstmt;
};

struct Sstmtlet {
	Tstmt tag;
	binding Xgslbind;
	stmt Xgslstmt;
};

struct Sstmtbind {
	Tstmt tag;
	tree Xgsbpat;
	tree Xgsbexp;
	stmt Xgsbstmt;
};

Tstmt tstmt(t)
 stmt t;
{
	return(t -> tag);
}


/************** stmtexp ******************/

stmt mkstmtexp(PPgseexp)
 tree PPgseexp;
{
	register struct Sstmtexp *pp =
		(struct Sstmtexp *) malloc(sizeof(struct Sstmtexp));
	pp -> tag = stmtexp;
	pp -> Xgseexp = PPgseexp;
	return((stmt)pp);
}

tree *Rgseexp(t)
 struct Sstmtexp *t;
{
	if(t -> tag != stmtexp)
		printf("gseexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgseexp);
}

/************** stmtexpstmt ******************/

stmt mkstmtexpstmt(PPgsesexp, PPgsesstmt)
 tree PPgsesexp;
 stmt PPgsesstmt;
{
	register struct Sstmtexpstmt *pp =
		(struct Sstmtexpstmt *) malloc(sizeof(struct Sstmtexpstmt));
	pp -> tag = stmtexpstmt;
	pp -> Xgsesexp = PPgsesexp;
	pp -> Xgsesstmt = PPgsesstmt;
	return((stmt)pp);
}

tree *Rgsesexp(t)
 struct Sstmtexpstmt *t;
{
	if(t -> tag != stmtexpstmt)
		printf("gsesexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsesexp);
}

stmt *Rgsesstmt(t)
 struct Sstmtexpstmt *t;
{
	if(t -> tag != stmtexpstmt)
		printf("gsesstmt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsesstmt);
}

/************** stmtlet ******************/

stmt mkstmtlet(PPgslbind, PPgslstmt)
 binding PPgslbind;
 stmt PPgslstmt;
{
	register struct Sstmtlet *pp =
		(struct Sstmtlet *) malloc(sizeof(struct Sstmtlet));
	pp -> tag = stmtlet;
	pp -> Xgslbind = PPgslbind;
	pp -> Xgslstmt = PPgslstmt;
	return((stmt)pp);
}

binding *Rgslbind(t)
 struct Sstmtlet *t;
{
	if(t -> tag != stmtlet)
		printf("gslbind: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgslbind);
}

stmt *Rgslstmt(t)
 struct Sstmtlet *t;
{
	if(t -> tag != stmtlet)
		printf("gslstmt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgslstmt);
}

/************** stmtbind ******************/

stmt mkstmtbind(PPgsbpat, PPgsbexp, PPgsbstmt)
 tree PPgsbpat;
 tree PPgsbexp;
 stmt PPgsbstmt;
{
	register struct Sstmtbind *pp =
		(struct Sstmtbind *) malloc(sizeof(struct Sstmtbind));
	pp -> tag = stmtbind;
	pp -> Xgsbpat = PPgsbpat;
	pp -> Xgsbexp = PPgsbexp;
	pp -> Xgsbstmt = PPgsbstmt;
	return((stmt)pp);
}

tree *Rgsbpat(t)
 struct Sstmtbind *t;
{
	if(t -> tag != stmtbind)
		printf("gsbpat: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsbpat);
}

tree *Rgsbexp(t)
 struct Sstmtbind *t;
{
	if(t -> tag != stmtbind)
		printf("gsbexp: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsbexp);
}

stmt *Rgsbstmt(t)
 struct Sstmtbind *t;
{
	if(t -> tag != stmtbind)
		printf("gsbstmt: illegal selection; was %d\n", t -> tag);
	return(& t -> Xgsbstmt);
}
