

#include "include.h"
#include "icmd.h"
struct SIexpr {
	Ticmd tag;
	tree XgIexpr;
};

struct SIbinding {
	Ticmd tag;
	list XgIbinding;
};

struct SItload {
	Ticmd tag;
	list XgItloads;
};

struct SIcload {
	Ticmd tag;
	list XgIcloadnames;
	tree XgIcloadimp;
};

struct SImload {
	Ticmd tag;
	id XgImloadfile;
	tree XgImloadtree;
};

struct SImsg {
	Ticmd tag;
	tree XgImsg;
};

struct SImsgnp {
	Ticmd tag;
	tree XgImsgnp;
};

struct SInull {
	Ticmd tag;
};

struct SIwhatis {
	Ticmd tag;
	id XgIwhatid;
};

struct SIhelp {
	Ticmd tag;
};

struct SIshow_ {
	Ticmd tag;
	id XgIshow_;
};

struct SImbind {
	Ticmd tag;
	tree XgImbpat;
	tree XgImbexp;
};

Ticmd ticmd(t)
 icmd t;
{
	return(t -> tag);
}


/************** Iexpr ******************/

icmd mkIexpr(PPgIexpr)
 tree PPgIexpr;
{
	register struct SIexpr *pp =
		(struct SIexpr *) malloc(sizeof(struct SIexpr));
	pp -> tag = Iexpr;
	pp -> XgIexpr = PPgIexpr;
	return((icmd)pp);
}

tree *RgIexpr(t)
 struct SIexpr *t;
{
	if(t -> tag != Iexpr)
		printf("gIexpr: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIexpr);
}

/************** Ibinding ******************/

icmd mkIbinding(PPgIbinding)
 list PPgIbinding;
{
	register struct SIbinding *pp =
		(struct SIbinding *) malloc(sizeof(struct SIbinding));
	pp -> tag = Ibinding;
	pp -> XgIbinding = PPgIbinding;
	return((icmd)pp);
}

list *RgIbinding(t)
 struct SIbinding *t;
{
	if(t -> tag != Ibinding)
		printf("gIbinding: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIbinding);
}

/************** Itload ******************/

icmd mkItload(PPgItloads)
 list PPgItloads;
{
	register struct SItload *pp =
		(struct SItload *) malloc(sizeof(struct SItload));
	pp -> tag = Itload;
	pp -> XgItloads = PPgItloads;
	return((icmd)pp);
}

list *RgItloads(t)
 struct SItload *t;
{
	if(t -> tag != Itload)
		printf("gItloads: illegal selection; was %d\n", t -> tag);
	return(& t -> XgItloads);
}

/************** Icload ******************/

icmd mkIcload(PPgIcloadnames, PPgIcloadimp)
 list PPgIcloadnames;
 tree PPgIcloadimp;
{
	register struct SIcload *pp =
		(struct SIcload *) malloc(sizeof(struct SIcload));
	pp -> tag = Icload;
	pp -> XgIcloadnames = PPgIcloadnames;
	pp -> XgIcloadimp = PPgIcloadimp;
	return((icmd)pp);
}

list *RgIcloadnames(t)
 struct SIcload *t;
{
	if(t -> tag != Icload)
		printf("gIcloadnames: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIcloadnames);
}

tree *RgIcloadimp(t)
 struct SIcload *t;
{
	if(t -> tag != Icload)
		printf("gIcloadimp: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIcloadimp);
}

/************** Imload ******************/

icmd mkImload(PPgImloadfile, PPgImloadtree)
 id PPgImloadfile;
 tree PPgImloadtree;
{
	register struct SImload *pp =
		(struct SImload *) malloc(sizeof(struct SImload));
	pp -> tag = Imload;
	pp -> XgImloadfile = PPgImloadfile;
	pp -> XgImloadtree = PPgImloadtree;
	return((icmd)pp);
}

id *RgImloadfile(t)
 struct SImload *t;
{
	if(t -> tag != Imload)
		printf("gImloadfile: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImloadfile);
}

tree *RgImloadtree(t)
 struct SImload *t;
{
	if(t -> tag != Imload)
		printf("gImloadtree: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImloadtree);
}

/************** Imsg ******************/

icmd mkImsg(PPgImsg)
 tree PPgImsg;
{
	register struct SImsg *pp =
		(struct SImsg *) malloc(sizeof(struct SImsg));
	pp -> tag = Imsg;
	pp -> XgImsg = PPgImsg;
	return((icmd)pp);
}

tree *RgImsg(t)
 struct SImsg *t;
{
	if(t -> tag != Imsg)
		printf("gImsg: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImsg);
}

/************** Imsgnp ******************/

icmd mkImsgnp(PPgImsgnp)
 tree PPgImsgnp;
{
	register struct SImsgnp *pp =
		(struct SImsgnp *) malloc(sizeof(struct SImsgnp));
	pp -> tag = Imsgnp;
	pp -> XgImsgnp = PPgImsgnp;
	return((icmd)pp);
}

tree *RgImsgnp(t)
 struct SImsgnp *t;
{
	if(t -> tag != Imsgnp)
		printf("gImsgnp: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImsgnp);
}

/************** Inull ******************/

icmd mkInull()
{
	register struct SInull *pp =
		(struct SInull *) malloc(sizeof(struct SInull));
	pp -> tag = Inull;
	return((icmd)pp);
}

/************** Iwhatis ******************/

icmd mkIwhatis(PPgIwhatid)
 id PPgIwhatid;
{
	register struct SIwhatis *pp =
		(struct SIwhatis *) malloc(sizeof(struct SIwhatis));
	pp -> tag = Iwhatis;
	pp -> XgIwhatid = PPgIwhatid;
	return((icmd)pp);
}

id *RgIwhatid(t)
 struct SIwhatis *t;
{
	if(t -> tag != Iwhatis)
		printf("gIwhatid: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIwhatid);
}

/************** Ihelp ******************/

icmd mkIhelp()
{
	register struct SIhelp *pp =
		(struct SIhelp *) malloc(sizeof(struct SIhelp));
	pp -> tag = Ihelp;
	return((icmd)pp);
}

/************** Ishow_ ******************/

icmd mkIshow_(PPgIshow_)
 id PPgIshow_;
{
	register struct SIshow_ *pp =
		(struct SIshow_ *) malloc(sizeof(struct SIshow_));
	pp -> tag = Ishow_;
	pp -> XgIshow_ = PPgIshow_;
	return((icmd)pp);
}

id *RgIshow_(t)
 struct SIshow_ *t;
{
	if(t -> tag != Ishow_)
		printf("gIshow_: illegal selection; was %d\n", t -> tag);
	return(& t -> XgIshow_);
}

/************** Imbind ******************/

icmd mkImbind(PPgImbpat, PPgImbexp)
 tree PPgImbpat;
 tree PPgImbexp;
{
	register struct SImbind *pp =
		(struct SImbind *) malloc(sizeof(struct SImbind));
	pp -> tag = Imbind;
	pp -> XgImbpat = PPgImbpat;
	pp -> XgImbexp = PPgImbexp;
	return((icmd)pp);
}

tree *RgImbpat(t)
 struct SImbind *t;
{
	if(t -> tag != Imbind)
		printf("gImbpat: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImbpat);
}

tree *RgImbexp(t)
 struct SImbind *t;
{
	if(t -> tag != Imbind)
		printf("gImbexp: illegal selection; was %d\n", t -> tag);
	return(& t -> XgImbexp);
}
