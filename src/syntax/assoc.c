

#include "include.h"
#include "assoc.h"
struct Sleftassoc {
	Tassoc tag;
};

struct Srightassoc {
	Tassoc tag;
};

struct Snonassoc {
	Tassoc tag;
};

struct Sbothassoc {
	Tassoc tag;
};

Tassoc tassoc(t)
 assoc t;
{
	return(t -> tag);
}


/************** leftassoc ******************/

assoc mkleftassoc()
{
	register struct Sleftassoc *pp =
		(struct Sleftassoc *) malloc(sizeof(struct Sleftassoc));
	pp -> tag = leftassoc;
	return((assoc)pp);
}

/************** rightassoc ******************/

assoc mkrightassoc()
{
	register struct Srightassoc *pp =
		(struct Srightassoc *) malloc(sizeof(struct Srightassoc));
	pp -> tag = rightassoc;
	return((assoc)pp);
}

/************** nonassoc ******************/

assoc mknonassoc()
{
	register struct Snonassoc *pp =
		(struct Snonassoc *) malloc(sizeof(struct Snonassoc));
	pp -> tag = nonassoc;
	return((assoc)pp);
}

/************** bothassoc ******************/

assoc mkbothassoc()
{
	register struct Sbothassoc *pp =
		(struct Sbothassoc *) malloc(sizeof(struct Sbothassoc));
	pp -> tag = bothassoc;
	return((assoc)pp);
}
