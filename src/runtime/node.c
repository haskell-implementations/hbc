#include <varargs.h>

#define mkconstr __mkconstr
#include "runtime.h"
#include "vars.h"
#include "../mcode/limit.h"
#include "tagtable.h"

#undef mkconstr

#ifdef HPROFILE
#include "sample.h"
#ifdef SLOP2
#define HPROF *hp++ = SLOP1; *hp++ = SLOP2
#else
#define HPROF *hp++ = SLOP1
#endif /* SLOP2 */
#else
#define HPROF 
#endif /* HPROFILE */


extern Tag 	INT, SFLOAT, DFLOAT, CHAR, TAG, TAG0, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4,
		STRING, VEK, DVEK, CAP, MARKED, MOVED, AP, FUN, VAP,
                MARKED, MOVED,
                AP, FUN, VAP, APG, VAPG,
                BIGNUM,
		HOLE, INPUT, INPUTD, GCRET, ZAP, INDIR;

static Tag *pairtag[] = {
    &PAIR0, &PAIR1, &PAIR2, &PAIR3, &PAIR4,
};

PTR 
mkconstr(va_alist)
va_dcl
{
    va_list ap;
    PTR r;
    int i;
    Int cno, npart;

    va_start(ap);
    cno = va_arg(ap, Int);
    npart = va_arg(ap, Int);
    r = (PTR)hp;
    if (npart == 0) {
	r->tag = &TAG0;
	r->node20.val0 = cno;
	INCHP(sizeof(Node20));
	HPROF;
    } else if (npart == 1) {
	r->tag = &TAG;
	r->node21.val0 = cno;
	r->node21.ptr0 = va_arg(ap, PTR);
	INCHP(sizeof(Node21));
	HPROF;
    } else {
	PTR *last = 0;
	if (cno < MAXPAIR) {
	    r->tag = pairtag[cno];
	    if (npart == 2) {
		r->node12.ptr0 = va_arg(ap, PTR);
		r->node12.ptr1 = va_arg(ap, PTR);
		INCHP(sizeof(Node12));
		HPROF;
		npart -= 2;
	    } else if (npart == 3) {
		PTR n;
		INCHP(sizeof(Node12));
		HPROF;
		n = (PTR)hp;
		r->node12.ptr0 = n;
		n->tag = &PAIR;
		n->node12.ptr0 = va_arg(ap, PTR);
		n->node12.ptr1 = va_arg(ap, PTR);
		r->node12.ptr1 = va_arg(ap, PTR);
		INCHP(sizeof(Node12));
		HPROF;
		npart -= 3;
	    } else {
		INCHP(sizeof(Node12));
		HPROF;
		r->node12.ptr0 = (PTR)hp;
		last = &r->node12.ptr1;
		npart--;
	    }
	} else {
	    r->tag = &TAG;
	    r->node21.val0 = cno;
	    INCHP(sizeof(Node21));
	    HPROF;
	    r->node21.ptr0 = (PTR)hp;
	    last = 0;
	}
	if (npart) {
	    PTR n;
	    n = (PTR)hp;
	    n->tag = &VEK;
	    n->nodevek.size = npart;
	    for(i = 0; i < npart; i++) {
		n->nodevek.ptr[i] = va_arg(ap, PTR);
	    }
	    INCHP(sizeof(Nodevek) + sizeof(PTR) * (npart-1));
	    HPROF;
	    if (last) {
		*last = va_arg(ap, PTR);
	    }
	}
    }
    va_end(ap);
    return r;
}

PTR
mknode1(tag, i)
Tag *tag;
Int i;
{
    PTR r;
    r = (PTR)hp;
    r->tag = tag;
    r->node20.val0 = i;
    INCHP(sizeof(Node20));
    HPROF;
    return r;
}

PTR
mknode1p(tag, p)
Tag *tag;
PTR p;
{
    PTR r;
    r = (PTR)hp;
    r->tag = tag;
    r->node11.ptr0 = p;
    INCHP(sizeof(Node11));
    HPROF;
    return r;
}

PTR
mknode2(tag, p, q)
Tag *tag;
Int p, q;
{
    PTR r;
    r = (PTR)hp;
    r->tag = tag;
    r->node30.val0 = p;
    r->node30.val1 = q;
    INCHP(sizeof(Node30));
    HPROF;
    return r;
}

PTR
mknode11(tag, p, q)
Tag *tag;
Int p;
PTR q;
{
    PTR r;
    r = (PTR)hp;
    r->tag = tag;
    r->node21.val0 = p;
    r->node21.ptr0 = q;
    INCHP(sizeof(Node21));
    HPROF;
    return r;
}

PTR
mknode2p(tag, p, q)
Tag *tag;
PTR p, q;
{
    PTR r;
    r = (PTR)hp;
    r->tag = tag;
    r->node12.ptr0 = p;
    r->node12.ptr1 = q;
    INCHP(sizeof(Node12));
    HPROF;
    return r;
}

PTR 
mknil()
{
    return mkconstr(0, 0);
}

PTR 
mkbool(b)
int b;
{
    return mkconstr(b != 0, 0);
}

PTR
mkcons(h, t)
PTR h, t;
{
    return mkconstr(1, 2, h, t);
}

PTR
mkpair(h, t)
PTR h, t;
{
    return mkconstr(0, 2, h, t);
}

PTR
mkap(f, a)
PTR f, a;
{
    PTR r;
    r = (PTR)hp;
    r->tag = &AP;
    r->node12.ptr0 = f;
    r->node12.ptr1 = a;
    INCHP(sizeof(Node12));
    HPROF;
    return r;
}

PTR
mkint(i)
Int i;
{
    return mknode1(&INT, i);
}

PTR
mkchar(i)
Int i;
{
#ifdef HPROFILE
    extern struct { Node20 n20; 
		    struct hprofinfo *pi; 
		    int slop2;	/* present even if SLOP2 is undefined! */
		} cchartab[];
#else
    extern Node20 cchartab[];
#endif
    if (0 <= i && i <= 255)
	return (PTR)&cchartab[i];
    else
	return mknode1(&CHAR, i);
}

PTR
mkno(p)
PTR p;
{
    return mkconstr(0, 1, p);
}

PTR
mkyes(p)
PTR p;
{
    return mkconstr(1, 1, p);
}

PTR
mkcstring(s)
/*const*/ char *s;
{
    return mknode2(&STRING, (Int)s, 0);
}

PTR
mkestring(as)
char *as;
{
    unsigned char *s = (unsigned char *)as;
    PTR p;
    int l = strlen((char *)s);

    for(s += l, p = mknil(); l > 0; l--) {
	if (hp > ehp) {
	    *--ep = p;
	    dogc();
	    p = *ep++;
	}
	p = mkcons(mkchar(*--s), p);
    }
    return p;
}

void
update(dst, src)
PTR dst, src;
{
/*    extern int useindir;*/
    if (0 /*useindir*/) {
    } else {
	dst->tag = src->tag;
	dst->node12.ptr0 = src->node12.ptr0;
	dst->node12.ptr1 = src->node12.ptr1;
#ifdef HPROFILE
	{
	    struct hnode12 { 
		Node12 n12;
		struct hprofinfo *hpi;
#ifdef SLOP2
		int slop2;
#endif
	    } *ndst, *nsrc;
	    ndst = (struct hnode12*)dst;
	    nsrc = (struct hnode12*)src;
	    ndst->hpi = nsrc->hpi;
#ifdef SLOP2
	    ndst->slop2 = nsrc->slop2;
#endif
	}
#endif /* HPROFILE */
    }
}

PTR
mkdouble(d)
double d;
{
#if IntSize == 8
    union { double d; Int i; } u;
    u.d = d;
    return mknode1(&DFLOAT, u.i);
#else
    union { double d; int i[2]; } u;
    u.d = d;
    return mknode2(&DFLOAT, u.i[0], u.i[1]);
#endif
}

double
getdouble(p)
PTR p;
{
#if IntSize == 8
    union { double d; Int i; } u;
    u.i = p->node20.val0;
#else
    union { double d; int i[2]; } u;
    u.i[0] = p->node30.val0;
    u.i[1] = p->node30.val1;
#endif
    return u.d;
}

PTR
mkfloat(d)
double d;
{
    union { float d; Int i; } u;
    u.d = (float)d;
    return mknode1(&SFLOAT, u.i);
}

double
getfloat(p)
PTR p;
{
    union { float d; Int i; } u;
    u.i = p->node20.val0;
    return (double)u.d;
}

int
getcno(p)
PTR p;
{
    if (p->tag->tagNumber == O_PAIR0) return 0;
    if (p->tag->tagNumber == O_PAIR1) return 1;
    if (p->tag->tagNumber == O_PAIR2) return 2;
    if (p->tag->tagNumber == O_PAIR3) return 3;
    if (p->tag->tagNumber == O_PAIR4) return 4;
    return p->node20.val0;
}

#define ISPAIR(p) ((p)->tag->tagNumber == O_PAIR0 || \
                   (p)->tag->tagNumber == O_PAIR1 || \
                   (p)->tag->tagNumber == O_PAIR2 || \
                   (p)->tag->tagNumber == O_PAIR3 || \
                   (p)->tag->tagNumber == O_PAIR4)

PTR
getpart(p, n, max)
PTR p;
int n, max;
{
    if (n >= max || max < 1) {
    bad:
	fprintf(stderr, "Bad arg to getpart\n");
	abort();
    }
    switch (max) {
    case 1:			/* has to be a (TAG _) */
	return p->node21.ptr0;	/* n has to be 0 */
    case 2:			/* (PAIRn _ _) or TAG (PAIR _ _) */
	if (p->tag->tagNumber == O_TAG) {
	    p = p->node21.ptr0;
	    if (p->tag->tagNumber != O_PAIR)
		goto bad;
	} else if (!ISPAIR(p))
	    goto bad;
	return n == 0 ? p->node12.ptr0 : p->node12.ptr1; /* n has to be 0 or 1 */		
    case 3:			/* (PAIRn (PAIR _ _) _) or (TAG (VEK ...)) */
	if (ISPAIR(p)) {
	    if (n == 2) {
		return p->node12.ptr1;
	    } else {
		p = p->node12.ptr0;
		if (p->tag->tagNumber != O_PAIR)
		    goto bad;
		return n == 0 ? p->node12.ptr0 : p->node12.ptr1; /* n has to be 0 or 1 */
	    }
	}
	/* fall into */
    default:			/* (PAIRn (VEK ...) _) or (TAG (VEK ...)) */
	if (p->tag->tagNumber == O_TAG) {
	    p = p->node21.ptr0;
	    if (p->tag->tagNumber != O_VEK)
		goto bad;
	    return p->nodevek.ptr[n];
	}
	if (!ISPAIR(p))
	    goto bad;
	if (n == max-1) {
	    return p->node12.ptr1;
	} else {
	    p = p->node12.ptr0;
	    if (p->tag->tagNumber != O_VEK)
		goto bad;
	    return p->nodevek.ptr[n];
	}
    }
}

PTR
evaluate(p)
PTR p;
{
    extern Tag CANON;

    if (p->tag >= &CANON)
	return p;
    *--ep = p;
    eval();
    return *ep++;
}

PTR
evalargs(p, n)
PTR p;
int n;
{
    int i;

    *--ep = p;
    for(i = 0; i < n; i++)
	(void)evaluate(getpart(*ep, i, n));
    return *ep++;
}

int
evalstring(s, acp, n)
PTR s;
char *acp;
int n;
{
    char *cp = acp;
    --n;
    s = evaluate(s);
    while(!ISNIL(s)) {
	*--ep = s;		/* save in case of GC */
	(void)evaluate(HDOF(s));
	s = *ep++;		/* restore */
	if (--n > 0)
	    *cp++ = CHAROF(HDOF(s));
	s = evaluate(TLOF(s));	/* eval tail */
    }
    *cp = '\0';
    return cp - acp;
}

PTR
forceeval(p)
PTR p;
{
    *--ep = p;
    forceit();
    return *ep++;
}

PTR
mkarray(low, count)
int low, count;
{
    PTR a, r;

    a = (PTR)hp;
    INCHP(sizeof(Node21));
    HPROF;
    a->tag = &TAG;
    a->node21.val0 = low;
    a->node21.ptr0 = r = (PTR)hp;
    r->tag = &VEK;
    r->nodevek.size = count;
    INCHP(sizeof(Nodevek) + (count-1)*sizeof(PTR));
    HPROF;
    return a;
}

PTR *
arrayref(arr, i)
PTR arr;
int i;
{
    return &arr->node21.ptr0->nodevek.ptr[i];
}

void
inchp(n)
int n;
{
    INCHP(n);
}

void
getbytevector(ptr, s_return, n_return)
/* getPackedString returns the pointer to and the length of the character
 * array of a PackedString (ByteVector).
 * Note! The pointer returned points into the heap, but is not a PTR, so
 *       it will NOT survive a garbage collection.
 */
PTR ptr;
char **s_return;
unsigned *n_return;
{
    PTR ixp = getpart(ptr,0,2);
    PTR vecp = getpart(ptr,1,2);
    int ixstart = ixp->node2int.val0;
    int ixend = ixp->node2int.val1;
    char *v = (char *)vecp->nodedvek.val;
    *s_return = v+ixstart;
    *n_return = ixend-ixstart;
}

PTR
mkbytevector(s, n)
char *s;
unsigned n;
{
    PTR ixp = mkdouble(0.0);	/* dummy double */
    PTR vecp = mkarray(0, (n + sizeof(Int) - 1) / sizeof(Int));

    ixp->node2int.val0 = 0;	/* set real string size */
    ixp->node2int.val1 = n;
    return mkpair(ixp, vecp);
}
