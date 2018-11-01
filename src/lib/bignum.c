#include <stdio.h>
#include <stdlib.h>

/*
** Bignum representation.
** BIGNUM -----> DVEK n bigit ...
**
*/

#include "../runtime/node.h"
extern void noheapleft();
#ifndef mips
extern int fputs();
#endif

#ifdef HPROFILE
#include "../runtime/sample.h"
#endif
#include "amp.h"

extern PTR mknode1();
extern Tag DVEK, INT, PAIR1, TAG0;

#define SLOP 10

extern VPTR hp, ehp;
extern PTR *ep;

#define TSTGC2(x, y) do {\
    int size = (AMPOF(x)->alloclen + AMPOF(y)->alloclen)*2 + SLOP;\
    if (hp + size > ehp) {\
	*--ep = x;\
	*--ep = y;\
	dogc();\
	y = *ep++;\
	x = *ep++;\
    }\
    if (hp + size > ehp)\
	noheapleft();\
    } while (0)

#define TSTGC1(x) do {\
    int size = AMPOF(x)->alloclen*2 + SLOP;\
    if (hp + size > ehp) {\
	*--ep = x;\
	dogc();\
	x = *ep++;\
    }\
    if (hp + size > ehp)\
	noheapleft();\
    } while (0)

#define TSTGC(x, size) do {\
    if (hp + (size) > ehp) {\
	*--ep = x;\
	dogc();\
	x = *ep++;\
    }\
    if (hp + (size) > ehp)\
	noheapleft();\
    } while (0)

#define TOTSIZE(n) (sizeof(struct amp_struct) + ((n)-1)*sizeof(unsigned long int)) / sizeof(int*)

AMP*
amp_alloc(n)
int n;
{
    AMP* r;
    int m;

    m = TOTSIZE(n);
    r = (AMP*)hp;
    r->tag = (long int)&DVEK;
    r->alloclen = m - 2;
    INCHP(m*sizeof(int*));
#ifdef HPROFILE
#ifdef SLOP2
    *hp++ = SLOP1; *hp++ = SLOP2;
#else
    *hp++ = SLOP1;
#endif /* SLOP2 */
#endif
    return r;
}

AMP*
bigadd(x, y)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_add(AMPOF(x), AMPOF(y));
}

AMP*
bigsub(y, x)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_sub(AMPOF(x), AMPOF(y));
}

AMP*
bigmul(x, y)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_mul(AMPOF(x), AMPOF(y));
}

AMP*
bigdiv(y, x)
PTR x, y;
{
    TSTGC2(x, y);
    if (AMPOF(y)->size == 0)
        return 0;
    return amp_div(AMPOF(x), AMPOF(y));
}

AMP*
bigmod(y, x)
PTR x, y;
{
    TSTGC2(x, y);
    if (AMPOF(y)->size == 0)
        return 0;
    return amp_mod(AMPOF(x), AMPOF(y));
}

PTR
bigdivmod(y, x)
PTR x, y;
{
    AMP *z, *w;
    extern Tag BIGNUM;

    TSTGC2(x, y);
    if (AMPOF(y)->size == 0)
        return 0;
    w = amp_divmod(&z, AMPOF(x), AMPOF(y));
    return mkpair(mknode1p(&BIGNUM, (PTR)z), mknode1p(&BIGNUM, (PTR)w));
}

AMP*
bigpowm(z, y, x)
PTR x, y, z;
{
    TSTGC1(z);
    return amp_powm(AMPOF(x), AMPOF(y), AMPOF(z));
}

AMP*
int2big(x)
PTR x;
{
    TSTGC(x, SLOP);
    return amp_from_int(INTOF(x));
}

int
big2int(x)
PTR x;
{
    TSTGC1(x);
    return amp_to_int(AMPOF(x));
}

PTR
big2intlist(x)
PTR x;
{
    AMP* xv;
    int i;
    Int *p;
    PTR r;
    int size;

    size = TOTSIZE(abs(AMPOF(x)->size)) - 2;
    TSTGC(x, size * 5 + SLOP);
    xv = AMPOF(x);
    i = size;
    p = (Int *)xv;
    r = mknil();
    for(; i > 0; i--) {
	r = mkcons(mkint(p[i+1]), r);
    }
    return r;
}

AMP*
bigneg(x)
PTR x;
{
    TSTGC1(x);
    return amp_neg(AMPOF(x));
}

int
bicmp(y, x)
PTR x, y;
{
    return amp_cmp(AMPOF(x), AMPOF(y));
}

void
fprintb(f, x)
FILE *f;
PTR x;
{
    AMP *m = AMPOF(x);
    mp_size msize = abs (m->size);
    int base = 10;
    int out_len = ((size_t) msize * BITS_PER_MP_LIMB * __mp_bases[base].chars_per_bit_exactly) + 2 + (m->size < 0);
    char *tmp = alloca(out_len);

    amp_str(tmp, base, m);
    fputs(tmp, f);
}

void
printb(x)
PTR x;
{
    extern FILE *curoutfile;
    fprintb(curoutfile, x);
}

PTR
big2str(x, y)
PTR x, y;
{
    AMP *m = AMPOF(x);
    mp_size msize = abs (m->size);
    int base = INTOF(y);
    int out_len = ((size_t) msize * BITS_PER_MP_LIMB * __mp_bases[base].chars_per_bit_exactly) + 2 + (m->size < 0);
    char *tmp = alloca(out_len);

    amp_str(tmp, base, m);
    return mkestring(tmp);
}

AMP*
biggcd(x, y)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_gcd(AMPOF(x), AMPOF(y));
}

AMP*
bigand(x, y)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_and(AMPOF(x), AMPOF(y));
}

AMP*
bigor(x, y)
PTR x, y;
{
    TSTGC2(x, y);
    return amp_ior(AMPOF(x), AMPOF(y));
}

PTR
bigsqrt(x)
PTR x;
{
    AMP *z, *w;
    extern Tag BIGNUM;

    TSTGC1(x);
    w = amp_sqrt(&z, AMPOF(x));
    return mkpair(mknode1p(&BIGNUM, (PTR)w), mknode1p(&BIGNUM, (PTR)z));
}

