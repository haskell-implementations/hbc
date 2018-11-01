/*
** Implementation of encode/decode operations.
*/

#include <stdio.h>
#include "../runtime/node.h"

#if defined(sequent) || defined(sun) || defined(mips) || defined(hp300) || defined(_IBMR2) || defined(linux) || defined(__386BSD__) || defined(__alpha) || defined(__NetBSD__) || defined(irix) || defined(hppa) || defined(__FreeBSD__) || defined(__CYGWIN32__)
#define IEEE
#endif
#if (defined(sun) && !defined(i386)) || defined(hp300) || defined(_IBMR2) || defined(sgi) || defined(hppa)
#define BIGENDIAN
#endif
#include "flt.h"

#ifdef BIGENDIAN
#define L 1
#define H 0
#else
#define L 0
#define H 1
#endif

#include "amp.h"
double frexp(), ldexp();

extern Tag BIGNUM;

#if defined(__alpha)
#define DNBIGIT 1
#else
#define DNBIGIT 2
#endif
#define SNBIGIT 1

/*
** Encode a mantissa (an Integer) and an exponent (an Int) as a floating point number.
** f = mant * 2^expo
** This operation is not as efficient as it could be, but it's portable.
*/
PTR
dencode(mant, expo)
PTR mant, expo;
{
    int iexp = INTOF(expo);		/* The exponent as an int */
    AMP *man = AMPOF(mant);		/* The mantissa as a bignum */
    double r;
    double base = BASE;
    int i;
    
    /* Convert bignum to a double */
    i = abs(man->size)-1;
    if (i < 0) {
	r = 0.0;
    } else {
	for(r = man->d[i], i--; i >= 0; i--)
	    r = r * base + man->d[i];
    }
    /* Load new exponent */
/*fprintf(stderr, "dencode ldexp %g %d\n", r, iexp);*/
    if (r != 0.0)		/* bug in mips ldexp  */
	r = ldexp(r, iexp);
    /* We should test errno here and check for EHUGE */
    if (man->size < 0)
	r = -r;
/*fprintf(stderr, "dencode = %g\n", r);*/
    return mkdouble(r);
}

/*
** Decode a floating point number into a mantissa an an exponent.
** This operation is tricky to do in a portable and efficient way!
** Current implementation is really bad!
*/
PTR
ddecode(p)
PTR p;
{
#ifdef IEEE
#define MINEXP (DBL_MINEXP - DBL_DIGITS - 1)
#define HIGHBIT 0x00100000
#define MSBBIT  0x80000000
    /* Do some bit fiddling on IEEE */
    unsigned int low, high;
    int iexp, sign;
    AMP *man;
    union { double d; int i[2]; } u;

    u.d = getdouble(p);
    low = u.i[L];
    high = u.i[H];
    man = amp_alloc(DNBIGIT);
    if (low == 0 && (high & ~MSBBIT) == 0) {
	man->size = 0;
	iexp = 0;
    } else {
	man->size = DNBIGIT;
	iexp = ((high >> 20) & 0x7ff) + MINEXP;
	sign = high;
/*fprintf(stderr, "decode %g %08x %08x %d\n", u.d, high, low, iexp);*/
	high &= HIGHBIT-1;
	if (iexp != MINEXP)	/* don't add hidden bit to denorms */
	    high |= HIGHBIT;
	else {
	    iexp++;
	    /* A denorm, normalize the mantissa */
	    while (! (high & HIGHBIT)) {
		high <<= 1;
		if (low & MSBBIT)
		    high++;
		low <<= 1;
		iexp--;
	    }
	}
#if DNBIGIT == 2
	man->d[0] = low;
	man->d[1] = high;
#else
#if DNBIGIT == 1
	man->d[0] = ((unsigned long)high) << 32 | (unsigned long)(unsigned)low;
#else
	Bad DNBIGIT !!
#endif
#endif
	if (sign < 0)
	    man->size = -man->size;
    }
#else
    double r, sc;
    int iexp;
    AMP *man;
    int i, b;
    double base = BASE;

    r = getdouble(p);

/*printf("ddecode r=%g\n", r);*/
    man = amp_alloc(DNBIGIT);
    man->sign = 1;
    if (r == 0.0) {
	man->size = 0;
	iexp = 0;
    } else {
	man->size = DNBIGIT;
	if (r < 0.0) {
	    man->sign = -1;
	    r = -r;
	}
	for(sc = 1.0, i = 0; i < DNBIGIT; i++)
	    sc *= base;
	r /= sc;
/*printf("ddecode / r=%g\n", r);*/
	r = frexp(r, &iexp);
/*printf("ddecode frexp r=%g, iexp=%d\n", r, iexp);*/
	for(i = DNBIGIT-1; i >= 0; i--) {
	    r *= base;
	    b = (int)r;
/*printf("ddecode r=%g b=%d\n", r, b);*/
	    man->d[i] = b;
	    r -= b;
	}
	if (r >= 0.5)
	    man->d[0]++;
    }
#endif
/*printf("ddecode man="); fprintb(stdout, bman); printf("\n");*/
    return mkpair(mknode1p(&BIGNUM, (PTR)man), mkint(iexp));
}

#if 1
/*
** Encode a mantissa (an Integer) and an exponent (an Int) as a floating point number.
** f = mant * 2^expo
** This operation is not as efficient as it could be, but it's portable.
*/
PTR
sencode(mant, expo)
PTR mant, expo;
{
    int iexp = INTOF(expo);		/* The exponent as an int */
    AMP *man = AMPOF(mant);		/* The mantissa as a bignum */
    double r;
    double base = BASE;
    int i;
    
    /* Convert bignum to a double */
    for(r = 0.0, i = abs(man->size)-1; i >= 0; i--)
	r = r * base + man->d[i];
    /* Load new exponent */
/*printf("dencode ldexp %g %d\n", r, iexp);*/
    if (r != 0.0)		/* bug in mips ldexp  */
	r = ldexp(r, iexp);
    if (man->size < 0)
	r = -r;
    return mkfloat(r);		/* convert double to single */
}

#undef MINEXP
#undef HIGHBIT
#undef MSBBIT
/*
** Decode a floating point number into a mantissa an an exponent.
** This operation is tricky to do in a portable and efficient way!
** Current implementation is really bad!
*/
PTR
sdecode(p)
PTR p;
{
#ifdef IEEE
#define MINEXP (FLT_MINEXP - FLT_DIGITS - 1)
#define HIGHBIT 0x00800000
#define MSBBIT  0x80000000
    /* Do some bit fiddling on IEEE */
    int high, iexp, sign;
    AMP *man;
#if 0
    union { float d; int i; } u;

    u.d = getfloat(p);
    high = u.i;
#else
    high = INTOF(p);
#endif
    man = amp_alloc(SNBIGIT);
    if ((high & ~MSBBIT) == 0) {
	man->size = 0;
	iexp = 0;
    } else {
	man->size = SNBIGIT;
	iexp = ((high >> 23) & 0xff) + MINEXP;
	sign = high;
/*fprintf(stderr, "decode %g %08x %08x %d\n", u.d, high, low, iexp);*/
	high &= HIGHBIT-1;
	if (iexp != MINEXP)	/* don't add hidden bit to denorms */
	    high |= HIGHBIT;
	else {
	    iexp++;
	    /* A denorm, normalize the mantissa */
	    while (! (high & HIGHBIT)) {
		high <<= 1;
		iexp--;
	    }
	}
	man->d[0] = high;
	if (sign < 0)
	    man->size = -man->size;
    }
#else
    float r, sc;
    int iexp;
    AMP *man;
    int i, b;
    float base = BASE;

    r = getsingle(p);

/*printf("ddecode r=%g\n", r);*/
    man = amp_alloc(SNBIGIT);
    man->sign = 1;
    if (r == 0.0) {
	man->size = 0;
	iexp = 0;
    } else {
	man->size = SNBIGIT;
	if (r < 0.0) {
	    man->sign = -1;
	    r = -r;
	}
	for(sc = 1.0, i = 0; i < SNBIGIT; i++)
	    sc *= base;
	r /= sc;
/*printf("ddecode / r=%g\n", r);*/
	r = frexp(r, &iexp);
/*printf("ddecode frexp r=%g, iexp=%d\n", r, iexp);*/
	for(i = SNBIGIT-1; i >= 0; i--) {
	    r *= base;
	    b = (int)r;
/*printf("ddecode r=%g b=%d\n", r, b);*/
	    man->d[i] = b;
	    r -= b;
	}
	if (r >= 0.5)
	    man->d[0]++;
    }
#endif
/*printf("ddecode man="); fprintb(stdout, bman); printf("\n");*/
    return mkpair(mknode1p(&BIGNUM, (PTR)man), mkint(iexp));
}
#endif

#ifdef IEEE
PTR
disNaN(p) 
PTR p; 
{
    union { double d; int i[2]; } u;
    int hx,lx;
    int r;

    u.d = getdouble(p);
    hx = u.i[H];
    lx = u.i[L];
    hx &= 0x7fffffff;
    hx |= (unsigned int)(lx|(-lx))>>31;        
    hx = 0x7ff00000 - hx;
    r = (int)((unsigned int)(hx))>>31;
    return mkbool(r);
}

PTR
disInfinity(p) 
PTR p;
{
    union { double d; int i[2]; } u;
    int hx,lx;

    u.d = getdouble(p);
    hx = u.i[H];
    lx = u.i[L];
    hx &= 0x7fffffff;
    hx ^= 0x7ff00000;
    hx |= lx;
    return mkbool(hx == 0);
}

PTR
disDenormalized(p) 
PTR p;
{
    union { double d; int i[2]; } u;
    int high, iexp;

    u.d = getdouble(p);
    high = u.i[H];
    iexp = high & (0x7ff << 20);
    return mkbool(iexp == 0);
}

PTR
disNegativeZero(p) 
PTR p;
{
    union { double d; int i[2]; } u;
    int high, iexp;

    u.d = getdouble(p);
    return mkbool(u.i[H] == 0x80000000 && u.i[L] == 0);
}

PTR
fisNaN(p) 
PTR p;
{
    int ix;
    int r;

    ix = INTOF(p);
    ix &= 0x7fffffff;
    ix = 0x7f800000 - ix;
    r = (int)(((unsigned int)(ix))>>31);
    return mkbool(r);
}

PTR
fisInfinity(p) 
PTR p;
{
    int ix;

    ix = INTOF(p);
    ix &= 0x7fffffff;
    ix ^= 0x7f800000;
    return mkbool(ix == 0);
}

PTR
fisDenormalized(p) 
PTR p;
{
    int high, iexp;

    high = INTOF(p);
    iexp = high & (0xff << 23);
    return mkbool(iexp == 0);
}

PTR
fisNegativeZero(p) 
PTR p;
{
    int high = INTOF(p);
    return mkbool(high == 0x80000000);
}
#else
PTR disNaN(p) PTR p; { return mkbool(0); }
PTR disInfinity(p) PTR p; { return mkbool(0); }
PTR disDenormalized(p) PTR p; { return mkbool(0); }
PTR disNegativeZero(p) PTR p; { return mkbool(0); }
PTR fisNaN(p) PTR p; { return mkbool(0); }
PTR fisInfinity(p) PTR p; { return mkbool(0); }
PTR fisDenormalized(p) PTR p; { return mkbool(0); }
PTR fisNegativeZero(p) PTR p; { return mkbool(0); }
#endif
