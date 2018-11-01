#include "../runtime/node.h"

#define xmkint(i) mkint(i)
#define xmklong(i) mkint(i)
#define xmkfloat(f) mkfloat((double)f)
#define xmkshort(s) mkint((int)s)
#define xmkdouble(d) mkdouble(d)
#define xgetint(p) INTOF(p)
#define xgetlong(p) INTOF(p)
#define xgetshort(p) (short)INTOF(p)
#define xgetfloat(p) (float)getfloat(p)
#define xgetdouble(p) getdouble(p)

#if defined(__ANSI__) || defined(__STRICT_ANSI__) || defined(__GNUC__) || defined(__STDC__)
#define CAT2(a,b) a##b
#else
#define CAT2(a,b) a/**/b
#endif

extern PTR *ep;

#define X2BYTES(type) \
PTR \
CAT2(type,2bytes)(ip, t) \
PTR ip, t; \
{ \
    union { \
	type i; \
	unsigned char cs[sizeof (type)]; \
    } u; \
    int k; \
    PTR res; \
\
    PUSHPTR(t); \
    ip = evaluate(ip); \
    POPPTR(t); \
    u.i = CAT2(xget,type)(ip); \
    res = t; \
    for(k = sizeof (type) - 1; k >= 0; k--) \
	res = mkcons(mkchar(u.cs[k]), res); \
    return res; \
}

X2BYTES(long)
X2BYTES(int)
X2BYTES(short)
X2BYTES(float)
X2BYTES(double)
    
extern Node badbytes;

#define BYTES2X(type) \
PTR \
CAT2(bytes2,type)(bp) \
PTR bp; \
{ \
    int k; \
    union { \
	type i; \
	unsigned char cs[sizeof (type)]; \
    } u; \
 \
    for(k = 0; k < sizeof(type); k++) { \
	bp = evaluate(bp); \
	if (ISNIL(bp)) \
	    goto bad; \
	PUSHPTR(bp); \
	u.cs[k] = CHAROF(evaluate(HDOF(bp))); \
	POPPTR(bp); \
	bp = TLOF(bp); \
    } \
    return mkpair(CAT2(xmk,type)(u.i), bp); \
  bad: \
    return &badbytes; \
}
    
BYTES2X(long)
BYTES2X(int)
BYTES2X(short)
BYTES2X(float)
BYTES2X(double)
