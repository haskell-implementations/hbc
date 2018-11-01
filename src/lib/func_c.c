#include <math.h>
#include <stdio.h>
#include "../runtime/node.h"

double cgamma(x)
double x;
{
#if defined(_AIX) || defined(__CYGWIN32__)
    /* AIX has a bug handling signgam */
    return 0;
#else
    double y;
    extern int signgam;

    y = exp(lgamma(x));
    if (signgam<0)
	y = -y;
    return y;
#endif
}

double asinh(x)
double x;
{
    return log(x + sqrt(1.0 + x*x));
}

double acosh(x)
double x;
{
    return log(x + (x+1.0) * sqrt((x-1.0)/(x+1.0)));
}

double atanh(x)
double x;
{
    return log((x+1.0) * sqrt(1.0 - 1.0/(x*x)));
}

PTR
Fatan2(x, y)
PTR x, y;
{
    return mkdouble(atan2(getdouble(x), getdouble(y)));
}

#define BODY(fun) \
PTR p;\
{\
    return mkdouble(fun(getdouble(p)));\
}

PTR Fexp(p) BODY(exp)

PTR Fsin(p) BODY(sin)

PTR Fcos(p) BODY(cos)

PTR Ftan(p) BODY(tan)

PTR Flog(p) BODY(log)

PTR Fsqrt(p) BODY(sqrt)

PTR Fasin(p) BODY(asin)

PTR Facos(p) BODY(acos)

PTR Fatan(p) BODY(atan)

PTR Fgamma(p) BODY(cgamma)

PTR Fsinh(p) BODY(sinh)

PTR Fcosh(p) BODY(cosh)

PTR Ftanh(p) BODY(tanh)

PTR Fceil(p) BODY(ceil)

PTR Ffloor(p) BODY(floor)

PTR Ffabs(p) BODY(fabs)

PTR Fasinh(p) BODY(asinh)

PTR Facosh(p) BODY(acosh)

PTR Fatanh(p) BODY(atanh)

