#include <stdio.h>
#include "../runtime/node.h"

PTR
ftosf(i1, i2, d)
PTR i1, i2, d;
{
    char buff[1000];

    sprintf(buff, "%*.*g", (int)INTOF(i1), (int)INTOF(i2), getdouble(d));
    return mkestring(buff);
}
