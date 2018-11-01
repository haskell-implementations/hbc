#include <stdio.h>
#include "../runtime/node.h"

PTR
ftos(p)
PTR p;
{
    char buff[1000];
    sprintf(buff, "%g", getdouble(p));
    return mkestring(buff);
}
