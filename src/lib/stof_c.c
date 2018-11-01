#include <stdio.h>
#include "../runtime/node.h"

PTR
stof(s)
PTR s;
{
    char buf[1024];
    double d;

    tocstring(s, buf, sizeof buf);
    sscanf(buf, "%lf", &d);

    return mkdouble(d);
}
