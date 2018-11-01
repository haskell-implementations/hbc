#include <stdio.h>
#include "../runtime/node.h"

PTR
fmtf(fmt, d)
PTR fmt, d;
{
    char buf[1024];
    char buff[500];

    tocstring(fmt, buf+1, sizeof buf-1);
    buf[0] = '%';
    sprintf(buff, buf, getdouble(d));
    return mkestring(buff);
}
