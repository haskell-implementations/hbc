#include <stdio.h>
#include "../runtime/node.h"

extern char* getcwd();

PTR
cgetwd()
{
    char b[1024];
    getcwd(b, 1024);
    return mkestring(b);
}
