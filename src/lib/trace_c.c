#include <stdio.h>
#include "../runtime/node.h"
#include "../runtime/funs.h"

void
trace()
{
    static int tracelevel = 0;

    flushlow();
    fprintf(stderr, "Enter trace(%d):\n", tracelevel++);
    printtop(stderr);
    fprintf(stderr, "\nExit  trace(%d)\n", --tracelevel);
}

