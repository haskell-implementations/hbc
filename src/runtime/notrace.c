#include "node.h"

void
inittrace0()
{
}

void
inittrace()
{
    fprintf(stderr, "Sorry, the program has not been linked with the -T flag.\n");
    finish(1);
}

void
traceerror()
{
}

void
tracesignal()
{
}

void
tracefail()
{
}
