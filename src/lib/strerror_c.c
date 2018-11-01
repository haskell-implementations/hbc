#include <string.h>
#include "../runtime/node.h"
#include "../runtime/funs.h"

PTR
cstrerror(p)
PTR p;
{
    int i = INTOF(p);
    return mkestring(strerror(i));
}
