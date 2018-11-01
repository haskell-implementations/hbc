#include "include.h"

id
installid(s)
char *s;
{
    char *p;

    p = malloc(strlen(s)+1);
    strcpy(p, s);
    return p;
}
