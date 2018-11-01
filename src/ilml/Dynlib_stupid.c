/* dlopen & co seems to be a hack - they cannot be used from a
statically linked program... Grr! */

#include "../runtime/node.h"
#include <stdio.h>

static void err()
{
  fprintf(stderr, "\nShared libraries cannot be loaded in a statically linked binary.\n");
  exit(1);
}

PTR
dlopen_c(file)
PTR file;
{
  err();
}
PTR
dlsym_c(handle,sym)
PTR handle,sym;
{
  err();
}
PTR
dlclose_c(handle)
PTR handle;
{
  err();
}

PTR
dlcall_c(i)
PTR i;
{
  return (void*)INTOF(i);
}
