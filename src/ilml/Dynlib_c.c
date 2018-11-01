
#include <dlfcn.h>
#include "../runtime/node.h"

int debug;

static PTR
err(f)
char *f;
{
  char buf[1024];
  PTR res;
  sprintf(buf, "%s: %s",f,dlerror());
  return mkno(mkestring(buf));
}

PTR
dlopen_c(file)
PTR file;
{
    char buf[1024];
    PTR res,handle;

    if (debug)
	fprintf(stderr, "dlopen_c ");
    tocstring(file, buf, sizeof buf);
    if (debug)
	fprintf(stderr, "'%s' ", buf);
    
    handle=dlopen(buf, RTLD_LAZY
#if defined(SOLARIS) || defined(linux)
		     | RTLD_GLOBAL
#endif
		  );
    if (!handle) {
        res = err(buf);
    } else {
	res = mkyes(mkint((int)handle));
    }
    if(debug)
      fprintf(stderr, "%d\n",handle);
    return res;
}

PTR
dlsym_c(handle,sym)
PTR handle,sym;
{
    char buf[1024];
    PTR res;

    if (debug)
	fprintf(stderr, "dlsym_c ");
    handle=(void*)INTOF(handle);
    tocstring(sym, buf, sizeof buf);
    if (debug)
	fprintf(stderr, "%d '%s'", handle,buf);
    
    handle=dlsym(handle,buf);
    if (debug)
	fprintf(stderr, " = %d\n", handle);
    
    if (!handle) {
	res = err(buf);
    } else {
	res = mkyes(mkint((int)handle));
    }

    return res;
}

PTR
dlclose_c(handle)
PTR handle;
{
    PTR res;
    int i;

    if (debug)
	fprintf(stderr, "dlclose_c ");
    handle=(void*)INTOF(handle);
    if (debug)
	fprintf(stderr, "%d ", handle);
    i = dlclose(handle);
    if(debug)
      fprintf(stderr, "%d\n",i);
    res = mkyes(mkint(i));
    return res;
}

PTR
dlcall_c(i)
PTR i;
{
  return (void*)INTOF(i);
}
