#include "../runtime/node.h"
#include "../runtime/funs.h"

#include <sys/utsname.h>

PTR
cuname()
{
    struct utsname un;

    uname(&un);
    return mkconstr(0, 5, 
		        mkestring(un.sysname), mkestring(un.nodename), mkestring(un.release),
		        mkestring(un.version), mkestring(un.machine));
}
