#include <sys/types.h>
#include <pwd.h>
#include <grp.h>

#include <stdio.h>
#include "../runtime/node.h"
#include "../runtime/funs.h"

char *
uid2str(uid)
int uid;
{
    struct passwd *pw;

    pw = getpwuid(uid);
    if (pw) {
	return pw->pw_name;
    } else {
	static char buf[20];
	sprintf(buf, "%u", uid);
	return buf;
    }
}

char *
gid2str(uid)
int uid;
{
    struct group *gr;

    gr = getgrgid(uid);
    if (gr) {
	return gr->gr_name;
    } else {
	static char buf[20];
	sprintf(buf, "%u", uid);
	return buf;
    }
}

PTR
pwuid(uid)
PTR uid;
{
    return mkestring(uid2str(INTOF(uid)));
}

PTR
grgid(gid)
PTR gid;
{
    return mkestring(gid2str(INTOF(gid)));
}

