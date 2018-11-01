#include "../runtime/node.h"

void loadmodule PROTO((char *));
void chkundefs PROTO((void));
void scantab PROTO((void (*)()));

extern char *load_errmsg;

static PTR syms;

extern int debug;

static void
pick(name, val)
char *name;
unsigned long val;
{
    char c0 = name[0];
    char c1 = name[1];
    extern PTR *hp, *ehp;

    if (c0 == 'C' && (c1=='_' || c1=='P' || c1=='V' || c1=='M' || c1=='D')) {
	if (debug)
	    fprintf(stderr, "%-40s at %08x\n", name, val);
	if (NEEDGC) {
	    PUSHPTR(syms);
	    DOGC;
	    POPPTR(syms);
	}
	syms = mkcons(mkpair(mkcstring(name+1),
			     (PTR)val),
		      syms);
    }
}

static void
ignore()
{
}

PTR
cloadmodule(n)
PTR n;
{
    char buf[1024];
    PTR res;

    if (debug)
	fprintf(stderr, "cloadmodule ");
    tocstring(n, buf, sizeof buf);
    if (debug)
	fprintf(stderr, "'%s'\n", buf);
    loadmodule(buf);
    chkundefs();
    if (load_errmsg) {
	res = mkno(mkestring(load_errmsg));
    } else {
	syms = mknil();
	scantab(pick);
	res = mkyes(syms);
    }
    return res;
}

PTR
cloadmodules(ns)
PTR ns;
{
    char buf[1024];

    for(;;) {
	if (ISNIL(ns))
	    break;
	tocstring(HDOF(ns), buf, sizeof buf);
	loadmodule(buf);
	ns = TLOF(ns);
    }
    chkundefs();
    if (load_errmsg) {
	scantab(ignore);
	return mkno(mkestring(load_errmsg));
    }
    syms = mknil();
    scantab(pick);
    return mkyes(syms);
}

int divi(x,y) int x, y; { return x/y; }

