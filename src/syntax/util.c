#include "include.h"
#if defined(__svr4__)
/* Damn SCUMSOFT.  They can't even make their own compatibility headers to
** work correctly.
*/
typedef struct {                /* signal set type */
	unsigned long   __sigbits[4];
} sigset_t;
struct sigaltstack {
	char    *ss_sp;
	int     ss_size;
	int     ss_flags;
};
typedef struct sigaltstack stack_t;
#endif
#include <setjmp.h>
#include "curry.h"

#include "proto.h"

extern list Lnil;
extern int h1_3;

tree
mkterop(s, l, m, r)
char *s;
tree l, m, r;
{
    return mkap(mkap(mkap(mkident(s), l), m), r);
}

tree
mkbinop(s, l, r)
char *s;
tree l, r;
{
    return mkap(mkap(mkident(s), l), r);
}

tree
mkunop(s, o)
char *s;
tree o;
{
    return mkap(mkident(s), o);
}

tree
mkcons(h, t)
tree h, t;
{
    return mkap(mkap(mkident("_."), h), t);
}


tree
mkfcomp(f1, f2)
tree f1, f2;
{
    tree t = mkident("x");
    return mklam(t, mkap(f1, mkap(f2, t)));
}

char *
tupstr(l, n)
list l;
int n;
{
    if (tlist(l) == lcons) {
	return tupstr(ltl(l), n+1);
    } else {
	char *p = malloc(10);
	sprintf(p, h1_3 ? "P#%d" : "_#%d", n);
	return p;
    }
}

list
lconc(l1, l2)
list l1, l2;
{
    list t;

    if (tlist(l1) == lnil)
	return(l2);
    for(t = l1; tlist(ltl(t)) != lnil; t = ltl(t))
	;
    ltl(t) = l2;
    return l1;
}

list
lapp(l1, l2)
list l1;
void *l2;
{
    list t;

    if (tlist(l1) == lnil)
	return(mklcons(l2, mklnil()));
    for(t = l1; tlist(ltl(t)) != lnil; t = ltl(t))
	;
    ltl(t) = mklcons(l2, mklnil());
    return l1;
}

list
nrev(l)
list *l;
{
    list cur, prev, next;

    prev = Lnil;
    cur = *l;
    while(tlist(cur) != lnil) {
	next = ltl(cur);
	ltl(cur) = prev;
	prev = cur;
	cur = next;
    }
    *l = prev;
    return *l;
}


/* Temporary fix for the type id problem. */
#define TIDMAX 2000
int
typeno(s)
char *s;
{
    static char *tn[TIDMAX];
    static int tno = 1;
    int i;

    for(i = 1; i < tno; i++) {
	if (strcmp(tn[i], s) == 0)
	    return *s == '?' ? -i : i;
    }
    if (tno >= TIDMAX) {
	error("Too many type id\n");
    }
    tn[tno] = malloc(strlen(s)+1);
    strcpy(tn[tno], s);
    if (*s == '?') {
	return - (tno++);
    }
    return tno++;
}

tree
mksection(i, e)
id i;
tree e;
{
    tree x = mkident("xx");

    return mklam(x, mkbinop(i, x, e));
}

#define T_PAT 1
#define T_EXP 2
#define T_ANY 3

static jmp_buf tjmp;

static void
tcheck(t)
tree t;
{
    list l;

    switch(ttree(t)) {
    case par:
	tcheck(gpare(t));
	break;
    case ident: 
	if (strcmp(gident(t), "_") == 0)
	    longjmp(tjmp, T_PAT);
	break;
    case integer:
    case bignum:
    case charr:
    case floatt:
    case string:
    case ratnum:
	break;
    case restr:
/*	tcheck(grestre(t));*/
	longjmp(tjmp, T_EXP);
	break;
    case eannot:
	tcheck(geannote(t));
	break;
    case ap: 
	tcheck(gfun(t)); 
	tcheck(garg(t)); 
	break;
    case letv: 
    case lam: 
    case casee:
    case listf:
    case listg:
    case doexp:
	longjmp(tjmp, T_EXP);
	break;
    case tuple:
	for(l = gtuplelist(t); tlist(l) != lnil ; l = ltl(l))
	    tcheck(lhd(l));
	break;
    case as:
    case condp:
    case lazyp:
	longjmp(tjmp, T_PAT);
	break;
    case record:
	for(l = grecfields(t); tlist(l) != lnil ; l = ltl(l))
	    tcheck(psnd(lhd(l)));
	break;
    default:
	error("Bad tcheck");
    }
}

static int
check(t)
tree t;
{
    int r;

    if ((r = setjmp(tjmp)))
	return r;
    tcheck(t);
    return T_ANY;
}

tree
checkpat(t)
tree t;
{
    if (check(t) == T_EXP)
	syntaxerror();
    return t;
}

tree
checkexp(t)
tree t;
{
    if (check(t) == T_PAT)
	syntaxerror();
    return t;
}

static id
expname(e)
expidt e;
{
    switch(texpidt(e)) {
    case expid:
	return (gexpid(e));
    case expdd:
	return (gexpdd(e));
    case exppdd:
	return (gexppdd(e));
    case expl:
	return(gexplid(e));
    default:
	error("Bad prexpid");
    }
    return 0;
}

/* add all imported infixes to the lexical analyser */
void
addinfixes(i)
impstuff i;
{
    list fixs = gifixes(i);
    impstuff spec = gispec(i);
    list rens = girename(i);
    int show = giexpose(spec);
    list expids = giids(spec);
    static int tok[] = { INFIX, INFIXL, INFIXR, 0, 0, NONFIX };

    for(; tlist(fixs) == lcons; fixs = ltl(fixs)) {
	impstuff fix = (impstuff)lhd(fixs);
	list lfix;
	int as = gifixass(fix);
	int prec = gifixprec(fix);

	for(lfix = gifixids(fix); tlist(lfix) == lcons; lfix = ltl(lfix)) {
	    id fi = (id)lhd(lfix);
	    int inlist;
	    list l;

	    for(inlist = 0, l = expids; tlist(l) == lcons; l = ltl(l)) {
		if (strcmp(fi, expname(lhd(l))) == 0) {
		    inlist++;
		    break;
		}
		/* A hack to allow importing of constructors with fixity */
		/* XXX this is wrong, we will get too much */
		if (fi[1] == ':' && (texpidt(lhd(l)) == exppdd || texpidt(lhd(l)) == expl)) {
		    inlist++;
		    break;
		}
	    }
	    if ((show && inlist) || (!show && !inlist)) {
		/* keep this unless it is renamed */
		int ren;
		list r;

		for(ren = 0, r = rens; tlist(r) == lcons; r = ltl(r)) {
		    if (strcmp(fi, girensrc(lhd(r))) == 0) {
			ren++;
			break;
		    }
		}
		if (!ren) {
		    makefixop(fi, tok[as], prec);
                }
	    }
	}
    }
}

#define PATHSEP ':'
#define DIRSEP '/'

char loadname[2000];

/* Open a file (like fopen) buth search a path for it. */
FILE *
pathopen(path, name, mode)
char *path, *name, *mode;
{
    char *p, *n, *q;
    FILE *f = NULL;
    extern int debug;

    if (!path || *name == DIRSEP)
	return fopen(name, mode);
    for(p = path; *p && f == NULL; ) {
	for(n = p, q = loadname; *n && *n != PATHSEP; n++)
	    *q++ = *n;
	if (n != p)
	    *q++ = DIRSEP;
	if (*n)
	    n++;
	p = n;
	for(n = name; *n && *n != PATHSEP; n++)
	    *q++ = *n;
	*q = 0;
	f = fopen(loadname, mode);
    }
    if (debug) fprintf(stderr, "opening %s\n", loadname);
    return f;
}

