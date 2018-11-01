#include <ctype.h>

#include "runtime.h"
#include "vars.h"
#include "node.h"

#if !defined(SOLARIS) && !defined(HPUX) &&!defined(__CYGWIN32__)

#if defined(mips) || defined(__osf__)
#include <filehdr.h>
#include <syms.h>
#include <ldfcn.h>
#else
#include <a.out.h>
#endif

extern int debug;
extern Tag 	INT, SFLOAT, DFLOAT, CHAR, TAG, TAG0, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4,
		STRING, VEK, DVEK, CAP, MARKED, MOVED, AP, FUN, VAP, INDIR,
                APG,VAPG,
		HOLE, INPUT, INPUTD, GCRET, BIGNUM, ZAP, STRINGN;
extern Tag      AP_1, VAP_1, ZAP_1, HOLE_1, STRING_1, STRINGN_1, INPUTD_1, INPUT_1, 
                STRING_F, STRINGN_F, INPUTD_F, INPUT_F;

void fprintb PROTO((FILE *, PTR));

#ifndef clearerr
void clearerr();
#endif

static void interact PROTO((int));

extern char *progname;
extern int traceflag;

static int traceinteract;
static int tracing = 0;
static int prfilename = 0;
static int fromintr = 0;
static int tracedepth = -1;

static int fundepth = 0;
struct fentry {
    VPTR ret;
    PTR *sp;
    char traceit;
};
#define TRACEIT 1
#define INTERACT 2
#define STOPAT 4
static struct fentry *entrystack;
static int maxfundepth;

struct fsym {
    char *name;
    char *file;
    int arity;
    VPTR addr;
    char trace;
};
struct fsym *fsyms;
static int maxfsyms;
static int nfsyms;

FILE *trout, *trin;

static int 
cmpfsym(a, b)
struct fsym *a, *b;
{
    if (a->addr < b->addr)
	return -1;
    else if (a->addr > b->addr)
	return 1;
    else
	return 0;
}

/* undo the special stuff to keep the assembler happy */
void
unasm(p)
char *p;
{
    char *q;
    for(q = p; *p; p++) {
	if (*p == '$') {
	    if (*++p == '_') {
		*q++ = '.';
		if (p[1] == '_')
		    p++;
	    } else {
		static char *hex = "0123456789ABCDEF";
		*q++ = (strchr(hex, p[0])-hex) * 16 + strchr(hex, p[1]) - hex;
		p++;
	    }
	} else
	    *q++ = *p;
    }
    *q = 0;
}

static char *
getsymbols()
{
#ifdef _AIX
    fprintf(stderr, "Cannot get symbol table for AIX\n");
    exit(1);
#else
#if defined(mips) || defined(__osf__)
    LDFILE *l;
    int i;
    SYMR sym;
    char *name;
    int nsyms;
    extern char *ldgetname();
    extern int ltbread(), ldtbread(), ldclose(), ldreadst();
    extern LDFILE *ldopen();
    char *fn = "";
    extern char *copystring PROTO((char *));

    l = ldopen(progname, (LDFILE*)0);
    if (!l) 
	return "Cannot open program";
    nsyms = SYMHEADER(l).isymMax+SYMHEADER(l).iextMax;
    if (SYMTAB(l) == 0 || nsyms == 0)
	return "Program has been stripped";
    nfsyms = 0;
    maxfsyms = 1000;
    fsyms = malloc(maxfsyms * sizeof * fsyms);
    for(i = 0; i < nsyms; i++) {
	if (!ldtbread (l, i, &sym))
	    return "Symbol table trouble";
	name = ldgetname(l, &sym);
	if (name == NULL)
	    return "Symbol table trouble";
	if (sym.st == stFile) {
	    char *p;
	    if (p = strrchr(name, '.'))
		*p = 0;
	    fn = copystring(name);
	} else if (sym.sc = scText && (sym.st == stLabel || sym.st == stStatic) && 
		   name[0] == 'J' && isdigit(name[1])) {
	    char *p = name;
	    int arity;
	    p++;		/* skip 'J' */
	    for(arity = 0; isdigit(*p); p++)
		arity = arity*10 + *p - '0';
	    if (p[0] == 'L' && p[1] == 'F') {
		p += 2;
		while (isdigit(*p))
		    p++;
	    }
	    if (*p == '_')
		p++;
	    else if (p[0] == 'C' && p[1] == '_')
		p += 2;
	    /* Add symbol */
	    if (nfsyms >= maxfsyms) {
		maxfsyms += 1000;
		fsyms = xrealloc(fsyms, maxfsyms * sizeof *fsyms);
	    }
	    unasm(p);
	    fsyms[nfsyms].name = copystring(p);
	    fsyms[nfsyms].file = fn;
	    fsyms[nfsyms].arity = arity;
	    fsyms[nfsyms].trace = 1;
	    fsyms[nfsyms].addr = (VPTR)sym.value;
	    nfsyms++;
	    if (debug)
	        fprintf(stderr, "Added %s %s %d %lx\n", fn, p, arity, (Int)sym.value);
	}
    }
    ldclose(l);
#else
    struct exec e;
    int fd, len, i;
    char *strings;
    struct nlist *syms;
    int nsyms;
    char *fn = "";

    fd = open(progname, 0, 0);
    if (fd < 0) {
	return "Cannot open program";
    }
    read(fd, &e, sizeof e);
    lseek(fd, N_SYMOFF(e), 0);
    syms = (struct nlist *)xmalloc(e.a_syms);
    read(fd, syms, e.a_syms);
    nsyms = e.a_syms / sizeof(struct nlist);
    if (nsyms == 0) {
	return "Program has been stripped";
    }
    lseek(fd, N_STROFF(e), 0);
    read(fd, &len, sizeof len);
    strings = (char *)xmalloc(len);
    read(fd, strings, len);
    close(fd);
    for(i = 0; i < nsyms; i++)
	syms[i].n_un.n_name = strings + syms[i].n_un.n_strx - sizeof len;
    /* Scan symbol table and keep interesting symbols */
    nfsyms = 0;
    maxfsyms = 1000;
    fsyms = (struct fsym *)xmalloc(maxfsyms * sizeof * fsyms);
    for(i = 0; i < nsyms; i++) {
/*	fprintf(stderr, "%s %x\n", syms[i].n_un.n_name, syms[i].n_value);*/
	if ((syms[i].n_type & N_TYPE) == N_FN) {
	    char *p;
	    fn = syms[i].n_un.n_name;
	    /*fprintf(stderr, "file '%s'\n", fn);*/
	    if ((p = strrchr(fn, '.')))
		*p = 0;
	} else if ((syms[i].n_type & N_TYPE) == N_TEXT && syms[i].n_un.n_name[0] == 'J' && isdigit(syms[i].n_un.n_name[1])) {
	    char *p = syms[i].n_un.n_name;
	    int arity;
	    p++;		/* skip 'J' */
	    for(arity = 0; isdigit(*p); p++)
		arity = arity*10 + *p - '0';
	    if (p[0] == 'L' && p[1] == 'F') {
		p += 2;
		while (isdigit(*p))
		    p++;
	    }
	    if (*p == '_')
		p++;
	    else if (p[0] == 'C' && p[1] == '_')
		p += 2;
	    /* Add symbol */
	    if (nfsyms >= maxfsyms) {
		maxfsyms += 1000;
		fsyms = (struct fsym *)xrealloc(fsyms, maxfsyms * sizeof *fsyms);
	    }
	    unasm(p);
	    fsyms[nfsyms].name = p;
	    fsyms[nfsyms].file = fn;
	    fsyms[nfsyms].arity = arity;
	    fsyms[nfsyms].trace = 1;
	    fsyms[nfsyms].addr = (VPTR)syms[i].n_value;
	    nfsyms++;
	    if (debug)
	        fprintf(stderr, "Added %s %s %d %x\n", fn, p, arity, syms[i].n_value);
	}
    }	
    free(syms);
#endif
    qsort(fsyms, nfsyms, sizeof(struct fsym), cmpfsym);
    fsyms[nfsyms].name = "EOF";
    fsyms[nfsyms].file = "EOF";
    fsyms[nfsyms].arity = 0;
    fsyms[nfsyms].trace = 0;
    fsyms[nfsyms].addr = (VPTR)-1;
    nfsyms++;
#endif
    return 0;
}

void
inttrace()
{
    sigsetmask(sigsetmask(~0) &~ sigmask(SIGINT));
    fromintr++;
    traceinteract++;
}

void
inittrace0()
{
    maxfundepth = 1000;
    entrystack = (struct fentry *)xmalloc(maxfundepth * sizeof *entrystack);
}

void
inittrace()
{
    char *s;

    inittrace0();
#if 0
    trout = stderr;
    trin = stdin;
#else
    trout = fopen("/dev/tty", "w");
    trin = fopen("/dev/tty", "r");
    if (trout == NULL || trin == NULL) {
	fprintf(stderr, "Couldn't open /dev/tty.\n");
	exit(1);
    }
    setlinebuf(trout);
#endif
    traceinteract = traceflag == 1;
    tracing = 1;
    if (traceinteract) {
	fprintf(trout, "Processing symbol table..."); fflush(trout);
    }
    if ((s = getsymbols())) {
	fprintf(stderr, "%s: %s\n", progname, s);
	exit(1);
    }
    if (traceinteract) {
	fprintf(trout, "done.\nUse help to get help.\n");
    }
    setbuf(stdout, NULL);
    signal(SIGINT, inttrace);
    signal(SIGQUIT, finish);
}

struct fsym *
findsym(a)
VPTR a;
{
    int i;
    int l, h, m;

    l = 0;
    h = nfsyms;
    while(l+2 < h-2) {
	m = (l+h)/2;
	if (a < fsyms[m].addr)
	    h = m+1;
	else if (a > fsyms[m].addr)
	    l = m;
	else
	    break;
    }
    for(i = l; i < h; i++)
	if (a < fsyms[i].addr) {
	    return &fsyms[i-1];
	}
    return 0;
}

static char *
funcname(f)
VPTR f;
{
    static char buf[500];
    struct fsym *s;

    s = findsym(f);
    if (s)
	if (prfilename) {
	    sprintf(buf, "%s.%s", s->file, s->name);
	    return buf;
	} else
	    return s->name;
    else {
	sprintf(buf, "UNKNOWN-%lx", (Int)f);
	return buf;
    }
}

static void dumpvalue();

static void
dumppair(p, d)
PTR p;
int d;
{
    if (p->tag == &PAIR) {
	dumpvalue(HDOF(p), d);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d);
    } else if (p->tag == &VEK) {
	int i, m;
	m = p->nodevek.size-1;
	for(i = 0; i <= m; i++){
	    dumpvalue(p->nodevek.ptr[i], d);
	    if (i < m)
		fprintf(trout, " ");
	}
    } else {
	dumpvalue(p, d);
    }
}

char *
prchar(c)
int c;
{
    static char b[10];

    c &= 0xff;
    if (c < ' ') {
	if (c == '\n')
	    return "\\n";
	else if (c == '\t')
	    return "\\t";
	else
	    sprintf(b, "\\%03o", c);
    } else
	b[0] = c, b[1] = 0;
    return b;
}

static void dumpvalue();
static void 
dumpvaluefun(p, d)
PTR p;
int d;
{
    if (p != 0 && (
	p->tag == &AP || p->tag == &APG || p->tag == &CAP || p->tag == &AP_1)) {
	dumpvaluefun(FUNOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(ARGOF(p), d-1);
    } else {
	dumpvalue(p, d);
    }
}

static void
dumpvalue(p, d)
PTR p;
int d;
{
    if (p == 0 
#ifndef __osf__
	|| p > (PTR)endheap+10000
#endif
	) {
	fprintf(trout, "***Bogus addr %lx (>%lx)***", (unsigned long)p, (unsigned long)endheap);
    } else if(d == 0){
	fprintf(trout, "_");
    } else if(p->tag == &INT){
	fprintf(trout, "%d", INTOF(p));
    } else if(p->tag == &DFLOAT){
#if IntSize == 8
	union u { double d; Int i; } u;
	u.i = p->node20.val0;
#else
	union u { double d; int i[2]; } u;
	u.i[0] = p->node30.val0;
	u.i[1] = p->node30.val1;
#endif
	fprintf(trout, "%.14e", u.d);
    } else if(p->tag == &SFLOAT){
	union u { float d; int i; } u;
	u.i = p->node30.val0;
	fprintf(trout, "%.6e", (double)u.d);
    } else if(p->tag == &BIGNUM){
	fprintb(trout, p);
    } else if(p->tag == &CHAR){
	fprintf(trout, "'%s'", prchar(INTOF(p)));
    } else if(p->tag == &TAG){
	fprintf(trout, "(CON%d ", p->node21.val0);
	dumppair(p->node21.ptr0, d-1);
	fprintf(trout, ")");
    } else if(p->tag == &TAG0){
	fprintf(trout, "CON%d", p->node20.val0);
    } else if(p->tag == &PAIR){
	fprintf(trout, "***PAIR***");
    } else if(p->tag == &PAIR0){
	fprintf(trout, "(CON0 ");
	dumppair(HDOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &PAIR1){
	fprintf(trout, "(CON1 ");
	dumppair(HDOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &PAIR2){
	fprintf(trout, "(CON2 ");
	dumppair(HDOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &PAIR3){
	fprintf(trout, "(CON3 ");
	dumppair(HDOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &PAIR4){
	fprintf(trout, "(CON4 ");
	dumppair(HDOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(TLOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &AP || p->tag == &APG || p->tag == &CAP || p->tag == &AP_1){
	fprintf(trout,  "(");
	dumpvaluefun(FUNOF(p), d-1);
	fprintf(trout, " ");
	dumpvalue(ARGOF(p), d-1);
	fprintf(trout, ")");
    } else if(p->tag == &INDIR){
	dumpvalue(p->node11.ptr0, d-1);
    } else if(p->tag == &FUN){
	fprintf(trout, "%s", funcname(p->nodefun.fun->jmpcode));
    } else if(p->tag == &VAP || p->tag == &VAPG || p->tag == &VAP_1){
	int i, m;
	m = p->nodevap.fun->arity;
	fprintf(trout, "%s%s", m ? "(" : "", funcname(p->nodevap.fun->jmpcode));
	for(i = 0; i < m; i++){
	    fprintf(trout, " ");
	    dumpvalue(p->nodevap.ptr[i], d-1);
	}
	if (m)
	    fprintf(trout, ")");
    } else if(p->tag == &STRING || p->tag == &STRING_1 ||
	      p->tag == &STRING_F || p->tag == &STRINGN_F ||
	      p->tag == &STRINGN || p->tag == &STRINGN_1){
	fprintf(trout, "\"%s\"", STRINGOF(p));
    } else if(p->tag == &VEK){
	fprintf(trout, "***VEK***");
    } else if(p->tag == &DVEK){
	fprintf(trout, "***DVEK***");
    } else if(p->tag == &HOLE || p->tag == &HOLE_1){
	fprintf(trout, "HOLE");
    } else if(p->tag == &ZAP || p->tag == &ZAP_1){
	fprintf(trout, "ZAP-%s", funcname(p->nodezap.fun->jmpcode));
    } else if(p->tag == &INPUT || p->tag == &INPUT_1 || p->tag == &INPUT_F){
	fprintf(trout, "(READFILE 0x%x) ", (unsigned int)p->node30.val0);
    } else if(p->tag == &INPUTD || p->tag == &INPUTD_1 || p->tag == &INPUTD_F){
	fprintf(trout, "(READDIR 0x%x) ", (unsigned int)p->node30.val0);
    } else {
	fprintf(trout, "***BADTAG %lx***", (unsigned long)p->tag);
    }
}

static int prdepth = 3;

static void
prvalue(p)
PTR p;
{
    dumpvalue(p, prdepth);
}

static void
prcall(ret, sp)
VPTR ret;
PTR *sp;
{
    struct fsym *s;
    int i;

    s = findsym(ret);
    if (!s) {
	fprintf(trout, "Unknown function call");
	return;
    }
    if (prfilename)
	fprintf(trout, "%s.%s", s->file, s->name);
    else
	fprintf(trout, "%s", s->name);
    for(i = 0; i < s->arity; i++) {
	fprintf(trout, " ");
	prvalue(sp[i]);
    }
}

static void
prenter(ret, sp)
VPTR ret;
PTR *sp;
{
    fprintf(trout, "---- %*sEnter ", fundepth, "");
    prcall(ret, sp);
    fprintf(trout, "\n");
}

static int nstop;
#define SLOP 48

void
cdo_enter(ret)
VPTR ret;
{
    struct fsym *s = 0;

    if(!traceflag) return;
    if (fundepth >= maxfundepth) {
	maxfundepth *= 2;
	entrystack = (struct fentry *)xrealloc(entrystack, maxfundepth * sizeof *entrystack);
    }
    entrystack[fundepth].ret = ret;
    entrystack[fundepth].sp = ep;
    entrystack[fundepth].traceit = 0;
    if (nstop > 0 && (s = findsym(ret)) && (s->trace & STOPAT) && (Int)ret - (Int)s->addr < SLOP) {
	traceinteract = 1;
	tracing = 1;
    }
    if (tracing && (s || (s = findsym(ret))) && s->trace) {
	prenter(ret, ep);
	entrystack[fundepth].traceit = TRACEIT;
    }
    if (traceinteract) {
	interact(1);
    }
    fundepth++;
}

static void
cdo_exit(p, s)
PTR p;
char *s;
{
    if(!traceflag) return;
    fundepth--;
    if (entrystack[fundepth].traceit) {
	fprintf(trout, "---- %*s%s ", fundepth, "", s);
	if (p) 
	    prvalue(p);
	fprintf(trout, "\n");
	if (entrystack[fundepth].traceit == INTERACT) {
	    tracing = 1;
	    traceinteract = 1;
	    interact(0);
	}
    }
}

void
cdo_return(r0)
PTR r0;
{
    cdo_exit(r0, "Return");
}

void
cdo_unwind(r0)
PTR r0;
{
    cdo_exit((PTR)0, "Unwind");
}

void
cdo_evalupdunw(r0)
PTR r0;
{
    cdo_exit(r0, "Return variable");
}

void
cdo_jfun(r0)
PTR r0;
{
    cdo_exit((PTR)0, "Jump (unknown)");
}

void
cdo_jglobal()
{
    cdo_exit((PTR)0, "Jump");
}

#if defined(_AIX) && !defined(__GNUC__)
static char *helpmsg = "No help, because the braindead AIX C compiler can't handle long strings.\n";
#else
static char *helpmsg = "\
help\t\tget this message\n\
quit\t\tquit the tracer\n\
leave\t\tleave this recursive invocation of the tracer\n\
next\t\tcontinue until next function\n\
\
cont\t\ttrace without interaction\n\
rcont\t\trun without interaction\n\
exit\t\ttrace until this function exits\n\
rexit\t\trun until this function exits\n\
stop <re>\tstop when <re> is entered\n\
nostop <re>\tremove stop for <re>\n\
\
arg <int>\tevaluate (to WHNF) and print an argument.\n\
farg <int>\tfully evaluate and print an argument.\n\
on <re>\t\tturn on tracing for functions matching <re>\n\
off <re>\tturn off tracing for functions matching <re>\n\
where\t\tshow call stack\n\
depth <int>\tset print depth\n\
file y/n\tturn on/off file name print\n\
<id> is an identifier, which may be prefixed by its file name.\n\
<re> is a simple regular expression with * for matching <id>.\n\
";
#endif

#define BUFS 500

static int 
match(pat, str)
char *pat, *str;
{
 top:
    if (!*pat && !*str)
	return 1;		/* both are empty */
    if (*pat == '*') {
	/* may match any number of chars, try longest first */
	int l;
	for(l = strlen(str); l >= 0; l--) {
	    if (match(pat+1, str+l))
		return 1;
	}
	return 0;
    } else {
	if (*pat != *str)
	    return 0;
	else {
	    pat++;
	    str++;
	    goto top;		/* avoid tail call */
	}
    }
}

static void
splitname(mp, np, s)
char **mp, **np, *s;
{
    char *name, *mod;

    if ((name = strchr(s, '.'))) {
	*name++ = 0;
	mod = s;
    } else {
	mod = 0;
	name = s;
    }
    *mp = mod;
    *np = name;
}

struct fsym *
locate(mod, name)
char *mod, *name;
{
    int i;

    for(i = 0; i < nfsyms; i++)
	if ((!mod || strcmp(fsyms[i].file, mod) == 0) && strcmp(fsyms[i].name, name) == 0)
	    return &fsyms[i];
    return 0;
}

static int
forallsyms(s, g)
void (*g)();
char *s;
{
    int nhit, i;

    if (!strchr(s, '*')) {	/* fast special case */
	struct fsym *f;
	char *mod, *name;

	splitname(&mod, &name, s);
	f = locate(mod, name);
	if (f) {
	    g(f);
	    nhit = 1;
	} else
	    nhit = 0;
    } else {
	char *mod, *name;

	splitname(&mod, &name, s);
	for(nhit = i = 0; i < nfsyms; i++) {
	    if ((!mod || match(mod, fsyms[i].file)) && match(name, fsyms[i].name)) {
		nhit++;
		g(&fsyms[i]);
	    }
	}
    }
    return nhit;
}

static void
prnamemsg(s, f)
char *s;
struct fsym *f;
{
    if (prfilename)
	fprintf(trout, "%s %s.%s\n", s, f->file, f->name);
    else
	fprintf(trout, "%s %s\n", s, f->name);
}

static void
turnon(f)
struct fsym *f;
{
    if (!(f->trace & TRACEIT)) {
	prnamemsg("Trace on", f);
	f->trace |= TRACEIT;
    }
}

static void
turnoff(f)
struct fsym *f;
{
    if (f->trace & TRACEIT) {
	prnamemsg("Trace off", f);
	f->trace &= ~TRACEIT;
    }
}

static void
setstop(f)
struct fsym *f;
{
    if (!(f->trace & STOPAT)) {
	prnamemsg("Stop on", f);
	f->trace |= STOPAT;
	nstop++;
    }
}

static void
setnostop(f)
struct fsym *f;
{
    if (f->trace & STOPAT) {
	prnamemsg("Stop off", f);
	f->trace &= ~STOPAT;
	nstop--;
    }
}

#define CMD(x) (strlen(cmd) <= strlen(x) && strncmp(cmd, x, strlen(cmd)) == 0)
#define PREF 'r'

static void
interact(fromenter)
int fromenter;
{
    static char last[BUFS] = "\n";
    char buf[BUFS];
    char *cmd, *arg, *p;

    tracedepth++;
    if (fromintr) {
	fprintf(trout, "\nInterrupted\n");
	fromintr = 0;
    }
    for(;;) {
	fprintf(trout, tracedepth ? "trc-%d> " : "trc> ", tracedepth);
	fflush(trout);
	if (fgets(buf, sizeof buf, trin) == NULL) {
	    fprintf(trout, "\n");
	    clearerr(trin);
	    continue;
	}
	if (buf[0] == '\n')
	    strcpy(buf, last);
	else
	    strcpy(last, buf);
	for(cmd = buf; isspace(*cmd); cmd++)
	    ;
	for(arg = cmd; isalpha(*arg); arg++)
	    if (isupper(*arg))
		*arg += ' ';
	*arg++ = 0;
	if ((p = strrchr(arg, '\n')))
	    *p = 0;
	if (CMD("")) {
	    /* Nothing */
	} if (CMD("help")) {
	    fprintf(trout, helpmsg);
	} else if (CMD("quit")) {
	    exit(0);
	} else if (CMD("next")) {
	    break;
	} else if (CMD("leave")) {
	    if (tracedepth)
		return;
	    else
		fprintf(trout, "Not in recursive trace\n");
	} else if (CMD("cont") || CMD("rcont")) {
	    tracing = cmd[0] != PREF;
	    traceinteract = 0;
	    break;
	} else if (CMD("exit") || CMD("rexit")) {
	    int i = fundepth;
	    if(!fromenter && i > 0)
		i--;
	    entrystack[i].traceit = INTERACT;
	    tracing = cmd[0] != PREF;
	    traceinteract = 0;
	    break;
	} else if (CMD("stop")) {
	    if (!*arg) {
		/* show all stop points */
		int i;
		for(i = 0; i < nfsyms; i++) {
		    if (fsyms[i].trace & STOPAT)
			prnamemsg("Stop", &fsyms[i]);
		}
	    } else if (!forallsyms(arg, setstop)) {
		fprintf(trout, "Symbol '%s' not found\n", arg);
	    }
	} else if (CMD("nostop")) {
	    if (!forallsyms(arg, setnostop)) {
		fprintf(trout, "Symbol '%s' not found\n", arg);
	    }
	} else if (CMD("arg") || CMD("farg")) {
	    if (!fromenter) {
		fprintf(trout, "Can only be used at function entry\n");
	    } else if (!isdigit(*arg)) {
		fprintf(trout, "Missing argument number\n");
	    } else {
		int n = atoi(arg);
		struct fsym *f = findsym(entrystack[fundepth].ret);
		if (!f || n < 1 || n > f->arity) {
		    fprintf(trout, "Bad arity, should be >= 1 and <= %d\n", f->arity+1);
		} else {
		    PTR *sp = entrystack[fundepth].sp;
		    int onstop = nstop;
		    int otracing = tracing, otraceinteract = traceinteract;
		    nstop = 0; tracing = 0; traceinteract = 0;
		    fundepth++;
		    if (cmd[0] == 'f')
			forceeval(sp[n-1]);
		    else
			evaluate(sp[n-1]);
		    fundepth--;
		    nstop = onstop; tracing = otracing; traceinteract = otraceinteract;
		    fprintf(trout, "Arg %d = ", n);
		    prvalue(sp[n-1]);
		    fprintf(trout, "\n");
		}
	    }
	} else if (CMD("on")) {
	    if (!forallsyms(arg, turnon)) {
		fprintf(trout, "Symbol '%s' not found\n", arg);
	    }
	} else if (CMD("off")) {
	    if (!forallsyms(arg, turnoff)) {
		fprintf(trout, "Symbol '%s' not found\n", arg);
	    }
	} else if (CMD("where")) {
	    int i;
	    for(i = 0; i <= fundepth; i++) {
		prcall(entrystack[i].ret, entrystack[i].sp);
		fprintf(trout, "\n");
	    }
	} else if (CMD("depth")) {
	    if (isdigit(*arg))
		prdepth = atoi(arg);
	    else
		fprintf(trout, "Print depth is %d\n", prdepth);
	} else if (CMD("file")) {
	    if (*arg)
		prfilename = *arg == 'y';
	    else
		fprintf(trout, "File printing is %s\n", prfilename ? "on" : "off");
	} else {
	    fprintf(trout, "Unknown command, use help to get help\n");
	}
    }
    tracedepth--;
}

void
traceerror()
{
    fprintf(trout, "Stopping after severe error\n");
    interact(0);
}

void
tracesignal()
{
    fprintf(trout, "Stopping after signal\n");
    interact(0);
}

void
tracefail()
{
    fprintf(trout, "Stopping after fail\n");
    interact(0);
}

#else /* SOLARIS */

#endif /* SOLARIS */
