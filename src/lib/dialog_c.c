/*
** Haskell I/O.
*/

#include <stdio.h>
#include <errno.h>

#ifdef __CYGWIN32__
#include <sys/types.h>
#include <sys/socket.h>
#endif

#include "dialog.h"
#include "dialogdef.h"
#include "../runtime/node.h"
#include "../runtime/tagtable.h"

#define GET1OF1(p) getpart(p, 0, 1)
#define GET1OF2(p) getpart(p, 0, 2)
#define GET2OF2(p) getpart(p, 1, 2)

#if defined(_AIX)
#include <time.h>
#endif

#ifdef HPROFILE
#include "../runtime/sample.h"
#endif

#if defined(__svr4__)
/* Seems SCUMSOFT forgot fdopen. */
FILE *fdopen();
#endif

extern VPTR ehp, hp;

#define Stdin "stdin"
#define Stdout "stdout"
#define Stderr "stderr"
#define Stdecho "stdecho"

#define MAXNAME 5120

void (*xhandler) PROTO((int, PTR));

void (*shandler) PROTO((int, PTR));

void xsetenv PROTO((char *, char *));

extern int debug;
extern PTR concargs;

static void doreq();
static PTR nextresp;
extern Node respfail;
extern Node readchan;
extern char *progname;

#define MAXSIGS 100
#define MYSIGOFFS 1000
#define MYSIGFAIL 0
#define MYSIGFORK MAXSIGS
static PTR sigptrs[MAXSIGS+1];

static jmp_buf topjmp;

static int lastreq;

static void
gcnextresp(after)
int after;
{
    int i;
    if (after) {
	for(i = 0; i < MAXSIGS; i++)
	    if (sigptrs[i]) {
		POPPTR(sigptrs[i]);
		if (debug > 2) fprintf(stderr, "before GC signal handler %d = %lx\n", i, (unsigned long)sigptrs[i]);
	    }
	POPPTR(nextresp);
	ADDGEN(nextresp);
    } else {
	PUSHPTR(nextresp);
	for(i = MAXSIGS-1; i >= 0; i--)
	    if (sigptrs[i]) {
		PUSHPTR(sigptrs[i]);
		if (debug > 2) fprintf(stderr, "after  GC signal handler %d = %lx\n", i, (unsigned long)sigptrs[i]);
	    }
    }
}

static void
handlefail()
{
    if (sigptrs[MYSIGFAIL]) {
	if (debug) fprintf(stderr, "Handling fail ep=%lx, *ep=%lx\n", (unsigned long)ep, (unsigned long)*ep);
	longjmp(topjmp, MYSIGFAIL+MYSIGOFFS);
    }
    /* otherwise just return and exit as usual. */
}

static void
setupfail()
{
    extern void (*failhandler)();
    extern char *failstring;
    static char msg[100];

    failstring = msg;
    sprintf(msg, "%.80s: Error: ", progname);
    failhandler = handlefail;
}

/*
** This is the top loop where everything happens.
** Evaluate next request, execute it, and stick the response on the response list.
** On entry p = AP main 0
*/
void
toploop(ap)
PTR ap;
{
    static PTR *oep;
#if defined(sparc) || defined(hppa)
    static VPTR ovp;
    extern VPTR vp;
#endif
    PTR p;
    PTR q;
    int signo;
    extern Tag APG, TAG0;
    extern Node fileStdin, fileStdout, fileStderr;

    setupfail();

    fileStdin.nodeinp.file = stdin;
    fileStdout.nodeinp.file = stdout;
    fileStderr.nodeinp.file = stderr;

    p = ap;
/*fprintf(stderr, "enter p=%lx %lx\n", (unsigned long)p, (unsigned long)p->tag);*/

    signo = setjmp(topjmp);
    if (signo) {
	signo -= MYSIGOFFS;
	q = sigptrs[signo];
	if (debug) fprintf(stderr, "Using handler at %lx\n", (unsigned long)q);
	if (signo == MYSIGFAIL) {
	    q = mkap(q, *ep++);
	} else if (signo != MYSIGFORK) {
	    char nbuf[50];
	    sprintf(nbuf, "%d", signo);
	    q = mkap(q, mkestring(nbuf));
	}
	p = mknode2(&APG, (Int)q, (Int)0);

	/* Restore old stack pointer */
	ep = oep;
#if defined(sparc) || defined(hppa)
	vp = ovp;
#endif
    } else {
	oep = ep;
#if defined(sparc) || defined(hppa)
	ovp = vp;
#endif
	add_gc_hook(gcnextresp);
    }
    nextresp = mknode2(respfail.tag, respfail.node30.val0, respfail.node30.val1);
    p->node12.ptr1 = nextresp;
    if (debug) fprintf(stderr, "Starting toploop nextresp=%lx\n", (unsigned long)nextresp);
    for(;;) {
/*fprintf(stderr, "before p=%lx %lx\n", p, p->tag);*/
	p = evaluate(p);	/* evaluate request list */
/*fprintf(stderr, "after p=%lx %lx\n", p, p->tag);*/
	if (ISNIL(p))
	    break;		/* end of list */
	*--ep = TLOF(p);	/* save tail of list */
	q = evaluate(HDOF(p));	/* evaluate request */
	doreq(q);		/* execute the request */
	p = *ep++;		/* set p to tail of list */
    }
    /* Terminate responses */
    nextresp->tag = &TAG0;
    nextresp->node20.val0 = 0;
#ifdef HPROFILE
    HPSET2(nextresp);
#endif	
}

PTR
rundialogue(p)
PTR p;
{
    PTR q;
    extern Tag TAG;

    if (debug) fprintf(stderr, "Enter rundialogue ep=%lx, p=%lx\n", (unsigned long)ep, (unsigned long)p);
    *--ep = nextresp;
    nextresp = mknode2(respfail.tag, respfail.node30.val0, respfail.node30.val1);
    p->node12.ptr1 = nextresp;
    for(;;) {
	p = evaluate(p);	/* evaluate request list */
	if (ISNIL(p))
	    abort();		/* end of list */
	*--ep = TLOF(p);	/* save tail of list */
	q = evaluate(HDOF(p));	/* evaluate request */
	if (getcno(q) == RunStop)
	    break;
	doreq(q);		/* execute the request */
	p = *ep++;		/* set p to tail of list */
    }
    ep++;			/* pop tail */
    nextresp = *ep++;
    q = GET1OF1(GET1OF1(q));
    if (debug) fprintf(stderr, "Exit rundialogue ep=%lx, ans=%lx\n", (unsigned long)ep, (unsigned long)q);
    return q;
}

static double
usertime()
{
#if defined(SYSV) && !defined(irix) || defined(SOLARIS) || defined(__hpux) || defined(hpux)
    struct tms t;
#ifndef HZ
#define HZ CLK_TCK
#endif

    (void)times(&t);
    return (double)(t.tms_utime)/HZ;
#else
    struct rusage t;

    getrusage(0, &t);
    return(t.ru_utime.tv_sec + 1e-6*t.ru_utime.tv_usec);
#endif
}

void
mkresponse(n)
PTR n;
{
    extern Tag PAIR1;
    PTR tl;

    if (debug>1) {
	fprintf(stderr, "Response %d\n", getcno(n));
    }
    tl = mknode2(nextresp->tag, nextresp->node30.val0, nextresp->node30.val1);
    nextresp->tag = &PAIR1;		/* A cons */
    nextresp->node12.ptr0 = n;		/* The response */
    nextresp->node12.ptr1 = tl;		/* New failure */
    nextresp = tl;			/* Next response goes here */
}

void
mkerrresp(k, msg)
int k;
char *msg;
{
  if(debug) fprintf(stderr, "mkerrresp(%d,\"%s\")\n",k,msg);
    mkresponse(mkconstr(Failure, 1, mkconstr(k, 1, mkestring(msg))));
}

extern int errno;
void
fileerror(r, fname, op)
int r;
char *fname, *op;
{
    char buf[10000];

    if (fname)
	sprintf(buf, "%s: %s: %s", op, sys_errlist[errno], fname);
    else
	sprintf(buf, "%s: %s", op, sys_errlist[errno]);
    mkerrresp(r, buf);
}

/* Output a list of char, assumes type correctness */
int
outstring(f, p)
FILE *f;
PTR p;
{
    *--ep = p;
    printtop(f);
    return 0;
}

static struct chnames {
    struct chnames *next;
    char *chname;
} *chused = 0;

static int
channelused(s)
char *s;
{
    struct chnames *p;

    for(p = chused; p; p = p->next)
	if (strcmp(s, p->chname) == 0)
	    return 1;
    p = (struct chnames *)malloc(sizeof(struct chnames));
    p->next = chused;
    chused = p;
    p->chname = malloc(strlen(s)+1);
    strcpy(p->chname, s);
    return 0;
}

void
mksuccess()
{
    mkresponse(mkconstr(RSuccess, 0));
}

static void
mkstrresp(s)
PTR s;
{
    mkresponse(mkconstr(Str, 1, s));
}

static void
mkintresp(i)
Int i;
{
    mkresponse(mkconstr(IntResp, 1, mkint(i)));
}

/*
** Binary value are internally coded as lists.
** This makes showBin(::*a->Bin->Bin) and readBin(::Bin->(*a,Bin) easy (just cons or split).
** Externally this list is flattened, and the tags are kept to make reading possible.
*/
#define T_INT 0xc0
#define T_DFLOAT 0xc1
#define T_CHAR 0xc2
#define T_TAG 0xc3
#define T_TAG0 0xc4
#define T_PAIR0 0xc5
#define T_PAIR1 0xc6
#define T_PAIR2 0xc7
#define T_PAIR3 0xc8
#define T_PAIR4 0xc9
#define T_VEK 0xca
#define T_BVEK 0xcb
#define T_DVEK 0xcc
#define T_BIGNUM 0xcd
#define T_PAIR 0xce
#define T_SFLOAT 0xcf

#define T(x) (b->tag->tagNumber == x)
static int
writebin1(f, b)
FILE *f;
PTR b;
{
    int n, i;

 top:
    b = evaluate(b);
    /* Don't bother with error check */
    if (T(O_INT)) {
	putc(T_INT, f);
	putw(b->node20.val0, f);
    } else if (T(O_DFLOAT)) {
	putc(T_DFLOAT, f);
	putw(b->node30.val0, f);
	putw(b->node30.val1, f);
    } else if (T(O_SFLOAT)) {
	putc(T_SFLOAT, f);
	putw(b->node20.val0, f);
    } else if (T(O_CHAR)) {
	putc(T_CHAR, f);
	putw(b->node20.val0, f);
    } else if (T(O_TAG)) {
	putc(T_TAG, f);
	putw(b->node21.val0, f);
	b = b->node21.ptr0;
	goto top;		/* avoid tail rec */
    } else if (T(O_BIGNUM)) {
	putc(T_BIGNUM, f);
	b = b->node11.ptr0;
	goto top;		/* avoid tail rec */
    } else if (T(O_TAG0)) {
	putc(T_TAG0, f);
	putw(b->node20.val0, f);
    } else if (T(O_PAIR)) {
	putc(T_PAIR, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_PAIR0)) {
	putc(T_PAIR0, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_PAIR1)) {
	putc(T_PAIR1, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_PAIR2)) {
	putc(T_PAIR2, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_PAIR3)) {
	putc(T_PAIR3, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_PAIR4)) {
	putc(T_PAIR4, f);
	*--ep = b;
	writebin1(f, b->node12.ptr0);
	b = *ep++;
	b = b->node12.ptr1;
	goto top;		/* avoid tail rec */
    } else if (T(O_VEK)) {
	putc(T_VEK, f);
	n = b->nodevek.size;
	putw(n, f);
	*--ep = b;
	for(i = 0; i < n; i++) {
	    writebin1(f, b->nodevek.ptr[i]);
	    b = *ep;
	}
	ep++;
    } else if (T(O_DVEK)) {
	putc(T_DVEK, f);
	n = b->nodedvek.size;
	putw(n, f);
	for(i = 0; i < n; i++) {
	    putw(b->nodedvek.val[i], f);
	    b = *ep;
	}
    } else {
	fprintf(stderr, "Bad tag in writebin %x %x %x\n", (unsigned long)b, (unsigned long)b->tag, b->tag->tagNumber);
	exit(1);
    }
    return 0;
}
#undef T

extern Tag TAG0;
#ifdef HPROFILE
static struct { Node20 n20; struct hprofinfo *hi; int slop; } snil = { { &TAG0, 0 }, SLOP1, 0 };
#else
static Node20 snil = { &TAG0, 0 };
#endif
#define SNIL ((PTR)&snil)

static void
readbin1(f, rp, k)
FILE *f;
PTR rp;				/* result of reading put in rp->node12.ptrN */
int k;
{
    extern Tag INT, DFLOAT, CHAR, TAG, BIGNUM, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4, VEK, DVEK, TAG0, SFLOAT;
    int n, i;
    PTR rr;

#define RET (*(k ? &rp->node12.ptr1 : &rp->node12.ptr0))

 top:
    if (NEEDGC) { 
	PUSHPTR(rp); 
	DOGC; 
	POPPTR(rp);
    }
    switch(getc(f)) {
    case EOF:
	/* Don't modify *rp, indicates EOF */
	break;
    case T_INT:
	RET = mknode1(&INT, getw(f));
	break;
    case T_SFLOAT:
	RET = mknode1(&SFLOAT, getw(f));
	break;
    case T_DFLOAT:
	i = getw(f);
	RET = mknode2(&DFLOAT, i, getw(f));
	break;
    case T_CHAR:
	RET = mknode1(&CHAR, getw(f));
	break;
    case T_TAG:
	RET = rr = mknode11(&TAG, getw(f), SNIL);
	rp = rr; k = 1;
	goto top;
    case T_BIGNUM:
	RET = rr = mknode1p(&BIGNUM, SNIL);
	rp = rr; k = 0;
	goto top;
    case T_TAG0:
	RET = rr = mknode1(&TAG0, getw(f));
	break;
    case T_PAIR:
	RET = rr = mknode2p(&PAIR, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_PAIR0:
	RET = rr = mknode2p(&PAIR0, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_PAIR1:
	RET = rr = mknode2p(&PAIR1, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_PAIR2:
	RET = rr = mknode2p(&PAIR2, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_PAIR3:
	RET = rr = mknode2p(&PAIR3, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_PAIR4:
	RET = rr = mknode2p(&PAIR4, SNIL, SNIL);
	PUSHPTR(rr);
	readbin1(f, rr, 0);
	POPPTR(rp);
	k = 1;
	goto top;
    case T_DVEK:
	n = getw(f);
	{
	    PTR ohp;

	    if (NEEDGCN(SIZEvek(n))) {
		PUSHPTR(rp);
		DOGC;
		POPPTR(rp);
	    }
	    ohp = (PTR)hp;
	    RET = ohp;
	    hp += n+2;
	    ohp->tag = &DVEK;
	    ohp->nodedvek.size = n;
	    for(i = 0; i < n; i++) {
		ohp->nodedvek.val[i] = getw(f);
	    }
	}
	break;
    case T_VEK:
	{
	    PTR ohp, tmp;

	    n = getw(f);		/* number of elems */
	    if (NEEDGCN(SIZEvek(n))+SIZEconstr(1)) {
		PUSHPTR(rp);
		DOGC;
		POPPTR(rp);
	    }
	    ohp = (PTR)hp;
	    RET = ohp;
	    hp += n+2;
	    ohp->tag = &VEK;
	    ohp->nodevek.size = n;
	    for(i = 0; i < n; i++) {
		ohp->nodevek.ptr[i] = SNIL;
	    }
	    tmp = mknode2p(&PAIR0, SNIL, SNIL); /* dummy to stuff result into */
	    for(i = 0; i < n; i++) {
		PUSHPTR(ohp);
		PUSHPTR(tmp);
		readbin1(f, tmp, 0);
		POPPTR(tmp);
		POPPTR(ohp);
		ohp->nodevek.ptr[i] = tmp->node11.ptr0;
	    }
	}
	break;
    default:
	fprintf(stderr, "Bad tag in readbin\n");
	exit(1);
    }
}
	
static PTR
readbin(f)
FILE *f;
{
    PTR b, p;
    extern Tag TAG0, PAIR1;

    if (debug) fprintf(stderr, "Start readbin\n");
    b = mknode2p(&PAIR1, SNIL, SNIL);
    PUSHPTR(b);
    for(p = b;;) {
	PUSHPTR(p);
	readbin1(f, p, 0);
	POPPTR(p);
	if (p->node12.ptr0 == SNIL) {
	    p->tag = &TAG0;
	    p->node20.val0 = 0;
	    POPPTR(b);
	    if (debug) fprintf(stderr, "End readbin\n");
	    return b;
	}
	TLOF(p) = mknode2p(&PAIR1, SNIL, SNIL); /* new tail */
	p = TLOF(p);
    }
}

static int
writebin(f, p)
FILE *f;
PTR p;
{
    PTR cp, q;

    if (debug) fprintf(stderr, "Start writebin\n");
    p = evaluate(p);
    if (ISNIL(p))
	return 0;		/* end of list */
    cp = HDOF(p);
    *--ep = p;		/* save p */
    for(;;) {
	if (writebin1(f, cp) < 0) {
	    ep++;
	    return -1;
	}
	p = *ep;		/* restore p */
	q = evaluate(TLOF(p));  /* evaluate rest */
	p = *ep;		/* restore p */
	if (ISNIL(q))
	    break;
	cp = HDOF(q);
	p->node12.ptr1 = q->node12.ptr1; /* copy this node on top of old cons to avoid space leak. */	
    }
    ep++;
    if (debug) fprintf(stderr, "End writebin\n");
    return 0;
}    

static int
canreadfrombuffer(f)
FILE *f;
{
#if defined(linux)
    return f->_IO_read_ptr < f->_IO_read_end;
#else
#if defined(__386BSD__) || defined(__CYGWIN32__)
#define _cnt _r
#endif
    /* This is utterly unportable! */
    return f->_cnt > 0;
#endif
}

static void
sighndl(sig, code, scp)
int sig, code;
int scp;			/* wrong! */
{
#if defined(HPUX) || defined(SOLARIS)
    signal(sig, sighndl); /* gotta re-establish the interrupt handler with braindead signal semantics */
#endif
    sigsetmask(sigsetmask(~0) & ~sigmask(sig)); /* Reenable signal */
    if (debug) fprintf(stderr, "sighndl %d %lx\n", sig, (unsigned long)sigptrs[sig]);
    longjmp(topjmp, (Int)(sig+MYSIGOFFS));
}

#if 1
#define DEB(x) do ; while(0)
#else
#define DEB(x) if (debug) x
#endif

#define IS_DEVFD(buf) (strncmp(buf, "/dev/fd", 7) == 0 && '0' <= buf[7] && buf[7] <= '9')
#define GET_DEVFD(buf) (atoi(buf+7))

static void
doreq(p)
PTR p;
{
    static int last_errno;
    char buf[MAXNAME], buf2[MAXNAME];
    PTR namep, stringp;
    FILE *f;
    int r, t;
    char *mode, *s;
    struct stat sb;
    extern Tag INPUT, STRINGN;
    char *oper = "?";

    if (debug>1) {
	fprintf(stderr, "Request %d\n", getcno(p));
    }
    if (hp > ehp) {
	*--ep = p;
	dogc();
	if (debug)
	    fprintf(stderr, "GC in doreq\n");
	p = *ep++;
    }
    errno = 0;
    switch(lastreq = t = getcno(p)) {
    case ReadFileScattered:
	{ int noffs, i, *offs; PTR q;
	oper = "ReadFileScattered";
	namep = GET1OF2(p);
	evalstring(namep, buf, sizeof buf);

	*--ep = p;
	q = GET2OF2(p);
	/* evaluate spine and count # of elems */
	for(noffs = 0;;noffs++) {
	    q = evaluate(q);
	    if (ISNIL(q))
		break;		/* end of list */
	    q = TLOF(q);
	}
	offs = (int *)malloc(noffs * sizeof(int));
	p = *ep++;
	q = GET2OF2(p);
	*--ep = q;
	/* get the elems */
	for(i = 0; i < noffs; i++) {
	    offs[i] = INTOF(evaluate(HDOF(*ep)));
	    if (debug)
	        fprintf(stderr, "scatter offset %d = %d\n", i, offs[i]);
	    *ep = TLOF(*ep);
	}
	ep++;
	if (NEEDGCN(noffs*(2*SIZEcons)))
	    DOGC;

	f = fopen(buf, "r");
	if (debug)
	    fprintf(stderr, "fopen(%s,\"r\")=%lx\n", buf, (unsigned long)f);
	if (f == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    f = fopen(buf, "r");
	}
	if (f == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
	    addgcfile(f, (DIR*)0);
	    q = mknil();
	    for(i = noffs-1; i >= 0; i--) {
		q = mkcons(mknode2(&INPUT, (Int)f, (Int)offs[i]), q);
	    }
	    mkresponse(mkconstr(StrList, 1, q));
	}
        }
	break;

    case ReadFileFast:
	oper = "ReadFileFast";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	f = fopen(buf, "r");
	if (f == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    f = fopen(buf, "r");
	}
	if (f == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
	    long size;
	    char *cont;

	    size = fseek(f, 0, 2);
	    if (size < 0)
		goto useread;
	    size = ftell(f);
	    if (size < 0)
		goto useread;
	    cont = malloc(size);	/* this memory is now lost :-( */
	    if (buf == 0)
		goto useread;
	    fseek(f, 0, 0);
	    size = fread(cont, 1, size, f);
	    fclose(f);
	    mkresponse(mkconstr(Str, 1, mknode2(&STRINGN, (Int)cont, (Int)size)));
	}
	break;

    case ReadFile:
	oper = "ReadFile";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
  inopen:
	f = fopen(buf, "r");
	if (debug)
	    fprintf(stderr, "fopen(%s,\"r\")=%lx\n", buf, (unsigned long)f);
	if (f == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    f = fopen(buf, "r");
	}
  doin:
	if (f == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
  useread:
	    addgcfile(f, (DIR*)0);
	    mkresponse(mkconstr(Str, 1, mknode2(&INPUT, (Int)f, (Int)-1)));
	}
	break;
    case WriteFile:
	oper = "WriteFile";
	mode = "w";
	goto wrapp;
    case AppendFile:
	oper = "AppendFile";
	mode = "a";
  wrapp:
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
  outopen:
	f = fopen(buf, mode);
	if (debug)
	    fprintf(stderr, "fopen(%s,%s)=%lx\n", buf, mode, (unsigned long)f);
	if (f == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    f = fopen(buf, mode);
	}
	if (f == NULL)
	    fileerror(WriteError, buf, oper);
	else {
  doout:
	    r = outstring(f, stringp);
	    if(debug) fprintf(stderr,"outstring: %d\n",r);
	    if (r < 0)
		fileerror(WriteError, buf, oper);
	    else
		mksuccess();
	    if (f != stdout && f != stderr)
		fclose(f);
	    else
		fflush(f);
	}
	break;
    case ReadChan:
	oper = "ReadChan";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (channelused(buf)) {
	    mkerrresp(ReadError, "Channel already used");
#ifndef HASDEVFD
	} else if (IS_DEVFD(buf)) {
	    int fd = GET_DEVFD(buf);
	    f = fdopen(fd, "r");
	    if (f == NULL && errno == EMFILE) {
		dogc();
		f = fdopen(fd, "r");
	    }
	    goto doin;
#endif
	} else {
	    if (strcmp(buf, Stdin) == 0) {
		f = stdin;
		goto doin;
	    } else
		goto inopen;
	}
	break;
    case AppendChan:
	oper = "AppendChan";
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
	if (strcmp(buf, Stdout) == 0) {
	    f = stdout;
	    goto doout;
	} else if (strcmp(buf, Stderr) == 0) {
	    f = stderr;
	    goto doout;
	} else if (strcmp(buf, Stdecho) == 0) {
	    f = stderr;		/* The best we can do under UNIX */
	    goto doout;
#ifndef HASDEVFD
        } else if (IS_DEVFD(buf)) {
	    int fd, d;
	    fd = GET_DEVFD(buf);
	    d = dup(fd);
	    if (d == -1 && errno == EMFILE) {
		dogc();
		d = dup(fd);
	    }
	    if (d == -1) {
		fileerror(WriteError, buf, oper);
		break;
	    }
	    f = fdopen(d, "w");
	    if (f == NULL) {
		close(d);
		fileerror(WriteError, buf, oper);
		break;
	    }
	    (void)fseek(f,0,2);
	    goto doout;
#endif
	} else
	    mode = "a";
	    goto outopen;
	break;
    case StatusChan:
	oper = "StatusChan";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (strcmp(buf, Stdout) == 0 || strcmp(buf, Stderr) == 0 || strcmp(buf, Stdecho) == 0) {
	    mkstrresp(mkcstring("0 0"));
	} else {
	    mkerrresp(SearchError, "No status available");
	}
	break;
    case DeleteFile:
	oper = "DeleteFile";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (unlink(buf) < 0) {
	    fileerror(errno == ENOENT ? SearchError : WriteError, buf, oper);
	} else {
	    mksuccess();
	}
	break;
    case StatusFile:
	oper = "StatusFile";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (stat(buf, &sb) < 0) {
	    fileerror(SearchError, buf, oper);
	} else {
	    char s[200];

	    s[0] = (sb.st_mode & S_IFMT) == S_IFREG ? 'f' : (sb.st_mode & S_IFMT) == S_IFDIR ? 'd' : 'u';
	    s[1] = access(buf, R_OK) == 0 ? 'r' : '-'; /* Must not use access if setuid!  */
	    s[2] = access(buf, W_OK) == 0 ? 'w' : '-';
	    s[3] = ' ';
	    sprintf(s+4, "%d %d %d %d %d %d %d %d %d %d %d", sb.st_dev, sb.st_ino, sb.st_mode,
		    sb.st_nlink, sb.st_uid, sb.st_gid, sb.st_rdev, (long)sb.st_size, sb.st_atime,
		    sb.st_mtime, sb.st_ctime);
	    mkstrresp(mkestring(s));
	}
	break;
    case CreateDirectory:
	oper = "CreateDirectory";
	{ int mode; char buf1[100];
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
	evalstring(stringp, buf1, sizeof buf1);
	if (sscanf(buf1, "%o", &mode) < 1) mode = 0777;
	if (mkdir(buf, mode) < 0) {
	    fileerror(OtherError, buf, oper);
	} else {
	    mksuccess();
	}
        }
	break;
    case Sleep:
	{
	    double d;
	    struct timeval tv;
	    d = getdouble(evaluate(GET1OF1(p)));
	    tv.tv_sec = d;
	    tv.tv_usec = 1e6 * (d - tv.tv_sec);
	    /* use select to sleep for short intervals, does not restart if other signals should occur */
	    select(0, (fd_set *)0, (fd_set *)0, (fd_set *)0, &tv);
	    mksuccess();
	}
	break;
    case ChangeDirectory:
	oper = "ChangeDirectory";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (chdir(buf) < 0) {
	    fileerror(OtherError, buf, oper);
	} else {
	    mksuccess();
	}
	break;
    case CreateProcess:
	oper = "CreateProcess";
     {  PTR pp;
	pp = GET1OF1(p);
	/* pp points to a function that should be run. Fork and try to run it. */
	fflush(stdout);
	switch(fork()) {
	case -1:
	    fileerror(OtherError, (char *)0, oper);
	    break;
	case 0:
	    /* Child */
	    freopen("/dev/null", "r", stdin); /* To avoid problems */
	    sigptrs[MYSIGFORK] = pp;
	    longjmp(topjmp, (Int)(MYSIGFORK+MYSIGOFFS));
	    break;
	default:
	    /* parent: No wait!  Trust init to take care of that.  Could disinherit child by double fork. */
	    mksuccess();
	    break;
	}
	break; }
    case Echo:
	oper = "Echo";
	{
	    static int echodone = 0;
	    if (echodone) {
		fileerror(OtherError, (char *)0, oper);
	    } else {
		set_tty(!INTOF(evaluate(GET1OF1(p))));
		echodone++;
		mksuccess();
	    }
	}
	break;
    case GetEnv:
	oper = "GetEnv";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if ((s = getenv(buf))) {
	    mkstrresp(mkcstring(s));
	} else {
	    mkerrresp(SearchError, "");
	}
	break;
    case SetEnv:
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
	{
	    char buf1[MAXNAME];
	    evalstring(stringp, buf1, sizeof buf1);
	    xsetenv(buf, buf1);
	}
	mksuccess();
	break;
    case GetArgs:
	mkresponse(mkconstr(StrList, 1, concargs));
	break;
    case WriteBinFile:
	mode = "w";
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	goto bwrapp;
    case AppendBinFile:
	oper = "AppendBinFile";
	mode = "a";
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
  bwrapp:
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
  boutopen:
	f = fopen(buf, mode);
	if (f == NULL && errno == EMFILE) {
	    dogc();
	    f = fopen(buf, mode);
	}
	if (f == NULL)
	    fileerror(WriteError, buf, oper);
	else {
  bdoout:
	    r = writebin(f, stringp);
	    if (r < 0)
		fileerror(WriteError, buf, oper);
	    else
		mksuccess();
	    if (f != stdout && f != stderr)
		fclose(f);
	    else
		fflush(f);
	}
	break;
    case AppendBinChan:
	oper = "AppendBinChan";
	namep = GET1OF2(p);
	stringp = GET2OF2(p);
	*--ep = stringp;
	evalstring(namep, buf, sizeof buf);
	stringp = *ep++;
	if (strcmp(buf, Stdout) == 0) {
	    f = stdout;
	    goto bdoout;
	} else if (strcmp(buf, Stderr) == 0) {
	    f = stderr;
	    goto bdoout;
	} else if (strcmp(buf, Stdecho) == 0) {
	    f = stdout;		/* The best we can do under UNIX */
	    goto bdoout;
	} else
	    mode = "a";
	    goto boutopen;
	break;
    case ReadBinFile:
	oper = "ReadBinFile";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
  binopen:
	f = fopen(buf, "r");
	if (f == NULL && errno == EMFILE) {
	    dogc();
	    f = fopen(buf, "r");
	}
	if (f == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
  bdoin:
/*	    addgcfile(f, (DIR*)0); should not be added, ref is lost in readbin */
	    mkresponse(mkconstr(Bn, 1, readbin(f)));
	    if (f != stdin)
		fclose(f);
	}
	break;
    case ReadBinChan:
	oper = "ReadBinChan";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (channelused(buf)) {
	    mkerrresp(ReadError, "Channel already used");
	} else {
	    if (strcmp(buf, Stdin) == 0) {
		f = stdin;
		goto bdoin;
	    } else
		goto binopen;
	}
	break;
    case ReadChannels:
	oper = "ReadChannels";
	{
	    PTR nlist = GET1OF1(p), pp, vec;
	    FILE *table[1000], *f;
	    int nf, i;
	    PTR tp = SNIL;
	    PTR ttp = SNIL;
	    extern Tag VEK;

	    /* evaluate the list and each string in it */
	    *--ep = nlist;
	    /* should stack tp&ttp here if gc should occur!!! */
	    for(pp = nlist, nf = 0;;) {
		pp = evaluate(pp);
		if (ISNIL(pp))
		    break;		/* end of list */
		*--ep = pp;
		evalstring(HDOF(pp), buf, sizeof buf); /* evaluate string */
		DEB(printf("ReadChannels %s\n", buf));
		if (strncmp(buf, "TICK:", 5) == 0) {
		    double timeout = fabs(atof(buf+5));
		    int sec = timeout;
		    int usec = (int)(1e6 * (timeout - sec));
		    struct timeval tv;

		    gettimeofday(&tv, (struct timezone *)0);
		    tp = HDOF(pp);
		    ttp = mkcons(mkpair(mkint(sec),       mkint(usec)),
				 mkpair(mkint(tv.tv_sec), mkint(tv.tv_usec)));
		    DEB(printf("prepare timeout %d %d, %d %d\n", sec, usec, tv.tv_sec, tv.tv_usec));
		} else if (strncmp(buf, "TIMEOUT:", 8) == 0) {
		    /* decode timer value */
		    double timeout = fabs(atof(buf+8));
		    int sec = timeout;
		    int usec = (int)(1e6 * (timeout - sec));
		    tp = HDOF(pp);
		    if (hp + 50 > ehp)
			dogc(); /* to avoid gc during mknodes below */
		    ttp = mkpair(mkint(sec), mkint(usec));
		    DEB(printf("prepare timeout %d %d\n", sec, usec));
		} else {
		    if (channelused(buf)) {
			mkerrresp(ReadError, "Channel already used");
			goto rcerr;
		    }
		    if (strcmp(buf, Stdin) == 0) {
			f = stdin;
#ifndef HASDEVFD
		    } else if (IS_DEVFD(buf)) {
			int fd = GET_DEVFD(buf);
			f = fdopen(fd, "r");
			if (f == NULL && errno == EMFILE) {
			    dogc();
			    f = fdopen(fd, "r");
			}
#endif
		    } else {
			DEB(printf("fopen %s\n", buf));
			f = fopen(buf, "r");
			if (f == NULL && errno == EMFILE) {
			    dogc();
			    f = fopen(buf, "r");
			}
		    }
		    if (f == NULL) {
			fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
			goto rcerr;
		    }
		    table[nf++] = f;
		    setbuf(f, NULL);
		    DEB(printf("opened=%s %d\n", buf, fileno(f)));
		}
		pp = *ep++;
		pp = TLOF(pp);
	    }
	    if (hp + nf*7 + 10 > ehp)
		dogc();
	    nlist = *ep++;
	    vec = (PTR)hp;
	    hp += nf+3;
	    vec->tag = &VEK;
	    vec->nodevek.size = nf+1;
	    vec->nodevek.ptr[0] = mkpair(tp, ttp);
	    /* Now build a vector */
	    for(i = 0, pp = nlist; i < nf; i++) {
		addgcfile(table[i],(DIR*)0);
		vec->nodevek.ptr[i+1] = mkpair(HDOF(pp), mknode2(&INPUT, (Int)table[i], (Int)-1));
		pp = TLOF(pp);
	    }
	    mkresponse(mkconstr(Tagg, 1, mkap(&readchan, vec)));
	    break;
	rcerr:
	    ep += 2;
	    for(i = 0; i < nf; i++)
		fclose(table[i]);
	}
	break;
    case GetTime:
	{
	    struct timeval tv;
	    gettimeofday(&tv, (struct timezone *)0);
	    mkresponse(mkconstr(Dbl, 1, mkdouble(tv.tv_sec + tv.tv_usec/1e6)));
	    break;
	}
    case GetLocalTime:
	{
	    time_t now;
	    struct tm gm, lt;
	    int d;
	    struct timeval tv;

	    gettimeofday(&tv, (struct timezone *)0);
	    now = tv.tv_sec;
	    gm = *gmtime(&now);
	    lt = *localtime(&now);
	    /* compute the difference between local and UTC to get the offset */
	    if (lt.tm_year > gm.tm_year) {
		/* Year has changed, count up month instead */
		/* lt.tm_year-- */
		lt.tm_mon++;
	    } else if (lt.tm_year < gm.tm_year) {
		/* Year has changed, count up month instead */
		/* gm.tm_year-- */
		gm.tm_mon++;
	    }
	    /* Years are now equal, months in 1-13 range */
	    if (lt.tm_mon > gm.tm_mon) {
		/* Month has changed ... */
		/* lt.tm_mon-- */
		lt.tm_mday++;
	    } else if (lt.tm_mon < gm.tm_mon) {
		/* Month has changed ... */
		/* gm.tm_mon-- */
		gm.tm_mday++;
	    }
	    /* Months are now equal, days in 1-32 range */
	    d = (((lt.tm_mday - gm.tm_mday)*24 + lt.tm_hour - gm.tm_hour)*60 + lt.tm_min - gm.tm_min)*60 + lt.tm_sec - gm.tm_sec;
	    mkresponse(mkconstr(Dbl, 1, mkdouble(tv.tv_sec + d + tv.tv_usec/1e6)));
	    break;
	}
    case GetCpuTime:
	{
	    double usertime();

	    mkresponse(mkconstr(Dbl, 1, mkdouble(usertime())));
	    break;
	}
    case DeleteDirectory:
	oper = "DeleteDirectory";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (rmdir(buf) < 0) {
	    fileerror(OtherError, buf, oper);
	} else {
	    mksuccess();
	}
	break;
    case System:
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	{
	    char b[10];
	    r = system(buf);
	    if (r == 0)
		mksuccess();
	    else {
		sprintf(b, "System: Return code=%d", r);
		mkerrresp(OtherError, b);
	    }
	}
	break;
    case ReadDirectory: 
        oper = "ReadDirectory";
    {
	DIR *d;
	extern Tag INPUTD;
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	d = opendir(buf);
	if (debug)
	    fprintf(stderr, "opendir(%s)=%lx\n", buf, (unsigned long)d);
	if (d == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    d = opendir(buf);
	}
	if (d == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
	    addgcfile((FILE*)0, d);
	    mkresponse(mkconstr(StrList, 1, mknode2(&INPUTD, (Int)d, 0)));
	}
	break; }

    /* X command and event handling */
    case XCommand:
    case XRequest:
	if (xhandler) {
	    (*xhandler)(t, GET1OF1(p));
	       /* Note: XRequest and XCommand both have one argument. */
	} else {
	    mkerrresp(OtherError, "No Xlib");
	}
	break;

    case SocketRequest:
    case Select:
	if(shandler) {
	  (*shandler)(t, GET1OF1(p));
	       /* Note: XRequest and XCommand both have one argument. */
	} else {
	    mkerrresp(OtherError, "No socketlib");
	}
	break;
    case GetAsyncInput:
	if(shandler) {
	  (*shandler)(t, NULL);
	} else {
	    mkerrresp(OtherError, "No socketlib");
	}
	break;
    case GetProgName:
	mkstrresp(mkcstring(progname));
	break;

    case SigAction:
	oper = "SigAction";
	{
	    int signo;
	    PTR act;
	    void (*r)();
	    void (*fun)() = 0;
	    PTR sigptr = 0;
	    PTR oldsigptr;

	    p = evalargs(p, 2);
	    signo = INTOF(GET1OF2(p));
	    act = GET2OF2(p);

	    switch(signo) {
	    case SIGHUP:
	    case SIGINT:
	    case SIGQUIT:
	    case SIGPIPE:
	    case SIGTERM:

#ifdef SIGSYS
	    case SIGSYS:
#endif
#ifdef SIGURG
#ifndef linux
	    case SIGURG:
#endif
#endif
#ifdef SIGCONT
	    case SIGCONT:
#endif
#ifdef SIGIO
	    case SIGIO:
#endif
#ifdef SIGXCPU
	    case SIGXCPU:
#endif
#ifdef SIGXFSZ
	    case SIGXFSZ:
#endif
#ifdef SIGWINCH
	    case SIGWINCH:
#endif
#ifdef SIGLOST
	    case SIGLOST:
#endif
#ifdef SIGUSR1
	    case SIGUSR1:
#endif
#ifdef SIGUSR2
	    case SIGUSR2:
#endif
	    case MYSIGFAIL:		/* fail handler */
		/* more signals could be allowed ... */
		switch(getcno(act)) {
		case SAIgnore:  fun = (void (*)())SIG_IGN; sigptr = 0; break;
		case SADefault: fun = (void (*)())SIG_DFL; sigptr = 0; break;
		case SACatch:   fun = sighndl; sigptr = GET1OF1(act); break;
		default:
		    fprintf(stderr, "Impossible SigAction\n");
		    finish(1);
		    break;
		}
		if (signo != MYSIGFAIL)
		    r = (void (*)())signal(signo, fun);
		else
		    r = SIG_DFL;
		oldsigptr = sigptrs[signo];
		sigptrs[signo] = sigptr;
		if (debug) fprintf(stderr, "SigAction signo=%d fun=%lx handler=%lx old=%lx\n", signo, (unsigned long)fun, (unsigned long)sigptr, (unsigned long)r);
		mkresponse(mkconstr(SigAc, 1, oldsigptr ? mkconstr(SACatch, 1, oldsigptr) :
				                          mkconstr(r == SIG_IGN ? SAIgnore : SADefault, 0)));
		break;
	    default: /* signal not allowed */
		fileerror(OtherError, (char *)0, oper);
		break;
	    }
	}
	break;

    case Exit:
	r = (int)INTOF(evaluate(GET1OF1(p)));
	if (debug)
	    fprintf(stderr, "Exit %d\n", r);
	finish(r);
	/* never returns */
	break;

#if 0
    case OpenSocket:
    case Shutdown:
	if (sockethandler) {
	    (*sockethandler)(t, p);
	} else {
	    mkerrresp(OtherError, "No socket lib");
	}
	break;
#endif

    case RenameFile:
	oper = "RenameFile";
	namep = GET1OF2(p);
	evalstring(namep, buf, sizeof buf);
	namep = GET2OF2(p);
	evalstring(namep, buf2, sizeof buf2);
	if (rename(buf, buf2) < 0) {
	    fileerror(errno == ENOENT ? SearchError : WriteError, buf, oper);
	} else {
	    mksuccess();
	}
	break;
    case GetCurrentDirectory:
	oper = "GetCurrentDirectory";
	if (getcwd(buf, sizeof buf)) {
	    mkstrresp(mkestring(buf));
	} else {
	    fileerror(OtherError, "", oper);
	}
	break;

#define CHECKFILE(f) if ((f)==0 || (f)==(FILE*)-1L) mkerrresp(OtherError, "Not open")
#define GETFILE(p) ((p)->nodeinp.file)

    case H_OpenFile:
	namep = GET1OF2(p);
	evalstring(namep, buf, sizeof buf);
	p = evaluate(GET2OF2(p));
	switch (INTOF(p)) {
	case READMODE: mode = "r"; break;
	case WRITEMODE: mode = "w"; break;
	case READWRITEMODE: mode = "r+"; break;
	case APPENDMODE: mode = "a"; break;
	case READMODE + BINARYMODE: mode = "rb"; break;
	case WRITEMODE + BINARYMODE: mode = "wb"; break;
	case READWRITEMODE + BINARYMODE: mode = "rb+"; break;
	case APPENDMODE + BINARYMODE: mode = "ab"; break;
	default: fprintf(stderr, "Bad H_OpenFile %d\n", INTOF(p)); break;
	}
	if (debug>1) fprintf(stderr, "H_OpenFile %s %s\n", buf, mode);
	f = fopen(buf, mode);
	if (f == NULL && errno == EMFILE) {
	    if (debug>1)
		fprintf(stderr, "Too many open files, trying GC\n");
	    dogc();
	    f = fopen(buf, mode);
	}
	if (f == NULL)
	    fileerror(errno == ENOENT ? SearchError : ReadError, buf, oper);
	else {
	    if (debug>1) fprintf(stderr, "H_OpenFile() = %lx\n", (unsigned long)f);
	    addgcfile(f, (DIR*)0);
	    mkresponse(mkconstr(Fil, 1, mknode2(&INPUT, (Int)f, (Int)-1)));
	}
	break;

    case H_Close:
	f = GETFILE(GET1OF1(p));
	if (debug>1) fprintf(stderr, "H_Close %lx\n", (unsigned long)f);
	p = GET1OF1(p);
	p->nodeinp.file = 0;	/* mark as closed */
	if (f==0)
	    mkerrresp(OtherError, "IllegalOperation");
	else if (f==(FILE*)-1L || closefile(f) == 1)
	    mksuccess();
	else
	    fileerror(OtherError, "");
	break;
    case H_FileSize:
	f = GETFILE(GET1OF1(p));
	{
	    int fd = fileno(f);
	    struct stat sb;
	    if (fstat(fd, &sb) < 0)
		fileerror(OtherError, "");
	    else {
		if ((sb.st_mode & S_IFMT) == S_IFREG)
		    mkintresp((Int)sb.st_size);
		else
		    mkerrresp(OtherError, "InappropriateType");
	    }
	}
	break;
    case H_IsEOF:
	f = GETFILE(GET1OF1(p));
	if (debug>1) fprintf(stderr, "H_IsEOF %lx\n", (unsigned long)f);
	CHECKFILE(f);
	if (feof(f))
	    mkintresp(1);
	/* Even if the EOF indicator is set there may not be any further input,
	   only way to find out is to try and read. */
	r = getc(f);
	if (r < 0) {
	    mkintresp(1);
	} else {
	    ungetc(r, f);
	    mkintresp(0);
	}
	break;
    case H_SetBuffering:
	f = GETFILE(GET1OF2(p));
	if (debug>1) fprintf(stderr, "H_SetBuffering %lx\n", (unsigned long)f);
	CHECKFILE(f);
	{
	    int bmode = INTOF(evaluate(GET2OF2(p)));

	    if (bmode == NOBUFFERING) {
		fflush(f);
		setvbuf(f, NULL, _IONBF, 0);
	    } else if (bmode == LINEBUFFERING) {
		fflush(f);
		setvbuf(f, NULL, _IOLBF, 0);
	    } else if (bmode == BLOCKBUFFERING) {
		fflush(f);
		setvbuf(f, NULL, _IOFBF, 0);
	    } else if (bmode < 0) {
		bmode = -bmode;
		fflush(f);
		setvbuf(f, malloc(bmode), _IOFBF, bmode); /* XXX malloc may fail */
	    }
	    mksuccess();
	}
	break;
    case H_GetBuffering:
	mkerrresp(OtherError, "IllegalOperation");
	break;
    case H_Flush:
	f = GETFILE(GET1OF1(p));
	if (debug>1) fprintf(stderr, "H_Flush %lx\n", (unsigned long)f);
	CHECKFILE(f);
	if (fflush(f) == 0)
	    mksuccess();
	else
	    fileerror(OtherError, "");
	break;
    case H_Seek:
	{
	    long pos;
	    int mode;
	    f = GETFILE(getpart(p, 0, 3));
	    CHECKFILE(f);
	    *--ep = p;
	    pos = INTOF(evaluate(getpart(p, 1, 3)));
	    p = *ep++;
	    mode = INTOF(evaluate(getpart(p, 2, 3)));
	    if (debug>1) fprintf(stderr, "H_Seek %lx %ld %d\n", (unsigned long)f, pos, mode);
	    if (fseek(f, pos, mode) < 0)
		fileerror(OtherError, "");
	    else
		mkintresp(ftell(f));
	}
	break;
    case H_GetFlags:
	{
	    int flags;
	    f = GETFILE(GET1OF1(p));
	    if (debug>1) fprintf(stderr, "H_GetFlags %lx\n", (unsigned long)f);
	    if (f == 0) {
		flags = F_ISCLOSED;
	    } else if (f == (FILE*)-1L) {
		flags = 0;
	    } else {
		flags = F_ISOPEN;
		if (lseek(fileno(f), (off_t)0, 1) == 0)
		    flags |= F_ISSEEKABLE;
	    }
	    mkintresp(flags);
	}
	break;
    case H_GetChar:
	f = GETFILE(GET1OF1(p));
	if (debug>1) fprintf(stderr, "H_GetChar %lx\n", (unsigned long)f);
	CHECKFILE(f);
	r = getc(f);
	if (r < 0) {
	    if (feof(f))
		mkerrresp(OtherError, "EOF");
	    else
		fileerror(OtherError, "");
	} else
	    mkintresp(r);
	break;
    case H_UnGetChar:
	f = GETFILE(GET1OF2(p));
	if (debug>1) fprintf(stderr, "H_UnGetChar %lx\n", (unsigned long)f);
	CHECKFILE(f);
	p = evaluate(GET2OF2(p));
	ungetc(INTOF(p), f);		/* may fail! XXX */
	mksuccess();
	break;
    case H_PutChar:
	f = GETFILE(GET1OF2(p));
	if (debug>1) fprintf(stderr, "H_PutChar %lx\n", (unsigned long)f);
	CHECKFILE(f);
	p = evaluate(GET2OF2(p));
	r = putc(INTOF(p), f);
	if (r < 0)
	    fileerror(OtherError, "");
	else
	    mksuccess();
	break;
    case H_PutString:
	f = GETFILE(GET1OF2(p));
	stringp = GET2OF2(p);
	if (debug>1) fprintf(stderr, "H_PutString %lx\n", (unsigned long)f);
	CHECKFILE(f);
	r = outstring(f, stringp);
	if(debug>1) fprintf(stderr,"outstring: %d\n",r);
	if (r < 0)
	    fileerror(WriteError, buf, oper);
	else
	    mksuccess();
	break;
    case H_GetFile:
	p = GET1OF1(p);
	f = GETFILE(p);
	if (debug>1) fprintf(stderr, "H_GetFile %lx\n", (unsigned long)f);
	CHECKFILE(f);
	p->nodeinp.file = (FILE*)-1;	/* mark as semi-closed */
	mkresponse(mkconstr(Str, 1, mknode2(&INPUT, (Int)f, (Int)-1)));
	break;
    case H_Select:
#if 1
	{
	    PTR canread, canwrite, *pp;
	    fd_set tryread, trywrite;
	    struct timeval tvt, tv, *tvp;
	    double dtime;
	    int nfd, mfd, fd;
	    PTR q, timo;
	    int n = 0;

	    if (debug) fprintf(stderr, "H_Select starts\n");
	    p = evaluate(GET1OF1(p)); /* one argument which is a triple */
	    p = evalargs(p, 3);
	    *--ep = p;		/* protect from gc */
	    /* Evaluate read files */
	    for(q = getpart(*ep, 0, 3); !ISNIL(q); q = evaluate(TLOF(q))) {
		n++;
	    }
	    /* Evaluate write files */
	    for(q = getpart(*ep, 1, 3); !ISNIL(q); q = evaluate(TLOF(q))) {
		n++;
	    }
	    /* Evaluate timeout */
	    q = getpart(*ep, 2, 3); /* [ Double ] */
	    if (!ISNIL(q))
		evaluate(HDOF(q));
	    if (hp + n*3 + 50 > ehp)
		dogc(); /* to avoid gc during mknodes below */
	    /* Everything is evaluated, just start the real work */
	    /* Find the read descriptors */
	    p = *ep++;
	    timo = getpart(p, 2, 3);
	    mfd = -1;
	    pp = &canread;
	    FD_ZERO(&tryread);
	    for(q = getpart(p, 0, 3); !ISNIL(q); q = TLOF(q)) {
		f = GETFILE(HDOF(q));
		if (f == 0 || f == (FILE*)-1)
		    continue;
		if (canreadfrombuffer(f)) {
		    *pp = mkcons(HDOF(q), 0);
		    pp = &TLOF(*pp);
		}
		fd = fileno(f);
		FD_SET(fd, &tryread);
		if (fd > mfd)
		    mfd = fd;
	    }
	    *pp = mknil();
	    if (!ISNIL(canread)) {
		/* we can read from the buffers */
	        if (debug > 1) fprintf(stderr, "select returns early\n");
		q = mkconstr(0, 3, canread, mknil(), timo); /* triple with info */
		q = mkcons(q, mknil()); /* Just */
		mkresponse(mkconstr(SelectResp, 1, q));
	    } else {
		/* Find the write descriptors */
		FD_ZERO(&trywrite);
		for(q = getpart(p, 1, 3); !ISNIL(q); q = TLOF(q)) {
		    f = GETFILE(HDOF(q));
		    if (f == 0 || f == (FILE*)-1)
			continue;
		    fd = fileno(f);
		    FD_SET(fd, &trywrite);
		    if (fd > mfd)
			mfd = fd;
		}
		if (!ISNIL(timo)) {
		    dtime = getdouble(HDOF(timo));
		    if (dtime < 0)
			dtime = 0;
		    tvp = &tv;
		    tv.tv_sec = (long)dtime;
		    tv.tv_usec = (long)(1e6 * (dtime - tv.tv_sec));
		} else {
		    tvp = 0;
		}
		if (tvp) {
		    /* Get current time to be able to compute how long we spent in select */
		    gettimeofday(&tvt, (struct timezone *)0);
		    dtime += tvt.tv_sec + tvt.tv_usec * 1e-6;
		}
		if (debug > 1) fprintf(stderr, "select %lx %lx %d\n", *(long*)&tryread, 
				       *(long*)&trywrite, tvp ? tvp->tv_sec : -1);
		nfd = select(mfd+1, &tryread, &trywrite, (fd_set *)0, tvp);
		if (debug > 1) fprintf(stderr, "select=%d %lx %lx\n", nfd, 
				       *(long*)&tryread, *(long*)&trywrite);
		if (nfd < 0) {
		    mkerrresp(OtherError, "select");
		} else if (nfd == 0) {
		    mkresponse(mkconstr(SelectResp, 1, mknil())); /* Nothing */
		} else {
		    /* Find active descriptors */
		    pp = &canread;
		    for(q = getpart(p, 0, 3); !ISNIL(q); q = TLOF(q)) {
			f = GETFILE(HDOF(q));
			if (f == 0 || f == (FILE*)-1)
			    continue;
			if (FD_ISSET(fileno(f), &tryread)) {
			    *pp = mkcons(HDOF(q), 0);
			    pp = &TLOF(*pp);
			}
		    }
		    *pp = mknil();
		    pp = &canwrite;
		    for(q = getpart(p, 1, 3); !ISNIL(q); q = TLOF(q)) {
			f = GETFILE(HDOF(q));
			if (f == 0 || f == (FILE*)-1)
			    continue;
			if (FD_ISSET(fileno(f), &trywrite)) {
			    *pp = mkcons(HDOF(q), 0);
			    pp = &TLOF(*pp);
			}
		    }
		    *pp = mknil();
		    if (tvp) {
			/* And now we know how long */
			gettimeofday(&tvt, (struct timezone *)0);
			dtime -= tvt.tv_sec + tvt.tv_usec * 1e-6;
			if (dtime < 0)
			    dtime = 0;
			q = mkcons(mkdouble(dtime), mknil());
		    } else {
			q = mknil();
		    }
		    q = mkconstr(0, 3, canread, canwrite, q); /* triple with info */
		    q = mkcons(q, mknil()); /* Just */
		    mkresponse(mkconstr(SelectResp, 1, q));
		}
	    }
	}
#else
	mkerrresp(OtherError, "Select");
#endif
	break;
    case H_CCall:
	{
	    PTR q, n, ret;
	    int rcno;
	    int nargs;
	    void *funptr;

	    if (debug>1) fprintf(stderr, "Enter CCall\n");
	    p = evalargs(p, 3);
	    *--ep = p;
	    q = getpart(p, 1, 3);
	    /* evaluate spine and count # of elems */
	    for(nargs = 0; ; nargs++) {
		if (ISNIL(q))
		    break;		/* end of list */
		*--ep = q = evalargs(q, 2);
		(void)evaluate(getpart(HDOF(q), 0, 1)); /* eval inside union */
		q = TLOF(*ep++);
	    }
	    p = *ep++;
	    funptr = (void *)INTOF(getpart(p, 0, 3));
	    if (debug>1) fprintf(stderr, "CCall with %d args of %lx\n", nargs, (unsigned long)funptr);
	    rcno = getcno(getpart(p, 2, 3));
#if defined(i386) || defined(vax) || defined(m68000)
#define CANCCALL
	    {
#define MAXNARG 15
		struct {
		  Int arg;
		  char type;
		} args[MAXNARG];
		union { double d; Int i[2]; } u;
		char *str;
		Int ri;
		double rd;
		int i;

		q = getpart(p, 1, 3);
		for(nargs = 0; nargs < MAXNARG; ) {
		    if (ISNIL(q))
			break;		/* end of list */
		    n = HDOF(q);
		    args[nargs].type = getcno(n);
		    switch(args[nargs].type) {
		    case CUPointer:
		    case CUInt:
			n = GET1OF1(n);
			args[nargs++].arg = INTOF(n);
			break;
		    case CUDouble:
			n = GET1OF1(n);
			u.d = getdouble(n);
			args[nargs++].arg = u.i[0];
			args[nargs].type = CUDouble;
			args[nargs++].arg = u.i[1];
			break;
		    case CUString:
			n = GET1OF1(n);
			/* XXX may cause GC! */
			i = evalstring(n, buf, sizeof buf);
			str = malloc(strlen(buf)+1);
			memcpy(str, buf, i);
			args[nargs++].arg = (Int)str;
			break;
		    case CUByteVector:
		        n = GET1OF1(n);	/* now points to byte vector */
			i = INTOF(GET1OF2(n)); /* lower bound */
			n = GET2OF2(n);	/* pointer to DVEK node */
			args[nargs++].arg = (Int)&n->nodedvek.val[0] + i;
			break;
		    default:
			fprintf(stderr, "bad arg to ccall\n");
			exit(1);
			break;
		    }
		    q = TLOF(q);
		}
		if (nargs >= MAXNARG) { mkerrresp(OtherError, "Too many args to ccall"); break; }
		if (debug>1) {
		    int a;
		    fprintf(stderr, "CCall calling (rcno=%d)", rcno);
		    for(a=0; a < nargs; a++)
			fprintf(stderr, " %x", args[a]);
		    fprintf(stderr, "\n");
		}
		if (rcno != CUDouble) {
		    ri = (*(Int (*)())funptr)(args[0].arg, args[1].arg, args[2].arg, args[3].arg, args[4].arg, 
					      args[5].arg, args[6].arg, args[7].arg, args[8].arg, args[9].arg,
					      args[10].arg, args[11].arg, args[12].arg, args[13].arg, args[14].arg);
		    if (rcno == CUString)
			ret = mkconstr(CUString, 1, mkestring((char *)ri));
		    else
			ret = mkconstr(rcno, 1, mkint(ri));
		} else {
		    rd = (*(double (*)())funptr)(args[0].arg, args[1].arg, args[2].arg, args[3].arg, args[4].arg, 
						 args[5].arg, args[6].arg, args[7].arg, args[8].arg, args[9].arg,
						 args[10].arg, args[11].arg, args[12].arg, args[13].arg, args[14].arg);
		    ret = mkconstr(CUDouble, 1, mkdouble(rd));
		}
		if (debug>1) fprintf(stderr, "CCall returning %x\n", ri);
		for(i = 0; i < nargs; i++ ) {
		    if(args[i].type == CUString)
		        free((void *)args[i].arg);
		}
	    }
	    mkresponse(mkconstr(CCallResp, 1, ret));
#endif /* i386 */
#if defined(hppa) || defined(sparc)
#define CANCCALL
	    {
#define MAXNARG 15
		unsigned long argmask;
		union { double d; Int i; } args[MAXNARG];
		char *str;
		Int ri;
		double rd;

		q = getpart(p, 1, 3);
		for(argmask = 0, nargs = 0; nargs < MAXNARG; ) {
		    if (ISNIL(q))
			break;		/* end of list */
		    n = HDOF(q);
		    switch(getcno(n)) {
		    case CUPointer:
		    case CUInt:
			n = GET1OF1(n);
			args[nargs++].i = INTOF(n);
			break;
		    case CUDouble:
			n = GET1OF1(n);
			argmask |= 1 << nargs;
			args[nargs++].d = getdouble(n);
			break;
		    case CUString:
			n = GET1OF1(n);
			evalstring(n, buf, sizeof buf);
			str = malloc(strlen(buf)+1);
			strcpy(str, buf);
			args[nargs++].i = (Int)str;
			break;
		    default:
			fprintf(stderr, "bad arg to ccall\n");
			exit(1);
			break;
		    }
		    q = TLOF(q);
		}
		if (nargs >= MAXNARG) { mkerrresp(OtherError, "Too many args to ccall"); break; }
		if (debug>1) fprintf(stderr, "CCall calling (rcno=%d), mask=%lx\n", rcno, argmask);
		if (argmask == 0) {
		  if (rcno != CUDouble) {
		    ri = (*(Int (*)())funptr)(args[0].i, args[1].i, args[2].i, args[3].i, args[4].i, 
					      args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
					      args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    if (rcno == CUString)
			ret = mkconstr(CUString, 1, mkestring((char *)ri));
		    else
			ret = mkconstr(rcno, 1, mkint(ri));
		  } else {
		    rd = (*(double (*)())funptr)(args[0].i, args[1].i, args[2].i, args[3].i, args[4].i, 
						 args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
						 args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    ret = mkconstr(CUDouble, 1, mkdouble(rd));
		  }
		} else if (argmask == 1) {
		  if (rcno != CUDouble) {
		    ri = (*(Int (*)())funptr)(args[0].d, args[1].i, args[2].i, args[3].i, args[4].i, 
					      args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
					      args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    if (rcno == CUString)
			ret = mkconstr(CUString, 1, mkestring((char *)ri));
		    else
			ret = mkconstr(rcno, 1, mkint(ri));
		  } else {
		    typedef double (*dblfun)();
		    extern double blubb();
		    rd = (*(double (*)())funptr)(args[0].d, args[1].i, args[2].i, args[3].i, args[4].i, 
						 args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
						 args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    ret = mkconstr(CUDouble, 1, mkdouble(rd));
		  }
		} else if (argmask == 2) {
		  if (rcno != CUDouble) {
		    ri = (*(Int (*)())funptr)(args[0].i, args[1].d, args[2].i, args[3].i, args[4].i, 
					      args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
					      args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    if (rcno == CUString)
			ret = mkconstr(CUString, 1, mkestring((char *)ri));
		    else
			ret = mkconstr(rcno, 1, mkint(ri));
		  } else {
		    rd = (*(double (*)())funptr)(args[0].i, args[1].d, args[2].i, args[3].i, args[4].i, 
						 args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
						 args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    ret = mkconstr(CUDouble, 1, mkdouble(rd));
		  }
		} else if (argmask == 3) {
		  if (rcno != CUDouble) {
		    ri = (*(Int (*)())funptr)(args[0].d, args[1].d, args[2].i, args[3].i, args[4].i, 
					      args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
					      args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    if (rcno == CUString)
			ret = mkconstr(CUString, 1, mkestring((char *)ri));
		    else
			ret = mkconstr(rcno, 1, mkint(ri));
		  } else {
		    rd = (*(double (*)())funptr)(args[0].d, args[1].d, args[2].i, args[3].i, args[4].i, 
						 args[5].i, args[6].i, args[7].i, args[8].i, args[9].i,
						 args[10].i, args[11].i, args[12].i, args[13].i, args[14].i);
		    ret = mkconstr(CUDouble, 1, mkdouble(rd));
		  }
		} else {
		  mkerrresp(OtherError, "ccall with double not implemented");
		  break;
		}
		if (debug>1) fprintf(stderr, "CCall returning\n");
	    }
	    mkresponse(mkconstr(CCallResp, 1, ret));
#endif
	}
#ifndef CANCCALL
	mkerrresp(OtherError, "ccall not implemented");
#else
#undef CANCCALL
#endif
	break;

    case H_GetErrno:
      mkresponse(mkconstr(IntResp, 1, mkint(last_errno)));
      break;

    case H_GetPermissions:
	oper = "H_GetPermissions";
	namep = GET1OF1(p);
	evalstring(namep, buf, sizeof buf);
	if (stat(buf, &sb) < 0) {
	    fileerror(SearchError, buf, oper);
	} else {
	    int r = 0;
	    if ((sb.st_mode & S_IFMT) == S_IFDIR) r |= 8;
	    /* Using access() here is wrong. XXX */
	    if (access(buf, R_OK)) r |= 4;
	    if (access(buf, W_OK)) r |= 2;
	    if (access(buf, X_OK)) r |= 1;
	    mkresponse(mkconstr(IntResp, 1, mkint(r)));
	}
	break;

    case H_Chmod:
	oper = "H_Chmod";
	*--ep = GET2OF2(p);
	namep = GET1OF2(p);
	evalstring(namep, buf, sizeof buf);
	ep++;
	if (chmod(buf, INTOF(*ep)) < 0)
	    fileerror(SearchError, buf, oper);
	else
	    mksuccess();
	break;

    case H_GetTimeZone:
        {
	    time_t now;
	    struct tm lt;
	    struct timeval tv;
	    int gmtoff;
	    char *tz;

	    now = (time_t)getdouble(GET1OF1(p));
	    lt = *localtime(&now);
#if defined(__NetBSD__) || defined(__FreeBSD__) || defined(OSF) || defined(SUNOS4)
	    gmtoff = lt.tm_gmtoff;
	    tz = lt.tm_zone;
#elif defined(HPUX) || defined(SOLARIS) || defined(_AIX) || defined(IRIX) || defined(linux) || defined(__CYGWIN32__)
	    {
	        extern long timezone;
		extern char *tzname[2];
		gmtoff = -timezone;
                if (lt.tm_isdst)
                    gmtoff += 3600;
		tz = tzname[lt.tm_isdst];
	    }
#else
	    XXXX No TZ
#endif
	    mkresponse(mkconstr(GetTimeResp, 3, mkbool(lt.tm_isdst), mkestring(tz), mkint(gmtoff)));
	    break;
	}

    case ReadBinChannels:
    case OpenFile:
    case OpenBinFile:
    case CloseFile:
    case ReadVal:
    case ReadBinVal:
    case WriteVal:
    case WriteBinVal:
	fprintf(stderr, "Uninplemented I/O request %d\n", t);
	exit(1);
	break;	
    default:
	fprintf(stderr, "Unknown I/O request %d\n", t);
	exit(1);
	break;
    }
    last_errno = errno;
}

#define TIMEADD(xp, ip) do { (xp)->tv_sec += (ip)->tv_sec; (xp)->tv_usec += (ip)->tv_usec; if ((xp)->tv_usec >= 1000000) (xp)->tv_usec -= 1000000, (xp)->tv_sec++; } while(0)
#define TIMESUB(xp, ip) do { (xp)->tv_sec -= (ip)->tv_sec; (xp)->tv_usec -= (ip)->tv_usec; if ((xp)->tv_usec < 0) (xp)->tv_usec += 1000000, (xp)->tv_sec--; } while(0)

/* Enter here when a readchannel request is evaluated. */
PTR
creadchan(vp)
PTR vp;
{
    fd_set mask;
    int nf, i, n, fd, mfd, ch;
    PTR p, ip;
    FILE *f;
    PTR tp, ttp;
    struct timeval tv, *tvp;
    struct timeval now, at, incr;

    DEB(printf("creadchan "));
    if (hp + 50 > ehp) {
	*--ep = vp;
	dogc();
	vp = *ep++;
    }
    if (vp->tag->tagNumber != O_VEK) {
	fprintf(stderr, "Bad channels arg to creadchan\n");
	exit(1);
    }
    tp = FSTOF(vp->nodevek.ptr[0]);
    ttp = SNDOF(vp->nodevek.ptr[0]);
    if (ttp->tag->tagNumber == O_TAG0) {
	/* no timeout */
	tvp = 0;
    } else if (ttp->tag->tagNumber == O_PAIR0) {
	/* simple timeout */
	tv.tv_sec = INTOF(FSTOF(ttp));
	tv.tv_usec = INTOF(SNDOF(ttp));
	tvp = &tv;
	DEB(printf("Timeout %d %d\n", tv.tv_sec, tv.tv_usec));
    } else if (ttp->tag->tagNumber == O_PAIR1) {
	/* tick based timeout */
	gettimeofday(&now, (struct timezone *)0);
	at.tv_sec = INTOF(FSTOF(SNDOF(ttp)));
	at.tv_usec = INTOF(SNDOF(SNDOF(ttp)));
	incr.tv_sec = INTOF(FSTOF(FSTOF(ttp)));
	incr.tv_usec = INTOF(SNDOF(FSTOF(ttp)));
	DEB(printf("ticking %d %d, %d %d, %d %d\n", now.tv_sec, now.tv_usec, at.tv_sec, at.tv_usec, incr.tv_sec, incr.tv_usec));
	TIMEADD(&at, &incr);
#undef timercmp
#define	timercmp(tvp, uvp, cmp)	\
	((tvp)->tv_sec cmp (uvp)->tv_sec || \
	 ((tvp)->tv_sec == (uvp)->tv_sec && (tvp)->tv_usec cmp (uvp)->tv_usec))

	if (timercmp(&at, &now, <)) {
	    /* already expired, generate timeout char at once */
	    DEB(printf("Expired\n"));
	    goto timeo;
	}
	tv = at;
	TIMESUB(&tv, &now);
	tvp = &tv;
	DEB(printf("Tick %d %d\n", tv.tv_sec, tv.tv_usec));
    } else {
	fprintf(stderr, "Bad timeout arg to creadchan\n");
	exit(1);
    }
	
    nf = vp->nodevek.size - 1;
    FD_ZERO(&mask);
    for(mfd = -1, i = 0; i < nf; i++) {
	p = vp->nodevek.ptr[i+1];
	ip = SNDOF(p);
	f = (FILE *)ip->node30.val0;
	fd = fileno(f);
	DEB(printf("%d ", fd));
	FD_SET(fd, &mask);
	if (fd > mfd)
	    mfd = fd;
    }
    fflush(stdout);
    DEB(printf("selecting\n"));
    n = select(mfd+1, &mask, (fd_set *)0, (fd_set *)0, tvp);
    DEB(printf("select returns %d\n", n));
    if (n < 0) {
	fprintf(stderr, "unexpected value from select %d, errno=%d\n", n, errno);
	exit(1);
    }
    if (n == 0) {
	DEB(printf("got timeout\n"));
    timeo:
	FSTOF(SNDOF(ttp)) = mkint(at.tv_sec);
	SNDOF(SNDOF(ttp)) = mkint(at.tv_usec);
	return mkcons(mkpair(tp, mkchar('*')), mkap(&readchan, vp));
    }
    /* figure out which file can deliver data (should have been fair!) */
    for(i = 0; i < nf; i++) {
	p = vp->nodevek.ptr[i+1];
	ip = SNDOF(p);
	f = (FILE *)ip->node30.val0;
	fd = fileno(f);
	if (FD_ISSET(fd, &mask)) {
	    /* fount it! */
	    DEB(printf("found input on %d\n", fd));
	    ch = getc(f);
	    DEB(printf("got %02x\n", ch));
	    return mkcons(mkpair(FSTOF(p), mkchar(ch)), mkap(&readchan, vp));
	}
    }
    fprintf(stderr, "No fd found!\n");
    exit(1);
}

#if defined(sequent) || defined(linux) || defined(__CYGWIN32__)
void
xsetenv(var, val)
char *val, *var;
{
    setenv(var, val, 1);
}
#endif

#if defined(sun) || defined(mips) || defined(_AIX) || defined(__386BSD__) || defined(__osf__) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(hpux) || defined(__hpux)

void
xsetenv(var, val)
char *var, *val;
{
    char b[MAXNAME];

    sprintf("%s=%s", b, var, val);
    putenv(b);
}
#endif

#if defined(vax) || defined(hp300)
void
xsetenv(var, val)
char *val, *var;
{
    fprintf(stderr, "warning: setenv not implemented\n");
}
#endif

/* xsetenv should be defined for other machines as well */

/*
** Utilities to access data given a pointer, for use with ccall.
*/
Int derefChar (p) char  *p; { return *p; }
Int derefShort(p) short *p; { return *p; }
Int derefInt  (p) int   *p; { return *p; }
Int derefLong (p) long  *p; { return *p; }
Int derefUChar (p) unsigned char  *p; { return *p; }
Int derefUShort(p) unsigned short *p; { return *p; }
Int derefUInt  (p) unsigned int   *p; { return *p; }
Int derefULong (p) unsigned long  *p; { return *p; }
double derefFloat (p) float *p; { return *p; }
double derefDouble (p) double *p; { return *p; }
void setChar (p, x) char  *p; Int x; { *p = x; }
void setShort(p, x) short *p; Int x; { *p = x; }
void setInt  (p, x) int   *p; Int x; { *p = x; }
void setLong (p, x) long  *p; Int x; { *p = x; }
void setFloat (p, x) float  *p; double x; { *p = x; }
void setDouble (p, x) double  *p; double x; { *p = x; }
void *addPtr(p, i) void *p; Int i; { return (char *)p + i; }
Int identity(x) Int x; { return x; }

int getErrno() { return errno; }

#ifdef __CYGWIN32__
char *tzname[2] = { "XX", "XX" };
int getw(FILE *f) { int l; fread(&l, 1, sizeof(int), f); return l; }
int putw(int l, FILE *f) { fwrite(&l, 1, sizeof(int), f); return l; }
#endif
