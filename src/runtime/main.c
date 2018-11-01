#include "runtime.h"
#define EXTERN
#include "vars.h"
#undef EXTERN
#include "tagtable.h"

#ifdef HPROFILE
#include "sample.h"
int USE_heap=1;
#else
int USE_none=1, USE_time=1;
#endif

#ifdef sun
#include <math.h>
#ifdef SUNOS4
#include <floatingpoint.h>
#endif
#endif

/*
**	Flags to the runtime system.
*/
#if DUMP
int dflag = 0;	/* Stack dump at error 			*/
int Gflag = 0;	/* Stack dump before and after GC	*/
#endif /* DUMP */
#if GCSTAT
int sflag = 0;	/* Produce a brief STAT file		*/
int Sflag = 0;	/* Produce a verbose STAT file		*/
char *Sfile = 0;
int Bflag = 0;	/* And sound the bell at GC		*/
#endif /* GCSTAT */
int uflag = 0;	/* Use unbuffered input and output	*/
int Hflag = 0;	/* Hiatonic input			*/
int Ttime = 0;	/* Hiaton timeout time			*/
int Cflag = 0;	/* Want core on BUS or SEGV		*/
#if 0
int aflag = 0;	/* print extra space between outputs	*/
#endif
int zapthem = 0;/* ZAP at gc time */
int debug = 0;
int MergeHiaton = 0;
int traceflag = 0;
int fullheap;			/* Set MinHeapsize = Heapsize */
#ifdef HSHOW
int showing = 0;
char *argColours = 0;
char *argOldColours = 0;
char *argDstColours = 0;
double milliseconds;
double sampleinterval = 0.1;
double nextsampletime;	
#endif
int gcflag = gcOrg;

int nodecodeflags;		/* Defaults to 0, can be set to 1 by linking in an ordinary .o file (from C). */
int minheap, maxheap;		/* These can be used to set the heapsize if argument nodecodeflags=1 */

extern Node *CCPmain;            /* Pointer to main function */
extern Node argvnp;
extern Node envpnp;
extern Node prognp;
extern Node PROCESSNO;

static int decode PROTO((char *));
static int s2i PROTO((char *));
static long atox PROTO((char *));
static void setupsigs PROTO((void));
SIGTYPE sighandl () /*PROTO((int, int, struct sigcontext *, char *))*/;
char *copystring PROTO((char *));
int decodehparg PROTO((char *));
#ifdef HPROFILE
void setuphpfile PROTO((void));
void setuptimer PROTO((void));
void DoSample PROTO((void));
void putres PROTO((FILE *, struct restrlist *));
#endif
void inittrace PROTO((void));
void inittrace0 PROTO((void));

#ifdef __ARM
int stacksize = 16000;
int Heapsize = 200000;
#else
int stacksize = 100000;
int Heapsize = 10000000/4;
#endif
int Utilize = 80;
int MinHeapsize = 500000/4;
int heapsize;
int tpmul = 1;
int Minleft;			/* minimum heap left to continue execution */
#if defined(sparc) || defined(hppa)
int vsize = 200000;
#endif
int cafflag = 0;

FILE *mystdin;

#define Bcopy(x,y,z) bcopy((char *)x, (char *)y, z)

int max(x, y)
int x, y;
{
  return x>y?x:y;
}

void
cleansig()
{
    sigsetmask(0);
}

int intrflag = 0;
int doinggc = 0;

void
sigintr()
{
    extern int *csbegin[], **cspointer;
    
    if (debug)
	fprintf(stderr, "sigintr %d\n", intrflag);
    intrflag++;
    if (intrflag > 1) {
	if (debug)
	    fprintf(stderr, "many sigintr %d\n", doinggc);
	if (!doinggc) {
	    intrflag = 0;
	    ehp = realehp;
	    cleansig();
	    failintr();
	}
    } else {
	if (debug)
	    fprintf(stderr, "first sigintr %d\n", cspointer < csbegin);
	if (cspointer < csbegin) {
	    /* There is an active catch */
	    ehp = 0;
	} else {
	    /* No active catch, remember that we had an intr */
	    /* intrflag is polled in catch */
	}
    }
}

#ifdef CRAY
static PTR
mkpstr(n, s)
PTR n;
char *s;
{
    return mkcons(mkestring(s), n);
}
#else

static PTR
mkpstr(n, s)
PTR n;
char *s;
{
    /* assume that mkpstr is only called with enough memory to avoid gc */
    return mkcons(mkcstring(s), n);
}
#endif

void
mkstrs(n, sp, pp)
int n;
char **sp;
PTR pp;
{
    PTR sap;
    int i;

    sap = mknil();
    for (i = n-1; i >= 0; i--) {
	sap = mkpstr(sap, sp[i]);
    }
    update(pp, sap);
}

int iswhite(c) int c; { return c == ' ' || c == '\t'; }

char *flagtext[] = {
"The following run time flags are available:",
"",
"  -B	Sound the bell at the start of garbage collection.",
"  -H	Set heap size, e.g. -H30000. Default is 2600000.",
"  -h	Set start size of heap. Default is 50000.",
"  -S	Produce a verbose 'STAT' file.",
"  -	Marks the end of decoded arguments.",
"and many many more...",
"",
0
};

extern int    argcOrg;
extern char **argvOrg;

void
main(argc, argv, envp)
int argc;
char **argv, **envp;
{
    char *s;
    int tenureUpdate = -1;  /* -1 == use default */
    int tenurePointer = -1;
    int tenureWord = -1;

    mystdin = stdin;
    progname = *argv;
    argvOrg = argv;
    argcOrg = argc;
    argc--;
    argv++;
    if ((s = getenv("LMLARGS"))) {
	int n;
	char *q;
	char *a[1000];		/* hope this is enough */
	char **na;

	q = copystring(s);
	while(iswhite(*q))
	    q++;
	for(n = 0; *q; ) {
	    a[n++] = q;
	    while(*q && !iswhite(*q))
		q++;
	    while(iswhite(*q))
		*q++ = 0;
	}
	na = (char **)xmalloc((argc+n+1) * sizeof(char *));
	Bcopy(a,    na,   n    * sizeof(char *));
	Bcopy(argv, na+n, argc * sizeof(char *));
	argc += n;
	na[argc] = 0;
	argv = na;
    }
#ifndef __ARM
    if ((s = getenv("LMLHEAP")))
	Heapsize = decode(s) / sizeof(UInt);
#endif
    if (argc && strcmp(argv[0], "+RTS") == 0) argc--, argv++, nodecodeflags = 0;
    if (!nodecodeflags) {
	while (argc && argv[0][0] == '-') {
	    if (argv[0][1] == '\0' || strcmp(argv[0], "-RTS") == 0) {
		argv++, argc--;
		break;
	    }
	    if (strcmp("-gc-caf", *argv) == 0) {
	        cafflag++;
		goto nextarg;
	    }
	    if (strcmp("-gc-slide", *argv) == 0) {
		if(gcflag)
		  gcflag = gcError;
		else
		  gcflag = gcSlide;
		goto nextarg;
	    } else if (strcmp("-gc-baker", *argv) == 0) {
		if(gcflag)
		  gcflag = gcError;
		else
		  gcflag = gcChenney;
		goto nextarg;
	    } else if (strcmp("-gc-gen", *argv) == 0) {
		if(gcflag)
		  gcflag = gcError;
		else
		  gcflag = gcGenSeward;
		goto nextarg;
	    } else if (strcmp("-gc-gen2", *argv) == 0) {
		if(gcflag)
		  gcflag = gcError;
		else
		  gcflag = gcGen2;
		goto nextarg;
	    } else if (strncmp("-tenureUpdate", *argv,13) == 0) {
	        fprintf(stderr,"tenureUpdate = %d\n",
			tenureUpdate = s2i((*argv)+13));
		goto nextarg;
	    } else if (strncmp("-tenurePointer", *argv,14) == 0) {
	        fprintf(stderr,"tenurePointer = %d\n",
			tenurePointer = s2i((*argv)+14));
		goto nextarg;
	    } else if (strncmp("-tenureWord", *argv,11) == 0) {
	        fprintf(stderr,"tenureWord = %d\n",
			tenureWord = s2i((*argv)+11));
		goto nextarg;
	    } else if (strncmp("-tpmul", *argv,6) == 0) {
	        fprintf(stderr,"tpmul = %d\n",
			tpmul = decode((*argv)+6));
		goto nextarg;
	    } else if (strcmp("-no-zap", *argv) == 0) {
		zapthem = 0;
		goto nextarg;
	    } else if (strcmp("-zap", *argv) == 0) {
		zapthem = 1;
		goto nextarg;
	    } else {
		char *ap = *argv;
		while (*++ap) {
		    switch (*ap) {
#if defined(HPROFILE) || defined(HSHOW)
		    case 'i':
			sampleinterval = atof(ap + 1);
			if (sampleinterval < 0.05) {
			    fprintf(stderr, "-i: too small\n"); finish(1);
			}
#ifndef HSHOW
			noautosample++;
#endif
			goto nextarg;
#endif
#ifdef HPROFILE
		    case 'g':
		    case 'm':
		    case 'p':
		    case 'c':
		    case 't':
			if (decodehparg(ap))
			    goto nextarg;
			break;
		    case 'k':
			gengcmark ^= 1;
			break;
#endif
#if DUMP
		    case 'M': 
			maxdump = atoi(ap + 1);
			goto nextarg;
		    case 'E': 
			dumpdepth = atoi(ap + 1);
			goto nextarg;
		    case 'd': 
			dflag++;
			break;
		    case 'G': 
			Gflag++;
			if (ap + 1)
			    Gno = atoi(ap + 1);
			goto nextarg;
		    case 'K':
			stopaddr = atox(ap + 1);
			goto nextarg;
		    case 'Q':
			chkaddr = (int **)atox(ap + 1);
			goto nextarg;
#endif				/* DUMP */
#if GCSTAT
		    case 'B': 
			Bflag++;
			break;
		    case 'S': 
			Sflag++;
			if (ap[1]) {
			    Sfile = ap+1;
			    goto nextarg;
			}
			break;
		    case 's': 
			sflag++;
			break;
#endif				/* GCSTAT */
		    case 'C': 
			Cflag++;
			break;
#if 0
		    case 'u': 
			uflag++;
			break;
		    case 'a': 
			aflag++;
			break;
#endif
		    case 'h': 
			MinHeapsize = decode(ap + 1) / sizeof(UInt);
			goto nextarg;
		    case 'H': 
			Heapsize = decode(ap + 1) / sizeof(UInt);
			goto nextarg;
			break;
		    case 'U': 
			Utilize = decode(ap + 1);
			goto nextarg;
			break;
		    case 'A': 
			stacksize = decode(ap + 1);
#if defined(sparc) || defined(hppa)
			vsize = stacksize;
#endif
			goto nextarg;
			break;
#if defined(sparc) || defined(hppa)
		    case 'V': 
			vsize = decode(ap + 1);
			goto nextarg;
			break;
#endif
		    case 'X':
			debug++;
			break;
#ifdef HSHOW
		    case 'x':
			showing = max(showing,1);
			if(*++ap == 'c') {
			  showing = max(showing,2);
			  argColours = ap+1;
			} else {
			  if(*ap == 'o') {
			    showing = max(showing,3);
			    argOldColours = ap+1;
			  } else {
			    if(*ap == 'p') {
			      showing = max(showing,4);
			      argDstColours = ap+1;
			    }
			  }
			}
			goto nextarg;
			break;
#endif
		    case 'T':
			traceflag++;
			break;
#if 0
		    case 'R':
			Hflag++;
			Ttime = atoi(ap + 1);
			goto nextarg;
			break;
#endif

		    default: 
			fprintf(stderr, "**** Illegal flag ****\n");
			/* fall into ... */
		    case 'f': 	/* Print an explanation of the v	
				 * run-time flags, and exit. */
			{
			    char  **p;
			    for (p = flagtext; *p; p++)
				fprintf(stderr, "%s\n", *p);
			}
			finish(1);
		    }
		}
	    }
	nextarg: ;
	    argc--;
	    argv++;
	}
    } else {
	if (minheap) MinHeapsize = minheap/sizeof (UInt);
	if (maxheap) Heapsize = maxheap/sizeof (UInt);
    }

    {
	/* Sanity check */
	extern Tag TAGFIRST, TAGLAST;
	if (((UInt)&TAGLAST - (UInt)&TAGFIRST) != TAGTABLESIZE * NTAGS * sizeof(PTR)) {
	    fprintf(stderr, "Corrupted tag table size %d instead of %d!\n",(UInt)&TAGLAST - (UInt)&TAGFIRST,TAGTABLESIZE * NTAGS * sizeof(PTR));
#if 0
	    exit(1);
#else
	    fprintf(stderr, "... but I will try anyway!\n");
#endif
	  }
      }

    if(!gcflag)
      gcflag = gcOrg;
    if(gcflag == gcError) {
      fprintf(stderr,"Only one garbage collector at a time, please.\n");
      finish(1);
    }

    heapsize = MinHeapsize;
#if DUMP
    if (Gflag)
	loadsymbols();
#endif
    if (traceflag)
	inittrace();
    else
	inittrace0();
#if defined(sun) && defined(SUNOS4)
    ieee_handler("set", "common", failfloat);
    signal(SIGIOT, failfloat);	/* integer zero divide */
#else /* sun */
#ifndef __ARM
    signal(SIGFPE, failfloat);
#endif
#endif /* sun */

#ifdef mips
    signal(SIGTRAP, failfloat);	/* mips generates SIGTRAP on fixed point divide by zero */
#endif

    setuptenure(tenureUpdate,tenurePointer,tenureWord);
    setupstack();
    setupheap();

#ifndef __ARM
    { 
	int i;
	for(i=0; envp[i] != 0; i++)
	    ;
	mkstrs(i, envp, &envpnp);
    }
#endif
    mkstrs(argc, argv, &argvnp);

    /* set program name */
    update(&prognp, mkcstring(progname));

    setupsigs(); 
#ifndef __ARM
    if (!Cflag) {
#ifndef linux
	(void)signal(SIGBUS, sighandl);
#endif
	(void)signal(SIGINT, sighandl);
	(void)signal(SIGSEGV, sighandl);
	(void)signal(SIGILL, sighandl);
    }
#endif
    if (uflag) {
	setbuf(stdout, NULL);
    }
    setbuf(stderr, NULL);

    GCstartup();

    update(&PROCESSNO, mkint(getpid()));

    {
	extern Tag INPUT;

	*--ep = &argvnp;
	*--ep = &envpnp;
	*--ep = mkap(CCPmain, mknode2(&INPUT, (Int)mystdin, (Int)-1));
    }

    initcaf();

#ifdef HSHOW
    if(showing) { /* setupheap must be done before calling setupDisplay */
      setupDisplay(sampleinterval);
/*      setupShowTimer(); */
    }
#endif

    initfiles();

#ifdef HPROFILE
    if (profiling) {
        setuphpfile();
        setuptimer();
        DoSample();
    }
#endif

    printtop(stdout); /* evaluate and print the user program */

#if 0
    if (aflag) fprintf(ofiles[1], "\n");
#endif

    GCfinal(starthp, hp);

#ifdef HPROFILE
    if (profiling) {
        DoSample();
    }
#endif

    finish(0);
}

char *
copystring(s)
char *s;
{
    char *r;
    r = (char *)xmalloc(strlen(s)+1);
    strcpy(r, s);
    return r;
}

void
finish(r)
int r;
{
    set_tty(0);
    exit(r);
}

void
lfinish(r)
PTR r;
{
    finish((int)INTOF(r));
}

void
xsignal(s, intr)
int s;
SIGTYPE (*intr)();
{
    if (signal(s, SIG_IGN) != SIG_IGN)
	(void)signal(s, intr);
}

SIGTYPE
killme()
{
    finish(2);
    SIGLAST;
}

static void
setupsigs()
{
    if (traceflag != 1)
	xsignal(SIGINT, killme);
#ifndef __ARM
    xsignal(SIGHUP, killme);
#endif
    xsignal(SIGTERM, killme);
}

#ifdef __ARM
void
set_tty(set)   /* !!! */
int set;
{
  return;
}
#else
#if defined(SYSV) || defined(linux) || defined(SOLARIS) || defined(HPUX) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__CYGWIN32__)
void
set_tty(set)
int set;
{
	static struct termio save, modes;
	static int sset = 0;
	if (set) {
	    if (!sset)
		ioctl(2, TCGETA, &save);
	    modes = save;
	    modes.c_iflag &= ~(IXON | IXOFF | ISTRIP);
	    /*modes.c_oflag &= ~OPOST;*/
	    modes.c_lflag &= ~(ICANON | ECHO);
	    modes.c_cc[VMIN] = 1;
	    modes.c_cc[VTIME] = 0;
	    ioctl(2, TCSETA, &modes);
	    sset = 1;
	} else if (sset) {
	    ioctl(2, TCSETA, &save);
	}
}
#else
void
set_tty(set)
int set;
{
    static struct sgttyb save, modes;
    static int sset = 0;

    if (set) {
	if (!sset)
	    ioctl(2, TIOCGETP, &save);
	modes = save;
	modes.sg_flags |= CBREAK;
	modes.sg_flags &= ~ECHO;
	ioctl(2, TIOCSETP, &modes);
	sset = 1;
    } else if (sset) {
	ioctl(2, TIOCSETP, &save);
    }
}
#endif
#endif /* __ARM */

static int
s2i(s)
char *s;
{
  if(*s) {
    int res = 0;
    while(*s) {
      res = res*10 + *s++ -'0';
    }
    return res;
  }
  return -1; /* default */
}


static int
decode(s)
char *s;
{
    int c;
    double m;
#ifndef atof
    /* atof might be a macro */
    extern double atof();
#endif

    if (!*s) {
	fprintf(stderr, "Missing number.\n");
	exit(1);
    }
    m = atof(s);
    c = s[strlen(s)-1];
    if (c == 'g') {
	gcflag = gcGenSeward;
	c = s[strlen(s)-2];
    }
    if (c == 'G')
	m *= 1000000000;
    else if (c == 'm' || c == 'M')
	m *= 1000000;
    else if (c == 'k' || c == 'K')
	m *= 1000;
    m *= sizeof(PTR)/sizeof(int); /* To simplify life on 64 bit machines */
    return (int)m;
}

#ifdef CRAY
STOFERR()
{
	printf("Stack overflow\n");
	finish(1);
}

STOFJMP()
{
    printf("Stack failed to allocate in same chunk\n");
    finish(1);
}
#endif

#if defined(sun) && !defined(SOLARIS)
#include <errno.h>
extern int sys_nerr;
char *
strerror(i)
int i;
{
	if (i < 0 || i >= sys_nerr)
		return "Unknown error";
	return sys_errlist[i];
}
#endif

static long
atox(s)
char *s;
{
    long i;

    sscanf(s, "%lx", &i);
    return i;
}

#ifdef sun
int
matherr(exc)
struct exception *exc;
{
    return 0;			/* use default value */
}
#endif

/* For gcc */
void __main() {}

#ifdef sparc
int old_npc;
extern int zero_g7();
#endif
#ifdef hppa
int old_npc;
#endif

#ifdef HSHOW
/* 
 * Called every 20ms...
 */

SIGTYPE
timerTickShow(sig,code,sigcx,addr)
int sig, code;
struct sigcontext *sigcx;
char *addr;
{
  if (showflag || doinggc) 
    SIGLAST;       /* clock is frozen during sampling and gc */
  milliseconds += 20.0;          /* another 20ms */
  showflag = (milliseconds >= nextsampletime);
  if (showflag) {
#ifdef sparc
#define JMPMASK  0x81c00000
#define ISJMP(x) (((x)&JMPMASK) == JMPMASK)
    if(ISJMP(*(int *)sigcx->sc_pc) ||
       ISJMP(*(int *)sigcx->sc_npc) ) { /* Wait ! */
      showflag =0;
      SIGLAST;
    }
    old_npc = sigcx->sc_npc;
    sigcx->sc_npc = (int)zero_g7;
#endif
#if mips
    if(sigcx->sc_regs[30] != (int)ehp) {
      showflag =0;
      SIGLAST;
    }
    sigcx->sc_regs[30] = 0;
#endif
    ehp = 0;
    nextsampletime += 1000.0 * sampleinterval;
  }
  SIGLAST;
}

void
setupShowTimer() 
{
    struct itimerval inttimer;

    inttimer.it_value.tv_sec = 0;
    inttimer.it_value.tv_usec = 20000;
    inttimer.it_interval = inttimer.it_value;
    signal(SIGVTALRM, timerTickShow);
    setitimer(ITIMER_VIRTUAL, &inttimer, (struct itimerval *)0);
    milliseconds = 0.0;
    nextsampletime = 1000.0 * sampleinterval;
}
#endif

#ifdef HPROFILE
/*
 *	Set up the heap profile file.
 */

void
setuphpfile()
{
    long thetime;
    char* timestr;
    char b[10000];

    strcpy(b, progname);
    strcat(b, ".hp");
    hpfilename = copystring(b);
    if ((hpfile = fopen(hpfilename, "w")) == NULL) {
        fprintf(stderr, "cannot open %s\n", hpfilename);
        finish(1);
    }   
    fprintf(hpfile, "JOB \"%s", progname);
    switch(hashon) {
    case GROUP: 
	fprintf(hpfile, " -g"); 
	putres(hpfile, grpres);
	break;
    case MODULE : 
	fprintf(hpfile, " -m"); 
	putres(hpfile, modres);
	break;
    case PRODUCER : 
	fprintf(hpfile, " -p");
	putres(hpfile, prores);
	break;
    case CONSTRUCTION : 
	fprintf(hpfile, " -c");
	putres(hpfile, conres);
	break;
    case TYPE: 
	fprintf(hpfile, " -t");
	putres(hpfile, typres);
	break;
    default:
	fprintf(hpfile, " -?");
	break;
    }
    if (hashon != GROUP && grpresflag) { 
	fprintf(hpfile, " -g"); 
	putres(hpfile, grpres); 
    }
    if (hashon != MODULE && modresflag) { 
	fprintf(hpfile, " -m"); 
	putres(hpfile, modres); 
    }
    if (hashon != PRODUCER && proresflag) { 
	fprintf(hpfile, " -p"); 
	putres(hpfile, prores); 
    }
    if (hashon != CONSTRUCTION && conresflag) { 
	fprintf(hpfile, " -c"); 
	putres(hpfile, conres); 
    }
    if (hashon != TYPE && typresflag) { 
	fprintf(hpfile, " -t"); 
	putres(hpfile, typres); 
    }
    if (noautosample)
	fprintf(hpfile, " -i%.2f", sampleinterval);
    fprintf(hpfile, "\"\n");
    thetime = time((long*) 0);
    timestr = ctime(&thetime);
    timestr[ strlen(timestr) - 1 ] = '\0';   /* blat '\n' */
    fprintf(hpfile, "DATE \"%s\"\n", timestr);
    fprintf(hpfile, "SAMPLE_UNIT \"seconds\"\n");
    fprintf(hpfile, "VALUE_UNIT \"bytes\"\n");
}


/* 
 * Called every 20ms...
 */
SIGTYPE
timertick()
{
#if defined(HPUX) || defined(SOLARIS)
    signal(SIGVTALRM, timertick); /* gotta re-establish the interrupt handler on stupid systems */
#endif
    if (sampleflag) 
	SIGLAST;       /* clock is frozen during sampling */
    milliseconds += 20.0;          /* another 20ms */
    sampleflag = (milliseconds >= nextsampletime);
    if (sampleflag) 
	nextsampletime += 1000.0 * sampleinterval;
    SIGLAST;
}

void
setuptimer() 
{
    struct itimerval inttimer;

    inttimer.it_value.tv_sec = 0;
    inttimer.it_value.tv_usec = 20000;
    inttimer.it_interval = inttimer.it_value;
    signal(SIGVTALRM, timertick);
    setitimer(ITIMER_VIRTUAL, &inttimer, (struct itimerval *)0);
    milliseconds = 0.0;
    nextsampletime = 1000.0 * sampleinterval;
}

#endif

#if 0
scanvap(f)
int (*f)();
{
    extern int etext, edata;
    int *p;
    extern Tag VAP;

    for(p = &etext; p<&edata; p++) {
	if (*p == (Int)&VAP && p[1] == (Int)(p+3)) {
	    /* found a VAP outside the heap */
	    f(p);
	}
    }
}

PTR *vaps, *svaps;

noticevap(p)
PTR *p;
{
    *vaps++ = p;
}

scandata()
{
    svaps = vaps = xmalloc(2000 * sizeof(PTR*));
    scanvap(noticevap);
    printf("%d CAFs found\n", vaps-svaps);
}

chkcafs()
{
    PTR *p;
    
    for(p = svaps; p<vaps; p++) {
	dumpgraph(*p, dumpdepth);
	fprintf(stderr, "\n");
    }
}
#endif

void *
xmalloc(n)
size_t n;
{
    void *r;
    extern char *progname;

    r = malloc(n);
    if (r == 0) {
        fprintf(stderr, "%s: Out of memory\n", progname);
        exit(1);
    }
    return r;
}

void *
Xmalloc(n,msg)
size_t n;
char *msg;
{
    void *r;
    extern char *progname;

    r = malloc(n);
    if (r == 0) {
        fprintf(stderr, "%s:",progname);
	fprintf(stderr,msg,n);
        exit(1);
    }
    return r;
}

void *
xrealloc(p, n)
void *p;
size_t n;
{
    void *r;
    extern char *progname;
    /*extern void *realloc();*/

    r = realloc(p, n);
    if (r == 0) {
        fprintf(stderr, "%s: Out of memory\n", progname);
        exit(1);
    }
    return r;
}

#ifdef irix
/* make a dummy reference to something from libm to make the linker shut up */
dummy_ref() { (void)sin(1.0); }
#endif

/* Define some variables to make sure they are defined somewhere */
int *BitHeap;
