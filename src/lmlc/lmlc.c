/*
 * LML compiler front end.
 * Literate preprocessing contributed by Mark P. Jones.
 */

#ifdef CRAY
#define CCLINK "/bin/cc"
#endif

#ifdef __CYGWIN32__
#define RUNTIME_AR
#endif

#if defined(__NetBSD__) || defined(__FreeBSD__) || defined(linux)
#define CCLINK "/usr/bin/cc"
#endif

#ifdef _AIX
#define CCLINK "/bin/cc"
#define vfork fork
#endif

#if defined(hpux) && !defined(HPUX)
#define HPUX
#endif
#ifdef HPUX
#define CCLINK "/bin/cc"
#endif

#if defined(sun) && defined(__svr4__) && !defined(SOLARIS)
#define SOLARIS
#endif
#ifdef SOLARIS
#ifdef GCCLINK
/* use GCC to link, use path to find it */
#define CCLINK "gcc"
#else
#define CCLINK "/opt/SUNWspro/bin/cc"
#endif
#endif

#if defined(i386) && defined(SOLARIS)
#define SOLARIS386
#endif

#if !defined(SUNOS3) && defined(sun) && !defined(SOLARIS)
#ifndef SUNOS4
#define SUNOS4
#endif
#endif

#ifdef mips
#define MIPSOS
#ifdef sgi
#ifndef irix
#define irix
#endif
#define vfork() fork()
#endif
#endif

#ifdef __osf__
#define MIPSOS
#define _BSD
#endif

#ifndef QDSTLIB
#define QDSTLIB "/usr/local/lib/lmlc"
#endif

#include <stdio.h>
#include <signal.h>
#undef BUS_OBJERR

#include <sys/wait.h>

#include <stdlib.h>
#include <string.h>
#include <unistd.h>

extern char *version, *versiondate;

#define LMLDIR "LMLDIR"			/* Environment name of lml directory */
#define HBCDIR "HBCDIR"			/* Environment name of lml directory */
#define VLMLDIRDEF "/usr/local/lib/vlmlc"	/* Default place to look */
#define LMLDIRDEF QDSTLIB	/* Default place to look */
#define TMPNAME "/tmp/lmlcXXXXXX"	/* Temp name template */
#define HBC_LIBRARY "hbc_library"
#define AOUT "a.out"

#if defined(__NetBSD__) || defined(__FreeBSD__)
#define CPP "/usr/libexec/cpp"
#elif defined(SOLARIS) || defined(_AIX)
#define CPP "/usr/ccs/lib/cpp"
#else
#define CPP "/lib/cpp"			/* default preprocessor */
#endif

#if defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
#define AS "/usr/bin/as"		/* assembler */
#elif defined(SOLARIS) || defined(_AIX)
#define AS "/usr/ccs/bin/as"
#else
#define AS "/bin/as"			/* assembler */
#endif /* linux */

#ifdef CCLINK
#define LD CCLINK			/* linker */
#elif defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
#define LD "/usr/bin/ld"                /* linker */
#elif defined(SOLARIS) || defined(_AIX)
#define LD "/usr/ccs/bin/ld"            /* linker */
#else /* SOLARIS */
#define LD "/bin/ld"                    /* linker */
#endif

#define LIBX11 "-lX11"		/* string used to link X11 */
#define LIBX11AUX "-lXext"	/* more stuff for X */
#if defined(ultrix)
#define LMLX "LMLX"		/* used to drag in stuff from xlib.a */
#else
#define LMLX "_LMLX"		/* used to drag in stuff from xlib.a */
#endif

#define Strcpy(a,b) (void)strcpy(a,b)
#define Strcat(a,b) (void)strcat(a,b)
#define Malloc(u) malloc((unsigned)(u))
#define Fflush(f) (void)fflush(f)
#define Fclose(f) (void)fclose(f)
    
static void deleteenv();

char
    *prefix,				/* Where to look for things */
    *lib = "/lib",			/* directory for runtime stuff & standard functions */
    *clib = "/hlib",			/* directory for runtime stuff & standard functions */
    *clib1_3 = "/hlib1.3",		/* directory for runtime stuff & standard functions */
    *include = "/include",		/* include files */
    *parse = "/bin/lmlp",		/* parser */
    *cparse = "/bin/curryp",		/* parser */
    *comp = "/bin/lmlcomp",		/* compiler */
    *unlit = "/bin/unlit",              /* literate script processor /mpj/ */
    *evalupdunw = "/evalupdunw",
    *indirunw = "/indirunw",
    *trc = "/trc",
    *notrc = "/notrc",
    *opt = "/opt.o",
    *tagtable = "/tagtable.o",
    *showheapo = "/showheap.o",
    *noflagso = "/noflags.o",
    *dot_o = ".o",
    *dot_so = ".so",
    *p_dot_o = "_p.o",
    *ph_dot_o = "_ph.o",
    *ph_dot_so = "_ph.so",
    *dot_a = ".a",
    *p_dot_a = "_p.a",
    *ph_dot_a = "_ph.a",
    *runtime = "/runtime",		/* runtime routines */
    *xlib = "/xlib.a";		        /* special X11 support */
char *Cpp = CPP;
char *As = AS;
char *Ld = LD;
char *currylib;
char *maino = "C_main";

#ifdef CRAY
char *rt2[] = {"/crun.o", "/rtcray.o", "\0"};
#else
char *rt2[] = { "\0" };
#endif
char   *thelib, *thelibstub;
char   *liba   = "/lib.a";			/* standard functions */
char   *libstuba = "/libstub.a";			/* standard functions */
char   *liba_p = "/lib_p.a";		/* profile standard functions */
char   *liba_ph = "/lib_ph.a";		/* heap profile standard functions */
#ifndef CRAY
char 
#if defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
    *libgcc = "-lgcc",        /* GCC library */
    *libbsd = "-lbsd",        /* BSD library */
#endif /* linux */
    *libc = "-lc",			/* C library */
    *libm = "-lm",		        /* C math library */
#if defined(_AIX)
    *libtermcap = "-lcurses",
#elif defined(__CYGWIN32__)
    *libtermcap = "-lncurses",
#else
    *libtermcap = "-ltermcap",
#endif
#ifdef MIPSOS
    *libc_p = "-lc",			/* C profile library */
    *libm_p = "-lm",		        /* C math library */
    *libextra_p = "/usr/lib/cmplrs/cc/libprof1.a",
#else /* MIPSOS */
#if defined(__NetBSD__) || defined(__FreeBSD__)
    *libc_p = "-lc",			/* C profile library */
    *libm_p = "-lm",		        /* C math library */
#else
    *libc_p = "-lc_p",			/* C profile library */
    *libm_p = "-lm_p",		        /* C math library */
#endif /* __NetBSD__ */
#endif /* MIPSOS */
    *libg = "-lg",			/* Debug library */
    *libpps = "-lpps",			/* Parallel library */
    *libseq = "-lseq",			/* Parallel library */

#if defined(SOLARIS)
/* Mega-yuck!  These compiler paths should not be wired in.  But I don't have time to do it right */
#if defined(i386)
    *mapfile = "/mapfile",
    *crt0 = "/opt/SUNWspro/SC3.0.1/lib/crti.o",
    *crt1 = "/opt/SUNWspro/SC3.0.1/lib/crt1.o",
    *crt2 = "/opt/SUNWspro/SC3.0.1/lib/__fstd.o",
    *crt3 = "/opt/SUNWspro/SC3.0.1/lib/values-xt.o",
    *crt4 = "-Y",
    *crt5 = "P,:/opt/SUNWspro/SC3.0.1/lib:/usr/ccs/lib:/usr/lib",
    *crt6 = "-Qy",
    *crtlast = "/opt/SUNWspro/SC3.0.1/lib/crtn.o",
    *crt0_p = "/X",	/* C profiling startup */
    *crt0_pg = "/X";	/* C profiling startup */
#else
    *crt0 = "/opt/SUNWspro/SC3.0/lib/crti.o",
    *crt1 = "/opt/SUNWspro/SC3.0/lib/crt1.o",
    *crt2 = "/opt/SUNWspro/SC3.0/lib/cg89/__fstd.o",
    *crt3 = "/opt/SUNWspro/SC3.0/lib/values-xs.o",
    *crt4 = "-Y",
    *crt5 = "P,:/usr/ucblib:/opt/SUNWspro/SC3.0/bin/../lib:/opt/SUNWspro/SC3.0/bin:/usr/ccs/lib:/usr/lib",
    *crt6 = "-R:/usr/ucblib",
    *crtlast = "/opt/SUNWspro/SC3.0/lib/crtn.o",
    *crt0_p = "/X",	/* C profiling startup */
    *crt0_pg = "/X";	/* C profiling startup */
#endif
#elif defined(SUNOS4)
    *crt0 = "/usr/lib/crt0.o",		/* C startup */
    *crt0_p = "/usr/lib/mcrt0.o",	/* C profiling startup */
    *crt0_pg = "/usr/lib/gcrt0.o",	/* C profiling startup */
    *crt1 = "/usr/lib/Mcrt1.o";		/* more startup */
#elif defined(MIPSOS)
#if defined(irix)
    *crtlast = "/usr/lib/crtn.o",
    *crt0 = "/usr/lib/crt1.o",		/* C startup */
#else
    *crt0 = "/usr/lib/cmplrs/cc/crt0.o",		/* C startup */
#endif
    *crt0_p = "/usr/lib/cmplrs/cc/mcrt0.o",		/* C profiling startup */
    *crt0_pg = "/No -pg on this machine type.";	/* C profiling startup */
#elif defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
    *crt0 = "/usr/lib/crt0.o",                /* C startup */
    *crt0_p = "/usr/lib/gcrt0.o",     /* C profiling startup */
    *crt0_pg = "/usr/lib/gcrt0.o";    /* C profiling startup */
#else /* "normal" */
    *crt0 = "/lib/crt0.o",		/* C startup */
    *crt0_p = "/lib/mcrt0.o",		/* C profiling startup */
    *crt0_pg = "/usr/lib/gcrt0.o";	/* C profiling startup */
#endif
#endif /* CRAY */

void addliba();

char *tmppre = TMPNAME;			/* temp name prefix */
char *tmps, *tmpm, *tmpi, *tmpp, *tmpt, *tmpg, *tmpot, *tmpl, *tmpbwm;
char *prog, *argv0;				/* program name */
char aout[] = AOUT;
char *outfile = aout;			/* name of result */
char *incpath;			        /* include path (also for ld) */
static char *copystring();
int cflag = 0;				/* separate compilation */
int Sflag = 0;				/* keep .s file */
int Gflag = 0;				/* produce .g file */
int bwmflag = 0;		        /* produce .bmc */
int gflag = 0;				/* link for debug */
int tflag = 1;				/* do typechecking */
int verbose = 0;			/* verbose front end */
int pflag = 0;				/* do profiling */
int pgflag = 0;				/* do graph profiling */
int phflag = 0;				/* do heap profiling */
int bothtypes = 0;			/* print both import files */
int useindir = 0;		        /* use indirection instead of updating */
int pedantic = 0;			/* be fuzzy about Haskell */
int unlitnoisy = 1;                     /* make noise about bad lit. scripts */
#ifdef vG
int nuflag = 1;
int Pflag = 0;				/* parallel version of the code */
int statistics = 0;
#else
int nuflag = 0;
int Pflag = 0;				/* parallel version of the code */
int statistics = 0;
#endif
int xflag = 0;				/* Use a compiler program other that the production one */
int curry = 0;				/* compile curry instead */
int h1_3 = 1;			        /* Haskell 1.3 */
int docpp = 0;			        /* do preprocessing */
int nounlink = 0;		        /* do not unlink temporaries */
int nohi = 0;			        /* do not produce .hi files */
int traceflag = 0;		        /* enable tracing */
int Oflag = 0;			        /* Optimize */
int lmlx = 0;			        /* add LMLX specific flags */
int showheap = 0;		        /* link with heap show stuff */
int noflags = 0;		        /* link with dummy flag decoder */
int staticlink = 0;			/* link statically */
int fstatic = 1;			/* don't look for lib/lib.so etc */
int pic = 0;				/* generate PIC */
int retry = 0;				/* retry execution */
int bad = 0;

#define MAXARGS 1000
struct args {
    int nflags;
    char *flags[MAXARGS];
    int nargs;
    char *args[MAXARGS];
};

#if defined(_AIX)
/*struct args ppargs = { 2, {"-C", "-qlanglvl=ansi"}, 0, {0}};*/
struct args ppargs = { 3, {"-C", "-D_IBMR2", "-D_AIX"}, 0, {0}}; /* ansi does not work well */
#elif defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__) || defined(__CYGWIN32__)
struct args ppargs = { 2, {"-C", "-traditional"}, 0, {0}};
#elif defined(__alpha) && defined(__osf__)
struct args ppargs = { 3, {"-C", "-D__alpha", "-D__osf__"}, 0, {0}};
#else /* "normal" */
struct args ppargs = { 1, {"-C"}, 0, {0}};
#endif

#ifdef CCLINK
#ifdef HPUX
struct args ldargs = { 1, {"-Wl,-a,archive"}, 0, {0} };
#else
struct args ldargs = { 0, {0}, 0, {0} };
#endif
#else
#ifdef SYSV
struct args ldargs = { 0, {0}, 0, {0} };
#else /* SYSV */
#ifdef SUNOS4
#ifdef mc68000
struct args ldargs = { 6, {"-dc", "-dp", "-e", "start", "-X", "-L/usr/lib/f68881" }, 0, {0} };
#endif
#ifdef sparc
struct args ldargs = { 5, {"-dc", "-dp", "-e", "start", "-X"}, 0, {0} };
#endif
#ifdef sun386
struct args ldargs = { 5, {"-dc", "-dp", "-e", "_start", "-X"}, 0, {0} };
#endif
#else /* SUNOS4 */
#ifdef _AIX
struct args ldargs = { 5, {"-T512", "-H512", "-bhalt:4", "-bnodelcsect", "-btextro"}, 0, {0} };
#else
#ifdef ultrix
struct args ldargs = { 1, {"-X"}, 0, {0} };
#else /* ultrix */
#ifdef __osf__
struct args ldargs = { 2, {"-G","8"}, 0, {0} };
#else /* __osf__ */
#ifdef SOLARIS
struct args ldargs = { 0, {0}, 0, {0} };
#else /* SOLARIS */
#if defined(__NetBSD__) || defined(__FreeBSD__)
struct args ldargs = { 3, {"-Bstatic", "-e", "start"}, 0, {0} };
#else
#ifdef irix
struct args ldargs = { 11, {"-elf", "-_SYSTYPE_SVR4", "-require_dynamic_link", "_rld_new_interface", "-no_unresolved", "-Wx,-G", "0", "-call_shared", "-g0", "-KPIC", "-nocount", },
			   0, {0} };
#else
#ifdef HPUX
struct args ldargs = { 4, {"-a", "archive", "-u", "main"}, 0, {0} };
#else
struct args ldargs = { 1, {"-X"}, 0, {0} };
#endif /* HPUX */
#endif /* irix */
#endif /* __NetBSD__ */
#endif /* SOLARIS */
#endif /* __osf__ */
#endif /* ultrix */
#endif /* _AIX */
#endif /* SUNOS4 */
#endif /* SYSV */
#endif /* CCLINK */
struct args ldeargs = { 0, {0}, 0, {0} };
#ifdef mips
#if defined(ultrix)
struct args asargs = { 1, {"-G0"}, 1, {"-o"} };
#else /* irix */
struct args asargs = { 2, {"-G0", "-w"}, 1, {"-o"} };
#endif
#else
struct args asargs = { 0, {0}, 1, {"-o"} };
#endif
struct args cargs  = { 0, {0}, 1, {"-"} };
struct args pargs  = { 0, {0}, 0, {0} };
struct args rmargs = { 0, {0}, 0, {0} };
struct args unlitargs  = { 0, {0}, 0, {0} };

/*VARARGS2*/
error(ecode, msg, a1, a2, a3)
char *msg;
long a1, a2, a3;
{
    fprintf(stderr, "%s: ", prog);
    fprintf(stderr, msg, a1, a2, a3);
    fprintf(stderr, "\n");
    if (ecode) {
	unlinkfiles();
	exit(ecode);
    }
}

addflag(ap, str)
struct args *ap;
char *str;
{
    if (ap->nflags >= MAXARGS)
	error(1, "Too many arguments");
    ap->flags[ap->nflags++] = str;
}

addarg(ap, str)
struct args *ap;
char *str;
{
    if (ap->nargs >= MAXARGS)
	error(1, "Too many arguments");
    ap->args[ap->nargs++] = str;
}

addarga(ap, str)
struct args *ap;
char *str;
{
    char *p;
    
    p = Malloc(strlen(str) + 1);
    if (p == 0)
	error(1, "Out of memory.");
    Strcpy(p, str);
    addarg(ap, p);
}

delarg(ap, n)
struct args *ap;
{
    ap->nargs -= n;
}

delflag(ap, n)
struct args *ap;
{
    ap->nflags -= n;
}

char *
setprefix(p, d)
char *p, *d;
{
    char *t;
    t = Malloc(strlen(p)+strlen(d)+1);
    if (t == 0)
	error(1, "Out of memory.");
    Strcpy(t, p);
    Strcat(t, d);
    return t;
}

static void set_suffix(f,s)
char *f,*s;
{
  int dp=strlen(f);
  while(dp>=0 && f[dp]!=s[0])
    dp--;
  if (dp<0) dp=strlen(f);
  strcpy(f+dp,s);
}

void
intr()
{
    unlinkfiles();
    exit(1);
}

#define SET(v, s) {char *p = getenv(s); if (p) v = p; }

init()
{
    int i;
    char *hlib;
    
    SET(As, "LML_AS");
    SET(Ld, "LML_LD");
    SET(Cpp, "LML_CPP");
    
    prefix = getenv(HBCDIR);
    if (prefix == 0)
        prefix = getenv(LMLDIR);
    if (prefix == 0) {
#ifdef __CYGWIN32__
        char *p;
	int ns;
	for(ns = 0, p = argv0+strlen(argv0)-1; p > argv0 && ns < 2; )
	    ns += *--p == '/';
	if (p > argv0) {
	    *p = 0;
	    prefix = copystring(argv0);
	    *p = '/';
	} else
#endif
	prefix = nuflag ? VLMLDIRDEF : LMLDIRDEF;
    }
    {
	char b[2000];
	sprintf(b, "%s=%s", LMLDIR, prefix);
	putenv(copystring(b));
    }
    parse = setprefix(prefix, curry ? cparse : parse);
    lib = setprefix(prefix, lib);
    if(!xflag)
	comp = setprefix(prefix, comp);
    unlit = setprefix(prefix,unlit);
#if 0
    {
        char *p, b[512];
	p = getenv("TMP");
	if (p == 0) p = getenv("TEMP");
	if (p != 0) {
	    sprintf(b, "%s/lmlcXXXXXX", p);
	    tmppre = b;
	}
        tmppre = copystring(tmppre);
    }
#else
    tmppre = copystring(tmppre);
#endif
    (void)mktemp(tmppre);
    tmps = setprefix(tmppre, ".s");
    tmpm = setprefix(tmppre, ".m");
    tmpi = setprefix(tmppre, ".i");
    tmpp = setprefix(tmppre, ".p");
    tmpt = setprefix(tmppre, ".t");
    tmpot = setprefix(tmppre, ".ot");
    tmpg = setprefix(tmppre, ".g");
    tmpl = setprefix(tmppre, ".l");
    tmpbwm = setprefix(tmppre, ".bmc");
    include = setprefix(lib, include);
    runtime = setprefix(lib, runtime);
#if defined(RUNTIME_AR) && 0
    runtime = setprefix(runtime, phflag ? ph_dot_a : pflag ? p_dot_a : dot_a);
#else
    runtime = setprefix(runtime, phflag ? (fstatic ? ph_dot_o : ph_dot_so) : pflag ? p_dot_o : fstatic ? dot_o : dot_so);
#endif
    evalupdunw = setprefix(lib, useindir ? indirunw : evalupdunw);
    evalupdunw = setprefix(evalupdunw, phflag ? ph_dot_o : pflag ? p_dot_o : dot_o);
    trc = setprefix(lib, traceflag ? trc : notrc);
#if defined(RUNTIME_AR) && 0
    trc = setprefix(trc, phflag ? ph_dot_o : pflag ? p_dot_o : dot_a);
#else
    trc = setprefix(trc, phflag ? ph_dot_o : pflag ? p_dot_o : dot_o);
#endif
    tagtable = setprefix(lib, tagtable);
    showheapo = setprefix(lib, showheapo);
    noflagso = setprefix(lib, noflagso);
    opt = setprefix(lib, opt);
    { int i;
      for(i=0;*(rt2[i]);i++)
	rt2[i] = setprefix(lib, rt2[i]);
    }
    liba = phflag ? liba_ph : pflag ? liba_p : liba;
    thelib = setprefix(lib, liba);
    thelibstub = setprefix(lib, libstuba);
    if (curry) {
	currylib = setprefix(setprefix(prefix, h1_3 ? clib1_3 : clib), liba);
    }
#ifndef CRAY
    crt0 = pflag ? (pgflag ? crt0_pg : crt0_p) : crt0;
    libc = pflag ? libc_p : libc;
    libm = pflag ? libm_p : libm;
#endif
    addarg(&ppargs, setprefix("-I", include));
#ifdef SOLARIS386
    mapfile = setprefix(lib, mapfile); /* tell the linker to put the text at a lower address */
#endif
}

void
xsignal(s)
int s;
{
    if (signal(s, SIG_IGN) != SIG_IGN)
	(void)signal(s, intr);
}

static char *lmldir;

static void
decodeargs(argcp, argvp)
int *argcp;
char ***argvp;
{
    int argc = *argcp;
    char **argv = *argvp;

    while (argc && argv[0][0] == '-') {
	while (*++*argv) {
	    if (strcmp(*argv, "pedantic") == 0) {
		pedantic++;
		goto nextarg;
	    } else if (strcmp(*argv, "no-pedantic") == 0) {
		pedantic = 0;
		goto nextarg;
	    } else if (strcmp(*argv, "litquiet") == 0) {
                unlitnoisy = 0;
                goto nextarg;
            } else if (strcmp(*argv, "litnoisy") == 0) {
                unlitnoisy = 1;
                goto nextarg;
            } else if (strcmp(*argv, "bwm") == 0) {
                bwmflag = 1;
                goto nextarg;
            } else if (strcmp(*argv, "nohi") == 0) {
                nohi = 1;
                goto nextarg;
            } else if (strcmp(*argv, "lmlx") == 0) {
                lmlx = 1;
                goto nextarg;
            } else if (strcmp(*argv, "showheap") == 0) {
                showheap = 1;
                goto nextarg;
            } else if (strcmp(*argv, "cpp") == 0) {
                docpp++;
                goto nextarg;
            } else if (strcmp(*argv, "noflags") == 0) {
                noflags = 1;
                goto nextarg;
            } else if (strcmp(*argv, "fshared") == 0) {
                fstatic = 0;
                goto nextarg;
            } else if (strcmp(*argv, "static") == 0 || strcmp(*argv, "Bstatic") == 0) {
		staticlink++;
		goto nextarg;
            } else if (strcmp(*argv, "dynamic") == 0 || strcmp(*argv, "Bdynamic") == 0) {
		staticlink = 0;
		goto nextarg;
	    } else if (strcmp(*argv, "1.3") == 0 || strcmp(*argv, "1.4") == 0) {
		h1_3++;
		goto nextarg;
	    } else if (strcmp(*argv, "1.2") == 0) {
		h1_3 = 0;
		goto nextarg;
	    } else if (strcmp(*argv, "pic") == 0) {
	        pic++;
		goto nextarg;
	    } else if (strcmp(*argv, "underscore") == 0) {
	        addflag(&pargs, "-_");
		goto nextarg;
	    } else if (strcmp(*argv, "view") == 0) {
	        addflag(&pargs, "-W");
	        addarg(&cargs, "-fview");
		goto nextarg;
	    } else if (strcmp(*argv, "retry") == 0) {
		retry++;
		goto nextarg;
	    }
	    switch(**argv) {
	    case 's':
		addflag(&ldargs, "-s");
		break;
	    case 'U':
		nounlink++;
		break;
	    case 'C':
		curry++;
		break;
	    case 'M':
		docpp++;
		break;
	    case 'n':
		if (strcmp(*argv, "nuG") == 0 || strcmp(*argv, "nu") == 0) {
		    nuflag++;
		    goto nextarg;
		} else
		    goto badarg;
#if 0
	    case 'w':	/* compiler debug flags */
		addarg(&cargs, "-fall");
		break;
#endif
	    case 'c':
		cflag++;
		break;
	    case 'o':
		if (--argc <= 0)
		    error(1, "-o name missing");
		outfile = *++argv;
		goto nextarg;
	    case 'S':
		Sflag++;
		break;
	    case 'G':
		Gflag++;
		break;
	    case 'v':
		verbose++;
		addarg(&cargs, "-fverbose");
		break;
	    case 'T':
		traceflag++;
		addarg(&cargs, "-ftrace");
		break;
            case 'O':
		Oflag++;
		addarg(&cargs, "-O");
		break;
	    case 'P':
		if (argv[0][1] == '0')
		    Pflag = 0, ++*argv;
		else if (argv[0][1] == '1')
		    Pflag = 1, ++*argv;
		else
		    Pflag ^= 1;
		break;
	    case 't':
		tflag = 0;
		break;
	    case '1':
		addflag(&pargs, "-o");
		break;
	    case 'Z':
		addflag(&pargs, "-Z");
		addarg(&cargs, "-ffullname");
		break;
	    case 'z':
		addflag(&pargs, "-z");
		break;
	    case 'A':
	    case 'H':
		/* Pass on stack and heap flags to the compiler */
		addflag(&cargs, *argv-1);
		goto nextarg;
	    case 'D':
	    case 'I':
		addflag(&ppargs, *argv-1);
		goto nextarg;
	    case 'f':
		if (strcmp(*argv, "fboth") == 0)
		    bothtypes++;
		else if (strcmp(*argv, "findir") == 0)
		    useindir++;
		else if (strcmp(*argv, "fccall") == 0)
		    addflag(&pargs, "-c");
		else if (strcmp(*argv, "funiv-type") == 0)
		    addflag(&pargs, "-u");
		/* fall into ... */
	    case 'm':
		addarg(&cargs, *argv-1);
		goto nextarg;
	    case 'X':
		addarg(&cargs, *argv+1);
		goto nextarg;
	    case 'Y':
		addflag(&cargs, *argv+1);
		goto nextarg;
	    case 'K':
		addflag(&ldargs, *argv+1);
		goto nextarg;
	    case 'p':
		if (argv[0][1] == 'h') {
		    phflag++;
		    goto nextarg;
		}
		pflag++;
		if (argv[0][1] == 'g') {
		    pgflag++;
		    goto nextarg;
		}
		break;
	    case 'x':
		xflag++;
		if (--argc <= 0)
		    error(1, "-x name missing");
		comp = *++argv;
		goto nextarg;
	    case 'g':
		gflag++;
		break;
	    case 'a':
		statistics ^= 1;
		break;
	    case 'B':
#ifdef SUNOS4
		addflag(&ldargs, *argv-1);
#endif
		goto nextarg;
	    case 'l':
		addflag(&ldeargs, *argv-1);
		goto nextarg;
	    case 'L':
		addflag(&ldargs, *argv-1);
		goto nextarg;
	    case '#':
		addflag(&ldargs, *argv);
		goto nextarg;
#ifdef SOLARIS
	    case 'R':
		addflag(&ldargs, *argv-1);
		goto nextarg;
#endif
	    case 'u':
		addflag(&ldargs, "-u");
		if (argc > 1) {
		    argc--, argv++;
		    addflag(&ldargs, *argv);
		} else {
		    fprintf(stderr, "Missing name to -u\n");
		    exit(1);
		}
		goto nextarg;
	    case 'i':
		addflag(&pargs, *argv-1);
		if (argv[0][1] == '\0') {
		    incpath = 0;
		} else {
		    char buf[10000];
		    char *arg = *argv + 1;
		    if (*arg == '%') {
			arg++;
			strcpy(buf, lmldir);
			strcat(buf, "/");
		    } else {
			strcpy(buf,"");
		    }
		    strcat(buf, arg);
		    if (incpath) {
			strcat(buf,":");
			strcat(buf, incpath);
		    }
		    incpath = copystring(buf);
		}
		goto nextarg;
	    default:
	badarg:
		fprintf(stderr, "%s: Warning, strange flag: -%s\n", prog, *argv);
		goto nextarg;
	    }
	}
    nextarg:
	argc--, argv++;
    }
    *argcp = argc;
    *argvp = argv;
}

#ifdef __CYGWIN32__
char *libdir;
#endif

void
main(argc, argv)
int argc;
char *argv[];
{
#ifdef SOLARIS
    addarg(&cargs, "-msolaris");
#endif
#ifdef linux
    addarg(&cargs, "-mlinux");
#endif

    deleteenv("LANG");		/* LANG confuses `as' on HPUX */

    argv0 = argv[0];
    prog = argv[0];
    {
	char *p;
	p = strrchr(prog, '/');
	if (!p)
	    p = prog;
	else
	    p++;
	prog = p;
	if (*p == 'v') {
	    nuflag++;
	    p++;
	}
	if (strncmp(p, "hbc", 3) == 0 || strncmp(p, "HBC", 3) == 0)
	    curry++;
    }
    if (curry)
	pedantic++;
    xsignal(SIGINT);
    xsignal(SIGHUP);
    xsignal(SIGTERM);
    argc--, argv++;
    {
	/* prescan argument list for certain arguments */
	char **p;
	for(p = argv; *p && **p == '-'; p++) {
	    if (strcmp(*p, "-o") == 0 && p[1])
	        p++;
	    if (strcmp(*p, "-1.3") == 0 || strcmp(*argv, "1.4") == 0)
		h1_3 = 1;
	    if (strcmp(*p, "-1.2") == 0)
		h1_3 = 0;
	}
    }
    {
	char *p, b[1000];
	lmldir = getenv("HBCDIR");
	if (!lmldir)
	  lmldir = getenv("LMLDIR");
	if (!lmldir)
	    lmldir = LMLDIRDEF;
	if (curry) {
	    if (h1_3)
		sprintf(b, "%s%s:.:%s/%s1.3", lmldir, clib1_3, lmldir, HBC_LIBRARY);
	    else
		sprintf(b, ".:%s/%s", lmldir, HBC_LIBRARY);
	    incpath = copystring(b);
	} else {
	    incpath = 0;
	}
	if ((p = getenv("LMLINCPATH")) && !curry)
	    incpath = p;
	if ((p = getenv("HBCINCPATH")) && curry)
	    incpath = p;
    }
#if defined(_AIX) || defined(__alpha) || defined(IRIX)
    /* some machines need more memory */
    addflag(&cargs, "-H20M");
#endif
    decodeargs(&argc, &argv);
    
    fstatic = fstatic | staticlink;
    if (!curry)
	h1_3 = 0;
    if (h1_3) {
	addflag(&pargs, "-3");
	addarg(&cargs, "-f1.3");
    } else {
	addflag(&pargs, "-2");
	addarg(&cargs, "-f1.2");
    }
    if (pic) {
        addarg(&cargs, "-fpic");
#ifdef SOLARIS
	addflag(&asargs, "-K");
	addflag(&asargs, "PIC");
#endif
    }
    if (pedantic) {
	addarg(&cargs, "-fpedantic");
	addarg(&pargs, "-P");
	if (pedantic > 1)
	    addarg(&pargs, "-P");	    
    }
    if (Pflag)
	nuflag++;
    if (getenv("nuG"))
	nuflag++;
    if (nuflag) {
/*	statistics ^= 1;*/
	addarg(&cargs, "-fnu");
    }
    if (Pflag) {
	addarg(&cargs, "-fparallel");
	addflag(&ldargs, "-Z100000");
    }
    if (curry) {
	addarg(&ppargs, "-D__HBC__");
	addarg(&ppargs, "-D__HASKELL__");
	if (h1_3)
	    addarg(&ppargs, "-D__HASKELL_1_3__");
    } else {
	addarg(&ppargs, "-D__LMLC__");
    }
    if (statistics)
	addarg(&cargs, "-fstatistics");
    init();

#ifdef HPUX
    /* Newer versions of HPUX have moved some binaries. */
    if (access(As, X_OK))
	As = "/usr/ccs/bin/as";
    if (access(Ld, X_OK))
	Ld = "/usr/ccs/bin/ld";
    if (access(Cpp, X_OK))
	Cpp = "/opt/langtools/lbin/cpp";
#endif
#ifdef __CYGWIN32__
    As = setprefix(prefix, "/cygbin/as");
    Ld = setprefix(prefix, "/cygbin/ld");
    if (access(Cpp, X_OK))
      Cpp = setprefix(prefix, "/cygbin/cpp");
    crt0 = setprefix(prefix, "/cyglib/crt0.o");
    libc = "-lcygwin";
    libdir = setprefix("-L", setprefix(prefix, "/cyglib"));
#endif

    if (tflag)
	;			/* -ftype is default */
    else
	addarg(&cargs, "-fno-type");
    if (Gflag) {
	addarg(&cargs, "-fG-code");
	addarg(&cargs, "-fno-code");
    }
    if (bwmflag) {
	addarg(&cargs, "-fbwm");
/*	addarg(&cargs, "-fnu");*/
    }
    if (pflag && phflag) {
	fprintf(stderr, "Sorry, cannot have both -p and -ph");
    }
    if (pflag) {
	addarg(&cargs, "-fprofile");
    }
    if (phflag) {
	addarg(&cargs, "-fprofile-heap");
    }
    if (curry) {
	addarg(&cargs, "-fcurry");
    } else {
	docpp++;
    }
#ifdef __osf__
    if (!Oflag) {
	addflag(&asargs, "-O0");
	addflag(&ldargs, "-O0");
    }
#endif
#if 0 /*def mips*/
    /* Force data segment to be in the same 256M segment as the text.
     * This ensures that dynamically loaded code can jump to the text segment.
     */
    addarg(&ldargs, "-D");
    addarg(&ldargs, "08000000");
#endif
#if 0
#ifdef __osf__
    staticlink++;		/* There is a bug in the dynamic linking XXX */
#endif
#endif
    if (staticlink) {
#if defined(SOLARIS) || defined(SUNOS4)
	addarg(&ldargs, "-Bstatic");
#endif
#if defined(SOLARIS) && !defined(CCLINK)
#ifdef i386
	addarg(&ldargs, "/opt/SUNWspro/SC3.0.1/lib/crti.o");
#else
	addarg(&ldargs, "/opt/SUNWspro/SC3.0/lib/crti.o");
#endif
#endif
#ifdef __osf__
	addarg(&ldargs, "-non_shared");
#endif
#ifdef _AIX
	addarg(&ldargs, "-bnoso");
	addarg(&ldargs, "-bI:/lib/syscalls.exp");
#endif
#if defined(__NetBSD__) || defined(__FreeBSD__) || defined(linux)
#if defined(CCLINK)
	addarg(&ldargs, "-static");
#endif /* CCLINK */
#endif /* __NetBSD__ */
    } else {
#ifdef __osf__
	addarg(&ldargs, "-call_shared");
#endif
    }

#ifdef SOLARIS386
    addflag(&ldargs, "-M");
    addflag(&ldargs, mapfile);
#endif

#if !(defined(CCLINK) || defined(CRAY) || defined(SOLARIS))
    addarg(&ldargs, crt0);
#endif
#if defined(CCLINK)
    if (pflag)
        addflag(&ldargs, "-p");
#endif
#if defined(SUNOS4) && defined(mc68000) && !defined(CCLINK)
    addarg(&ldargs, crt1);
#endif /* SUNOS4 && mc68000 */

#if defined(SOLARIS) && !defined(CCLINK)
    addarg(&ldargs, crt1);
    addarg(&ldargs, crt2);
    addarg(&ldargs, crt3);
    addarg(&ldargs, crt4);
    addarg(&ldargs, crt5);
    addarg(&ldargs, crt6);
#endif
    /* XXX add test for shared library */
#if !defined(RUNTIME_AR)
    addarg(&ldargs, runtime);
#endif
    addarg(&ldargs, tagtable);
    if (!nuflag) {
	addarg(&ldargs, evalupdunw);
#if !defined(RUNTIME_AR)
	addarg(&ldargs, trc);
#endif
    }
    if (Oflag) {
	addarg(&ldargs, opt);	/* use full heap size all the time */
    }
#if defined(CRAY)
    {
	int i;
	for(i=0;*(rt2[i]);i++) {
	    addarg(&ldargs, rt2[i]);
	}
    }
#endif
    {
	/* Run through args and look for -o */
	char **p, **q;
	for(p = q = argv; *p; ) {
	    if (strcmp(*p, "-o") == 0) {
		if (!p[1]) {
		    error(1, "-o name missing");
		}
		outfile = p[1];
		p += 2;
		argc -= 2;
	    } else
		*q++ = *p++;
	}
	*q = 0;
    }
    {
	/* sanity check the -o file. */
	int l = strlen(outfile);
	if (l > 3 && (strcmp(&outfile[l-4], ".lhs") == 0) ||
	    l > 2 && (strcmp(&outfile[l-3], ".hs") == 0 || strcmp(&outfile[l-3], ".hi")  == 0) ||
            l > 1 && (strcmp(&outfile[l-2], ".m")  == 0 || strcmp(&outfile[l-2], ".t")   == 0 || 
		      strcmp(&outfile[l-2], ".c")  == 0 || strcmp(&outfile[l-2], ".s")   == 0)) {
	    if (access(outfile, 2) == 0)
		error(1, "Sorry, but BIG BROTHER will not let you overwrite %s.\n", outfile);
	}
    }
    if (showheap) {
	addarg(&ldargs, showheapo);
	    addarg(&ldeargs, LIBX11);
    }
    if (lmlx) {
	addarg(&ldargs, "-u");
	addarg(&ldargs, LMLX);
    }
    if (argc == 0) {
        if (verbose)
            fprintf(stderr, "%s %s, %s\n", prog, version, versiondate);
        else
            fprintf(stderr, "Usage: %s [flags] files\n", prog);
	exit(1);
    }

    while(--argc >= 0)
	processfile(*argv++);
    if (!bad && !cflag && !Sflag && !Gflag && !bwmflag)
	linkit();
    rmofiles();
    unlinkfiles();
    exit(bad);
}

unlinkfiles()
{
    if (nounlink)
	return;
    (void)unlink(tmps);
    (void)unlink(tmpi);
    (void)unlink(tmpp);
    (void)unlink(tmpt);
    (void)unlink(tmpm);
    (void)unlink(tmpg);
    (void)unlink(tmpot);
    (void)unlink(tmpl);
    (void)unlink(tmpbwm);
}

void
addliba(liba)
char *liba;
{
    if (fstatic) {
	addarg(&ldargs, copystring(liba));
    } else {
	char *l = malloc(strlen(liba)+10);
	char *la;
	int fd;
	Strcpy(l, liba);
	la = l + strlen(l) - 1; /* points at the a in lib.a */
	Strcpy(la, "so");
	if ((fd = open(l, 0)) < 0) {
	    /* shlib not found :-( */
	    Strcpy(la, "a");
	} else {
	    close(fd);
	}
	addarg(&ldargs, l);	/* add whichever */
    }
}

linkit()
{
    int i;

#ifndef CRAY
    if (gflag) {
	addarg(&ldargs, libg);
    }
#endif
#ifdef sequent
    if (Pflag) {
	addarg(&ldargs, libpps);
    }
#endif
    if (incpath) {
#define PATHSEP ':'
	/* Add all lib.a in the incpath */
	char *p, *q, *n, buffer[1000];
	int fd;

	for(p = incpath; *p; ) {
	    for(n = p, q = buffer; *n && *n != PATHSEP; n++)
		*q++ = *n;
	    if (n != p)
		*q++ = '/';
	    if (*n)
		n++;
	    p = n;
	    *q = 0;
	    if (buffer[0] && strcmp(buffer, "./") != 0) {
		strcpy(q, liba+1);
		if ((fd = open(buffer, 0)) >= 0) {
		    FILE *linkf;
		    close(fd);
		    addliba(buffer);
		    set_suffix(buffer,"/linkargs");
		    if (linkf = fopen(buffer,"r")) {
		      char *linkarg[1000];
		      while(fscanf(linkf,"%s",linkarg)==1)
			addarg(&ldargs,copystring(linkarg));
		      fclose(linkf);
		    }
		}
	    }
	}
    }
    if (noflags)
	addarg(&ldargs, noflagso);
    if (curry) {
#if defined(_AIX)
#else
	addarg(&ldargs, "-u");	/* pull in main from currylib */
	addarg(&ldargs, maino);
#endif
	addliba(currylib);
    }
    addliba(thelib);
    addarg(&ldargs, thelibstub);
    for(i = 0; i < ldeargs.nargs; i++)
	addarg(&ldargs, ldeargs.args[i]);
#ifdef sequent
    addarg(&ldargs, libseq);
#endif /* sequent */
#if defined(CCLINK)
    if (pflag) {
	addflag(&ldargs, pgflag ? "-pg" : "-p");
    }
#endif
#if defined(MIPSOS) && !defined(CCLINK)
    if (pflag) {
	addarg(&ldargs, libextra_p);
    }
#endif /*MIPSOS */
    addarg(&ldargs, libtermcap);
#ifdef linux
    addarg(&ldargs, libbsd);
#endif /* linux */
#if defined(SOLARIS) && !defined(GCCLINK)
    if (strcmp(Ld, "gcc") != 0) {
        addarg(&ldargs, "-Bdynamic");
        /*        addarg(&ldargs, "-lsunmath");*/
    }
#endif /* SOLARIS */
#if defined(RUNTIME_AR)
    addarg(&ldargs, runtime);
    addarg(&ldargs, trc);
#endif
#ifndef CRAY
    addarg(&ldargs, libm);
#if defined(__alpha) && defined(__osf__)
    if (traceflag)
	addarg(&ldargs, "-lmld");
#endif
#ifdef __CYGWIN32__
    addflag(&ldargs, libdir);
    addarg(&ldargs, "-liberty");
#endif
#ifndef CCLINK
    addarg(&ldargs, libc);
#if defined(linux) || defined(__NetBSD__) || defined(__FreeBSD__)
    addarg(&ldargs, libgcc);
#endif /* linux */
#endif
#endif /* CRAY */
#if defined(SOLARIS)
#ifdef CCLINK
    addarg(&ldargs, "-lc");
    addarg(&ldargs, "-L/usr/ucblib");
    addarg(&ldargs, "-R/usr/ucblib");
#endif
    addarg(&ldargs, "-lucb");
#ifndef CCLINK
    addarg(&ldargs, crtlast);
#endif
#endif /* SOLARIS */
#if defined(irix) && !defined(CCLINK)
    addarg(&ldargs, crtlast);
#endif /* irix */
#ifdef __CYGWIN32__
    /*addarg(&ldargs, "-lcygwin");*/
    {
	int l = strlen(outfile);
	if (l <= 4 || strcmp(&outfile[l-4], ".exe") != 0)
	    outfile = setprefix(outfile, ".exe");
    }
#endif
    addflag(&ldargs, "-o");
    addflag(&ldargs, outfile);
    (void)run(Ld, &ldargs);
}

rmofiles()
{
    int i;
    
    for(i = 0; i < rmargs.nargs; i++)
	(void)unlink(rmargs.args[i]);
}

#define PLMLSRC 0
#define PHASKELLSRC 1
#define POBJ 2
#define PASSRC 3

static int isThisLit;    /* 0=> standard file, 1=>assume literate script */

int
splitname(src, dst)
char *src, *dst;
{
    char *p;

    isThisLit = 0;
    Strcpy(dst, src);
    if (p = strrchr(dst, '.')) {
	*p++ = 0;
	if (strcmp(p, "m") == 0)
	    return PLMLSRC;
	else if (strcmp(p, "lm") == 0) {
	    isThisLit++;
	    return PLMLSRC;
	} else if (strcmp(p, "hs") == 0 || strcmp(p, "has") == 0)
	    return PHASKELLSRC;
	else if (strcmp(p, "lhs") == 0) {
	    isThisLit++;
	    return PHASKELLSRC;
	} else if (strcmp(p, "s") == 0)
	    return PASSRC;
	else {
	    return -1;
	}
    } else {
	return -1;
    }
}

processfile(name)
char *name;
{
    char fname[1024], *suf, *osuf;

    switch(splitname(name, fname)) {
    case PHASKELLSRC:
	suf = "hi";
	osuf = "t";
	goto comp;
    case PLMLSRC:
	suf = "t";
	osuf = "hi";
    comp:
	if (!lmlfile(name)) {
	    if (bwmflag) {
		Strcat(fname, ".bmc");
		copy(tmpbwm, fname, "w");
		fname[strlen(fname)-2] = 0;
	    } else if (Gflag) {
		Strcat(fname, ".g");
		copy(tmpg, fname, "w");
	    } else if (Sflag) {
		Strcat(fname, ".s");
		copy(tmps, fname, "w");
	    } else {
		Strcat(fname, ".o");
		if (!asmfile(tmps, fname))
		    ofile(fname);
	    }
	    strcpy(&fname[strlen(fname)-1], suf);
	    if (!nohi && !equalfiles(tmpt, fname)) {
		if (strcmp(suf, "hi") == 0)
		    (void)unlink(fname); /* remove old file in case of hard links (requested by Will Partain) */
		copy(tmpt, fname, "w");
	    }
	    if (bothtypes) {
		strcpy(strrchr(fname, '.')+1, osuf);
		if (!nohi && !equalfiles(tmpot, fname)) {
		    copy(tmpot, fname, "w");
		}
	    }
	}
	break;
    case PASSRC:
	Strcat(fname, ".o");
	if (!asmfile(name, fname))
	    ofile(fname);
	break;
    default:
	if (strncmp(name, "-l", 2) == 0) {
	    addarga(&ldeargs, name);
	} else
	    addarga(&ldargs, name);
	break;
    }
}

ofile(s)
char *s;
{
    addarga(&ldargs, s);
    if (!cflag)
	addarga(&rmargs, s);
}

copy(from, to, mode)
char *from, *to, *mode;
{
    FILE *f, *t;
    int n;
    char buff[1024];
    
    if ((f = fopen(from, "r")) == NULL)
	error(1, "Cannot open %s", from);
    if ((t = fopen(to, mode)) == NULL)
	error(1, "Cannot open %s", to);
    while(n = fread(buff, 1, sizeof buff, f))
	(void)fwrite(buff, 1, n, t);
    Fclose(f);
    Fclose(t);
}

equalfiles(from, to)
char *from, *to;
{
    register int r, i;
    register int nf, nt;
    register FILE *f, *t;
    char fbuff[1024], tbuff[1024];
    
    if ((t = fopen(to, "r")) == NULL)
	return 0;
    if ((f = fopen(from, "r")) == NULL)
	error(1, "Cannot open %s", from);
    for(;;) {
	nf = fread(fbuff, 1, sizeof fbuff, f);
	nt = fread(tbuff, 1, sizeof tbuff, t);
	if (nf != nt) {
	    r = 0;
	    break;
	}
	if (nf == 0) {
	    r = 1;
	    break;
	}
	for(i = 0; i < nf; i++) {
	    if (fbuff[i] != tbuff[i]) {
		r = 0;
		break;
	    }
	}
	if (i != nf)
	    break;
    }
    Fclose(f);
    Fclose(t);
    return r;
}

asmfile(name, oname)
char *name, *oname;
{
    int r;
    
    if (cflag && outfile != aout)
	addarg(&asargs, outfile);
    else
	addarg(&asargs, oname);
    addarg(&asargs, name);
    r = run(As, &asargs);
    delarg(&asargs, 2);
    return r;
}

int
lmlfilex(name)
char *name;
{
    int r;
    char b[1000];
    char *out, *pre;
    
    if (isThisLit) {
        addarg(&unlitargs, (unlitnoisy ? "-n" : "-q"));
        addarg(&unlitargs, name);
        addarg(&unlitargs, tmpl);
        r = run(unlit, &unlitargs);
        delarg(&unlitargs, 3);
        if (r) return r;
        pre = tmpl;
    } else
	pre = name;

    if (docpp) {
	addarg(&ppargs, pre);
	addarg(&ppargs, tmpm);
	r = run(Cpp, &ppargs);
	delarg(&ppargs, 2);
	if (r) return r;
	out = tmpm;
    } else
	out = pre;
    
    addarg(&pargs, out);
    {
	/* Do parsing of flags */
	FILE *f;
	char b[1000], *p, *args[100], **argv;
	int argc;

	f = fopen(out, "r");
	if (f != NULL) {
	    b[0] = 0;
	    while (fgets(b, sizeof b, f) != NULL && strncmp(b, "# ", 2) == 0)
		;
	    p = b;
	    if (strncmp(p, "{-#", 3) == 0) {
		p += 3;
#define SKIPWHITE while(*p == ' ' || *p == '\t') p++
		SKIPWHITE;
		if (strncmp(p, "COMPILERFLAGS", 13) == 0) {
		    p += 13;
		    SKIPWHITE;
		    for(argc = 0; argc < 100 && *p == '-'; argc++) {
			args[argc] = p;
			while(*p && *p != ' ' && *p != '\t') 
			    p++;
			*p++ = 0;
			SKIPWHITE;
		    }
		    args[argc] = 0;
		    argv = args;
		    decodeargs(&argc, &argv);
		}
	    }
	    fclose(f);
	}
    }
    addarg(&pargs, tmpp);
    sprintf(b, "-f%s", name);
    addflag(&pargs, b);
    r = run(parse, &pargs);
    delarg(&pargs, 2);
    delflag(&pargs, 1);
    if (r) return r;
    
    addarg(&cargs, tmppre);
    addarg(&cargs, name);
    r = run(comp, &cargs);
    delarg(&cargs, 2);
    
    return r;
}

int
lmlfile(name)
char *name;
{
    int r;
    char buf[80];
    int c;

try:
    r = lmlfilex(name);
    if (r && retry && isatty(0)) {
	for(;;) {
	    fprintf(stderr, "Compilation of %s failed.\nAbort, Retry, Ignore?", name);
	    fgets(buf, sizeof buf, stdin);
	    c = tolower(buf[0]);
	    if (c == 'r')
		goto try;
	    if (c == 'a')
		return r;
	    if (c == 'i')
		return 0;
	}
    }
    return r;
}

int
run(name, ap)
char *name;
struct args *ap;
{
    char *args[2*MAXARGS+1];
    int i, j, r;
    
    i = 0;
    args[i++] = name;
    for(j = 0; j < ap->nflags; j++)
	args[i++] = ap->flags[j];
    for(j = 0; j < ap->nargs; j++)
	args[i++] = ap->args[j];
    args[i] = 0;
    if (verbose) {
	for(j=0; j<i; j++)
	    fprintf(stderr, "%s ", args[j]);
	fprintf(stderr, "\n");
    }
    r = callsys(name, args);
    bad |= r;
    return r;
}

callsys(f, v)
char *f, **v;
{
    int t;
    int sta;

    t = vfork();
    if (t == -1) {
	error(1, "No more processes\n");
    }
    if (t == 0) {
	if (*f != '/')
	    execvp(f, v);
	else
	    execv(f, v);
	fprintf(stderr, "Cannot exec %s\n", f);
	Fflush(stdout);	/* ? */
	_exit(100);
    }
    while (t != wait(&sta))
	;
    if (WIFSIGNALED(sta)) {
	if (WTERMSIG(sta) != SIGINT) {
	    error(0, "Fatal error in %s (%d)\n", f, (long)WTERMSIG(sta));
	}
	return 1;
    }
    return WEXITSTATUS(sta);
}

static char *
copystring(s)
char *s;
{
    char *r;
    r = malloc(strlen(s)+1);	/* allow for some munging at the end! */
    Strcpy(r, s);
    return r;
}

extern char **environ;

static int
findenv(name)
char *name;
{
    int len;
    char **p, *c;

    for (c = name, len = 0; *c && *c != '='; ++c, ++len)
	;
    for (p = environ; *p; ++p)
	if (!strncmp(*p, name, len))
	    if (*(c = *p + len) == '=') {
		return p - environ;
	    }
    return -1;
}

static void
deleteenv(name)
char *name;
{
    register char **p;
    int offset;

    while ( (offset = findenv(name)) >= 0) {
	for (p = &environ[offset]; ; ++p)
	    if (!(*p = *(p + 1)))
		break;
    }
}

#ifdef __CYGWIN32__
/* library version is buggy */
int
putenv(const char *s)
{
  extern char **environ;
  char **e;
  int i;
  for(i = 0; environ[i]; i++)
    ;
  e = malloc((i+1)*sizeof(char *));
  for(i = 0; environ[i]; i++)
    e[i] = environ[i];
  e[i++] = s;
  e[i] = 0;
  environ = e;
}
#endif
