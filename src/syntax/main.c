#include "include.h"
#include <signal.h>

#include "proto.h"

extern int warnflag, zyzflag, zyzid;

tree root; 		/* The root of the built syntax tree. */
impstuff iroot;
int Eflag = 0;
int interactive = 0;
int interfacef = 0;
int debug = 0;
int pedantic = 0;
int useascii = 0;
static int truncinterface = 0;
static char *initialname = 0;
int pre_1_1;
int h1_3 = 0;
int ccall = 0;
int localquant = 0;
int allowuscore = 0;
int allowmulti = 0;
int view = 0;
extern char *delimiter;
extern char *progname;
extern char *filename;
char *progname;
extern char *filename;
char *incpath;
char *hlibname;
char *copystring();

/*char *malloc();*/
char *mtupstr();

#define DEFLMLDIR "/usr/local/lib/lmlc"
#define HBC_LIBRARY "hbc_library"

static void killme() { exit(1); }
static void noproc() { fprintf(stderr, "Compiler died\n"); exit(1); }

int
main(argc, argv)
int argc;
char **argv;
{
    char *p;

    progname = *argv++;
    argc--;
    p = getenv("HBCDIR");
    if(!p)
      p = getenv("LMLDIR");
    {
	char **p;
	for(p = argv; *p && **p == '-' && !h1_3; p++)
	    if (strcmp(*p, "-3") == 0)
		h1_3 = 1;
    }
    if (!p)
	p = DEFLMLDIR;
    {
	char b[2000];
	if (h1_3) {
	    sprintf(b, "%s/hlib1.3", p);
	    hlibname = copystring(b);
	    sprintf(b, "%s:.:%s/%s1.3", hlibname, p, HBC_LIBRARY);
	} else {
            hlibname = p;
	    sprintf(b, ".:%s/%s", p, HBC_LIBRARY);
        }
	incpath = copystring(b);
	if ((p = getenv("LMLINCPATH")))
	    incpath = p;
	if ((p = getenv("HBCINCPATH")))
	    incpath = p;
    }
    while (argc && argv[0][0] == '-') {
	while (*++*argv)
	    switch(**argv) {
            case 'W':
                view++;
                break;
	    case 'u':
		localquant++;
		break;
	    case '_':
		allowuscore++;
		break;
	    case 'm':
		allowmulti++;
		break;
	    case '3':
		h1_3 = 1;
		break;
	    case '2':
		h1_3 = 0;
		break;
	    case 'P':
		pedantic++;
		break;
	    case 'w':
		warnflag++;
		break;
	    case 'Z':
		zyzflag = 1;
		break;
	    case 'z':
		zyzid++;
		break;
	    case 'E':
		Eflag++;
		break;
	    case 'I':
		interactive++;
		break;
	    case 'q':
		truncinterface++;
		/* fall into */
	    case 'p':
		interfacef++;
		break;
	    case 'd':
		debug++;
		break;
	    case 'o':
		pre_1_1++;
		break;
	    case 'f':
		initialname = *argv + 1;
		goto nextarg;
	    case 'S':
		delimiter = " ";
		break;
	    case 'c':
		ccall++;
		break;
	    case 'i':
		if (argv[0][1] == '\0') {
		    incpath = "";
		} else {
		    char buf[10000];
		    strcpy(buf, *argv + 1);
		    strcat(buf,":");
		    strcat(buf, incpath);
		    incpath = copystring(buf);
		}
		goto nextarg;
	    }
    nextarg:
	argc--, argv++;
    }
    allowuscore |= pedantic == 0;
    if (pedantic > 1)
	useascii++;
    if(argc >= 1 && freopen(argv[0], "r", stdin) == NULL) {
	error("Cannot open %s.\n", argv[0]);
    }
    if(argc >= 2 && freopen(argv[1], "w", stdout) == NULL) {
	error("Cannot open %s.\n", argv[1]);
    }

    if (initialname)
	filename = copystring(initialname);
    else {
	if (argc >= 1)
	    filename = copystring(argv[0]);
	else
	    filename = copystring("<stdin>");
    }
    init();
    initlex();
    if (interactive) {
	signal(SIGINT, SIG_IGN);
	signal(SIGQUIT, killme);
	signal(SIGPIPE, noproc);
	pushextra(INTERACTIVE);
	yyparse();
    } else if (interfacef) {
	pushextra(SINTERFACE);
	if(yyparse() == 0) {
	    /* No syntax errors. */
	    if (truncinterface) {
		if (timpstuff(iroot) != interface || tlist(giients(iroot)) != lcons) {
		    fprintf(stderr, "Bad syntax for truncated interface.\n");
		    exit(1);
		}
		pimpid(lhd(giients(iroot)));
	    } else
		pimpstuff(iroot);
	    exit(0);
	} else {
	    /* There was a syntax error. */
	    exit(1);
	}
    } else {
	if(yyparse() == 0) {
	    /* No syntax errors. */
	    ptree(root);
	    dumperrinfo();
	    printf("\n");
	    exit(0);
	} else {
	    /* There was a syntax error. */
	    exit(1);
	}
    }
    exit(0);
}

