#include "vars.h"
#include "funs.h"

static int beenhere = 0;

#ifdef linux
struct sigcontext;
#endif
void chkstk PROTO((int, int, struct sigcontext *, char *));

void tracesignal(), traceerror(), tracefail(), failintr();

extern int traceflag;

void
sighandl(sig,code,scp,addr)
int sig, code;
struct sigcontext *scp;
char *addr;
{
    if (debug)
	fprintf(stderr, "sighandl\n");
    if (sig == SIGINT) {
#if 0
      cleansig();
      failintr();
#else
      finish(1);
#endif
    }
    chkstk(sig, code, scp, addr);
    if (beenhere++) {
	write(2, "Nested signal!\n", 15); /* be extra careful not to cause any more signals */
	finish(1);
    }
    fprintf(stderr, "Signal: %s\n",
#if !defined(__ARM) && !defined(linux)
	    sig == SIGBUS ? "Bus error" :
#endif
	    sig == SIGSEGV ? "Segmentation fault" : "Illegal instruction");
    if (traceflag)
	tracesignal();
    finish(1);
}

static void
Domsg(m, s)
char *m;
char *s;
{ 
    if (beenhere++)
	finish(1);
    flushlow();
    fprintf(stderr, "%s %s\n", m, s);
    if (traceflag)
	traceerror();
    finish(1);
}

static void
Doerr(s)
char *s;
{
    Domsg("Error in runtime system:", s);
}

void Eunimpl() { Doerr("unimplemented"); }
void EError() { Doerr("gettag"); }
void EErre()  { Doerr("eval");}
void EErre_zap()  { Domsg("Black hole detected in", "eval zap");}
void EErre_hole()  { Domsg("Black hole detected in", "eval hole");}
void EErre_marked()  { Doerr("eval marked");}
char line[100];
void EErre_moved(x) Int x; {
  sprintf(line,"eval moved %lx\n",x); Doerr(line);
}
void EErrgc(x, str) Int x;char *str;  {
#if 0
  sprintf(line,"%s %lx\n",str,x); Doerr(line);
#else
  extern char *tagname();
  sprintf(line,"%s %lx = (%s)\n",str,x,tagname((PTR)x)); Doerr(line);
#endif
}
void EErre_gcret()  { Doerr("eval gcret");}
void EErru()  { Doerr("unwind");}
void EErrj()  { Doerr("jfun");}
void EErrmk() { Doerr("mkapl");}
void EErrg()  { Doerr("gettag");}
void ENolabel() { fprintf(stderr, "Jump to Nolabel: this cannot happen!\n"); finish(1);}
void EErrcm()  { Doerr("callmethod");}

void Mrkerror() { Doerr("Genc on mrk node");}

void (*failhandler)() = 0;
char *failstring = "Fail:";

void
fail()
{
    if (failhandler)
	(*failhandler)();

    /* If the handler returns we exit as usual. */
    if (beenhere++ > 1) {
	fprintf(stderr, "Too many nested calls to fail.\n");
	finish(1);
    }
    flushlow();
    fflush(stdout);
    fprintf(stderr, failstring);
    printtop(stderr);
    fprintf(stderr, "\n");
    if (traceflag)
	tracefail();
    finish(1);
}

void
gcerror(s, d)
char *s;
Int d;
{
    flushlow();
    fprintf(stderr, "\ngcerror %08x %d: %s\n",d,d,s);
    finish(1);
}

void
noheapleft()
{
    flushlow();
    fprintf(stderr, "\nOut of heap space\n");
    finish(1);
}

void
zapfail()
{
    flushlow();
    fprintf(stderr, "Severe problems with ZAPped node.\n");
/*    if (tracefail)*/
	traceerror();
    finish(1);
}

void
forceerr()
{
    flushlow();
    fprintf(stderr, "Force on illegal node\n");
/*    if (tracefail)*/
	traceerror();
    finish(1);
}
