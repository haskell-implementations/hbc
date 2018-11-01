#include "runtime.h"
#include "vars.h"
#include "../mcode/limit.h"

#if defined(HPUX)
#define getpagesize() sysconf(_SC_PAGE_SIZE)
#endif

#ifndef PROT_NONE
#define PROT_NONE 0
#endif

void failstkovfl();

/*
 * Allocating the heap
 *
 *
 *  |<----------------------------- Heapsize -------------------------------->|
 *                                                              |<-Tablesize->|
 *  ---------------------------------------------------------------------------
 *  |                          |                        |       |             |
 *  ---------------------------------------------------------------------------
 *  ^                          ^                        ^       ^   ^         ^
 *  |                          |                        |       |   etp       endtable = tp
 *  |                          curheap = hp*            |       endheap=starttable      
 *  startheap = nxtheap = oldhp = hp**                  realehp = ehp
 *
 *   hp** = If gcSlide
 *   hp*  = If gcGenAppel,gcGenSeward,gcOrg,gcCheney
 *   starthp = hp
 */

void
setupheap()
{
  int retry;
  int wordsNeeded;
  extern int tpmul;

  if (heapsize > Heapsize)
    heapsize = Heapsize;

  heapsize /= 2;  /* used only for deciding minimum heapsize */

  switch(gcflag) {
  case gcSlide:
    wordsNeeded = Heapsize;
    Tablesize = (wordsNeeded+32)/33;
    break;
  case gcGenAppel:  case gcGenSeward:
  case gcGen2:
    wordsNeeded = Heapsize;
    Tablesize = (wordsNeeded+32)/33 * tpmul; /* Ugly fix PHD */
    break;
  case gcOrg:  case gcChenney:
    wordsNeeded = Heapsize;
    Tablesize = 0;
    break;
  default:
    wordsNeeded = 0;  /* To get rid of a warning */
    fprintf(stderr,"Unknown gc (%d) in heapinit\n",gcflag);
    finish(1);
  }

  if(Tablesize && Tablesize < 2*TCLAIM)
    Tablesize = 2*TCLAIM;

#ifdef __ARM
    startheap = (VPTR) Xmalloc(sizeof(UInt)*wordsNeeded,
			       ,"Cannot allocate %d bytes for heap\n");
#else
    if (wordsNeeded < 5000) {
	fprintf(stderr, "Heap size too small\n");
	finish(1);
    }
    for (retry = 0;; retry++) {
      startheap = (VPTR) sbrk(sizeof(UInt)*wordsNeeded);
      if (startheap != (VPTR) - 1)
	break;
      /* No memory available, this is probably due to fragmentation
       * of the swap space and will (hopefully) disappear soon. */
      fprintf(stderr,
	      "*** Cannot allocate %d bytes for the heaps.\n",
	      sizeof(UInt)*wordsNeeded);
      if (retry > 10)
	finish(1);
      fprintf(stderr, "Trying again\n");
      sleep(20);
    }
#endif /* __ARM */

  endtable = startheap + wordsNeeded;
  starttable = endheap = endtable - Tablesize;

  oldstarthp = oldhp = nxtheap = startheap;
  curheap = (endheap-startheap)/2 + startheap;

  realehp = ehp = endheap - HCLAIM;       /* Limit of heap */
  etp = starttable + TCLAIM;

  Utilize *= endheap-startheap;
  Utilize /=100;

  switch(gcflag) {
  case  gcSlide:
    hp = startheap;
    clearTable(); /* Done once and for all */
    etp = tp = 0;
    break;
  case gcGenSeward:
    hp = curheap;
    tp = endtable;
    break;
  case gcGen2:
    hp = curheap = (2*(endheap-startheap))/3+startheap;
    nxtheap = (endheap-startheap)/3+startheap;
    oldhp = startheap;
    tp = endtable;
    Utilize = hp - startheap;
    break;
  case gcOrg:
    hp = curheap;
    etp = tp = 0;
    break;
  default:
    wordsNeeded = 0;  /* To get rid of a warning */
    fprintf(stderr,"Unknown gc (%d) in heapinit\n",gcflag);
    finish(1);
  }

#if GCSTAT
  if (Sflag>1) {
    fprintf(stderr,"startheap = %lx endheap = %lx size = %lx(%ld)\n",(UInt)startheap,(UInt)endheap,endheap-startheap,endheap-startheap);
    fprintf(stderr,"hp = %lx\n",(UInt)hp);
    fprintf(stderr,"Utilize = %d\n",Utilize);
    fprintf(stderr,"starttable = %lx endtable = %lx size = %x(%d)\n",(UInt)starttable,(UInt)endtable,Tablesize,Tablesize);
    fprintf(stderr,"endtable-starttable = %ld\n",endtable-starttable);
  }
#endif
  starthp = hp;
}

/*
 * Allocating the pointer stack and, if sparc or hppa, the dump stack.
 */

char *stackprot;
int stackpage;


/* Protect a page if possible */
char *
sprotect(p)
VPTR p;
{
#ifdef DO_MPROTECT
    extern int errno;

    stackpage = getpagesize();
    p = (VPTR) ((((Int)p + stackpage - 1) / stackpage) * stackpage);
    if (mprotect((void *)p, stackpage, PROT_NONE) < 0) {
	fprintf(stderr, "Cannot protect stack base %lx length=%d (errno=%d)\n", (UInt)p, stackpage, errno);
	exit(1);
    }
    return (char *)p;
#else
    return 0;
#endif
}

#if defined(sparc) || defined(hppa)
VPTR vp, vstack;
char *vstackprot;
extern int vsize;
#endif

void
setupstack()
{
    extern int stacksize;
  
    if (stacksize < 1000) {
	fprintf(stderr, "Stack size too small\n");
	finish(1);
    }
#ifdef __ARM
    stack = (PTR*) Xmalloc(sizeof(UInt) * stacksize,"Cannot allocate %d bytes for stack\n");
#else
    stack = (PTR*) sbrk(sizeof(UInt) * stacksize);/* stack */
    if (stack == (PTR*)-1) {
	fprintf(stderr, "No room for stack\n");
	finish(1);
    }
#endif /* __ARM */
    ep = bos = stack + stacksize;
    stackprot = sprotect(stack);
#if defined(sparc) || defined(hppa)
    if (vsize < 1000) {
	fprintf(stderr, "Stack size too small\n");
	finish(1);
    }
    vstack = (VPTR)sbrk(sizeof (UInt) * vsize);
    if (vstack == (VPTR)-1) {
	fprintf(stderr, "No room for dump stack\n");
	finish(1);
    }
    vp = vstack + vsize;
    vstackprot = sprotect(vstack);
#endif
}

void 
writeerr(s)
char *s;
{
    write(2, s, strlen(s));
}

#define SLOPS 4000 /* allow for offset from Sp */
#define SLOPV 100 /* allow for offset from Vp */

#ifdef linux
struct sigcontext;
#endif

void
chkstk(sig, code, scp, aaddr)
int sig, code;
struct sigcontext *scp;
void *aaddr;
{
#ifdef DO_MPROTECT
    extern void sighandl PROTO((int, int, struct sigcontext *, char *));
    char *addr;

#if defined(__NetBSD__) && defined(__i386__) || defined(__FreeBSD__)
    addr = (char *)scp->sc_ebp;
#else
    addr = aaddr;
#endif /* __NetBSD__ */

    if ((sig == SIGBUS || sig == SIGSEGV) &&
	/*code == SEGV_PROT &&*/ 
	((stackprot <= addr && addr <= stackprot + stackpage + SLOPS)
#if defined(sparc) || defined(hppa)
	 || (vstackprot <= addr && addr <= vstackprot + stackpage + SLOPV)
#endif
	 )) {
	/* A stack overflow has been detected */
	/*fprintf(stderr, "detected stovfl %d, reset handler\n", doinggc);*/
	cleansig();
	/*fprintf(stderr, "detected stovfl, calling\n");*/
        if (doinggc) {
	    fprintf(stderr, "%s: Stack overflow during GC\n", progname);
	    finish(1);
	} else {
	    failstkovfl();
	}
    }
    if (sig == SIGBUS)
	fprintf(stderr, "%s: Unexpected bus error\n", progname);
    else if (sig == SIGSEGV)
	fprintf(stderr, "%s: Unexpected segmentation fault\n", progname);
    else
	fprintf(stderr, "%s: Unexpected signal\n", progname);
#else /* M_PROTECT */
    if (*stack
#if defined(sparc) || defined(hppa)
	 || *vstack
#endif
	) {
	/* fprintf may no longer work */
	writeerr(progname); writeerr(": "); writeerr("Stack overflow");
	if (doinggc) writeerr(" during GC");
	writeerr("\n");
	finish(1);
    }
#endif /* M_PROTECT */
}

void
debstop()
{
}
