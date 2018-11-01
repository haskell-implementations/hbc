#include "runtime.h"
#include "vars.h"

#include "../mcode/limit.h"

#ifdef HPROFILE
#include "sample.h"
#endif

#include "gcd.h"
#include "gc.h"

extern int tpin,tpout,tpdupl,tpnew,tpdel;
void checkvp2 PROTO((VPTR, VPTR, int ***,int ***, PTR *,PTR *));

#define min(x,y) ((x)<(y)?(x):(y))
#define max(x,y) ((x)>(y)?(x):(y))

#ifndef abs
#define abs(x) ((x)<0?-(x):(x))
#endif

/* newheapsize is only used from sample.c, it's inlined in dogc() to take
   care of the different garbage collectors */
/* Compute the new value of the heapsize, ONLY FOR GCSAMPLE!!!! */
/*ARGSUSED*/
int
newheapsize(lastused, lastsize)
int lastused, lastsize;
{
  int s;
  extern int fullheap;

  s = (fullheap ? 10 : 4) * lastused;
  if (s < MinHeapsize)
    s = MinHeapsize;
  if (s > Heapsize/2)
    s = Heapsize/2;
  Minleft = s / 20 + 1000;
  return s;
}


/*
 * A picture of the heap
 *
 *
 *  |<--------------------------- Heapsize ---------------------------------->|
 *                                                              |<-Tablesize->|
 *  ---------------------------------------------------------------------------
 *  |OOOOOOOO                  |NNNNNNNNNNNNNNNNNNNNNN  |       |     PPPPPPPP|
 *  ---------------------------------------------------------------------------
 *  ^       ^                  ^                     ^  ^       ^     ^       ^
 *  |       oldhp              |                     hp |       |     tp      endtable
 *  |                          curheap                 |       endheap=starttable      
 *  startheap = nxtheap                                 realehp = ehp
 *
 *
 *
 */ 

VPTR garbvp;  /* Vp when gc started */
VPTR garbvp0; /* Vp when print was first called */
int nfunlink;

extern Tag VEK;

static void
gcstk()
{
  int stackdepth = bos - ep;
  PTR dummy;


  if (stopaddr)
    fprintf(stderr, "stop at %lx\n", (UInt)stopaddr);
  if (chkaddr)
    chkval = *chkaddr;

#ifdef EXTRA_DEBUG
  if(tp && Sflag && Gflag && Gno < 0) {
    VPTR st = tp;
    fprintf(stderr,"\ntp is %lx\n", (UInt)tp);
    for(st = tp; st<endtable; st++) {
      fprintf(stderr,"\n<%10lx> ", (UInt)st);
      dumpgraph(*st,3);
    }
  }
#endif


#if DUMP
  Gno--;
  if(Gflag && Gno <= 0){
#if 0        /* I got a bus error with the default gc */
    int i;
    for(i = 0; i < 50; i++) {
      fprintf(stderr, " %s", lookup(((unsigned long *)garbvp)[i]));
    }
#endif
    dumpstack("Before GC\n");
  }
#endif

  /* Do GC for each pointer on the stack. */
  nfunlink = 0;
  /* Make stack into one huge vector node and do GC on that */
      
  if(ep < stack+2) {
    fprintf(stderr, "\nPointer stack overflow (pushed %d to many)\n",(VPTR)stack-(VPTR)ep);
    finish(1);
  }

  *--ep = (PTR)(UInt)stackdepth;
  *--ep = (PTR)&VEK;
  ep[-1] = 0; /* This is a gclink word */
  if (Gflag) fprintf(stderr, "Calling gc");
  switch(gcflag) {  /* After this swith hp is pointing after the last collected node */
  case gcOrg:
    if (Gflag) fprintf(stderr, "Org\n");
    gc(ep, &dummy);
    break;
  case gcGen2:
    if (Gflag) fprintf(stderr, "Gen2\n");
    tpin = endtable - tp;
    if(Sflag>1) {
      fprintf(stderr,"nxtheap    = %08lx curheap =  %08lx\n",(UInt)nxtheap,(UInt)curheap);
      fprintf(stderr,"oldstarthp = %08lx oldhp =    %08lx\n",(UInt)oldstarthp,(UInt)oldhp);
      fprintf(stderr,"tp =         %08lx endtable = %08lx\n",(UInt)tp,(UInt)endtable);
      fprintf(stderr,"hp =         %08lx\n",(UInt)hp);
      fprintf(stderr,"ep =         %08lx bos-ep =   %d\n",(UInt)ep,bos-ep);
    }
    *--tp = (void *)(Int)tpin;
    *--tp = (void *)&VEK;
    if(tp<=endheap) {
      fprintf(stderr,"Exception table overflow before gc!\n");
      exit(-1);
    }
    doGen20(ep);
    if(tp<=endheap) {
      fprintf(stderr,"Exception table overflow during gc!\n");
      exit(-1);
    }
    checkvp((int***)garbvp0,(int***)garbvp,bos,ep+2);
    break;
  case gcGenSeward: /* make updated into one huge vector with stack on top */
    if (Gflag) fprintf(stderr, "GenSeward\n");
    tpin = endtable - tp;
    doGenSeward0(ep);
    packTable();
    checkvp((int***)garbvp0,(int***)garbvp,bos,ep+2);
    break;
  case gcGenSewardM:
    if (Gflag) fprintf(stderr, "GenSewardM\n");
    tp = endtable;
    tpin = 0;
    doGenSeward1(ep);
    checkvp((int***)garbvp0,(int***)garbvp,bos,ep+2);
    break;
  case gcSlide:
    if (Gflag) fprintf(stderr, "Slide\n");
    doSlide((PTR)ep);
    break;
  default:
    fprintf(stderr, "Unknown gcflag(%d) in gcstk\n",gcflag);
    finish(1);
  }

  if (Gflag) fprintf(stderr, "Return from gc\n");

  ep += 2;
#if 0
  if (stack[0]) {
    fprintf(stderr, "Stack overrun after GC.\n");
    exit(1);
  }
#endif
  if (Gflag) {
    fprintf(stderr, "nfunlink = %d\n", nfunlink);
  }
#if DUMP
  if(Gflag && Gno <= 0){
    dumpstack("after GC\n");
  }
#endif
#if GCSTAT
  if (Sflag>1){
    fprintf(stderr, "leaving gcstk\n");
  }
#endif /* GCSTAT */
}

static struct gc_hook {
    struct gc_hook *next;
    void (*hook)();
} *hooks;

static void
run_rev_hook(n, p)
int n;
struct gc_hook *p;
{
  if (p) {
    run_rev_hook(n, p->next);
    (*p->hook)(n);
  }
}

void
run_gc_hooks(n)
int n;
{
  if (n) {
    /* Run backwards after gc */
    run_rev_hook(n, hooks);
  } else {
    /* and forwards before */
    struct gc_hook *p;
    
    for(p = hooks; p; p = p->next)
      (*p->hook)(n);
  }
}

void
add_gc_hook(f)
void (*f)();
{
  struct gc_hook *p;
  
  p = (struct gc_hook *)xmalloc(sizeof (struct gc_hook));
  p->hook = f;
  p->next = hooks;
  hooks = p;
}

#define MAXSPINE 50
void 
gczapstack(bot,topp,sbot,stop)
int ***bot, ***topp;
PTR *sbot,*stop;
{
  int **tmp;
  int ***top;
  Tag *gctag;
  int zvap = 0, zap = 0, junk = 0, notfunc = 0, zzap = 0, hole = 0, other = 0;
  extern Tag HOLE, INPUT, INPUTD, STRING, VAP, VAPG, ZAP, AP, APG, CAP, FUN;
#if defined(_AIX)
#define etext _etext
#endif
#ifdef __CYGWIN32__
  int etext;
#else
  extern int etext;
#endif

  if(bot) {
#if defined(sparc) || defined(hppa)
    top = topp+2;		/* there are two junk elements on top */
#else
    top = &tmp;
#endif
/*    bot -= 2;*/			/* and two at the bottom */
#if GCSTAT
    if(Sflag>1)
    fprintf(stderr,"gczap bot = %lx top = %lx sbot = %lx stop = %lx\n",
	    (UInt)bot,(UInt)top,(UInt)sbot,(UInt)stop);
#endif
    for(; top < bot; top++) {
      tmp = *top;					/* something from the value stack */
      if(((sizeof(int *)-1)&(Int)tmp) == 0 && 		/* must be aligned to be a pointer */
	 --tmp<(int**)sbot && tmp>(int**)stop && 	/* must point inside pointer stack */
	 top[1] < (int**)&etext		 		/* must have a corresponding return address pushed just before */
        ) { 
	  PTR p = (PTR)*tmp;
	  top++;		/* skip pc */
	  gctag = p->tag;
	  if (gctag == &ZAP) {
	      /* already done */
	      zzap++;
	  } else if (gctag == &HOLE) {
	      /* ignore HOLE nodes */
	      hole++;
	  } else if (gctag == &INPUT || gctag == &INPUTD || gctag == &STRING) {
	      /* safe to ignore */
	      other++;
	  } else if (gctag == &VAP || gctag == &VAPG) {
	      p->tag = &ZAP;
	      /* FunInfo is at the same place for ZAP & VAP */
	      zvap++;
	  } else if (gctag == &AP || gctag == &APG) {
	      PTR q = p;
	      int i = 0;
	      do {
		  q = q->node12.ptr0; /* get function part */
	      } while (++i < MAXSPINE && (q->tag == &AP || q->tag == &APG || q->tag == &CAP));
	      if (q->tag == &FUN && q->nodefun.fun->arity == i) {
		  /* Found a function node, use it. */
		  p->tag = &ZAP;
		  p->nodezap.fun = q->nodefun.fun;
		  zap++;
	      } else {
		  /* Not a function node, ignore it */
		  notfunc++;
	      }
	  } else {
	      /* Mysterious node to update */
	      fprintf(stderr, "Bad node in gczap: %lx\n", (UInt)gctag);
	  }
        } else {
	  junk++;
	}
      }
#if GCSTAT
    if (Sflag>1)
	fprintf(stderr, "gczap done: %d VAP, %d AP, %d junk, %d bad func, %d ZAP, %d HOLE, %d other\n", zvap, zap, junk, notfunc, zzap, hole, other);
#endif
  } else {
    fprintf(stderr,"No bot yet\n");
  }
}


int calcGen2(oldGrowth,oldCurr) /* uses oldhp,hp,curheap and arguments top calculate nxtheap,ehp and realehp */ 
     int oldGrowth,oldCurr;
{
  extern int fullheap;
  if(curheap == hp) { /* No data in new heap! (This program is probably bad for gen. gc, but we will do our best.) */
    heapsize = (endheap-oldhp)/3;
    Minleft = heapsize / 20 + 1000;
    curheap = hp = oldhp+heapsize;
    nxtheap = curheap+heapsize;
    realehp = ehp = nxtheap - HCLAIM;
    return ehp > hp+Minleft;
  }
  if(curheap-oldhp < endheap - curheap) {
/*     Start|ooooooooooo|      |nnnnnnnn|                                  |End */
/*                   oldhp   curheap   hp                                       */
#if 0 
    heapsize = (endheap - curheap)/2;
    nxtheap = heapsize + curheap;
    realehp = ehp = nxtheap - HCLAIM;
    Minleft = heapsize / 20 + 1000;
    return ehp > hp+Minleft && hp-curheap < curheap - oldhp;
#else
    heapsize = min(curheap - oldhp,(endheap - curheap)/2);
    nxtheap = endheap - heapsize;
    realehp = ehp = curheap + heapsize - HCLAIM;
#endif
    
  } else {
/*     Start|ooooooooooo|                                  |nnnnnnnn|      |End */
/*                   oldhp                              curheap     hp          */
#if 0
    int hs = ((curheap-oldhp)-newSize)/((double)oldGrowth/(double)oldCurr+1.2); /* 1.0 is exact match */
#else
    int hs = (curheap-oldhp)/((double)oldGrowth/(double)oldCurr+2.2); /* 2.0 is exact match */

#endif
    nxtheap = curheap - hs;
    heapsize = min(hs,endheap-curheap);
    realehp = ehp = curheap+heapsize - HCLAIM;
  }
  Minleft = (endheap-startheap) / 20 + 1000;
  return ehp > hp+Minleft;
}
  
#ifndef __CYGWIN32__
#define SIGBLOCK sigmask(SIGINT) | sigmask(SIGQUIT) | sigmask(SIGTERM) | sigmask(SIGHUP)
#endif

#ifdef HSHOW
int showheapLeft,showheapStep;
VPTR showheapEhp;
extern void showHeap();
int showing;
#endif

#ifndef HPUX
extern int sigblock PROTO((int));
#endif

void fixgarbvp PROTO((void));
void
dogc()
{
  extern int zapthem;
  extern int fullheap;
  int moved,oldSize,newSize = hp - starthp;
  VPTR saveoldhp = oldhp;

  VPTR newhp;
  int stacksize;

    /* SHOULD BLOCK ALL SIGNALS HERE */
#ifdef SIGBLOCK
  int oldmask = sigblock(SIGBLOCK);
  if (Sflag > 2) fprintf(stderr, "oldmask=%x\n", oldmask);
#endif
  fixgarbvp();
  doinggc++;
  if (intrflag) {
    intrflag = 0;
    ehp = realehp;
#ifdef SIGBLOCK
    sigsetmask(oldmask);
    if (Sflag > 2) fprintf(stderr, "sigsetmask %x\n", oldmask);
#endif
    cleansig();
    failintr();
  }
  
  if (hp > realehp+HCLAIM) {  /* The heap has been overrun */
    fprintf(stderr, "Error: heap overrun: %lx %lx %lx\n", (UInt)hp, (UInt)ehp, (UInt)realehp);
    finish(1);
  }
  filegcinit();
  run_gc_hooks(0);
#ifdef HSHOW
  if(showing) {
    fprintf(stderr,"Show before gc %lx %lx %lx\n",(UInt)hp,(UInt)ehp,(UInt)realehp);
    /*    showHeap(); */
    if(ehp) {
      showheapEhp = ehp = ehp + showheapStep;
      if(ehp>realehp) {
	fprintf(stderr,"ehp>realehp\n");
	showheapEhp = ehp = realehp;
      }
      if(hp+HCLAIM > ehp) {
	fprintf(stderr,"Gc anyway      %lx %lx\n",(UInt)hp,(UInt)ehp);
	goto gcanyway;
      }
    } else {
      fprintf(stderr,"Ehp == 0\n");
      goto gcanyway;
    }
  } else {
  gcanyway:;
#endif
  /* Before we switch heaps, find out how much heap has been used etc.   */
  GCstart(bos-ep, newSize);

#ifdef HPROFILE
  if (profiling && gengcmark && hpfile) {
      fprintf(hpfile, "MARK %.2f\n", milliseconds / 1000.0);
  }
#endif

#if GCSTAT
  if (Sflag>1) {
    extern int no_of_GCs;
    fprintf(stderr, "enter dogc %d\n",no_of_GCs);
    fprintf(stderr, "dogc: cur=%lx nxt=%lx hp=%lx ehp=%lx\n",
	    (UInt)curheap, (UInt)nxtheap, (UInt)hp, (UInt)ehp);
  }
#endif /* GCSTAT */

  if (zapthem)
      gczapstack((int***)garbvp0,(int***)garbvp,bos,ep+2);
  switch(gcflag) {
  case gcOrg:              /*************** -gc-org ****************/
    newhp = oldstarthp = oldhp = nxtheap; nxtheap = curheap; curheap = oldhp;
    hp = newhp;
    gcstk();    /* returns with hp pointing after the last collected node */
    filegcfinish();
    heapsize = (fullheap ? 10 : 4) * (hp-oldstarthp);
    heapsize = max(heapsize,MinHeapsize);
    heapsize = min(heapsize,(endheap-startheap)/2);
    GCend(hp-newhp,bos-ep, hp - newhp, hp - oldstarthp, heapsize);/* Find out how much graph was moved. Stack,New,Old,Next */
    oldhp = hp;
    Minleft = heapsize / 20 + 1000;
    realehp = ehp = curheap + heapsize - HCLAIM;
    if(hp+Minleft > realehp) {
      noheapleft();
    }
    break;

  case gcSlide:              /*************** -gc-slide ****************/
    newhp = oldhp = oldstarthp = startheap;
    hp = newhp;
    gcstk();    /* returns with hp pointing after the last collected node */
    filegcfinish();
    heapsize = (fullheap ? 10 : 4) * (hp-oldstarthp);
    heapsize = max(heapsize,MinHeapsize);
    heapsize = min(heapsize,endheap-startheap);
    GCend(hp-startheap,bos-ep, hp - newhp, hp - startheap, heapsize);  /* Find out how much graph was moved. Stack,New,Old,Next */
    oldhp = hp;
    Minleft = heapsize / 20 + 1000;
    realehp = ehp =endheap - HCLAIM;
    heapsize = endheap - startheap;
    if(hp+Minleft > realehp)
      noheapleft();
    break;

  case gcGenSeward:          /**************** -gc-gen (minor) *****************/
    if(tp>etp) {
      newhp = oldhp;
      goto collectGenSeward;
    } else {               /* tp == 0 if table is full */
      gcflag = gcGenSewardM;  /* Force major */
      endheap -= (endtable-starttable)/8;
      if(hp > endheap)
	endheap = hp;
      if(endheap >= starttable) {
	fprintf(stderr,"Unlikely, but may happen, increase heap and try again.");
	noheapleft();
      }
#ifdef EXTRA_DEBUG
      if(Sflag) {
	fprintf(stderr,"Increasing table by %d\n",(endtable-starttable)/8);
      }
#endif
      starttable = endheap;
      etp = starttable + TCLAIM;
      realehp = ehp = endheap - HCLAIM;
    }
    /* Fall through */
  case gcGenSewardM:          /**************** -gc-gen (major) *****************/
    GCstartmajor();
    oldstarthp = newhp = oldhp = startheap;
  collectGenSeward:
    hp = newhp;
    gcstk();    /* returns with hp pointing after the last collected node */
    filegcfinish();
    heapsize = (fullheap ? 10 : 4) * (hp-oldstarthp);
    heapsize = max(heapsize,MinHeapsize);
    heapsize = min(heapsize,(endheap-hp)/2);
    GCend(hp-newhp,bos-ep, hp - newhp, hp - oldstarthp, heapsize);  /* Find out how much graph was moved. Stack,New,Old,Next */
    oldhp = hp;
    Minleft = heapsize / 20 + 1000;
    if(gcflag == gcGenSewardM) {  /******* this is a major *******/
      GCendmajor();
      if(hp+Minleft > realehp)
	noheapleft();
      gcflag = gcGenSeward;
    }
    hp = hp + heapsize;
    ehp = realehp = hp+heapsize-HCLAIM;
    if(oldhp-startheap < Utilize && hp+Minleft < realehp)
      break;
    /************** -gc-gen major next time ****************/
    hp = oldhp;
    gcflag = gcGenSewardM;
    tp = 0;
    realehp = ehp =endheap - HCLAIM;
    heapsize = endheap - startheap;
    if(hp+Minleft > realehp)
      noheapleft();
    break;

  case gcGen2:             /**************** -gc-gen2 *****************/
#if 0
    fprintf(stderr,"startheap = %08lx,  ",startheap);   /* !!! PHD !!! */
    fprintf(stderr,"oldhp = %08lx,  ",oldhp);   /* !!! PHD !!! */
#endif
    if(tp == 0) {
      fprintf(stderr,"No table extension for gcGen2 (yet)");
      finish(1);
    }
    newhp = nxtheap;
    hp = newhp;
    gcstk();    /* returns with hp pointing after the last collected node */
    filegcfinish();
    curheap = nxtheap;
    oldSize = oldhp-saveoldhp;
#if 0
    fprintf(stderr,"*GC* saveoldhp[0] = %08lx, ",*(Int *)saveoldhp);   /* !!! PHD !!! */
    fprintf(stderr,"saveoldhp = %08lx,  ",saveoldhp);   /* !!! PHD !!! */
    fprintf(stderr,"oldhp     = %08lx\n",oldhp);   /* !!! PHD !!! */
#endif
    moved = hp-newhp + oldSize;

#if 1
    if(curheap-oldhp < endheap - curheap) {
/*     Start|ooooooooooo|      |nnnnnnnn|                                  |End */
/*                   oldhp   curheap   hp                                       */
      heapsize = min(curheap - oldhp,(endheap - curheap)/2);
      nxtheap = endheap - heapsize;
    } else {
/*     Start|ooooooooooo|                                  |nnnnnnnn|      |End */
/*                   oldhp                              curheap     hp          */
      heapsize = min((curheap - oldhp)/2,endheap - curheap);
      nxtheap = (curheap - oldhp)/2+oldhp;
    }
    realehp = ehp = curheap+heapsize - HCLAIM;
    if(oldhp > Utilize+startheap) {
#else
    if(!calcGen2(oldSize,newSize)) {
#endif
      GCend(moved,bos-ep, hp-newhp, oldhp-startheap, 0); /* Find out how much graph was moved. Stack,New,Old,Next */
      gcflag = gcGen2M; /* Isn't global variables wonderful ? */
      GCstartmajor();
      GCstart(bos-ep, 0);
      stacksize = bos - ep;                              /********* -gc-gen2 major ******/
      *--ep = (PTR)(UInt)stacksize;
      *--ep = (PTR)&VEK;
      ep[-1] = 0; /* This is a gclink word */

      if(Sflag>1) {
	fprintf(stderr,"doGen21\n");
	fprintf(stderr,"nxtheap    = %08lx curheap =  %08lx\n",(UInt)nxtheap,(UInt)curheap);
	fprintf(stderr,"oldstarthp = %08lx oldhp =    %08lx\n",(UInt)oldstarthp,(UInt)oldhp);
	fprintf(stderr,"tp =         %08lx endtable = %08lx\n",(UInt)tp,(UInt)endtable);
	fprintf(stderr,"hp =         %08lx\n",(UInt)hp);
	fprintf(stderr,"ep =         %08lx bos-ep =   %d\n",(UInt)ep,bos-ep);
      }

      doGen21(ep);  /* major [startheap,oldhp] */
      ep += 2;
#if DUMP
      if(Gflag && Gno <= 0){
	dumpstack("after major GC\n");
      }
#endif
      checkvp2(oldhp,hp,(int***)garbvp0,(int***)garbvp,bos,ep);
      moved = oldhp - startheap;
      Utilize = 2*(endheap-oldhp);
      Utilize = Utilize/3+(oldhp-startheap);



      if(curheap-oldhp < endheap - curheap) {
	/*     Start|ooooooooooo|      |nnnnnnnn|                                  |End */
	/*                   oldhp   curheap   hp                                       */
	heapsize = min(curheap - oldhp,(endheap - curheap)/2);
	nxtheap = endheap - heapsize;
      } else {
	/*     Start|ooooooooooo|                                  |nnnnnnnn|      |End */
	/*                   oldhp                              curheap     hp          */
	heapsize = min((curheap - oldhp)/2,endheap - curheap);
	nxtheap = (curheap - oldhp)/2+oldhp;
      }
      realehp = ehp = curheap+heapsize - HCLAIM;


#if 0
      if(!calcGen2(oldSize,newSize)) {  /* No worry if nxtheap < curheap */
	extern gen2mall();
	GCend(moved,bos-ep, hp-newhp, oldhp-startheap, 0); /* Find out how much graph was moved. Stack,New,Old,Next */
	gcflag = gcGen2S; /* Isn't global variables wonderful ? */
	GCstart(bos-ep, 0);
       	hp = newhp = curheap;
	if(Sflag>1) {
	  fprintf(stderr,"gen2mall\n");
	  fprintf(stderr,"nxtheap    = %08lx curheap =  %08lx\n",(UInt)nxtheap,(UInt)curheap);
	  fprintf(stderr,"oldstarthp = %08lx oldhp =    %08lx\n",(UInt)oldstarthp,(UInt)oldhp);
	  fprintf(stderr,"tp =         %08lx endtable = %08lx\n",(UInt)tp,(UInt)endtable);
	  fprintf(stderr,"hp =         %08lx\n",(UInt)hp);
	  fprintf(stderr,"ep =         %08lx bos-ep =   %ld\n",(UInt)ep,bos-ep);
	}
	*--ep = (PTR)stacksize;
	*--ep = (PTR)&VEK;
	gen2mall();
	ep +=2;
	calcGen2(oldSize,newSize);
	moved = hp - newhp;
      }
	/*     Start|ooooooooooo|                          |        |nnnnnnn|      |End */
	/*                   oldhp                     nxtheap    curheap   hp          */
      nxtheap = oldhp + heapsize;
      heapsize = min(heapsize,endheap-curheap);
      realehp = ehp = curheap+heapsize - HCLAIM;
#endif
      GCendmajor();
    }
    GCend(moved,bos-ep, hp-newhp, oldhp-startheap, heapsize); /* Find out how much graph was moved. Stack,New,Old,Next */
    gcflag = gcGen2;
    if(hp+Minleft > realehp)
      noheapleft();
    break;
  default:
    newhp = 0;  /* Removes warning from compiler */
    fprintf(stderr,"dogc doesn't know gcflag %d\n",gcflag);
    finish(1);
  }
  
#if GCSTAT
  if (Sflag>1) {
    fprintf(stderr, "leave dogc\n");
    fprintf(stderr, "dogc: cur=%lx nxt=%lx hp=%lx ehp=%lx endheap=%lx\n",
	    (UInt)curheap, (UInt)nxtheap, (UInt)hp, (UInt)ehp, (UInt)endheap);
    fflush(stderr);
  }
#endif /* GCSTAT */
  
  starthp = hp;

#ifdef HSHOW
  if(showing) {
    fprintf(stderr,"Show after  gc %lx %lx %lx\n",(UInt)hp,(UInt)ehp,(UInt)realehp);
    /*    showHeap(); */
#if 0
    showheapEhp = ehp = hp + showheapStep;
    if(ehp>realehp || hp+HCLAIM>ehp) {
      showheapEhp = ehp = realehp;
    }
#endif
  }
}
#endif
  run_gc_hooks(1);

  if (intrflag) {
    intrflag = 0;
#ifdef SIGBLOCK
    sigsetmask(oldmask);
    if (Sflag > 2) fprintf(stderr, "sigsetmask %x\n", oldmask);
#endif
    cleansig();
    failintr();
  }
  doinggc = 0;
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr, "dogc finish\n");
#endif /* GCSTAT */
    /* SHOULD UNBLOCK ALL SIGNALS HERE */
#ifdef SIGBLOCK
  sigsetmask(oldmask);
  if (Sflag > 2) fprintf(stderr, "sigsetmask %x\n", oldmask);
#endif
}

/*
** Handle CAFs that should survive forever (e.g. because of
** dynamic loading.
*/

static PTR caflist;

static void
gccaf(after)
int after;
{
    if (after)
        caflist = *ep++;
    else
        *--ep = caflist;
}

void
initcaf()
{
    add_gc_hook(gccaf);
    caflist = mknil();
}

void
centercaf()
{
    extern int cafflag;

    if (!cafflag) return;
    if (debug) fprintf(stderr, "Entering CAF at %lx\n", (UInt)*ep);
    caflist = mkcons(*ep, caflist);
}
