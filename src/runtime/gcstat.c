#include "runtime.h"
#include "vars.h"

#include "../mcode/limit.h"

int    argcOrg;
char **argvOrg;

int tpin,tpout,tpdupl,tpnew,tpdel,tpstk;


/*********************************************************************/
/*
**	Stuff for printing out GC statistics when 
**	the S runtime flag has been given.
*/

static double GCstart_time, GC_tot_time = 0;  /* For measuring total GC time    */
static double GCrstart_time, GC_rtot_time = 0;  /* For measuring total real GC time    */
static double GCstart_major, GC_tot_major = 0;
static double GCrstart_major, GC_rtot_major = 0;
static double tot_used_heap = 0; /* For counting total heap usage. */
static double gc_tot_moved = 0; /* For counting total memory shuffling done by gc. */

int no_of_GCs = 0;
static int no_of_majors = 0;
FILE *statf;	/* statistics file. */

double
usertime()
{
#if defined(__ARM) 
    return 0.0;  /* !!! */
#else
#if defined(SYSV) && !defined(irix) || defined(SOLARIS) || defined(HPUX)
#ifndef HZ
#define HZ CLK_TCK
#endif
    struct tms t;

    (void)times(&t);
    return (double)(t.tms_utime)/HZ;
#else
    struct rusage t;

    getrusage(0, &t);
    return(t.ru_utime.tv_sec + 1e-6*t.ru_utime.tv_usec);
#endif
#endif /* __ARM */
}

static double now = 0.0;
double
realtime()
{
#ifdef __ARM
    return 0.0;  /* !!! */
#else
#if defined(SYSV) && !defined(irix) || defined(SOLARIS)
    struct tms t;

    return (double)times(&t)/HZ - now;
#else
    struct timeval tv;

    gettimeofday(&tv, (struct timezone *)0);
    return tv.tv_sec + 1e-6*tv.tv_usec - now;
#endif
#endif /* __ARM */
}

/* Called at the beginning of execution of the program.
*/
void
GCstartup()
{
    extern char *progname;
    char buf[10000];
    char *p;
    extern char *Sfile;

    if (sflag || Sflag) {
	if (Sfile) {
	    if (strcmp(Sfile, "stderr") == 0) {
		statf = stderr;
		goto opened;
	    }
	    strcpy(buf, Sfile);
	} else {
	    if ((p = strrchr(progname, '/')))
		p++;
	    else
		p = progname;
	    sprintf(buf, "STAT.%s", p);
	}
	statf = fopen(buf, "w");
    } else
	statf = NULL;
 opened:
    if(statf != NULL) {
	if(Sflag){
	    while(argcOrg--) {
	      fputs(*argvOrg++,statf);
	      putc(' ',statf);
	    }
	    fprintf(statf,
/*######### ######## ##### ##.## ###.# ###.## ####.# ####.## ####.# ####### ##### ##### #### ##### ##### #####*/
 "\n Nw Heap   Tt Heap   Stk    GC(real) GC acc (real)     tot (real)  newheap    in -dupl  -new  -del  +stk   out  %d\n",gcflag);
	    fflush(statf);
	}
    }
    now = realtime();
}

int gen2_gsret,gen2_out,gen2_in;   /* !!! */
int gen2_startHp,gen2_startOldHp;  /* !!! */
int gen2_stackHp,gen2_stackOldHp;  /* !!! */
int gen2_scanHp,gen2_scanOldHp;  /* !!! */
int gen2_scanTotal = 0;
int gen2_stackTotal = 0;

/* Called at the beginning of each GC.
*/
/*ARGSUSED*/
void
GCstart(stacksize, words)
int stacksize,words;
{
  gen2_gsret = 0;  /* !!! */
  gen2_out   = 0;  /* !!! */
  gen2_in    = 0;  /* !!! */

  no_of_GCs++;
  tot_used_heap += words;
  if(Bflag) {
    if (Bflag > 1)
      fprintf(stderr, " GC ");
    else
      fprintf(stderr, "\007");
  }
  GCstart_time = usertime();
  GCrstart_time = realtime();
}

double GCstart_scan, GC_tot_scan = 0.0;
double GCstart_stack, GC_tot_stack = 0.0;
double GCstart_marked, GC_tot_marked = 0.0;
double GCstart_moved, GC_tot_moved = 0.0;

void startTime(dt,ds)
double *dt,*ds;
{
  *ds = usertime();
}

void stopTime(dt,ds)
double *dt,*ds;
{
  *dt += usertime() - *ds;
}

void
GCstartmajor()
{
  no_of_majors++;
  GCstart_major = usertime();
  GCrstart_major = realtime();
}

void
GCendmajor()
{
  GC_tot_major += usertime() - GCstart_major;
  GC_rtot_major += realtime() - GCrstart_major;
}

/*	Called at the end of each GC.
*/
void
GCend(moved,stacksize, newwords,oldwords, newheapsize)
int moved,stacksize,newwords,oldwords;
int newheapsize;
{
  double time = usertime();
  double rtime = realtime();

  gc_tot_moved += moved*sizeof(UInt);

  if (Bflag > 1)
    fprintf(stderr, "\b\b\b  \b\b\b");
  GC_tot_time += time-GCstart_time;
  GC_rtot_time += rtime-GCrstart_time;
  if(statf != NULL && Sflag){
/*######### ######## ##### ##.## ###.# ###.## ####.# ####.## ####.# ####### ##### ##### #### ##### ##### #####*/
/*Nw Heap Tt Heap   Stk    GC(real) GC acc (real)     tot (real) newheap  in   -dupl -new  -del +stack  out*/
    fprintf(statf,
	    "%9d %8d %5d %5.2f %5.1f %6.2f %6.1f %7.2f %6.1f %8d %5d %5d %5d %5d %5d %5d %2d\n", 
	    sizeof(UInt)*newwords, 
	    sizeof(UInt)*oldwords, 
	    stacksize,
	    (time-GCstart_time), 
	    (rtime-GCrstart_time), 
	    GC_tot_time, 
	    GC_rtot_time, 
	    time,
	    rtime,
	    newheapsize*sizeof(UInt)
	    ,tpin
	    ,tpdupl
	    ,tpnew
	    ,tpdel
	    ,tpstk
	    ,tpout
	    ,gcflag
	    );

#if 0
    fprintf(statf,"stack = %7d + %7d = %7d ",                                                    /* !!! */
	    gen2_stackHp-gen2_scanHp, gen2_stackOldHp-gen2_scanOldHp, gen2_stackHp-gen2_scanHp + gen2_stackOldHp-gen2_scanOldHp);
    fprintf(statf,"scan = %7d + %7d = %7d ",                                                    /* !!! */
	    gen2_scanHp-gen2_startHp, gen2_scanOldHp-gen2_startOldHp, gen2_scanHp-gen2_startHp + gen2_scanOldHp-gen2_startOldHp);
    fprintf(statf,"Total = %7d\n",gen2_stackHp - gen2_startHp + gen2_stackOldHp - gen2_startOldHp);                        /* !!! */
#endif
    gen2_stackTotal += gen2_stackHp-gen2_scanHp + gen2_stackOldHp-gen2_scanOldHp;
    gen2_scanTotal += gen2_scanHp-gen2_startHp + gen2_scanOldHp-gen2_startOldHp;

#ifdef GCSTAT
    if((gcflag == gcGenSeward || gcflag == gcGenAppel) && Sflag>1) {
      extern int InsideOldHeap,InsidePrevHeap,OutsideHeap;
      extern int ngc20,ngcinp,ngcind,ngc30,ngc21,ngc12,ngc11,ngcvap,ngcvek,ngcap,ngccap,ngczap,ngcfun;
      extern int ongc20,ongcinp,ongcind,ongc30,ongc21,ongc12,ongc11,ongcvap,ongcvek,ongcap,ongccap,ongczap,ongcfun;
      int Total = InsideOldHeap+InsidePrevHeap+OutsideHeap;
      fprintf(statf,
	      "\tPointers to Old = %4d(%4.1f%%) Prev = %4d(%4.1f%%) Data = %4d(%4.1f%%) Total = %4d\n",
	      InsideOldHeap,Total?(InsideOldHeap*100.0)/Total:-1,
	      InsidePrevHeap,Total?(InsidePrevHeap*100.0)/Total:-1,
	      OutsideHeap,Total?(OutsideHeap*100.0)/Total:-1,
	      Total
	      );
      InsideOldHeap = InsidePrevHeap = OutsideHeap = 0;
      fprintf(statf, "\tOld 20=%5d inp=%2d ind=%2d 30=%5d 21=%5d 12=%5d 11=%5d vap=%5d vek=%5d ap=%5d cap=%5d zap=%5d fun=%5d\n",
	      ongc20,ongcinp,ongcind,ongc30,ongc21,ongc12,ongc11,ongcvap,ongcvek,ongcap,ongccap,ongczap,ongcfun);
      fprintf(statf, "\tNew 20=%5d inp=%2d ind=%2d 30=%5d 21=%5d 12=%5d 11=%5d vap=%5d vek=%5d ap=%5d cap=%5d zap=%5d fun=%5d\n",
	      ngc20,ngcinp,ngcind,ngc30,ngc21,ngc12,ngc11,ngcvap,ngcvek,ngcap,ngccap,ngczap,ngcfun);
      ngc20=ngcinp=ngcind=ngc30=ngc21=ngc12=ngc11=ngcvap=ngcvek=ngcap=ngccap=ngczap=ngcfun=0;
    }
#endif
#if STATISTICS
    {
      extern int ngc20,ngc20i,ngcinp,ngc30,ngc30i,ngc21,ngc21i,ngc12,ngc12i,ngc11,ngc11i,ngcvap,
      ngcvek,ngcveki,ngcmkd,ngcm,ngcmvd,ngcap,ngccap;
	  
      fprintf(statf, "20=%d 20i=%d inp=%d 30=%d 30i=%d ap=%d cap=%d 21=%d 21i=%d 12=%d 12i=%d 11=%d 11i=%d vap=%d vek=%d veki=%d mkd=%d m=%d mvd=%d\n",
	      ngc20,ngc20i,ngcinp,ngc30,ngc30i,ngcap,ngccap,ngc21,ngc21i,ngc12,
	      ngc12i,ngc11,ngc11i,ngcvap,ngcvek,ngcveki,ngcmkd,ngcm,ngcmvd);
      ngc20=ngc20i=ngcinp=ngc30=ngc30i=ngcap=ngccap=ngc21=ngc21i=ngc12=
	ngc12i=ngc11=ngc11i=ngcvap=ngcvek=ngcveki=ngcmkd=ngcm=ngcmvd=0;
    }
#endif
    fflush(statf);
  }
}

/*	Called a the end of execution of the program,
**	to print a summary of statistics.
*/

void
GCfinal(curheap, last_hp)
VPTR curheap, last_hp;
{

  double time = usertime();
  double rtime = realtime();

  tot_used_heap += last_hp - curheap;
  if(statf != NULL){
    fprintf(statf, "%10d GCs (%d major),\n", no_of_GCs, no_of_majors);
    fprintf(statf, "%10.2f (%3.1f) seconds total time,\n", time, rtime);
    fprintf(statf, "%10.2f (%3.1f) seconds GC time", GC_tot_time, GC_rtot_time);
    fprintf(statf, " (%4.1f(%4.1f)%% of total time)", 
	    (time!=0.0?GC_tot_time*100./time:0.0),
	    (rtime!=0.0?GC_rtot_time*100./rtime:0.0));
    fprintf(statf, " moved %10.0f bytes %10.2f(%10.2f) Mb/s\n", 
	    gc_tot_moved,
	    (GC_tot_time!=0.0?gc_tot_moved/GC_tot_time/1000000:0.0),
	    (GC_rtot_time!=0.0?gc_tot_moved/GC_rtot_time/1000000:0.0));
    fprintf(statf, "%10.2f (%3.1f) seconds major GC time", GC_tot_major, GC_rtot_major);
    fprintf(statf, " (%4.1f(%4.1f)%% of total time)\n", 
	    (time!=0.0?GC_tot_major*100./time:0.0),
	    (rtime!=0.0?GC_rtot_major*100./rtime:0.0));
#if 0
    fprintf(statf, "%10.2f seconds stack GC time", GC_tot_stack);
    fprintf(statf, " (%4.1f%% of total GC time time) moved %8d bytes %10.2f Mb/s\n", 
	    (GC_tot_time!=0.0?GC_tot_stack*100./GC_tot_time:0.0),gen2_stackTotal,(GC_tot_stack!=0.0?gen2_stackTotal/GC_tot_stack/1000000:0.0));
    fprintf(statf, "%10.2f seconds scan GC time", GC_tot_scan);
    fprintf(statf, " (%4.1f%% of total GC time time) moved %8d bytes %10.2f Mb/s\n", 
	    (GC_tot_time!=0.0?GC_tot_scan*100./GC_tot_time:0.0),gen2_scanTotal,(GC_tot_scan!=0.0?gen2_scanTotal/GC_tot_scan/1000000:0.0));
    fprintf(statf, "%10.2f seconds marked GC time", GC_tot_marked);
    fprintf(statf, " (%4.1f%% of total GC time time)\n", 
	    (GC_tot_time!=0.0?GC_tot_marked*100./GC_tot_time:0.0));
    fprintf(statf, "%10.2f seconds moved GC time", GC_tot_moved);
    fprintf(statf, " (%4.1f%% of total GC time time)\n", 
	    (GC_tot_time!=0.0?GC_tot_moved*100./GC_tot_time:0.0));
#endif

    fprintf(statf, "%10.0f bytes allocated from the heap (%6.2f Mb/s)\n", 
	    sizeof(UInt)*tot_used_heap, sizeof(UInt)*tot_used_heap/(time-GC_tot_time)/1000000
	    );
    fclose(statf);
  }
}

