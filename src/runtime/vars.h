#include "node.h"

/*
 * Layout of the heap (using top half/generation)
 *
 *
 *  |<------ Heapsize -------->|                                |<-Tablesize->|
 *  ---------------------------------------------------------------------------
 *  |OOOO                      |NNN                     |       |      PPPPPPP|
 *  ---------------------------------------------------------------------------
 *  ^   ^                       ^   ^                   ^       ^      ^      ^
 *  |   oldhp                   |   hp                  |       |      tp     endtable
 *  |                          currheap = starthp       |       endheap=starttable      
 *  startheap = nxtheap = oldstarthp                    realehp = ehp
 *
 * Layout of the heap (using bottom half,gcOrg)
 *
 *
 *  |<------ Heapsize -------->|                                |<-Tablesize->|
 *  ---------------------------------------------------------------------------
 *  |NNNN               |       |                               |      PPPPPPP|
 *  ---------------------------------------------------------------------------
 *  ^   ^               |       ^                               ^      ^      ^
 *  |   hp              |       nxtheap                         |      tp     endtable
 *  |                   realehp = ehp                           endheap=starttable      
 *  startheap = currheap = starthp = oldhp = oldstarthp
 *
 * Layout of the heap (gcSlide)
 *
 *
 *  |<------ Heapsize ----------------------------------------->|<-Tablesize->|
 *  ---------------------------------------------------------------------------
 *  |OOOOOOONNNNNNNNNNNNNNNNNN                           |      |      PPPPPPP|
 *  ---------------------------------------------------------------------------
 *  ^      ^                 ^                           ^      ^      ^      ^
 *  |      oldhp = starthp   hp                          |      |      tp     endtable
 *  |                                                    |      endheap=starttable      
 *  startheap = currheap = oldstarthp  realehp = ehp
 *
 */ 

#ifndef EXTERN
#define EXTERN extern
#endif

EXTERN int Gflag;
EXTERN int MergeHiaton;
EXTERN int Hflag;
EXTERN int Ttime;
EXTERN int debug;
EXTERN int sflag, Sflag;
EXTERN int Bflag;
EXTERN int aflag;
EXTERN int gcflag;
#ifdef HSHOW
EXTERN int showflag;
#endif

EXTERN int MinHeapsize;
EXTERN int Heapsize;
EXTERN int Utilize;
EXTERN int heapsize;
EXTERN int Minleft;
EXTERN VPTR oldhp,oldstarthp,starthp,hp, ehp, realehp;
EXTERN VPTR curheap, nxtheap, startheap, endheap;

EXTERN int Tablesize;
EXTERN VPTR etp,tp,starttable,endtable;

EXTERN FILE *mystdin;
EXTERN char *progname;

EXTERN PTR *bos, *ep;
EXTERN PTR *stack;

EXTERN int doinggc;
EXTERN int intrflag;

#if DUMP
EXTERN unsigned int stopaddr;
EXTERN int **chkaddr,*chkval;

extern int Gno;
extern int dumpdepth;
EXTERN int dflag;
extern int maxdump;
#endif

#define gcError     -1   /* Used if more than one gc selected */
#define gcOrg        0
#define gcChenney    1
#define gcSlide      2
#define gcGenSeward  3
#define gcGenSewardM 4
#define gcGenAppel   5
#define gcGenAppelM  6
#define gcGen2       7
#define gcGen2M      8
#define gcGen2S      9
