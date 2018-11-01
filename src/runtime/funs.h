#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#if defined(__NetBSD__) ||  defined(__FreeBSD__)
#define _cnt _r			/* unportable stdio hack */
#endif

#if defined(__ANSI__) || defined(__GNUC__)
#define PROTO(x) x
#else
#define PROTO(x) ()
#endif

#if defined(sequent)
#define SIGTYPE int
#define SIGLAST return 0
#else
#define SIGTYPE void
#define SIGLAST return
#endif

/*** M-code functions ***/
void gc PROTO((PTR *, PTR *));		/* M-code */
void genc PROTO((PTR *, PTR *));		/* M-code */
void doGenSeward0 PROTO((PTR*));
void doGenSeward1 PROTO((PTR*));
void doGen20 PROTO((PTR*));
void doGen21 PROTO((PTR*));
void doSlide PROTO((PTR));
void packTable PROTO((void));
void clearTable PROTO((void));
void eval PROTO((void));		/* M-code */
void forceit PROTO((void));		/* M-code */
SIGTYPE failintr () /*PROTO((void))*/;		/* M-code */
SIGTYPE failfloat () /*PROTO((void))*/;	/* M-code */
void print PROTO((PTR));		/* M-code */
void flushlow PROTO((void));		/* M-code */
void cleansig PROTO((void));
void finish PROTO((int));
void filegcfinish PROTO((void));
void printtop PROTO((FILE *));
void setuptenure PROTO((int,int,int));
void setupstack PROTO((void));
void setupheap PROTO((void));
void dogc PROTO((void));

void GCstartup PROTO((void)), GCfinal PROTO((VPTR, VPTR));
void GCstart PROTO((int,int));         /* stackdepth, allocated */
void GCminor PROTO((int,int,int,int)); /* stackdepth,new survivours,total resident,newheapsize */
void GCend PROTO((int,int,int,int, int));  /* totalmoved,stackdepth,new survivours,total resident,newheapsize */
void GCstartmajor PROTO((void));
void GCendmajor PROTO((void));


/*** ... ***/
void set_tty PROTO((int));


/*** backtrace ***/
void loadsymbols PROTO((void));
char *lookup PROTO((unsigned long));

#if DUMP
void dumpgraph PROTO((PTR, int));
void dumpstack PROTO((char *));
#endif


/*** GC stuff in C ***/
Node *do_baker PROTO((PTR *sep,Node *dptr,Node *sptr,int major));
Node *scanupdated PROTO((Node *dst));
void checkvp PROTO((int ***bot, int ***topp,PTR *sbot,PTR *stop));
void patchUpdated PROTO((int slide,PTR ep));
Node *heapslide PROTO((VPTR to, VPTR from, VPTR end, int slide));
void noheapleft PROTO((void));
void * xmalloc PROTO((size_t));
void * Xmalloc PROTO((size_t, char *));
void * xrealloc PROTO((void *, size_t));

void addgcfile PROTO((FILE*,DIR*));

/*** node.c stuff ***/
PTR mkconstr PROTO((Int, Int, ...));
PTR mkbool PROTO((int));
PTR mknil PROTO((void));
PTR mkcons PROTO((PTR,PTR));
PTR mkpair PROTO((PTR,PTR));
PTR mkap PROTO((PTR,PTR));
PTR mkint PROTO((Int));
PTR mkchar PROTO((Int));
PTR mkno PROTO((PTR));
PTR mkyes PROTO((PTR));
PTR mkcstring PROTO((/*const*/ char *));
PTR mkestring PROTO((char *));
PTR mknode2 PROTO((Tag *, Int, Int));
PTR mknode2p PROTO((Tag *, PTR, PTR));
PTR mknode11 PROTO((Tag *, Int, PTR));
PTR mknode1 PROTO((Tag *, Int));
PTR mknode1p PROTO((Tag *, PTR));
PTR mkdouble PROTO((double));
PTR mkfloat PROTO((double));
PTR mkbytevector PROTO((char *, unsigned));
PTR mkarray PROTO((int, int));
PTR *arrayref PROTO((PTR, int));
double getdouble PROTO((PTR));
double getfloat PROTO((PTR));
int getcno PROTO((PTR));
PTR getpart PROTO((PTR, int, int));
void getbytevector PROTO ((PTR, char **, unsigned *));
void update PROTO((PTR, PTR));
void tocstring PROTO((PTR, char *, int));
int evalstring PROTO((PTR, char *, int));
PTR evaluate PROTO((PTR));
PTR evalargs PROTO((PTR, int));
PTR forceeval PROTO((PTR));
void dogc PROTO((void));
void add_gc_hook PROTO(( void (*)() ));

#ifdef HSHOW
void  setupDisplay PROTO ((double));
void  setupShowTimer();
#endif
