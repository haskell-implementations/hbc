#ifndef node_h
#define node_h

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#if defined(__svr4__)
/* Damn SCUMSOFT.  They can't even make their own compatibility headers to
** work correctly.
*/
typedef struct {                /* signal set type */
	unsigned long   __sigbits[4];
} sigset_t;
struct sigaltstack {
	char    *ss_sp;
	int     ss_size;
	int     ss_flags;
};
typedef struct sigaltstack stack_t;
#endif
#include <signal.h>
#include <math.h>

#if defined(hpux) && !defined(HPUX)
#define HPUX
#endif

#if defined(mips) && defined(sgi)
#define SYSV
#ifndef irix
#define irix
#endif
#endif

#if (defined(i386) || defined(sparc)) && defined(sun) && !defined(SUNOS4)
#ifndef SOLARIS
#define SOLARIS
#endif
#endif

#ifdef CRAY
#define SYSV
#define index strchr
#define bcopy(a,b,c) memcpy(b,a,c)
#endif

#if defined(sun) && !defined(SUNOS3) && !defined(SOLARIS)
#define DO_MPROTECT
#ifndef SUNOS4
#define SUNOS4
#endif
#endif

#if defined(__NetBSD__) || defined(__FreeBSD__)
#define DO_MPROTECT
#endif

#if defined(__ARM)
#define SYSERROR(n)  strerror(n)
char *strerror();
#define index strchr
#define getpid() 1
#else /* __ARM */

#include <sys/file.h>
#include <sys/time.h>
/*#include <sys/timeb.h>*/
#include <sys/resource.h>
#if !defined(SOLARIS) && !defined(HPUX) && !defined(__CYGWIN32__)
#include <sys/dir.h>
#else
#include <sys/times.h>
#include <limits.h>
#endif
#if !defined(SUNOS4) && !defined(IRIX)
#include <dirent.h>
#endif
#include <sys/stat.h>
#ifndef linux
#if defined(SYSV) || defined(SOLARIS) || defined(HPUX) || defined(__CYGWIN32__)
#include <sys/termio.h>
#else
#if defined(__NetBSD__) || defined(__FreeBSD__)
#include <termios.h>
#define termio termios
#define TCGETA TIOCGETA
#define TCSETA TIOCSETA
#else
#include <sgtty.h>
#endif
#endif /* SYSV */
#else
#include <bsd/sgtty.h>
#endif /* linux */
#include <setjmp.h>

#ifdef DO_MPROTECT
#include <sys/mman.h>
#endif

#define SYSERROR(n)  sys_errlist[n]
#if !defined(__NetBSD__) && !defined(__FreeBSD__)
extern char *sys_errlist[];
#endif

#endif /* __ARM */

#ifdef _AIX
#include <sys/select.h>
#endif /* _AIX */

#if defined(__alpha) && 0
#define CASTPTR 1
#else
#define CASTPTR 0
#endif

#if CASTPTR
typedef int Int;
typedef unsigned int UInt;
typedef int Funptr;
#define FUNPTR(x) ((void (*)())(x))
#else
#if defined(__alpha)
#define IntSize 8
#endif
typedef long Int;
typedef unsigned long UInt;
typedef void (*Funptr)();
#define FUNPTR(x) (x)
#endif

#ifndef IntSize
#define IntSize 4
#endif

typedef struct TAG {
  Funptr eval;
  Funptr unwind;
  Funptr jfun;
  Funptr gtag;
  Funptr gcfun;
  Funptr cmp;    
  Funptr print;
  Funptr force;
  Int gc;
  Funptr genCopy;
  Funptr genScan;
  Funptr genCopyMajor;
  Funptr gcsamplefun;
#if 1
  Int dTagNumber;
  Int extra2;
  Int extra3;
#else
  Funptr bakerMove;
  Funptr bakeScan;
  Funptr bakeScanOutside;
#endif
  Funptr jonkerMove;
  Funptr gen2Scan;
  Funptr gen2Copy;
#if CASTPTR
  int gen2Tag;
#else
  struct TAG *gen2Tag;
#endif
  Funptr gen2Pointer;
  Int tagNumber;
  Funptr gen2Look;
  Funptr gen2Move;
  Int extra4;
} Tag;

#if CASTPTR
typedef int Tagptr;
#define TAGPTR(x) ((Tag*)(x))
typedef int Nodeptr;
#define NODEPTR(x) ((Node*)(x))
typedef int FILEptr;
#define FILEPTR(x) ((FILE*)(x))
typedef int Voidptr;
#define VOIDPTR(x) ((void *)(x))
typedef int Intptr;
#define INTPTR(x) ((void *)(x))
#else
typedef Tag *Tagptr;
#define TAGPTR(x) (x)
#define Nodeptr union NODE*
#define NODEPTR(x) (x)
typedef FILE *FILEptr;
#define FILEPTR(x) (x)
typedef void *Voidptr;
typedef Int *Intptr;
#define INTPTR(x) (x)
#endif

typedef struct {
  Tagptr  tag;
  Nodeptr ptr0;
} Node11;

typedef struct {
  Tagptr  tag;
  Nodeptr ptr0;
  Nodeptr ptr1;
} Node12;
#define FUNOF(p) ((p)->node12.ptr0)
#define ARGOF(p) ((p)->node12.ptr1)
#define HDOF(p) FUNOF(p)
#define TLOF(p) ARGOF(p)
#define FSTOF(p) FUNOF(p)
#define SNDOF(p) ARGOF(p)

typedef struct {
  Tagptr  tag;
  Int   val0;
} Node20;
#define INTOF(p) ((p)->node20.val0)
#define CHAROF(p) INTOF(p)
#define ISNIL(p) ((p)->node20.val0 == 0)
#define DOUBLEOF(p) ((p)->node30d.val0)

typedef struct {
  Tagptr  tag;
  Int   val0;
  Nodeptr ptr0;
} Node21;

typedef struct {
  Tagptr  tag;
  Int   val0;
  Int   val1;
} Node30;
#define STRINGOF(p) ((char*)(p)->node30.val0)
#define STRINGNOF(p) ((p)->node30.val1)

typedef struct {
  Tagptr  tag;
  int   val0;
  int   val1;
} Node2int;

typedef struct {
  Tagptr  tag;
  double val0;
} Node30d;

typedef struct {
  Tagptr  tag;
  Int   size;       /* size in words */
  Nodeptr ptr[1];     /* ptr[size] is what I mean */
} Nodevek;

#define Nodeuvek Nodevek

typedef struct {
  Tagptr  tag;
  Int   size;       /* size in words */
  Int   val[1];     /* val[size] is what I mean */
} Nodedvek;

typedef struct {
  Tagptr  tag;
  FILEptr file;
  Int   dummy;
} Nodeinp;


#define Nodefun struct NODEFUN
#define FunInfo struct FUNINFO

typedef struct FUNLINK {
#if CASTPTR
  int next;
#else
  struct FUNLINK *next;
#endif
  Int             nrefs;
  Nodeptr         refs[1];      /* refs[nrefs] is what I mean */
} FunLink;
 
typedef struct FUNINFO {
  Int         arity;
  Nodeptr     node;     /* Points at the corresponding NodeFun */
  Funptr      unw;
  Funptr      vunw;
  Funptr      jmpcode;
  Funptr      callcode;
  Funptr      stingy;
  Voidptr     hprofinfo;
  Voidptr     spare;
  FunLink link;
#undef FunInfo
} FunInfo;

#undef Nodefun

#if CASTPTR
typedef int FunInfoptr;
#else
typedef FunInfo *FunInfoptr;
#endif

typedef struct {
  Tagptr tag;
  FunInfoptr fun;
  Int      val0;
} Nodezap;

typedef struct NODEFUN {
  Tagptr     tag;
  FunInfoptr fun;
} Nodefun;

typedef struct {
  Tagptr     tag;
  FunInfoptr fun;
  Nodeptr    ptr[1];     /* ptr[fun->arity] is what I mean */
} Nodevap;

typedef struct {       /* This node is used to mark holes of size one */
  Tagptr  tag;
} Nodemkd;

typedef struct {      /*  This node is used to mark holes of size > one */
  Tagptr  tag;
  Nodeptr next;         /* Pointer to next node */
} Noderet;

typedef struct {
  Tagptr  tag;
  Nodeptr new;          /* New address of this node */
} Nodemvd;

typedef struct {      /* not used if NEWMARK non-zero */
  Tagptr  tag;
} Nodem;

typedef struct {
  Tagptr tag;
  Nodeptr ptr[1];
} Nodemark;

typedef union NODE {
  Tagptr   tag;
  Node11   node11;
  Node12   node12;
  Node20   node20;
  Node21   node21;
  Node30   node30;
  Node2int node2int;
  Node30d  node30d;
  Nodevek  nodevek;
#define nodeuvek nodevek
  Nodedvek nodedvek;
  Nodeinp  nodeinp;
  Nodefun  nodefun;
  Nodezap  nodezap;
  Nodevap  nodevap;
  Nodemkd  nodemkd;
  Noderet  noderet;
  Nodemvd  nodemvd;
  Nodem    nodem;
  Nodemark nodemark;
  Int      val;
  Intptr   intptr;
  Nodeptr  ptr;
  Nodeptr  indptr;
#undef Node
} Node;

typedef struct sGCLINK {   /* This struct(?) is before nodes outside the */
#if CASTPTR
  int next;
#else
  struct sGCLINK *next;    /*  heap that can be updated */
#endif
  Intptr          node;   /*  Node           node;  */  /* Here is the node */
} GcLink;

#if CASTPTR
typedef int VPTR;
typedef int PTR;
typedef int PTRptr;
#define PTRPTR(x) ((PTR*)(x))
#else
typedef void **VPTR;
typedef Node *PTR;
typedef PTR *PTRptr;
#define PTRPTR(x) (x)
#endif

#define INCHP(n) hp = (VPTR)((char *)hp + (n))

extern PTRptr ep;

#define PUSHPTR(p) *--ep = p
#define POPPTR(p) p = *ep++
#define ADDGEN(p) {extern VPTR tp;if(tp) *--tp = (PTR)(p);}
#define NEEDGC (hp > ehp)
#define NEEDGCN(n) (hp+(n) > ehp)
#define DOGC dogc()
#define GCCHECKDO if (NEEDGC) DOGC

#if !CASTPTR
#define SIZEdouble 2
#else
#define SIZEdouble 3
#endif
#define SIZEint 2
#define SIZEchar 2
#define SIZEcons 3
#define SIZEpair 3
#define SIZEconstr(np) (5+(np))
#define SIZEvek(n) (2+(n))
#define SIZEarray(n) (3+SIZEvek(n))

#endif

#include "funs.h"

#ifdef HPROFILE
#undef HSHOW
#endif

#ifndef sigmask
#define sigmask(n) (1 << (n))
#endif
