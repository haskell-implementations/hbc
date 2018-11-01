
#define tgc11     1
#define tgc12     2
#define tgccap    tgc12
#define tgc20     3 
#define tgc21     4
#define tgc30     5
#define tgcvek    6
#define tgcstr    7
#define tgcdvek   8
#define tgcinp    9
#define tgcuvek  10
#define tgcfun   11
#define tgchole  12
#define tgcind   13
#define tgczap   14
#define tgcvapG  15
#define tgcvap   16       /* Change to gcvapG node during gc */
#define tgcap    17       /* Change to gcapG  node during gc */
#define tgcapG   18
#define tgcbig   19
#define tgcmkd   20
#define tgcmvd   21
#define tgcret   22
#define tgcm     23
#define tgcint   24
#define tgcchr   25
#define tgctag0  26
#define tgcindi  27

/* Do not insert extra spaces here! */
#define VFarity	0
#define VFpointer 1
#define VFunwind 2
#define VFvapunwind 3
#define VFcode 4
#define VFcallcode 5
#define VFstingy 6
#define VFhprof 7
#define VFspare	8
#define	VFlink 9
#define	VFnref 10
#define	VFrefs 11

#define UPDTABSIZE 2000000


#define USETABELS       1
#define POINTERREVERSAL 1

#define isinder(tag)  (tag>=&TAGLAST || tag<&TAGFIRST)

#define BitPerWord   32
#define LnBitPerWord  5
#define BitMask      31

#if defined(mips)
#define SIZEBITS 4
#else
#if defined(i386) && defined(sun)
#define SIZEBITS 8
#else
#define SIZEBITS 8
#endif
#endif

#ifdef __alpha
#define BitPerPtr 64
#else
#define BitPerPtr 32
#endif

#define MAXSIZE ((1l<<SIZEBITS) -1l)
#define SHIFT   (BitPerPtr-SIZEBITS)
#define COUNTMASK ((1l<<SIZEBITS)-1l)
#define TAGMASK  ((1l<<SHIFT)-1l)
#define SETCOUNTER(t,n) t->tag = (Tag *)((((Int)n)<<SHIFT) | (TAGMASK&(Int)t->tag))
#define GETCOUNTER(t) ((((Int)t->tag)>>SHIFT)&COUNTMASK)
#define DELCOUNTER(t) t->tag = (Tag *)(TAGMASK & (Int)t->tag)
#define GETTAG(n) ((Tag *)(TAGMASK & (Int)n->tag))

#define GCLINK(p) ((GcLink *)(((int **)p)-1))
#define GCLINK2(p) ((GcLink *)(((int **)p)-linkOffset))

#define GCNODE2(p) ((Node *)(((int **)p)+linkOffset))
