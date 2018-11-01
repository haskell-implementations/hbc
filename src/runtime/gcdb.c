#include "runtime.h"
#include "vars.h"
#include "gc.h"
#include "gcd.h"

extern Tag MOVED;  /* defined in runtime.M */
extern Tag MARKED;  /* defined in runtime.M */
extern Tag DVEK;  /* defined in runtime.M */
int gc_gen;

void gcerror PROTO((char *c,Int d));
     void setdirref PROTO((FILE *f));
     void setfileref PROTO((FILE *f));
     extern int **chkaddr;
     
     extern GcLink *gclink;     /* Nodes outside heap */
     extern GcLink *oldgclink;
     extern FunLink *funlink;
     extern FunLink *oldfunlink;
     
     VPTR heap_low;
     VPTR heap_high;
     
#ifdef GEN_GC
     extern Tag INT;
     extern Tag DFLOAT;
     extern Tag SFLOAT;
     extern Tag CHAR;
     extern Tag TAG0;
     extern Tag AP;
     extern Tag APG;
     extern Tag VAP;
     extern Tag VAPG;
     extern Tag HOLE;
     extern Tag ZAP;
     
extern int tpin,tpout,tpdupl,tpnew,tpdel,tpstk;
     
#ifdef GCSTAT
     int InsideOldHeap,InsidePrevHeap,OutsideHeap;
int ngc20,ngcinp,ngcind,ngc30,ngc21,ngc12,ngc11,ngcvap,ngcvek,ngcbvek,ngcap,ngccap,ngczap,ngcfun,ngcmvd,ngcmkd;
int ongc20,ongcinp,ongcind,ongc30,ongc21,ongc12,ongc11,ongcvap,ongcvek,ongcbvek,ongcap,ongccap,ongczap,ongcfun,ongcmvd,ongcmkd;
#endif
#endif

#define INSIDEOLD(n) ((n)>=heap_low && (n)<heap_high) 
#define INSIDEHEAP(n) ((n)>=(PTR)startheap && (n)<(PTR)endheap) 

char *tagname();

void genc();
void gens();
void gensall();
void majorc();

double usertime();
double realtime();

Node *scanupdated(dst)
     Node *dst;
{
  return dst;
}

#if 0
  UpdateTable *ReadUpdated;
  UpdateTable *OldWrite;
  UpdateTable *Slide;
  Tag *tag;
#ifdef EXTRA_DEBUG
    if(Gflag && Gno<=0) {
      fprintf(stderr,"Scanupdated enter\n");
    }
#endif
  if(hp != (VPTR)dst)
    fprintf(stderr,"scanupdated hp = %lx dst = %lx\n",(UInt)hp,(UInt)dst);
  hp = (VPTR)dst;
  InUpdated = WriteUpdated-Updated;

  ReadUpdated = Updated;
  DuplUpdated = NewUpdated = DelUpdated = 0;
  while(ReadUpdated < WriteUpdated) {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)ReadUpdated->ptr)
      fprintf(stderr, "scanupdated found %lx %s\n", (UInt)chkaddr,tagname((PTR)ReadUpdated->ptr));
    if(&MOVED == ((PTR)ReadUpdated->ptr)->tag)
      fprintf(stderr, "scanupdated enter with MOVED %lx\n",(UInt)ReadUpdated->ptr);
#endif
    if(INSIDEOLD(ReadUpdated->ptr)) {              /* This is a fix to ignore (V)APG which have */
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)ReadUpdated->ptr)
      fprintf(stderr, "scanupdated says it is in previous heap\n");
    if(Gflag && Gno<=0) {
      fprintf(stderr,"Pre %4d(%4d):",ReadUpdated-Updated,WriteUpdated-Updated);
      dumpgraph((PTR)(ReadUpdated->ptr),1);
      fprintf(stderr,"\n");
    }
#endif
      ReadUpdated->ptr = (--WriteUpdated)->ptr;         /* been used to update cells in newspace */
      NewUpdated++;
    } else {
#ifdef EXTRA_DEBUG
    if(Gflag && Gno<=0) {
      fprintf(stderr,"Old %4d(%4d):",ReadUpdated-Updated,WriteUpdated-Updated);
      dumpgraph((PTR)(ReadUpdated->ptr),1);
      fprintf(stderr,"\n");
    }
#endif
    if(&MARKED == (ReadUpdated->tag = ((PTR)ReadUpdated->ptr)->tag)) {  /* Not the first reference  */
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)ReadUpdated->ptr)
      fprintf(stderr, "scanupdated says not first reference\n");
#endif
	ReadUpdated->ptr = (--WriteUpdated)->ptr;
	DuplUpdated++;
      } else {                                                            /* First reference */
#ifdef EXTRA_DEBUG
	if(chkaddr == (int **)ReadUpdated->ptr)
	  fprintf(stderr, "scanupdated says is is first reference\n");
#endif
	if(&CHAR == ReadUpdated->tag || &INT == ReadUpdated->tag ||
	   &TAG0 == ReadUpdated->tag || &DFLOAT == ReadUpdated->tag ||
           &SFLOAT == ReadUpdated->tag) {
#ifdef EXTRA_DEBUG
	if(chkaddr == (int **)ReadUpdated->ptr)
	  fprintf(stderr, "scanupdated ignores it\n");
#endif
	  ReadUpdated->ptr = (--WriteUpdated)->ptr;
	} else {
#ifdef EXTRA_DEBUG
	if(chkaddr == (int **)ReadUpdated->ptr)
	  fprintf(stderr, "scanupdated marks it\n");
#endif
	  ((PTR)ReadUpdated->ptr)->tag = &MARKED;
#ifdef EXTRA_DEBUG
	  if(&MOVED == ReadUpdated->tag)
	    fprintf(stderr, "scanupdated keep MOVED %lx\n",(UInt)ReadUpdated->ptr);
#endif
	  ReadUpdated++;
	}
      }
    }
  }
  OldWrite = WriteUpdated;
  gensall(Updated,OldWrite);
  /* Try to compress the table */
  for(Slide = ReadUpdated = Updated; ReadUpdated < OldWrite; ReadUpdated++) {
    tag = ((PTR)ReadUpdated->ptr)->tag = ReadUpdated->tag;          /* Restore tag */ 
    if(tag == &AP || tag == &APG || tag == &VAP || tag == &VAPG ||  /* Keep pointer if the node is updateable */
       tag == &HOLE || tag == &ZAP )  {   /* This is not necessary according to LA, but who trusts him? */
#ifdef EXTRA_DEBUG
      if(chkaddr == (int **)ReadUpdated->ptr)
	fprintf(stderr, "scanupdated keeps %lx\n", (UInt)chkaddr);
#endif
      (Slide++)->ptr = ReadUpdated->ptr;
    } else  {
#ifdef EXTRA_DEBUG
      if(chkaddr == (int **)ReadUpdated->ptr)
	fprintf(stderr, "scanupdated forgets %lx\n", (UInt)chkaddr);
#endif
      DelUpdated++;
    }
  }
  
  for(;ReadUpdated<WriteUpdated;(Slide++)->ptr=(ReadUpdated++)->ptr) {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)Slide->ptr)
      fprintf(stderr, "scanupdated slide %lx\n", (UInt)chkaddr);
#else
    ;
#endif
  }
  WriteUpdated = Slide;
#ifdef GCSTAT
  ongc20 = ngc20;
  ongcinp = ngcinp;
  ongcind = ngcind;
  ongc30 = ngc30;
  ongc21 = ngc21;
  ongc12 = ngc12;
  ongc11 = ngc11;
  ongcvap = ngcvap;
  ongcvek = ngcvek;
  ongcbvek = ngcbvek;
  ongcap = ngcap;
  ongccap = ngccap;
  ongczap = ngczap;
  ongcfun = ngcfun;
  ongcmvd = ngcmvd;
  ongcmkd = ngcmkd;
  ngc20=ngcinp=ngcind=ngc30=ngc21=ngc12=ngc11=ngcvap=ngcvek=ngcbvek=ngcap=ngccap=ngczap=ngcfun=ngcmvd=ngcmkd=0;
#endif
#ifdef EXTRA_DEBUG
    if(Gflag && Gno<=0) {
      fprintf(stderr,"Scanupdated finished\n");
    }
#endif
  return (Node *)hp;
}
#endif /* scanupdated */

void addupdated(n)
PTR n;
{
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)n)
      fprintf(stderr, "addupdated %lx\n", (UInt)chkaddr);
#endif
    *(int **)--tp = (int *)n;
}

extern FILE *statf;	/* statistics file. */
extern Tag ZAP,HOLE;

void checkvp(bot,topp,sbot,stop)
int ***bot, ***topp;
PTR *sbot,*stop;
{
  int **tmp;
  int ***top;
#if GCSTAT
  if(Sflag>1)
    fprintf(stderr,"checkvp bot = %lx topp = %lx sbot = %lx stop = %lx\n",
	    (UInt)bot,(UInt)topp,(UInt)sbot,(UInt)stop);
#endif
  if(bot) {
    tpstk = 0;
#if defined(sparc) || defined(hppa)
    top = topp;
#else
    top = &tmp;
#endif
#if GCSTAT
    if(Sflag>1)
    fprintf(stderr,"checkvp bot = %lx top = %lx sbot = %lx stop = %lx\n",
	    (UInt)bot,(UInt)top,(UInt)sbot,(UInt)stop);
#endif
    for(; top!=bot; top++) {
      tmp = *top;
      if(!((sizeof(int *)-1)&(Int)tmp) &&
	 --tmp<=(int**)sbot &&
	 tmp>=(int**)stop) {
/* &&	 ((UInt)*tmp)<(UInt)endheap+10000) { */

#ifdef EXTRA_DEBUG
	  if(Gflag && Gno<=0) {
	    fprintf(stderr,"checkvp %4d:",endtable - tp);
	    dumpgraph((PTR)(*tmp),1);
	    fprintf(stderr,"\n");
	  }
#endif /* EXTRA_DEBUG */
	  addupdated((PTR)*tmp);            /* Remember it */
	  tpstk++;
	} 
    }
    tpout = endtable - tp;
  } else {
    fprintf(stderr,"No bot yet\n");
  }
}

Node *move(inode,dst) /* returns pointer to next free word */
     Node **inode;
     Node *dst; 
{
  Node *node = *inode;
  int gctag;
  int size;
  Node *new = dst; 
  Tag *tag=node->tag;
  
#ifdef EXTRA_DEBUG
  if(chkaddr == (int **)node)
    fprintf(stderr, "move chkaddr from %lx\n", (UInt)chkaddr);
#endif
  if(1&(Int)tag) /* This node does not need to be moved */
    return dst;
  gctag = tag->gc;


  if(INSIDEOLD((VPTR)node)) {
#ifdef EXTRA_DEBUG
  if(chkaddr == (int **)dst)
    fprintf(stderr, "move chkaddr to %lx\n", (UInt)chkaddr);
#endif
#ifdef GCSTAT
    InsidePrevHeap++;
#endif
    switch(gctag) {
    case tgcbig:
#ifdef EXTRA_DEBUG
      if(node->node11.ptr0->tag != &DVEK)
	fprintf(stderr, "Bignum (1) %lx: %lx->%lx\n",
		(UInt)node,
		(UInt)node->node11.ptr0,
		(UInt)node->node11.ptr0->tag);
#endif
    case tgc11:      case tgc20:
      dst->node20.tag = node->node20.tag;
      dst->node20.val0 = node->node20.val0;
      node->nodemvd.tag = &MOVED;
      *inode = node->nodemvd.new = dst;
      return (Node *)(2+(int *)dst);
    case tgcind:		/* set bit and jump into nodes of size 3 */
/*      addupdated(dst);		will remember in scan */
      setdirref(node->nodeinp.file);
      goto move3;
    case tgcinp:		/* set bit and fall into nodes of size 3 */
/*      addupdated(dst);		will remeber in scan */
      setfileref(node->nodeinp.file);
    case tgchole: case tgczap:
    case tgcapG:  /* Possible because of updates. I hate this!!! */ 
    case tgc12:      case tgc21:      case tgc30:
    move3:
      dst->node30.tag = node->node30.tag;
      dst->node30.val0 = node->node30.val0;
      dst->node30.val1 = node->node30.val1;
      node->nodemvd.tag = &MOVED;
      *inode = node->nodemvd.new = dst;
      return (Node *)(3+(int *)dst);
    case tgcap:			/* change to APG */
      dst->node12.tag = &APG;
      dst->node12.ptr0 = node->node12.ptr0;
      dst->node12.ptr1 = node->node12.ptr1;
      node->nodemvd.tag = &MOVED;
      *inode = node->nodemvd.new = dst;
      return (Node *)(3+(int *)dst);
    case tgcfun:
      { FunInfo *info = node->nodefun.fun;
	if(!info->link.next) {
	  info->link.next = funlink;
	  funlink = &(info->link);
	}
	*inode = info->node;
      } return dst;		/* Fix references when following funlink */
    case tgcvap: case tgcvapG:
      { FunInfo *info = node->nodevap.fun;
	if(!info->link.next) {
	  info->link.next = funlink;
	  funlink = &(info->link);
	}
	{ int *idst = (int *)dst;
	  int *isrc = (int *)node;
	  for(size = info->arity+2; size; size--)
	    *idst++ = *isrc++;
	  dst->tag = &VAPG;	/* this is nasty !! */
	  dst = (Node *)idst;
	}
	node->nodemvd.tag = &MOVED;
	*inode = node->nodemvd.new = new;
      }
      return dst;
    case tgcvek: case tgcdvek: /*  constants and values are treated equal */ 
      { int *idst = (int *)dst;
	int *isrc = (int *)node;
	for(size = node->nodedvek.size+2; size; size--)
	  *idst++ = *isrc++;
	dst = (Node *)idst;
      }
      node->nodemvd.tag = &MOVED;
      *inode = node->nodemvd.new = new;
      return dst;
    case tgcmvd:
      *inode = node->nodemvd.new;
      return dst;
    case tgcmkd:	gcerror("gcmkd in move",(Int)node); /* No return from gcerror */
    case tgcret:	gcerror("gcret in move",(Int)node); /* No return from gcerror */
    case tgcm:  	gcerror("gcm in move",(Int)node);	/* No return from gcerror */
    default:  	         fprintf(stderr,"Weird addr %lx tag = %x\n",(UInt)node,gctag);
      gcerror("Illegal tag in move",(Int)node);	/* No return from gcerror */
    }
    gcerror("What this is not possible!!!",(Int)node); /** this is never executed **/

  } else if (INSIDEHEAP(node)) {
#ifdef GCSTAT
    InsideOldHeap++;
#endif
    return dst;			/* If pointers then in update list */
  } else {			/* Node is outside heap */
#ifdef GCSTAT
    OutsideHeap++;
#endif
    switch(gctag) {
    case tgcind:		/* set bit and break */
      addupdated(node);		/* Remember it (never scaned) */
      setdirref(node->nodeinp.file);
      break;
    case tgcinp:		/* set bit and fall into nodes of size 3 */
      addupdated(node);		/* Remember it (never scanned) */
      setfileref(node->nodeinp.file);
    case tgchole:
      addupdated(node);		/* Remember it (never scaned) */
    case tgc20:      case tgc30:     case tgcdvek:
      break;			/* No pointers */
    case tgcap:
      node->tag = &APG;
      if(!GCLINK(node)->next) {
	GcLink *tmp = GCLINK(node);	  
	tmp->next = gclink;
	gclink = tmp;
      }
      break;
    case tgcbig:
#ifdef EXTRA_DEBUG
      if(node->node11.ptr0->tag != &DVEK)
	fprintf(stderr, "Bignum (2) pointing at %lx", (UInt)node->node11.ptr0->tag);
#endif
    case tgcapG:     case tgc11:      case tgc12:      case tgc21: case tgcvek:
      if(!GCLINK(node)->next) {
	GcLink *tmp = GCLINK(node);	  
	tmp->next = gclink;
	gclink = tmp;
      }
      break;
    case tgczap:
      { FunInfo *info = node->nodezap.fun;
	if(!info->link.next) {
	  info->link.next = funlink;
	  funlink = &(info->link);
	}
	*inode = info->node;
      } return dst;		/* Fix references when following funlink */
    case tgcfun:
      { FunInfo *info = node->nodefun.fun;
	if(!info->link.next) {
	  info->link.next = funlink;
	  funlink = &(info->link);
	}
	*inode = info->node;
      } return dst;		/* Fix references when following funlink */
    case tgcvap: case tgcvapG:
      node->tag = &VAPG;
      { FunInfo *info = node->nodevap.fun;
	if(!info->link.next) {
	  info->link.next = funlink;
	  funlink = &(info->link);
	}	  
      }
      if(!GCLINK(node)->next) {
	GcLink *tmp = GCLINK(node);
/*	addupdated(node);       will remember in scan */
	tmp->next = gclink;
	gclink = tmp;
      }
      break;
    case tgcmvd:	gcerror("gcmvd in move (2)",(Int)node); /* No return from gcerror */
    case tgcmkd:	gcerror("gcmkd in move (2)",(Int)node); /* No return from gcerror */
    case tgcret:	gcerror("gcret in move (2)",(Int)node); /* No return from gcerror */
    case tgcm: 	gcerror("gcm in move (2)",(Int)node); /* No return from gcerror */
    default:  	gcerror("Illegal tag in move (2)",(Int)node); /* No return from gcerror */
    }
  }
  return dst;
}

Node *scan(nhp,dst,once)
     Node *nhp; 
     Node *dst;
     int once; 
{
  int *sptr = (int *)nhp;
  int size;

  while(dst != (Node *)sptr) {
    int gctag = ((Node *)sptr)->tag->gc;
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)sptr)
      fprintf(stderr, "scan chkaddr from %lx\n", (UInt)chkaddr);
#endif
    switch(gctag) {
    case tgc20:
#ifdef GCSTAT
      ngc20++;
#endif
      sptr += 2;
      break;
    case tgcbig:
#ifdef EXTRA_DEBUG
      if(((Node*)sptr)->node11.ptr0->tag != &DVEK)
	fprintf(stderr, "Bignum (3) %lx: %lx->%lx\n",
		(UInt)sptr, (UInt)((Node*)sptr)->node11.ptr0,
		(UInt)((Node *)sptr)->node11.ptr0->tag);
#endif
    case tgc11:
#ifdef GCSTAT
      ngc21++;
#endif
      dst = move(&((Node *)sptr)->node11.ptr0,dst);
      sptr += 2;
      break;
    case tgcinp:
#ifdef GCSTAT
      ngcinp++;
#endif
      if(gc_gen) addupdated((Node*)sptr); /* Remeber */
      sptr += 3;
      break;
    case tgcind:
#ifdef GCSTAT
      ngcind++;
#endif
      if(gc_gen) addupdated((Node*)sptr); /* Remeber */
      sptr += 3;
      break;
    case tgczap:   /* !!!!!!? */
#ifdef GCSTAT
      ngczap++;
#endif
      if(gc_gen) addupdated((Node*)sptr); /* Remeber */
      sptr += 3;
      break;
    case tgchole:
      if(gc_gen) addupdated((Node *)sptr); /* Remeber */
    case tgc30:
#ifdef GCSTAT
      ngc30++;
#endif
      sptr += 3;
      break;
    case tgcap:
#ifdef EXTRA_DEBUG
      if(!once)   /* possible during scanupdates */
	gcerror("Scanning an AP should be an APG",(Int)sptr);      /* No return from gcerror */
#else
      break;
#endif
    case tgcapG:
#ifdef GCSTAT
      ngcap++;
#endif
      dst = move(&((Node *)sptr)->node12.ptr0,dst);
      dst = move(&((Node *)sptr)->node12.ptr1,dst);
      sptr += 3;
      break;
    case tgc12:
#ifdef GCSTAT
      ngc12++;
#endif
      dst = move(&((Node *)sptr)->node12.ptr0,dst);
      dst = move(&((Node *)sptr)->node12.ptr1,dst);
      sptr += 3;
      break;
    case tgc21:
#ifdef GCSTAT
      ngc21++;
#endif
      dst = move(&((Node *)sptr)->node21.ptr0,dst);
      sptr += 3;
      break;
    case tgcfun:
#ifdef GCSTAT
      ngcfun++;
#endif
#ifdef EXTRA_DEBUG
      if(!once)   /* possible during scanupdates */
	gcerror("gcfun in scan",(Int)sptr);      /* No return from gcerror */
#else
      break;
#endif
    case tgcvap:
      if(!once)   /* possible during scanupdates */
	gcerror("Scanning a VAP should be a VAPG",(Int)sptr);      /* No return from gcerror */
     case tgcvapG:
#ifdef GCSTAT
      ngcvap++;
#endif
      { FunInfo *info = ((Node *)sptr)->nodevap.fun;
#if 0        /* Nice test but does not work with major collection */
	if(!gc_gen && !info->link.next)  /* Can happen because of updates */
	  gcerror("gcvap moved without link?",(Int)sptr);      /* No return from gcerror */
#endif
	sptr +=2;
	size = info->arity;
	for(; size; size--) {
	  dst = move((Node **)(sptr++),dst);
	}
      } break;
    case tgcvek:
#ifdef GCSTAT
      ngcvek++;
#endif
      size = ((Node *)sptr)->nodevek.size;
      sptr += 2;
      for(; size; size--) {
	dst = move((Node **)(sptr++),dst);
      }
      break;
    case tgcdvek:
#ifdef GCSTAT
      ngcbvek++;
#endif
      sptr += 2+((Node *)sptr)->nodevek.size;
      break;
    case tgcmkd:      gcerror("gcmkd in scan",(Int)sptr);      /* No return from gcerror */
    case tgcmvd:      gcerror("gcmvd in scan",(Int)sptr);      /* No return from gcerror */
    case tgcret:      gcerror("gcret in scan",(Int)sptr);        /* No return from gcerror */
    case tgcm:        gcerror("gcm in scan",(Int)sptr);          /* No return from gcerror */
    default:         fprintf(stderr,"Weird addr %lx tag = %x\n",(UInt)sptr,gctag);
      gcerror("Illegal tag in scan",(Int)sptr);  /* No return from gcerror */
    }
    if(once) break;
  }
  return dst;
}

Node *followlinks(dst)
     Node *dst;
{

  while(gclink != (GcLink *)-1 || funlink != (FunLink *)-1) {
    while(funlink != (FunLink *)-1) {               /* First check all function info tables */
      FunLink *tmp = funlink;
      int    nrefs;
      Node **refs;
      funlink = funlink->next;
      tmp->next = oldfunlink;
      oldfunlink = tmp;
      nrefs = tmp->nrefs;
      refs = tmp->refs;
      while(nrefs--)
	dst = move(refs++,dst);
    }
    while(gclink != (GcLink *)-1) {                /* Then scan all nodes outside the heap */
      GcLink *tmp = gclink;
      gclink = gclink->next;
      tmp->next = oldgclink;
      oldgclink = tmp;
      dst = scan(&tmp->node,dst,1);
    }
  }                                      /* Iterate until finished */
  return dst;
}

#ifndef SLIDE_GC
void clearlinks()
{

  while(oldfunlink != (FunLink *)-1) {
    FunLink *tmp = oldfunlink;
    oldfunlink = oldfunlink->next;
    tmp->next = 0;
  }
  while(oldgclink != (GcLink *)-1) {
    GcLink *tmp = oldgclink;
    oldgclink = oldgclink->next;
    tmp->next = 0;
  }
}
    
#endif



#define PATCH(p,s) if(INSIDEHEAP((PTR)p)) p = (Node *)(((VPTR)p)-s)
#define SLIDE(f,s) (void *)(INSIDEHEAP((PTR)*f) ? ((VPTR)*f)-s : (VPTR)*f)

void recslide(sptr,slide)
     Node *sptr; 
     int slide;
{
  int size;
  Node **isptr = (Node **)sptr;

  int gctag = sptr->tag->gc;
#ifdef EXTRA_DEBUG
  if(chkaddr == (int **)sptr)
    fprintf(stderr, "recslide chkaddr from %lx\n", (UInt)chkaddr);
#endif
  switch(gctag) {
  case tgc20:
    break;
  case tgcbig:
#ifdef EXTRA_DEBUG
    if(sptr->node11.ptr0->tag != &DVEK)
      fprintf(stderr, "Bignum (recslide) pointing at %lx", (UInt)sptr->node11.ptr0->tag);
#endif
  case tgc11:
    PATCH(sptr->node11.ptr0,slide);
    break;
  case tgcinp:    case tgcind:   case tgchole:
/*    addupdated(sptr); */
  case tgc30:
    break;
  case tgcap:    case tgcapG:
    PATCH(sptr->node12.ptr0,slide);
    PATCH(sptr->node12.ptr1,slide);
    break;
  case tgc12:
    PATCH(sptr->node12.ptr0,slide);
    PATCH(sptr->node12.ptr1,slide);
    break;
  case tgc21:
    PATCH(sptr->node21.ptr0,slide);
    break;
  case tgczap:
    { FunLink *tmp = &(sptr->nodezap.fun->link);
      int    nrefs = tmp->nrefs;
      Node **refs = tmp->refs;
      Node *r;
      while(nrefs--) {
	r = *refs++;
	PATCH(r,slide);
      }
    }
    break;
  case tgcfun:
    { FunLink *tmp = &(sptr->nodefun.fun->link);
      int    nrefs = tmp->nrefs;
      Node **refs = tmp->refs;
      Node *r;
      while(nrefs--) {
	r = *refs++;
	PATCH(r,slide);
      }
    }
    break;
  case tgcvap:     case tgcvapG:
    { FunInfo *info = sptr->nodevap.fun;
      { FunLink *tmp = &(info->link);
	int    nrefs = tmp->nrefs;
	Node **refs = tmp->refs;
	Node *r;
	while(nrefs--) {
	  r = *refs++;
	  PATCH(r,slide);
	}
      }
      isptr += 2;
      size = info->arity;
      for(; size; size--,isptr++)
	PATCH(*isptr,slide);
    } break;
  case tgcvek:
    size = sptr->nodevek.size;
    isptr += 2;
    for(; size; size--,isptr++) {
      PATCH(*isptr,slide);
    }
    break;
  case tgcdvek:
    break;
  case tgcmkd:      gcerror("gcmkd in recslide",(Int)sptr); /* No return from gcerror */
  case tgcmvd:      gcerror("gcmvd in recslide",(Int)sptr); /* No return from gcerror */
  case tgcret:      gcerror("gcret in recslide",(Int)sptr); /* No return from gcerror */
  case tgcm:        gcerror("gcm in recslide",(Int)sptr);	/* No return from gcerror */
  default:
    gcerror("Illegal tag in recslide",(Int)sptr); /* No return from gcerror */
  }
}

void patchUpdated(slide,ep)
     int slide;
     PTR ep;
{
  VPTR Slide;
  Tag *tag;
  for(Slide = endtable-1; Slide > tp;) {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)*Slide)
      fprintf(stderr, "patchUpdated from chkaddr  %lx\n", (UInt)chkaddr);
    if(chkaddr == -slide+(int **)*Slide)
      fprintf(stderr, "patchUpdated to chkaddr  %lx\n", (UInt)chkaddr);
#endif
    recslide(*Slide,slide);   /* Do not want add updated */
    if(*Slide != (void *)ep && !INSIDEHEAP((PTR)*Slide)) {
      tag = ((PTR)*Slide)->tag;
      if(tag == &AP || tag == &APG || tag == &VAP || tag == &VAPG || /* Keep pointer if the node is updateable */
	 tag == &HOLE || tag == &ZAP )  {   /* This is not necessary according to LA, but who trusts him? */
	Slide--;
      } else
        *Slide = *tp++;
    } else
      *Slide = *tp++;
  }
}



Node *heapslide(to,from,end,slide)
     VPTR to; 
     VPTR from; 
     VPTR end; 
     int slide;
{
  int size;

  while(from != end) {
    int gctag = ((Node *)from)->tag->gc;
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)from)
      fprintf(stderr, "heapslide chkaddr from %lx\n", (UInt)chkaddr);
    if(chkaddr == (int **)to)
      fprintf(stderr, "heapslide chkaddr to %lx\n", (UInt)chkaddr);
#endif
    switch(gctag) {
    case tgczap:
      { FunLink *tmp = &(((Node *)from)->nodezap.fun->link);
	int    nrefs = tmp->nrefs;
	Node **refs = tmp->refs;
	Node *r;
	addupdated((Node *)to);
	*to++ = *from++;
	*to++ = *from++;
	to++;
	from++;
	while(nrefs--) {
	  r = *refs++;
	  PATCH(r,slide);
	}
      }
      break;
    case tgcinp:    case tgcind:
    case tgchole:
      addupdated((Node *)to);
    case tgc30:
      *to++ = *from++;
    case tgc20: case tgcint: case tgcchr: case tgctag0:
      *to++ = *from++;
      *to++ = *from++;
      break;
    case tgcbig:
    case tgc11:
      *to++ = *from++;
      *to++ = SLIDE(from,slide);      from++;
      break;
    case tgc12:    case tgcapG:    case tgcap:
      *to++ = *from++;
      *to++ = SLIDE(from,slide);      from++;
      *to++ = SLIDE(from,slide);      from++;
      break;
    case tgc21:
      *to++ = *from++;
      *to++ = *from++;
      *to++ = SLIDE(from,slide);      from++;
      break;
    case tgcvap:
    case tgcvapG:
      { FunInfo *info = ((Node *)from)->nodevap.fun;
	{ FunLink *tmp = &(info->link);
	  int    nrefs = tmp->nrefs;
	  Node **refs = tmp->refs;
	  Node *r;
	  while(nrefs--) {
	    r = *refs++;
	    PATCH(r,slide);
	  }
	}
	*to++ = *from++;
	*to++ = *from++;
	size = info->arity;
	for(; size; size--) {
	  *to++ = SLIDE(from,slide);      from++;
	}
      } break;
    case tgcvek:
      size = ((Node *)from)->nodevek.size;
      *to++ = *from++;
      *to++ = *from++;
      for(; size; size--) {
	*to++ = SLIDE(from,slide);      from++;
      }
      break;
    case tgcdvek:
      size = ((Node *)from)->nodevek.size;
      *to++ = *from++;
      *to++ = *from++;
      for(; size; size--) {
	*to++ = *from++;
      }
      break;
    case tgcmkd:      gcerror("gcmkd in slide",(Int)from);      /* No return from gcerror */
    case tgcmvd:      fprintf(stderr,"Moved %s  (%08x)\n",
			      tagname(((Node *)from)->nodemvd.new),(Int)(((Node *)from)->nodemvd.new));
		      gcerror("gcmvd in slide",(Int)from);      /* No return from gcerror */
    case tgcret:      gcerror("gcret in slide",(Int)from);        /* No return from gcerror */
    case tgcm:        gcerror("gcm in slide",(Int)from);          /* No return from gcerror */
    case tgcfun:
      gcerror("fun node in heapslide",(Int)from);  /* No return from gcerror */
    default:
      fprintf(stderr, "Heapslide doesn't know about %lx %s\n", (UInt)((PTR)from)->tag,tagname((PTR)from));
      gcerror("Illegal tag in heapslide",(Int)from);  /* No return from gcerror */
    }
  }
  return (Node *)to;
}
#if 0
void patchlinks(slide)
int slide;
{
  FunLink *ftmp;
  GcLink *gtmp;

  for(ftmp=oldfunlink; ftmp != (FunLink *)-1; ftmp = ftmp->next) {
    int    nrefs;
    Node **refs;
    nrefs = ftmp->nrefs;
    refs = ftmp->refs;
    for(; nrefs--; refs++)
      if(INSIDEHEAP(*refs))
	*refs -= slide;
  }
  for(gtmp=oldgclink; gtmp != (GcLink *)-1; gtmp = gtmp->next) {
    recslide(&gtmp->node,slide);
  }
}
#endif


#if 0
Node *do_baker(sep,dptr,sptr,major)
PTR *sep;
Node *dptr;
Node *sptr;
int major;
{
#ifdef GCSTAT
  if (Sflag>1){
    fprintf(stderr,"do_baker ");
  }
#endif /* GCSTAT */
  if(!major) {
    int dummy;
#ifdef GCSTAT
  if (Sflag>1){
    fprintf(stderr,"minor\n");
  }
#endif /* GCSTAT */
    if(hp != (VPTR)dptr)
      fprintf(stderr,"do_baker hp = %lx dptr = %lx\n",(UInt)hp,(UInt)dptr);
    hp = (VPTR)dptr;
    genc(sep,&dummy);
    if(funlink != (FunLink *)-1)
      fprintf(stderr,"do_baker funlink  = %lx \n",(UInt)funlink);
    if(gclink != (GcLink *)-1)
      fprintf(stderr,"do_baker gclink  = %lx \n",(UInt)gclink);
    return (Node *)hp;
  } else {
    int dummy;
#ifdef GCSTAT
  if (Sflag>1){
    fprintf(stderr,"major\n");
  }
#endif /* GCSTAT */
    if(hp != (VPTR)dptr)
      fprintf(stderr,"do_baker hp = %lx dptr = %lx\n",(UInt)hp,(UInt)dptr);
    hp = (VPTR)dptr;
    WriteUpdated = Updated;
    majorc(sep,&dummy);
    if(funlink != (FunLink *)-1)
      fprintf(stderr,"do_baker funlink  = %lx \n",(UInt)funlink);
    if(gclink != (GcLink *)-1)
      fprintf(stderr,"do_baker gclink  = %lx \n",(UInt)gclink);
    return (Node *)hp;
    return dptr;
  }
}
#endif

void pr(msg)
char *msg;
{
  if(Gflag && Gno <= 0){
    fprintf(stderr,"%s; ",msg);
    fflush(stderr);
  }
}

void prd(msg)
int msg;
{
  if(Gflag && Gno <= 0){
   fprintf(stderr," 0x%04x;",msg);
   fflush(stderr);
  }
}

void prdump(msg)
PTR msg;
{
  if(Gflag && Gno <= 0){
   dumpgraph(msg,1);
   fprintf(stderr,"\n");
  }
}

void prF(msg)
char *msg;
{
  fprintf(stderr,"%s; ",msg);
  fflush(stderr);
}

void prdF(msg)
int msg;
{
  fprintf(stderr," 0x%04x;",msg);
  fflush(stderr);
}

void prdumpF(msg)
PTR msg;
{
  dumpgraph(msg,1);
  fprintf(stderr,"\n");
}

#if 0   /* A nice piece of code that isn't used any more */
    dptr = scan((Node **)sep,dptr,1);
    do {
      dptr = followlinks(dptr);
      sptr = dptr = scan(sptr,dptr,0);
    } while(funlink != (FunLink *)-1 || gclink != (GcLink *)-1);
#endif
