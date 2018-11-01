#define USETABELS

/* #define POINTERREVERSAL */

#ifndef POINTERREVERSAL
#ifndef RECURSION
#define RECURSION
#endif
#endif

#define EXTRA_DEBUG

#include "runtime.h"
#include "vars.h"
#include "gc.h"
#include "gcd.h"
#include "../mcode/limit.h"

static void recmark PROTO((Node *));
static void mark PROTO((Node **));
static void marklinks PROTO((void));
static void clearlinks PROTO((void));
static void flip PROTO((Node **));

extern Tag MOVED;  /* defined in runtime.M */
extern Tag MARKED;
extern Tag GCRET;
extern Tag TAGFIRST,TAGLAST;

void gcerror PROTO((char *c,int ));
void setdirref PROTO((FILE *f));
void setfileref PROTO((FILE *f));

extern GcLink *gclink;
extern GcLink *oldgclink;
extern FunLink *funlink;
extern FunLink *oldfunlink;

char *tagname();

Node *heap_low;
Node *heap_high;

#define INSIDEHEAP(n) ((n)>=heap_low && (n)<=heap_high) 
#define isinder(tag)  (tag>=&TAGLAST || tag<&TAGFIRST)


int   BitWords;
extern int  *BitHeap;

typedef union LIST {
  Tag        *tag;
  union LIST *next;
  Node       *new;
} List;

#define BitPerWord   32
#define LnBitPerWord  5
#define BitMask      31

#if 1
extern int getgc PROTO((Node *));
extern int nodesize PROTO((Node *));
#else
static int getgc(node)
     Node *node;
{
  while(isinder(node->tag)) { /* This is not the tag */
    node = (Node *)node->tag;
  }
  return node->tag->gc;
}

static int nodesize(node)
     Node *node;
{
  switch(node->tag->gc) { /*   switch(getgc(node)) { */
  case tgczap: case tgcind: case tgcinp: case tgc30:  case tgc12:  case tgc21:
  case tgchole: case tgcap: case tgcapG: /* case tgccap == tgc12 */
    return 3;
  case tgc20:   case tgc11: case tgcbig: case tgcint: case tgcchr: case tgctag0:
    return 2;
  case tgcdvek:  case tgcvek:  case tgcuvek:
    return 2+node->nodevek.size;
  case tgcfun:
    return 2;
  case tgcvap:  case tgcvapG:
    return 2+node->nodevap.fun->arity;
  default:
    fprintf(stderr,"Nodesize don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
    gcerror("Illegal tag in nodesize (1)",0); /* No return from gcerror */
  }
  return 0; /* This line is never reached */
}
#endif
static int marknode(node) /* set bit and return 1 if node was unmarked */
     Node *node;
{
  int offset = (int **)node-(int**)heap_low;
  int mask = 1<<(offset&BitMask);

  offset >>= LnBitPerWord;
  if(BitHeap[offset] & mask)
    return 0;
  else
    BitHeap[offset] |= mask;
  return 1;
}

#if defined(POINTERREVERSAL) || defined(USETABELS)

static void unmarknode(node) /* remove markbit */
Node *node;
{
  int offset = (int **)node-(int**)heap_low;
  int mask = 1<<(offset&BitMask);

  offset >>= LnBitPerWord;
  BitHeap[offset] &= ~mask;
}
#endif


static void linkfun(fun)
     FunInfo *fun;
{
  if(!fun->link.next) {
    fun->link.next = funlink;
    funlink = &fun->link;
  }
}

#define SETCOUNTER(t,n) t->tag = (Tag *)((n<<24) | (0xffffff&(int)t->tag))
#define GETCOUNTER(t) ((((int)t->tag)>>24)&0xff)
#define DELCOUNTER(t) t->tag = (Tag *)(0x00ffffff & (int)t->tag)
#define GETTAG(n) ((Tag *)(0x00ffffff & (int)n->tag))

static void recmark(node)  /* follow all pointers in a marked node */
     Node *node;
{
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "Recmark on chkaddr %x %s\n", (unsigned int)chkaddr,tagname(node));
#endif

  switch(node->tag->gc) {
  case tgcind: /* set dirref */
    setdirref(node->nodeinp.file);
    break;
  case tgcinp: /* set fileref and fall into nodes of size 3 */
    setfileref(node->nodeinp.file);
  case tgc20:      case tgc30:      case tgcdvek: case tgchole: case tgcint: case tgcchr: case tgctag0:
    break;       /* No pointers */
  case tgc11: case tgcbig:
    mark(&node->node11.ptr0);
    break;
  case tgcap: case tgcapG: /* case tgccap: == tgc12 */
  case tgc12:
    mark(&node->node12.ptr0);
    mark(&node->node12.ptr1);
    break;
  case tgc21:
    mark(&node->node21.ptr0);
    break;
  case tgcvek:
  case tgcuvek:
    { int i = node->nodevek.size;
      for(; i--; ) {                    /* Mark "backwards" but who cares? */
	mark(&node->nodevek.ptr[i]);
      }
    }
    break;
  case tgczap:
    linkfun(node->nodezap.fun);
    break;
  case tgcfun:
#if 0     /* Possible if used as update outside of heap? */
    if(node != node->nodefun.fun->node) {
      fprintf(stderr,"Recmark gcfun differs\n");
    }
#endif
    linkfun(node->nodefun.fun);
    break;
  case tgcvap:   case tgcvapG:
    linkfun(node->nodevap.fun);
    { int i = node->nodevap.fun->arity;
      for(; i--; )                     /* Mark "backwards" but who cares? */
	mark(&node->nodevap.ptr[i]);
    }
    break;
  default:
    fprintf(stderr,"Recmark don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
    gcerror("Illegal tag in recmark (1)",(Int)node); /* No return from gcerror */
  }
}

#ifdef POINTERREVERSAL

Node11 stop = {&GCRET,0};
 
static void mark(inode)                   /* This is the pointer reversal version */
     Node **inode;
{
  Node *node = *inode;
  Node *prev = (Node *)&stop;
  Node *curr = node;
  Node *next,*back;
  Node **tmp = &stop.ptr0;
  int   numptr = 0;
  int   maxptr = 0;
  int tnumptr = 0;
  stop.ptr0 = (Node *)&stop;
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)*inode)
      fprintf(stderr, "Mark starts with %x %s\n", (unsigned int)chkaddr,tagname(*inode));
#endif
  do {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)curr)
      fprintf(stderr, "Mark found %x %s\n", (unsigned int)chkaddr,tagname((PTR)curr));
#endif
    if(INSIDEHEAP(curr)) {
      if(marknode(curr)) {
/*************** readfirst begin *************************/
	if(0xff000000 & (int)curr->tag) {       /* DEBUG */
	  gcerror("tag address is to large in readfirst.",0);
	}
	switch(curr->tag->gc) {
        case tgcind: /* set dirref */
          setdirref(curr->nodeinp.file);
          goto next_ptr; /* No pointers */
	case tgcinp: /* set fileref and fall into nodes of size 3 */
	  setfileref(curr->nodeinp.file);
	case tgc20:      case tgc30:   case tgcdvek: case tgchole:
	  goto next_ptr; /* No pointers */
	case tgcint:
#ifdef USETABELS
	  if(curr->node20.val0 >= MININTINT && curr->node20.val0 <= MAXINTINT) {
	    extern int *cinttab[];
	    unmarknode(curr);
#ifdef HPROFILE	    
	    curr = (Node *)&cinttab[4*curr->node20.val0];
#else
	    curr = (Node *)&cinttab[2*curr->node20.val0];
#endif
	  }
#endif
	  goto next_ptr;
	case tgcchr:
#ifdef USETABELS
	  if(curr->node20.val0 >= MINCHAR && curr->node20.val0 <= MAXCHAR) {
	    extern int *cchartab[];
	    unmarknode(curr);
#ifdef HPROFILE	    
	    curr = (Node *)&cchartab[4*curr->node20.val0];
#else
	    curr = (Node *)&cchartab[2*curr->node20.val0];
#endif
	  }
#endif
	  goto next_ptr;
	case tgctag0:
#ifdef USETABELS
	  if(curr->node20.val0 >= MINTAG0 && curr->node20.val0 <= MAXTAG0) {
	    extern int *ctag0tab[];
	    unmarknode(curr);
#ifdef HPROFILE	    
	    curr = (Node *)&ctag0tab[4*curr->node20.val0];
#else
	    curr = (Node *)&ctag0tab[2*curr->node20.val0];
#endif
	  }
#endif
	  goto next_ptr;
        case tgczap:
          linkfun(curr->nodezap.fun);
          goto next_ptr;
	case tgcfun:
	  if(curr != curr->nodefun.fun->node) {
	    unmarknode(curr);
	    curr = curr->nodefun.fun->node;
	  }
	  linkfun(curr->nodefun.fun);
	  goto next_ptr;
	case tgc11: case tgcbig:
	  tnumptr = maxptr = 0;
	  break;
	case tgc12: case tgcap: case tgcapG: /* case tgccap: == tgc12 */
	  tnumptr = 0;
	  maxptr = 1;
	  break;
	case tgc21:
	  tnumptr = maxptr = 1;
	  break;
	case tgcvap:	case tgcvapG:
	  linkfun(curr->nodevap.fun);
	  tnumptr = curr->nodevap.fun->arity;
	  goto first_gcvek;
	case tgcvek:	case tgcuvek:
	  tnumptr = curr->nodevek.size;
  first_gcvek:
	  if(tnumptr==0)
	    goto next_ptr; /* No pointers */
	  if(tnumptr>255) {
	    gcerror("Node too big! (>255)!",0);
	  }
	  maxptr = tnumptr;
	  tnumptr = 1;
	  break;
        default:
	  fprintf(stderr,"Readfirst(inlined mark 1) don't know how to handle %s (%lx)\n",tagname((PTR)curr),(UInt)curr->tag);
	  gcerror("Illegal tag in readfirst(inlined)",0); /* No return from gcerror */
	}
/******************** readfirst end ***************************************/
	SETCOUNTER(prev,numptr);
	numptr = tnumptr;
	tmp = &curr->nodemark.ptr[numptr];
	next = *tmp;
	*tmp = prev;
	prev = curr;
	curr = next;
      } else {     /* if(marknode(c....  Node is marked so get next pointer in prev */
	goto next_ptr;
      }
    } else {       /* if(INSIDEHEAP(c... Node is outside the heap.  Put it on a list if it */ 
      switch(curr->tag->gc) {       /* contains pointers and get next pointer in previous. */
      case tgcind: /* set bit and fall into nodes of size 3 */
        setdirref(curr->nodeinp.file);
        break;
      case tgcinp: /* set bit and fall into nodes of size 3 */
	setfileref(curr->nodeinp.file);
      case tgc20:      case tgc30:  case tgcdvek: case tgchole:
      case tgcint:     case tgcchr:  case tgctag0:   /* No need to compact outside heap */
	break;       /* No pointers */
	
      case tgcvap:      case tgcvapG:
	linkfun(curr->nodevap.fun);         /* Falltrough */
      case tgc11:   case tgcbig:  case tgc12:    case tgc21:     case tgcvek:
      case tgcuvek: case tgcap: case tgcapG: /* case tgccap: = tgc12 */
	if(!GCLINK(curr)->next) {       /* Pointers */
	  GcLink *tmp = GCLINK(curr);
	  tmp->next = gclink;
	  gclink = tmp;
	}
	break;
      case tgczap:
        linkfun(curr->nodezap.fun);
        break;
      case tgcfun:
	if(curr != curr->nodefun.fun->node) {
	  unmarknode(curr);
	  curr = curr->nodefun.fun->node;
	}
	linkfun(curr->nodefun.fun);
	break;
      default:
	  fprintf(stderr,"Readfirst(inlined mark 2) don't know how to handle %s (%lx)\n",tagname((PTR)curr),(UInt)curr->tag);
	gcerror("Illegal tag in mark (1)",(Int)prev); /* No return from gcerror */
      }   /* Fix pointers later */
/******************** readfirst jumps here if no pointers in curr *******/
   next_ptr:
      back = *tmp;           /* Restore pointer */
      *tmp = curr;
      if(numptr++<maxptr) { /* More pointers ? (Fast test if readnext needed) */
	curr = *++tmp;
	*tmp = back;       /* There is never a hole betweeen pointers */
      } else {               /* Follow next pointer in prev */
/******************** readnext begin ***************************************/
	do {                     /* No more pointers (or outside heap) so back up */
	  SETCOUNTER(prev,0);              /* Clear counter */
	  switch(GETTAG(back)->gc) {
	  case tgcret:
	    if(prev == (Node *)&stop) {
	      if(*inode != curr) {
/*		fprintf(stderr,"Mark fixing pointer1\n"); */
		*inode = curr;
	      }
	    } else {
	      if(*inode != prev) {
		fprintf(stderr,"Mark fixing pointer2\n");
		*inode = prev;
	      }
	    }
	    return;                          /* the only way out !! */
	  case tgc11:  case tgcbig:
	    maxptr = 0;
	    break;
	  case tgc21:
	    maxptr = 1;
	    break;
	  case tgc12:	  case tgcap:	  case tgcapG: /* case tgccap: = tgc12 */
	    maxptr = 1;
	    break;
	  case tgcvap:	  case tgcvapG:
	    maxptr = back->nodevap.fun->arity;
	    break;
	  case tgcvek:	  case tgcuvek:
	    maxptr = back->nodevek.size;
	    break;
	  default:
	    fprintf(stderr,"Rednext(inlined) don't know how to handle (%lx)\n",(UInt)GETTAG(back));
	    gcerror("Illegal tag in readnext (inlined)",0); /* No return from gcerror */
	  }
	  curr = prev;
	  prev = back;
	  numptr = GETCOUNTER(prev);
	  if(numptr>maxptr)
	    gcerror("numptr to large",(Int)prev);
	  tmp = &prev->nodemark.ptr[numptr];
	  back = *tmp; 
	  *tmp = curr;
	} while(numptr == maxptr); 
	curr = *++tmp;                      /* New node to visit */
	*tmp = back;                        /* Save backpointer */
	numptr++;
      } /* if(numptr++<maxptr) ... else ... */
    }  /* if(INSIDEHEAP ... else .... */
  } while (curr != (Node *)&stop);
  gcerror("This should not be possible",0); /* No return from gcerror */
}
#endif /* POINTERREVERSAL */

#ifdef  RECURSION
static void mark(inode)                   /* Depth first using recursion */
     Node **inode;
{
  Node *node = *inode;
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "Mark on %x %s\n", (unsigned int)chkaddr,tagname(node));
#endif

  if(INSIDEHEAP(node)) {
    if(marknode(node)) {
      switch(node->tag->gc) {
      case tgcind: /* set dirref */
	setdirref(node->nodeinp.file);
	break;
      case tgcinp: /* set fileref and fall into nodes of size 3 */
	setfileref(node->nodeinp.file);
      case tgc20:      case tgc30:      case tgcdvek: case tgchole:
	break;
      case tgcint:
#ifdef USETABELS
	if(node->node20.val0 >= MININTINT && node->node20.val0 <= MAXINTINT) {
	  extern int *cinttab[];
	  unmarknode(node);
#ifdef HPROFILE	    
	  *inode = (Node *)&cinttab[4*node->node20.val0];
#else
	  *inode = (Node *)&cinttab[2*node->node20.val0];
#endif
	}
#endif
	break;
      case tgcchr:
#ifdef USETABELS
	if(node->node20.val0 >= MINCHAR && node->node20.val0 <= MAXCHAR) {
	  extern int *cchartab[];
	  unmarknode(node);
#ifdef HPROFILE	    
	  *inode = (Node *)&cchartab[4*node->node20.val0];
#else
	  *inode = (Node *)&cchartab[2*node->node20.val0];
#endif
	}
#endif
	break;
      case tgctag0:
#ifdef USETABELS
	if(node->node20.val0 >= MINTAG0 && node->node20.val0 <= MAXTAG0) {
	  extern int *ctag0tab[];
	  unmarknode(node);
#ifdef HPROFILE	    
	  *inode = (Node *)&ctag0tab[4*node->node20.val0];
#else
	  *inode = (Node *)&ctag0tab[2*node->node20.val0];
#endif
	}
#endif
	break;
      case tgc11: case tgcbig:
	mark(&node->node11.ptr0);
	break;
      case tgcap: case tgcapG: /* case tgccap: == tgc12 */
      case tgc12:
	mark(&node->node12.ptr0);
	mark(&node->node12.ptr1);
	break;
      case tgc21:
	mark(&node->node21.ptr0);
	break;
      case tgcvek:
      case tgcuvek:
	{ int i = node->nodevek.size;
	  for(; i--; ) {                    /* Mark "backwards" but who cares? */
	    mark(&node->nodevek.ptr[i]);
	  }
	}
	break;
      case tgczap:
	linkfun(node->nodezap.fun);
	break;
      case tgcfun:
	linkfun(node->nodefun.fun);
	break;
      case tgcvap:   case tgcvapG:
	linkfun(node->nodevap.fun);
	{ int i = node->nodevap.fun->arity;
	  for(; i--; )                     /* Mark "backwards" but who cares? */
	    mark(&node->nodevap.ptr[i]);
	}
	break;
      default:
	fprintf(stderr,"Mark(recmark inlined) don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
	gcerror("Illegal tag in recmark (1)",(Int)node); /* No return from gcerror */
      }
    }
  } else { /* OUTSIDEHEAP */
    switch(node->tag->gc) {
    case tgcind: /* set dirref */
      setdirref(node->nodeinp.file);
      break;
    case tgcinp: /* set bit and fall into nodes of size 3 */
      setfileref(node->nodeinp.file);
    case tgc20:      case tgc30:      case tgcdvek: case tgchole:
      case tgcint:     case tgcchr:  case tgctag0:   /* No need to compact outside heap */
      break;       /* No pointers */

    case tgcvap: case tgcvapG:
      linkfun(node->nodevap.fun);         /* Falltrough */
    case tgc11:    case tgc12:    case tgc21:     case tgcvek:     case tgcuvek:
    case tgcbig:  case tgcap:    case tgcapG:  /* case tgccap: == tgc12 */
      if(!GCLINK(node)->next) {       /* Pointers */
	GcLink *tmp = GCLINK(node);
	tmp->next = gclink;
	gclink = tmp;
      }
      break;
    case tgczap:
      linkfun(node->nodezap.fun);
      break;
    case tgcfun:
      linkfun(node->nodefun.fun);
      break;
    default:
      fprintf(stderr,"Mark don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
      gcerror("Illegal tag in mark (2)",(Int)node); /* No return from gcerror */
    }
  }
}
#endif /* RECURSION */

static void marklinks()
{
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"marklinks begin\n");
#endif /* GCSTAT */
  while(gclink != (GcLink *)-1 || funlink != (FunLink *)-1) {
#if GCSTAT
    if (Sflag>1)
      fprintf(stderr,"funlink %x\n",(int)funlink);
#endif /* GCSTAT */
    while(funlink != (FunLink *)-1) {               /* First check all function info tables */
      FunLink *tmp = funlink;
      int    nrefs;
      Node **refs;
      funlink = funlink->next;
      tmp->next = oldfunlink;
      oldfunlink = tmp;
      nrefs = tmp->nrefs;
      refs = tmp->refs;
      while(nrefs--) {
	mark(refs++);
      }
    }
#if GCSTAT
    if (Sflag>1) {
      fprintf(stderr,"gclink %x (%x)\n",(int)gclink,(int)gclink->next);
    }
#endif /* GCSTAT */
    while(gclink != (GcLink *)-1) {                /* Then scan all nodes outside the heap */
      GcLink *tmp = gclink;
      gclink = gclink->next;
      tmp->next = oldgclink;
      oldgclink = tmp;
      recmark(&tmp->node);
    }
  }                                      /* Iterate until finished */
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"marklinks end\n");
#endif /* GCSTAT */
}


static void clearlinks()
{
  FunLink *ftmp;
  GcLink *gtmp;
    
  while(oldfunlink != (FunLink *)-1) {
    ftmp = oldfunlink;
    oldfunlink = oldfunlink->next;
    ftmp->next = 0;
  }
  while(oldgclink != (GcLink *)-1) {
    gtmp = oldgclink;
    oldgclink = oldgclink->next;
    gtmp->next = 0;
  }
}

static void flip(inode)
     Node **inode;
{
  Tag *tag;
  Node *node = *inode;
  if(INSIDEHEAP(node)) {
    tag = node->tag;
    node->tag = (Tag *)inode;
    *inode = (Node *)tag;
  }   /* Ignore nodes outside heap they are already on the oldgclink */
}

static int recflip(node)  /* flip all pointers in a marked node, returns size of node */
     Node *node;
{
  switch(getgc(node)) {
    /***************************************** No pointers */
  case tgcind: case tgcinp: case tgc30: case tgczap: case tgchole:
    return 3;

  case tgc20:   case tgcfun:
  case tgcint:     case tgcchr:  case tgctag0:
    return 2;
  case tgcdvek:
    return 2+node->nodedvek.size;
    /***************************************** Pointers */
  case tgc11:  case tgcbig:
    flip(&node->node11.ptr0);
    return 2;
  case tgc12: case tgcap: case tgcapG: /* case tgccap: == tgc12 */
    flip(&node->node12.ptr0);
    flip(&node->node12.ptr1);
    return 3;
  case tgc21:
    flip(&node->node21.ptr0);
    return 3;
  case tgcvek:  case tgcuvek:
    { int i = node->nodevek.size;
      for(; i--; )                     /* Flip "backwards" but how care? */
	flip(&node->nodevek.ptr[i]);
    }
    return 2+node->nodevek.size;
  case tgcvap:   case tgcvapG:
    { int i = node->nodevap.fun->arity;
      for(; i--; )                     /* Flip "backwards" but how care? */
	flip(&node->nodevap.ptr[i]);
    }
    return 2+node->nodevap.fun->arity;
  default:
    fprintf(stderr,"recflip don't know how to handle %s (%x)\n",tagname((PTR)node),(int)node->tag);
    gcerror("Illegal tag in flip (1)",0); /* No return from gcerror */
  }
  return 0; /* Never executed */
}
  
static void fliplinks()
{
  GcLink *tmp;
  for(tmp = oldgclink; tmp != (GcLink *)-1; tmp = tmp->next) {
    (void)recflip(&tmp->node);
  }
}

static void flipheap()
{
  Node *node = heap_low;
  Node *new = heap_low;
  for(;;) {                  /* loop exit done by return */
/********** findnext begin **********/
    {
      int wptr;
      register unsigned int bmask;
      register unsigned int bcash;
      int tmp = (int **)node - (int **)heap_low;   /* Next node offset */
      
      if((wptr = tmp>>LnBitPerWord) <BitWords) {
	bmask = 1<<(tmp&BitMask);
	if(bmask&(bcash = BitHeap[wptr])) {  /* No hole ! */
	  BitHeap[wptr] = bcash ^ bmask;
	  node = (Node *)(tmp + (int *)heap_low); /**** return ****/
	  goto nextfound;
	} else {
	  Node *hole = node;
	  while(wptr<BitWords) {
	    while(bmask && !(bmask&bcash)) {
	      bmask <<= 1;
	      bmask &= 0xffffffff;
	      tmp++;
	    }
	    if(bmask) {
	      node = (Node *)(tmp + (int *)heap_low);
	      if(((int *)node)-((int *)hole) == 1) {
		hole->nodemkd.tag =  &MARKED;
	      } else {
		hole->noderet.tag =  &GCRET;
		hole->noderet.next = node;
	      }
	      BitHeap[wptr] = bcash^bmask;
	      goto nextfound; /**** return ****/
	    } else {
	      for(wptr++; wptr<BitWords; wptr++,tmp+=BitPerWord) {
		if(bcash = BitHeap[wptr]) {
		  bmask = 1;
		  break;
		}
	      }
	    }
	  }
	  node = (Node *)(tmp + (int *)heap_low);
	  if(((int )node)-((int)hole) == 1) {
	    hole->nodemkd.tag =  &MARKED;
	  } else {
	    hole->noderet.tag =  &GCRET;
	    hole->noderet.next = node;
	  }      
	  return;  /* End of heap with a hole */
	}
      } else 
	return;    /* End of heap without a hole */
    }
  nextfound:;
/************ findnext end ************/
/*****  recnew begin ******/
    if(isinder(node->tag)) { 
      List *tmp,*next;
      next = (List *)node->tag;
      do {
	tmp = next;
	next = tmp->next;
	tmp->new = new;
      } while(isinder((Tag *)next)); 
      node->tag = (Tag *)next;
    }
/***** recnew end ******/
    { int s = recflip(node);
      new = (Node *)(s+(Node **)new);
      node = (Node *)(s+(Node **)node);
    }
  }
}

#if 0
static Node *newheap()
{
  Node *node = heap_low;
  Node *new = heap_low;
  List *tmp,*next;
  int s;
  do{
    if(node->tag == &GCRET) {
      node = node->noderet.next;
      s = 0;
    } else {
      if(node->tag != &MARKED) {
/***** recnew begin ******/
	{
	  for(tmp = (List *)node; isinder(tmp->tag); tmp = next) {
	    next = tmp->next;
	    tmp->new = new;
	  }

	  if(node == (Node *)tmp) {
	    gcerror("newheap has found an unused node",(int)node);
	  }

	  node->tag = tmp->tag;
	  tmp->new = new;
	  new = (Node *)((s=nodesize(node))+(Node **)new);
	}
/***** recnew end ******/
      } else {
	s = 1;
      }      
    }
  } while((node = (Node *)(s+(int **)node))<heap_high);
  return new;  
}
#endif

static Node *moveheap()
{
  Node *node = heap_low;
  Node *new = heap_low;
  int s;
  do {
    if(node->tag == &GCRET) {
      s = 0;
      node = node->noderet.next;
    } else {
      if(node->tag != &MARKED) {
/*****  recnew begin ******/
	if(isinder(node->tag)) { 
	  List *tmp,*next;
	  next = (List *)node->tag;
	  do {
	    tmp = next;
	    next = tmp->next;
	    tmp->new = new;
	  } while(isinder((Tag *)next)); 
	  node->tag = (Tag *)next;
	}
/***** recnew end ******/
/****** recmove begin ******/
	s=nodesize(node);
	{
	  int i = s;
	  int *dst = (int *)new;
	  int *src = (int *)node;
	  while(i--)
	    *dst++ = *src++;
	  new =  (Node *)dst;
	}
/****** recmove end   ******/
      } else {
	s = 1;
      }      
    }
  } while((node = (Node *)(s+(int **)node))<heap_high);
  return new;
}
	

Node *restofgc()
{
  Node *new;
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"fliplinks\n");
#endif /* GCSTAT */
  fliplinks();
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"flipheap\n");
#endif /* GCSTAT */
  flipheap();
#if 0
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"newheap\n");
#endif /* GCSTAT */
  (void)newheap();                                  /* Flip back with new address */
#endif
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"moveheap\n");
#endif /* GCSTAT */
  new = moveheap();                          /* Move to new address */
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"clearlinks\n");
#endif /* GCSTAT */
  clearlinks();
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"restofgc finished new = %x\n",(int)new);
#endif /* GCSTAT */
  return new;
}
