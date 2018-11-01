
#include "runtime.h"
#include "vars.h"
#include "gc.h"
#include "gcd.h"
#include "../mcode/limit.h"

#ifndef POINTERREVERSAL
#ifndef RECURSION
#define RECURSION
#endif
#else
#ifdef RECURSION
#undef RECURSION
#endif
#endif

extern Tag MOVED;  /* defined in runtime.M */
extern Tag MARKED;
extern Tag GCRET;
extern Tag TAGFIRST,TAGLAST;

void gcerror PROTO((char *c,Int ));
void setdirref PROTO((FILE *f));
void setfileref PROTO((FILE *f));

void mark PROTO((Node **));

GcLink *gclink   = (GcLink *)-1;     /* Nodes outside heap */
GcLink *oldgclink  = (GcLink *)-1;
FunLink *funlink  = (FunLink *)-1;
FunLink *oldfunlink  = (FunLink *)-1;

char *tagname();

#define INSIDEHEAP(n) ((n)>=(Node *)startheap && (n)<=(Node *)endheap) 

extern int *BitHeap;

typedef union LIST {
  Tag        *tag;
  union LIST *next;
  Node       *new;
} List;

void clearTable()
{
  register VPTR p = starttable;
  while(p<endtable)
    *p++ = 0;
}


int getgc(node)
     Node *node;
{
  while(isinder(node->tag)) { /* This is not the tag */
    node = (Node *)node->tag;
  }
  return node->tag->gc;
}
  
int nodesize(node)
     Node *node;
{
  switch(node->tag->gc) { /*   switch(getgc(node)) { */
  case tgczap: case tgcind: case tgcinp: case tgc30:  case tgc12:  case tgc21: case tgcstr:
  case tgchole: case tgcap: case tgcapG: /* case tgccap == tgc12 */
  case tgcindi:
    return 3;
  case tgc20:   case tgc11: case tgcbig: case tgcint: case tgcchr: case tgctag0:
    return 2;
  case tgcdvek:  case tgcvek:
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

int marknode(node) /* set bit and return 1 if node was unmarked */
     Node *node;
{
  int offset = (int **)node-(int**)startheap;
  int mask = 1<<(offset&BitMask);
  int v;

  offset >>= LnBitPerWord;
/*  fprintf(stderr,"offset = %x,mask = %x ",offset,mask); */
  if((v=BitHeap[offset]) & mask)
    return 0;
  else
    BitHeap[offset] = v|mask;
  return 1;
}

#if defined(POINTERREVERSAL) || defined(USETABELS)

void unmarknode(node) /* remove markbit */
Node *node;
{
  int offset = (int **)node-(int**)startheap;
  int mask = 1<<(offset&BitMask);
  int v;

  offset >>= LnBitPerWord;
  v = BitHeap[offset];
  BitHeap[offset] &= ~mask;
}
#endif


void linkfun(fun)
     FunInfo *fun;
{
  if(!fun->link.next) {
    fun->link.next = funlink;
    funlink = &fun->link;
  }
}

void recmark(node)  /* follow all pointers in a marked node */
     Node *node;
{
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "Recmark on chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
#endif

  switch(node->tag->gc) {
  case tgcind: /* set dirref */
    setdirref(node->nodeinp.file);
    break;
  case tgcinp: /* set fileref and fall into nodes of size 3 */
    setfileref(node->nodeinp.file);
  case tgc20:      case tgc30:      case tgcdvek: case tgchole: case tgcint: case tgcchr: case tgctag0:
  case tgcstr:
    break;       /* No pointers */
  case tgc11: case tgcbig: case tgcindi:
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
    fprintf(stderr,"Recmark don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
    gcerror("Illegal tag in recmark (1)",(Int)node); /* No return from gcerror */
  }
}

#ifdef POINTERREVERSAL

int keepIndir = 0;

Node11 stop = {&GCRET,0};
 
void mark(inode)                   /* This is the pointer reversal version */
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
  Node *originode = *inode;
  extern int Gno, Gflag;

  stop.ptr0 = (Node *)&stop;

#if 1 /* EXTRA_DEBUG */
    {
      if(Gflag && Gno <= 0){
	fprintf(stderr, "mark %lx:%lx\n",(UInt)inode,(UInt)node);
      }
    }
#endif

  if((~TAGMASK) & ((Int)&TAGLAST | (Int)&TAGFIRST)) {       /* DEBUG */
    fprintf(stderr, "I can't do pointer reversal on this machine (%lx, %d)\n", ~TAGMASK, SIZEBITS);
    gcerror("tag address is to large in readfirst.",(Int)&TAGLAST | (Int)&TAGFIRST);
  }


#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)*inode)
      fprintf(stderr, "Mark starts with %lx %s\n", (UInt)chkaddr,tagname(*inode));
#endif
  do {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)curr)
      fprintf(stderr, "Mark found %lx %s\n", (UInt)chkaddr,tagname((PTR)curr));
#endif
  checkAgain:
#if 1 /* EXTRA_DEBUG */
    { 
      if(Gflag && Gno <= 0){
	if(GETCOUNTER(curr)) {
	  Tag *t = curr->tag;
	  DELCOUNTER(curr);              /* Clear counter */
	  dumpgraph(curr, 1);
	  curr->tag = t;
	} else {
	  dumpgraph(curr, 1);
	}
	fprintf(stderr, "\n");
      }
    }
#endif
    if(INSIDEHEAP(curr)) {
      if(marknode(curr)) {
/*************** readfirst begin *************************/
	switch(curr->tag->gc) {
	case tgcindi:
	  if(keepIndir) { 
	    tnumptr = maxptr = 0;
	    break;
	  } else { 
	    unmarknode(curr);
	    curr = curr->node11.ptr0;
	    goto checkAgain; 
	  }
        case tgcind: /* set dirref */
          setdirref(curr->nodeinp.file);
          goto next_ptr; /* No pointers */
	case tgcinp: /* set fileref and fall into nodes of size 3 */
	  setfileref(curr->nodeinp.file);
	case tgc20:      case tgc30:   case tgcdvek: case tgchole: case tgcstr:
	  goto next_ptr; /* No pointers */
	case tgcint:
#ifdef USETABELS
	  if(curr->node20.val0 >= MININTTAB && curr->node20.val0 <= MAXINTTAB) {
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
#ifdef EXTRA_DEBUG
	  if(chkaddr == (int **)curr) {
	    fprintf(stderr,"Vektor at %lx size %d\n",(UInt)chkaddr,curr->nodevek.size);
	  }
#endif
  first_gcvek:
	  if(tnumptr==0)
	    goto next_ptr; /* No pointers */
	  if(tnumptr>MAXSIZE) {
	    gcerror("Node too big! (>MAXSIZE)!",0);
	  }
	  maxptr = tnumptr;
	  tnumptr = 1;
	  break;
        default:
	  fprintf(stderr,"Readfirst(inlined mark 1) don't know how to handle %s (%lx,%lx)\n",tagname((PTR)curr),(UInt)curr,(UInt)curr->tag);
	  gcerror("Illegal tag in readfirst(inlined)",0); /* No return from gcerror */
	}
/******************** readfirst end ***************************************/
	SETCOUNTER(prev,numptr);
/*	fprintf(stderr,"SETCOUNTER %3d ",numptr); */
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
      case tgcindi:  /* Do something better perhaps ?? */
	if(!GCLINK(curr)->next) {       /* Pointers */
	  GcLink *tmp = GCLINK(curr);
	  tmp->next = gclink;
	  gclink = tmp;
	}
	break;
      case tgcvek:
#ifdef EXTRA_DEBUG
	if(chkaddr == (int **)curr) {
	  fprintf(stderr,"Vektor at %lx size %d\n",(UInt)chkaddr,curr->nodevek.size);
	}
#endif
      case tgc11:   case tgcbig:  case tgc12:    case tgc21:
      case tgcap: case tgcapG: /* case tgccap: = tgc12 */
      case tgcstr:  /* No pointers but needs to be remembered if generational gc */
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
	  case tgcret:       /* It would be nice if I could understand this !! */
	    if(keepIndir)
	      *inode = originode;
	    else {
	      if(prev == (Node *)&stop) {
		if(*inode != curr) {
/*		  fprintf(stderr,"Mark fixing pointer1\n"); */
		  *inode = curr;
		}
	      } else {
		if(*inode != prev) {      /* Indir nodes makes this happens ???? */
		  /*		  fprintf(stderr,"Mark fixing pointer2\n"); */
		  *inode = prev;
		}
	      }
	    }
	    return;                          /* the only way out !! */
	  case tgc11:  case tgcbig: case tgcindi:
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
	    fprintf(stderr,"Readnext(gcslide inlined) don't know how to handle (%lx:%lx)\n",(UInt)GETTAG(back),(UInt)(GETTAG(back)->gc));
	    gcerror("Illegal tag in readnext (inlined)",0); /* No return from gcerror */
	  }
	  curr = prev;
	  prev = back;
	  numptr = GETCOUNTER(prev);
/*	  fprintf(stderr,"GETCOUNTER %3d ",numptr); */
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
void mark(inode)                   /* Depth first using recursion */
     Node **inode;
{
  Node *node = *inode;
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "Mark on %lx %s\n", (UInt)chkaddr,tagname(node));
#endif

  if(INSIDEHEAP(node)) {
    if(marknode(node)) {
      switch(node->tag->gc) {
      case tgcind: /* set dirref */
	setdirref(node->nodeinp.file);
	break;
      case tgcinp: /* set fileref and fall into nodes of size 3 */
	setfileref(node->nodeinp.file);
      case tgc20:      case tgc30:      case tgcdvek: case tgchole: case tgcstr:
	break;
      case tgcint:
#ifdef USETABELS
	if(node->node20.val0 >= MININTTAB && node->node20.val0 <= MAXINTTAB) {
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
      case tgcindi:
	if(!keepIndir) {
	  unmarknode(node);
	  mark(&node->node11.ptr0);
	  *inode = node->node11.ptr0;
	  break;
	}
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
    case tgcindi:
    case tgc11:    case tgc12:    case tgc21:     case tgcvek:     case tgcuvek:
    case tgcbig:  case tgcap:    case tgcapG:  /* case tgccap: == tgc12 */
    case tgcstr: /* For gen gc */
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

void marklinks()
{
  FunLink *tmpfun;
  GcLink *tmpgc;
  int    nrefs;
  Node **refs;

#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"marklinks begin\n");
#endif /* GCSTAT */
  while(gclink != (GcLink *)-1 || funlink != (FunLink *)-1) {
#if GCSTAT
    if (Sflag>1)
      fprintf(stderr,"funlink %lx\n",(UInt)funlink);
#endif /* GCSTAT */
    while(funlink != (FunLink *)-1) {               /* First check all function info tables */
      tmpfun = funlink;
      funlink = funlink->next;
      tmpfun->next = oldfunlink;
      oldfunlink = tmpfun;
      nrefs = tmpfun->nrefs;
      refs = tmpfun->refs;
      while(nrefs--) {
	mark(refs++);
      }
    }
#if GCSTAT
    if (Sflag>1) {
      fprintf(stderr,"gclink %lx\n",(UInt)gclink);
    }
#endif /* GCSTAT */
    while(gclink != (GcLink *)-1) {                /* Then scan all nodes outside the heap */
      tmpgc = gclink;
      gclink = gclink->next;
      tmpgc->next = oldgclink;
      oldgclink = tmpgc;
      recmark(&tmpgc->node);
    }
  }                                      /* Iterate until finished */
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"marklinks end\n");
#endif /* GCSTAT */
}

void clearlinks()
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
#if 1
#define flip(inode) \
  { register Node *tmp = *inode;    \
     if(INSIDEHEAP(tmp)) {          \
       *inode = (Node *)tmp->tag;   \
       tmp->tag = (Tag *)inode;     \
     }                              \
  }

#else
void flip(inode)
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
#endif

int recflip(node)  /* flip all pointers in a marked node, returns size of node */
     Node *node;
{
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "recflip chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
#endif
  switch(getgc(node)) {
    /***************************************** No pointers */
  case tgczap:
  case tgcind: case tgcinp: case tgc30: case tgchole: case tgcstr:
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
  case tgcindi:
    flip(&node->node11.ptr0);
    return 3;
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
    fprintf(stderr,"recflip don't know how to handle %s (%lx)\n",tagname((PTR)node),(UInt)node->tag);
    gcerror("Illegal tag in flip (1)",0); /* No return from gcerror */
  }
  return 0; /* Never executed */
}
  
void fliplinks()
{
  GcLink *tmp;
  for(tmp = oldgclink; tmp != (GcLink *)-1; tmp = tmp->next) {
    (void)recflip(&tmp->node);
  }
}

void flipheap(start,stop)
     VPTR start,stop;
{
  PTR node = (PTR)start;
  PTR new = (PTR)start;
  unsigned int *wptr = (unsigned int *)BitHeap;
  unsigned int bcash = *wptr; /* Get first word */
  unsigned int bmask  = 1;     /* Init bitmask */
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"flipheap begin\n");
#endif /* GCSTAT */
  for(;;) {                  /* loop exit done by return */
/********** findnext begin **********/
    {
      if((VPTR)wptr < endtable) {
	if(bmask&bcash)                 {  /* No hole ! */
	  goto nextfound;
	} else {
	  Node *hole = node;
	  while((VPTR)wptr<endtable) {
	    while(bmask && !(bmask&bcash)) {
	      bmask <<= 1;
	      bmask = 0xffffffffl & bmask;
	      ++*(Int **)&node;	/* was ++(int *)node;  /LA */
	    }
	    if(bmask) {
	      if(((Int *)node)-((Int *)hole) == 1) {
		hole->nodemkd.tag =  &MARKED;
	      } else {
		hole->noderet.tag =  &GCRET;
		hole->noderet.next = node;
	      }
	      goto nextfound; /**** return ****/
	    } else {
	      for(*wptr++=0; (VPTR)wptr<endtable; *wptr++=0,node = (Node *)(BitPerWord+(Int*)node)) {
		if((bcash = *wptr)) {
		  bmask = 1;
		  break;
		}
	      }
	    }
	  }
	  if(((Int)node)-((Int)hole) == 1) {
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
    bcash ^= bmask;
/************ findnext end ************/
/*****  recnew begin ******/
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "flipheap1 from chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
    if(chkaddr == (int **)new)
      fprintf(stderr, "flipheap1 to chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
#endif
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
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "flipheap from chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
    if(chkaddr == (int **)new)
      fprintf(stderr, "flipheap to chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
#endif
    { int s = recflip(node);
      new = (Node *)(s+(Node **)new);
      node = (Node *)(s+(Node **)node);

      { register int tmp = (VPTR)node-startheap;
	register unsigned int *tptr = (unsigned int*)BitHeap+(UInt)(tmp>>LnBitPerWord);
	if(tptr != wptr) {
	  *wptr = 0;
	  wptr = tptr;
	  bcash = *wptr;
	  bmask = 1<<(tmp&BitMask);
	} else
	  bmask <<= s;  /* Same word so it must fit */
      }
    }
  }
}

Node *moveheap(start,stop)
     VPTR start,stop;
{
  PTR node = (PTR)start;
  PTR new = (PTR)start;
  int s;
#if GCSTAT
  if (Sflag>1)
    fprintf(stderr,"moveheap begin\n");
#endif /* GCSTAT */
  do {
#ifdef EXTRA_DEBUG
    if(chkaddr == (int **)node)
      fprintf(stderr, "moveheap from chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
    if(chkaddr == (int **)new)
      fprintf(stderr, "moveheap to chkaddr %lx %s\n", (UInt)chkaddr,tagname(node));
#endif
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
	  Int *dst = (Int *)new;
	  Int *src = (Int *)node;
	  while(i--)
	    *dst++ = *src++;
	  new =  (Node *)dst;
	}
/****** recmove end   ******/
      } else {
	s = 1;
      }      
    }
  } while((node = (PTR)(s+(VPTR)node))<(PTR)stop);
  return new;
}

void doSlide(ep)
     PTR ep;
{
  BitHeap = (int*)starttable;
  recmark((Node **)ep);
  marklinks();
  recflip(ep);
  fliplinks();
  flipheap(startheap,endheap);               /* New address for back pointers */
  hp = (VPTR)moveheap(startheap,endheap);    /* New address for forward pointers and move to new address */
  clearlinks();
}
