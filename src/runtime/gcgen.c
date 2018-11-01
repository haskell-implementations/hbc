#include "runtime.h"
#include "vars.h"
#include "gc.h"
#include "gcd.h"
#include "../mcode/limit.h"

extern Tag MOVED;  /* defined in runtime.M */
extern Tag MARKED;
extern Tag GCRET;
extern Tag CHAR;
extern Tag INT;
extern Tag AP,APG,VAP,VAPG;
extern Tag TAG0;
extern Tag DFLOAT,SFLOAT;
extern Tag ZAP,HOLE,INPUTD,INPUT,STRING,INDIR;
extern Tag TAGFIRST,TAGLAST;

void gcerror PROTO((char *c,Int ));
void setdirref PROTO((FILE *f));
void setfileref PROTO((FILE *f));

extern GcLink *gclink;     /* Nodes outside heap */
extern GcLink *oldgclink;
extern FunLink *funlink;
extern FunLink *oldfunlink;

char *tagname();

#define INSIDEHEAP(n) ((n)>=(Node *)startheap && (n)<=(Node *)endheap) 
#define isinder(tag)  (tag>=&TAGLAST || tag<&TAGFIRST)

extern int *BitHeap;

typedef union LIST {
  Tag        *tag;
  union LIST *next;
  Node       *new;
} List;


#define INSIDEPREVIOUS(n) ((n)>=starthp && (n)<endheap) 

extern int tpin,tpout,tpdupl,tpnew,tpdel;

void packTable()
{
  VPTR readtp;
  Tag *tag;
  PTR ptr;

  readtp = endtable-1;
  tpdupl = tpnew = tpdel = 0;

  while(tp <= readtp) {
    if((ptr = (PTR)*readtp)) {   /* Zero if gensall zapped the entry */
      if(INSIDEPREVIOUS((VPTR)ptr)) {   /* This is a fix to ignore (V)APG which have (not necessary in minor)*/
#if 0
	if(ptr->tag != &MOVED)
	  fprintf(stderr,"Skumt tp -> %s %d\n",tagname(ptr),(Int)ptr);
#endif
	*readtp = *tp++;                /* been used to update cells in newspace */
	tpnew++;
      } else {
	if(1&(Int)(tag = ((PTR)*readtp)->tag)) {  /* Not the first reference  */
	  *readtp = *tp++;
	  tpdupl++;
	} else {                                                            /* First reference */
	  if(&ZAP == (Tag*)tag || &HOLE == (Tag *)tag ||
	     &INPUTD == (Tag *)tag || &INPUT == (Tag *)tag ||
	     &STRING == (Tag *)tag || &INDIR == (Tag *)tag) {
	    **(int **)readtp = (Int)(1 | (Int)tag);
	    readtp--;
	    tpdel++;
	  } else {
	    *readtp = *tp++;
	  }
	}
      }
    } else {
      *readtp = *tp++;
#ifdef EXTRA_DEBUG
      fprintf(stderr,"packTable 0\n");
#endif
    }
  }
  while(!*tp)
    tp++;
  for(readtp = tp; readtp < endtable; readtp++) {
    **(int**)readtp = (Int)(~1 & **(int **)readtp);
  }
}

static void addupdated(n)
PTR n;
{
  *--tp = (void *)n;
}

void clearlinksGen()
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
    switch(((Tag *)(gtmp = oldgclink)->node)->gc) {
    case tgcinp: case tgcind:
    case tgchole: case tgczap:
    case tgcstr:
      addupdated(&gtmp->node);
      break;
    case tgcint: case tgc30: case tgcbig: case tgcchr: case tgc12: case tgc21: case tgctag0: case tgcvek:
    case tgcdvek: case tgcapG: case tgcfun: case tgcvapG:
      break;
    case tgcindi:  fprintf(stderr,"clearlinksGen tgcindi\n"); break;
    case tgcap:  fprintf(stderr,"clearlinksGen tgcap\n"); break;
    case tgcvap:
      gtmp->node = (Int *)&VAPG;
      break;
    case tgcmkd:  fprintf(stderr,"clearlinksGen tgcmkd\n"); break;
    case tgcmvd:  fprintf(stderr,"clearlinksGen tgcmvd\n"); break;
    case tgcret:  fprintf(stderr,"clearlinksGen tgcret\n"); break;
    default:   fprintf(stderr,"clearlinksGen %d at %lx\n",
                              ((Tag *)gtmp->node)->gc,
                              (UInt)&gtmp->node); break;
    }
    oldgclink = oldgclink->next;
    gtmp->next = 0;
  }
}

#if 1
void moveheapGen();
#else
void moveheapGen()
{
  PTR node = (PTR)startheap;
  PTR new = (PTR)startheap;
  int s;
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
	if(&AP == node->tag) node ->tag = &APG;
	else if(&VAP == node->tag) node ->tag = &VAPG;

	switch(node->tag->gc) {
	case tgcinp: case tgcind:
	case tgchole: case tgczap:
	case tgcstr:
	  addupdated(new);
	  break;
	case tgcint: case tgc30: case tgcbig: case tgcchr: case tgc12: case tgc21:
	case tgctag0: case tgcvek: case tgcdvek:
	case tgcapG: case tgcfun: case tgcvapG:
	  break;
	case tgcindi:  fprintf(stderr,"moveheapGen tgcindi\n"); break;
	case tgcap:  fprintf(stderr,"moveheapGen tgcap\n"); break;
	case tgcvap:  fprintf(stderr,"moveheapGen tgcvap\n"); break;
	case tgcmkd:  fprintf(stderr,"moveheapGen tgcmkd\n"); break;
	case tgcmvd:  fprintf(stderr,"moveheapGen tgcmvd\n"); break;
	case tgcret:  fprintf(stderr,"moveheapGen tgcret\n"); break;
	default:   fprintf(stderr,"moveheapGen %d at %lx\n",node->tag->gc,(UInt)node); break;
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
  } while((node = (PTR)(s+(VPTR)node))<(PTR)endheap);
  hp = (VPTR) new;
}
#endif

void recmark PROTO((Node **));
void marklinks PROTO((void));
void recflip PROTO((PTR *));
void fliplinks PROTO((void));
void flipheap PROTO((VPTR,VPTR));

void doGenSeward1(ep)
     PTR *ep;
{
  BitHeap = (int*)starttable;
  clearTable();
  recmark(ep);
  marklinks();
  recflip(ep);
  fliplinks();
  flipheap(startheap,endheap);               /* New address for back pointers */
  moveheapGen();    /* New address for forward pointers and move to new address */
  clearlinksGen();
}

void gensall PROTO((VPTR,VPTR));

void doGenSeward0(ep)
     PTR *ep;
{
  PTR dummy;
  gensall(tp,endtable);
  genc(ep,&dummy);
}

