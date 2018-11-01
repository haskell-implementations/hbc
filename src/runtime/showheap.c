/*
  showheap.c
 */

#ifdef HSHOW
#include <stdio.h>
#include <math.h>
#include <memory.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>

#include "gc.h"
#include "gcd.h"
#include "runtime.h"
#include "vars.h"

#ifndef TRUE
#define TRUE  1
#define FALSE 0
#endif

#define EXTRA_DEBUG 1

/* Structures used by the X server. */

static Display *display;

static XImage *ximage = NULL;
static XImage *xscale = NULL;
static Colormap cmap;
static Window window;
static int windowWidth;
static int windowHeight;
VPTR showheapEhp;
int showheapStep;
int showheapLeft;
#define ScaleWidth 8
static GC xgc;
#define  COLOUR_SFLOAT	 1
#define  COLOUR_INT      2
#define  COLOUR_DFLOAT   3 
#define  COLOUR_BIGNUM   4
#define  COLOUR_CHAR     5
#define  COLOUR_TAG      6
#define  COLOUR_TAG0     7
#define  COLOUR_PAIR     8
#define  COLOUR_PAIR0    9
#define  COLOUR_PAIR1   10  
#define  COLOUR_PAIR2   11
#define  COLOUR_PAIR3   12
#define  COLOUR_PAIR4   13
#define  COLOUR_VEK     14
#define  COLOUR_DVEK    15
#define  COLOUR_HOLE    16
#define  COLOUR_ZAP     17
#define  COLOUR_STRING  19
#define  COLOUR_INPUT   18
#define  COLOUR_INPUTD  20
#define  COLOUR_AP      21
#define  COLOUR_AP1     22
#define  COLOUR_APG     23
#define  COLOUR_CAP     24
#define  COLOUR_FUN     25
#define  COLOUR_VAP     26
#define  COLOUR_VAP1    27
#define  COLOUR_VAPG    28
#define  COLOUR_MARKED  29
#define  COLOUR_MOVED   30
#define  COLOUR_GCRET   31
#define  COLOUR_INDIR   32
#define  COLOUR_function 33
#define  COLOUR_last 33

#define  COLOUR_all     -1
#define  COLOUR_end     -2


#define NoColours 128

int pixel[NoColours];
int oldpixel[NoColours];
int dstpixel[NoColours];
int outpixel;

typedef struct {
  int   no;
  char *str;
  char *colour;
} tagInfo;

tagInfo colours[] =  {
    {0,            "dead",      "black"},
    {COLOUR_all,   "all",       "white"},
    {COLOUR_SFLOAT,"SFLOAT",    0},
    {COLOUR_INT,   "INT",       0},
    {COLOUR_DFLOAT,"DFLOAT",    0},
    {COLOUR_BIGNUM,"BIGNUM",    0},
    {COLOUR_CHAR,  "CHAR",      0},
    {COLOUR_TAG,   "TAG",       0},
    {COLOUR_TAG0,  "TAG0",      0},
    {COLOUR_PAIR,  "PAIR",      0},
    {COLOUR_PAIR0, "PAIR0",     0},
    {COLOUR_PAIR1, "PAIR1",     0},
    {COLOUR_PAIR2, "PAIR2",     0},
    {COLOUR_PAIR3, "PAIR3",     0},
    {COLOUR_PAIR4, "PAIR4",     0},
    {COLOUR_VEK,   "VEK",       0},
    {COLOUR_DVEK,  "DVEK",      0},
    {COLOUR_HOLE,  "HOLE",      "purple"},
    {COLOUR_ZAP,   "ZAP",       "orange"},
    {COLOUR_STRING,"STRING",    "yellow"},
    {COLOUR_INPUT, "INPUT",     0},
    {COLOUR_INPUTD,"INPUTD",    0},
    {COLOUR_AP,    "AP",        "red"},
    {COLOUR_AP1,   "AP1",       "red"},
    {COLOUR_APG,   "APG",       "red"},
    {COLOUR_CAP,   "CAP",       0},
    {COLOUR_FUN,   "FUN",       0},
    {COLOUR_VAP,   "VAP",       "red"},
    {COLOUR_VAP1,  "VAP1",      "red"},
    {COLOUR_VAPG,  "VAPG",      "red"},
    {COLOUR_MARKED,"MARKED",    0},
    {COLOUR_MOVED, "MOVED",     0},
    {COLOUR_GCRET, "GCRET",     0},
    {COLOUR_INDIR, "INDIR",     "blue"},
    {COLOUR_function,"function",0},
    /* Last entry */
    {COLOUR_end,0,0}
};

extern Tag 	INT, SFLOAT, DFLOAT, CHAR, TAG, TAG0, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4,
		STRING, VEK, DVEK, CAP, MARKED, MOVED, AP, FUN, VAP, INDIR,
                APG,VAPG,
		HOLE, INPUT, INPUTD, GCRET, BIGNUM, ZAP;

int *showBits = 0;
unsigned int *endBits = 0;
int showSize = 0;
char *colourHeap = 0;
extern int showing;

/*
 *--------------------------------------------------------------
 *
 * MakeWindow --
 *
 *	Create X Window
 *
 * Results:
 *	Read the code.
 *
 * Side effects:
 *      None.
 *
 *--------------------------------------------------------------
 */

static void 
MakeWindow(width,height)
     int width,height;
{
  
  XSizeHints hint;
  unsigned int fg, bg;
  char *hello = "Heap bits";
  int screen;
  Window CreateFullColorWindow();

  display = XOpenDisplay("");
  if (display == NULL) {
    fprintf(stderr, "Can not open display\n");
    exit(-2);
  }

  screen = DefaultScreen (display);
  
  /* Fill in hint structure */

  hint.x = 200;
  hint.y = 300;
  if(showing == 4)
    hint.width = width+ScaleWidth*2;
  else
    hint.width = width;
  hint.height = height;
  hint.flags = PPosition | PSize;
  
  /* Get some colors */
  
  bg = WhitePixel (display, screen);
  fg = BlackPixel (display, screen);
  
  /* Make the window */

  if(showing == 1) {
    window = XCreateSimpleWindow (display,
				  DefaultRootWindow (display),
				  hint.x, hint.y,
				  hint.width, hint.height,
				  4, fg, bg);
  } else {
    XVisualInfo vinfo;
    
    if (!XMatchVisualInfo (display, screen, 8, PseudoColor, 
			   &vinfo)) {

      if (!XMatchVisualInfo(display, screen, 8, GrayScale, 
			    &vinfo)) {

	fprintf(stderr, "-requires 8 bit display\n");
	exit(-1);
      }
    }

    window = XCreateSimpleWindow (display,
				 DefaultRootWindow (display),
				 hint.x, hint.y,
				 hint.width, hint.height,
				 4, fg, bg);
  }

  /* Tell other applications about this window */
  
  XSetStandardProperties (display, window, hello, hello, None, NULL, 0, &hint);
  
  /* Map window. */

  XMapWindow(display, window);
}

void SetColourString(i, c, p)
     int i;
     char *c;
     int *p;
{
  XColor aprox,exact;

  if(!c) return;
  if(BadColor == XAllocNamedColor(display, cmap, c, &aprox,&exact))
    fprintf(stderr,"Can't find colour \"%s\"\n",c);
  
  switch(i) {
  case COLOUR_end:
    break;
  case COLOUR_all:
    for (i=1; i<=COLOUR_last; i++) /* Leave background */
      p[i] =  exact.pixel;
    break;
  default:
    p[i] = exact.pixel;
  }
}

int string2no(tag)
     char *tag;
{
  tagInfo *p;
  for(p = colours; p->str; p++) {
    if(!strcmp(tag,p->str))
      return p->no;
  }
  return p->no;
}

#define DELIM " \t\n:,./-"

void decodeColours(col,p)
     char *col;
     int *p;
{
  char *tag;
  char *c;
  if(!col)
    return;
  while ((tag = strtok(col,DELIM))) {
    col = NULL;
    c = strtok(NULL,DELIM);
    if(tag && c)
      SetColourString(string2no(tag),c,p);
  }
}




void decodeDstColours(col, p)
     char *col;
     int *p;
{
  int n,i;
  XColor tmp[NoColours],aprox,exact;
  char *c = 0;
  if(col) {
    c = strtok(col,DELIM);
  }
  if(c) {  
    double d;
    if(BadColor == XAllocNamedColor(display, cmap, c, &aprox,&exact))
      fprintf(stderr,"Can't find colour \"%s\"\n",c);
    outpixel = exact.pixel;

    for(n=0; (c = strtok(0,DELIM)); ) {
      if(BadColor == XAllocNamedColor(display, cmap, c, &aprox,&tmp[n]))
	fprintf(stderr,"Can't find colour \"%s\"\n",c);
      else
	++n;
    }
    if(n<2) {
      fprintf(stderr,"-xp must be followed by either no colours or at least three colours\n");
      exit(-1);
    }
    tmp[(NoColours-1)] = tmp[n-1];
    d = (double)(NoColours-1)/(double)(n-1);
    for(i = n-2; i>0; i--)
      tmp[(int)(i*d+0.5)] = tmp[i];
    for(i=0; i<n-1; i++) {
      int fr = i*d+0.5;
      int to = (i+1)*d+0.5;
      double sred   = tmp[fr].red;
      double sgreen = tmp[fr].green;
      double sblue  = tmp[fr].blue;
      double dred   = tmp[to].red   - sred;   
      double dgreen = tmp[to].green - sgreen;
      double dblue  = tmp[to].blue  - sblue;
      dred   /= to-fr;
      dgreen /= to-fr;
      dblue  /= to-fr;
      fprintf(stderr,"from %3d to %3d\n",fr,to);

      while(fr++<to-1) {
	sred   += dred;
	sgreen += dgreen;
	sblue  += dblue;
	fprintf(stderr,"(%f,%f,%f)\n",sred,sgreen,sblue);
	tmp[fr].flags = DoRed | DoGreen | DoBlue;
	tmp[fr].red   = sred+0.5;
	tmp[fr].green = sgreen+0.5;
	tmp[fr].blue  = sblue+0.5;
      }
    }
  } else {                      /* Default */
    XColor xcolor;
    xcolor.flags = DoRed | DoGreen | DoBlue;
    xcolor.red = xcolor.blue = 0;
    xcolor.blue = 0xffff;
    XAllocColor(display,cmap,&xcolor);
    outpixel = xcolor.pixel;
    xcolor.blue = 0;
    for (i = 0; i<NoColours; i++) {
      xcolor.red =  i*514;
      xcolor.green = ((NoColours-1)-i)*514;
      tmp[i].flags = xcolor.flags;
      tmp[i].red   = xcolor.red;
      tmp[i].green = xcolor.green;
      tmp[i].blue  = xcolor.blue;
    } 
  }
  for (i = 0; i<NoColours; i++) {
    XAllocColor(display,cmap,&tmp[i]);
    dstpixel[i] = tmp[i].pixel;
  }
}

  
void InitDisplay(width,height)
     int width,height;
{
  extern char *argColours;
  extern char *argOldColours;
  extern char *argDstColours;
  tagInfo *p;

  MakeWindow(width,height);
  xgc = XCreateGC(display, window, 0, 0);
  cmap = XDefaultColormap(display, DefaultScreen(display));
  for (p = colours; p->no != COLOUR_end; p++) {
    SetColourString(p->no,p->colour,pixel);
    SetColourString(p->no,p->colour,oldpixel);
  } 
  decodeColours(getenv("TAGCOLOURS"),pixel);
  decodeColours(getenv("TAGOLDCOLOURS"),oldpixel);
  decodeColours(argColours,pixel);
  decodeColours(argOldColours,oldpixel);
  decodeDstColours(argDstColours,dstpixel);
  ximage = NULL;
}

void InitMonoDisplay(width,height)
     int width,height;
{
  XGCValues xgcv;

  MakeWindow(width,height);

  xgcv.background = BlackPixel(display, DefaultScreen(display));
  xgcv.foreground = WhitePixel(display, DefaultScreen(display));

  xgc = XCreateGC(display, window, GCForeground | GCBackground, &xgcv);

  ximage = NULL;
}

void setupDisplay(si)
double si;
{
  double s = Heapsize;
  showheapLeft = showheapStep = Heapsize*si;
/*  ehp = hp+showheapStep; */
  showheapEhp = ehp;
  windowWidth = sqrt(s);
  windowWidth = (windowWidth+7)&~7;
  windowHeight = s/windowWidth;

  if(showing == 1)
    InitMonoDisplay(windowWidth,windowHeight);
  else
    InitDisplay(windowWidth,windowHeight);
}

/*
 *--------------------------------------------------------------
 *
 * ExecuteDisplay --
 *
 *	Actually displays display plane in previously created window.
 *
 * Results:
 *	None.
 *
 * Side effects:
 *	None.
 *
 *--------------------------------------------------------------
 */


void
ExecuteDisplay(bits,width,height)
     char *bits;
     int width,height;
{
  char dummy;

  if (ximage == NULL) {
    if(showing == 1)
      ximage = XCreateImage (display, None, 1, XYBitmap, 0, &dummy,
			     width,height, 8, 0);
    else {
      ximage = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			    width,height, 8, 0);
      if(showing == 4) {
	int i;
	char *colourScale;
	if(!(colourScale = malloc(height*ScaleWidth))) {
	  fprintf(stderr,"No memory for colourScale\n");
	  exit(-1);
	}
	xscale = XCreateImage(display, None, 8, ZPixmap, 0, &dummy,
			      ScaleWidth,height, 8, 0);
	for(i = 0; i<height*ScaleWidth; i+=ScaleWidth) {
	  int pi = i*NoColours/height/ScaleWidth;
	  int j;
	  for(j=0; j<ScaleWidth; j++)
	    colourScale[i+j] = dstpixel[pi];
	}
	xscale->data = colourScale;
      }   
    }
  }
  ximage->data = bits;
  if(showing == 4) {
    XPutImage(display, window, xgc, xscale, 0, 0, 0, 0, xscale->width, xscale->height);
    XPutImage(display, window, xgc, ximage, 0, 0, xscale->width, 0, ximage->width, ximage->height);
    XPutImage(display, window, xgc, xscale, 0, 0,
	      xscale->width+ximage->width, 0, xscale->width, xscale->height);
  } else {
    XPutImage(display, window, xgc, ximage, 0, 0, 0, 0, ximage->width, ximage->height);
  }
  XFlush(display);
}

extern int *BitHeap;

#define BitPerWord   32
#define LnBitPerWord  5
#define BitMask      31

void clearTable();
void recmark();
void mark();
void marklinks();
void clearlinks();
int marknode();
int nodesize();

void fillHeap()
{
  PTR node = (PTR)startheap;
  unsigned int *wptr = (unsigned int *)showBits;
  unsigned int bcash = *wptr; /* Get first word */
  unsigned int bmask  = 1;     /* Init bitmask */
  for(;;) {                  /* loop exit done by return */
/********** findnext begin **********/
    {
      if(wptr < endBits) {
	if(bmask&bcash)                 {  /* No hole ! */
	  goto nextfound;
	} else {
	  while(wptr<endBits) {
	    while(bmask && !(bmask&bcash)) {
	      bmask <<= 1;
	      bmask &= 0xffffffff;
	      ++*(int **)&node;	/* was ++(int *)node;  /LA */
	    }
	    if(bmask) {
	      goto nextfound; /**** return ****/
	    } else {
	      for(wptr++; wptr<endBits; wptr++,node = (Node *)(BitPerWord+(int*)node)) {
		if((bcash = *wptr)) {
		  bmask = 1;
		  break;
		}
	      }
	    }
	  }
	  return;  /* End of heap with a hole */
	}
      } else 
	return;    /* End of heap without a hole */
    }
  nextfound:;
    bcash ^= bmask;
/************ findnext end ************/
    { int s = nodesize(node);
      int i;
      for(i = 1; i<s; i++)
	marknode((Node *)(i+(int *)node));
      node = (Node *)(s+(Node **)node);

      { register int tmp = (VPTR)node-startheap;
	register unsigned int *tptr = (unsigned int*)BitHeap+(unsigned int)(tmp>>LnBitPerWord);
	if(tptr != wptr) {
	  wptr = tptr;
	  bcash = *wptr;
	  bmask = 1<<(tmp&BitMask);
	} else
	  bmask <<= s;  /* Same word so it must fit */
      }
    }
  }
}

    
int getcolour(tag)
     Tag *tag;
{
  if(tag == &INT) return COLOUR_INT;
  if(tag == &DFLOAT) return COLOUR_DFLOAT;
  if(tag == &SFLOAT) return COLOUR_SFLOAT;
  if(tag == &BIGNUM) return COLOUR_BIGNUM;
  if(tag == &CHAR) return COLOUR_CHAR;
  if(tag == &TAG) return COLOUR_TAG;
  if(tag == &TAG0) return COLOUR_TAG0;
  if(tag == &PAIR) return COLOUR_PAIR;
  if(tag == &PAIR0) return COLOUR_PAIR0;
  if(tag == &PAIR1) return COLOUR_PAIR1;
  if(tag == &PAIR2) return COLOUR_PAIR2;
  if(tag == &PAIR3) return COLOUR_PAIR3;
  if(tag == &PAIR4) return COLOUR_PAIR4;
  if(tag == &VEK) return COLOUR_VEK;
  if(tag == &DVEK) return COLOUR_DVEK;
  if(tag == &HOLE) return COLOUR_HOLE;
  if(tag == &ZAP) return COLOUR_ZAP;
  if(tag == &STRING) return COLOUR_STRING;
  if(tag == &INPUT) return COLOUR_INPUT;
  if(tag == &INPUTD) return COLOUR_INPUTD;
  if(tag == &AP) return COLOUR_AP;
  if(tag == &APG) return COLOUR_APG;
  if(tag == &CAP) return COLOUR_CAP;
  if(tag == &FUN) return COLOUR_FUN;
  if(tag == &VAP) return COLOUR_VAP;
  if(tag == &VAPG) return COLOUR_VAPG;
  if(tag == &MARKED) return COLOUR_MARKED;
  if(tag == &MOVED) return COLOUR_MOVED;
  if(tag == &GCRET) return COLOUR_GCRET;
  if(tag == &INDIR) return COLOUR_INDIR;
  return COLOUR_function;
}


PTR ColourNode(node, cnode, pixtab)
     PTR node;
     int cnode;
     int *pixtab;
{
  int s = nodesize(node);
  int c = pixtab[getcolour(node->tag)];
  int i;
  for(i = 0; i<s; i++)
    colourHeap[cnode+i] = c;
  return (Node *)(s+(Node **)node);
}


int ptr2pixel(p)
     PTR p;
{
  VPTR vp = (VPTR)p;
  int c;

  if( vp<startheap || vp>=endheap)
    return outpixel;
  c = (vp-startheap)*NoColours;
  c /= endheap-startheap;
  return dstpixel[c];
}

PTR ColourPointers(node, cnode, pixtab)
     PTR node; 
     int cnode; 
     int *pixtab;
{
  int s = nodesize(node);
  int tc = pixtab[getcolour(node->tag)];
  int i;

  colourHeap[cnode] = tc;  /* Colour tag */

  switch(node->tag->gc) {
       /* No pointers */
  case tgcind:  case tgcinp:
  case tgc20:  case tgc30:
  case tgcdvek:  case tgchole:
  case tgcint:  case tgcchr:
  case tgctag0:  case tgcstr:
       /* Pointers of special kind */
  case tgcfun:  case tgczap:
    for(i = 1; i<s; i++)
      colourHeap[cnode+i] = tc;
    break;
    
  case tgc11:
  case tgcbig:
  case tgcindi:
    colourHeap[cnode+1] = ptr2pixel(node->node11.ptr0);
    break;
  case tgcap: case tgcapG: /* case tgccap: == tgc12 */
  case tgc12:
    colourHeap[cnode+1] = ptr2pixel(node->node12.ptr0);
    colourHeap[cnode+2] = ptr2pixel(node->node12.ptr1);
    break;
  case tgc21:
    colourHeap[cnode+1] = tc;
    colourHeap[cnode+2] = ptr2pixel(node->node21.ptr0);
    break;
  case tgcvek:
    colourHeap[cnode+1] = tc;
    for(i = node->nodevek.size; i--; )
      colourHeap[cnode+2+i] = ptr2pixel(node->nodevek.ptr[i]);
    break;
  case tgcvap:   case tgcvapG:
    colourHeap[cnode+1] = tc;
    for(i = node->nodevap.fun->arity; i--; )
      colourHeap[cnode+2+i] = ptr2pixel(node->nodevap.ptr[i]);
    break;
  default:
    for(i = 1; i<s; i++)
      colourHeap[cnode+i] = tc;
  }
  return (Node *)(s+(Node **)node);
}

void fillHeapColour(pixtab)
     int *pixtab;
{
  PTR node = (PTR)startheap;
  unsigned int *wptr = (unsigned int *)showBits;
  unsigned int bcash = *wptr; /* Get first word */
  unsigned int bmask  = 1;     /* Init bitmask */
  for(;;) {                  /* loop exit done by return */
/********** findnext begin **********/
    {
      if(wptr < endBits) {
	if(bmask&bcash)                 {  /* No hole ! */
	  goto nextfound;
	} else {
	  while(wptr<endBits) {
	    while(bmask && !(bmask&bcash)) {
	      bmask <<= 1;
	      bmask &= 0xffffffff;
	      ++*(int **)&node;	/* was ++(int *)node;  /LA */
	    }
	    if(bmask) {
	      goto nextfound; /**** return ****/
	    } else {
	      for(wptr++; wptr<endBits; wptr++,node = (Node *)(BitPerWord+(int*)node)) {
		if((bcash = *wptr)) {
		  bmask = 1;
		  break;
		}
	      }
	    }
	  }
	  return;  /* End of heap with a hole */
	}
      } else 
	return;    /* End of heap without a hole */
    }
  nextfound:;
    bcash ^= bmask;
/************ findnext end ************/
    {
      int s = nodesize(node);
      register int p = (VPTR)node-startheap;
      if(colourHeap[p] == pixel[0]) {
	if(showing == 4)
	  node = ColourPointers(node,p,pixtab);
	else
	  node = ColourNode(node,p,pixtab);
      } else
	node = (PTR)(s + (PTR *)node);

      { register int tmp = (VPTR)node-startheap;
	register unsigned int *tptr = (unsigned int*)BitHeap+(unsigned int)(tmp>>LnBitPerWord);
	if(tptr != wptr) {
	  wptr = tptr;
	  bcash = *wptr;
	  bmask = 1<<(tmp&BitMask);
	} else
	  bmask <<= s;  /* Same word so it must fit */
      }
    }
  }
}

void showHeap()
{
  int stackdepth = bos - ep;
  int *oldBitHeap = BitHeap;
  extern int keepIndir;

#if EXTRA_DEBUG
  fprintf(stderr,"Begin showHeap\n");
#endif
  keepIndir = 1;
  funlink = oldfunlink = (FunLink *) -1;
  gclink = oldgclink = (GcLink *) -1;
  *--ep = (PTR)stackdepth;
  *--ep = (PTR)&VEK;
  ep[-1] = 0; /* This is a gclink word */

  if(!showBits) {
    showSize = (Heapsize+7)/8;
    if(!(showBits = malloc(showSize))) {
      fprintf(stderr,"No memory for showbits\n");
      exit(-1);
    }
    endBits =(unsigned int *)(showSize+(int)showBits);
    if(showing >= 2) {
      if(!(colourHeap = malloc(showSize*8))) {
	fprintf(stderr,"No memory for colourHeap\n");
	exit(-1);
      }
    }
  }

  bzero(showBits,showSize);
  BitHeap = (int*)showBits;
  recmark((Node **)ep);
  marklinks();
  clearlinks();
  if(showing == 1) {
    fillHeap();
    ExecuteDisplay(showBits,windowWidth,windowHeight);
  } else {
    memset(colourHeap,pixel[0],showSize*8);
    fillHeapColour(pixel);
    if(showing==3 && tp) {
      VPTR p;
      for(p = tp; p<endtable; p++)
	mark(p);
      marklinks();
      clearlinks();
      fillHeapColour(oldpixel);
    }
    ExecuteDisplay(colourHeap,windowWidth,windowHeight);
  }    
  BitHeap = oldBitHeap;
  ep += 2;
  keepIndir = 0;
#if EXTRA_DEBUG
  fprintf(stderr,"End showHeap\n");
#endif
}

#else
int _dummy_show_heap = 1;
#endif
