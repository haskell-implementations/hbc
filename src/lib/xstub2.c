#include "../runtime/node.h"

#if 0
static void sorry()
{
  fprintf(stderr,"Sorry this program must be linked with X11\n");
  exit(-1);
}

void XMapWindow(){sorry();}
void XResizeWindow (){sorry();}
void XCreateImage (){sorry();}
void XPutImage (){sorry();}
void XCreateSimpleWindow (){sorry();}
void XOpenDisplay (){sorry();}
void XSetStandardProperties (){sorry();}
void XCreateGC (){sorry();}
void XFlush () {sorry();}
void XMatchVisualInfo() {sorry();}
void XAllocColor() {sorry();}
void XAllocNamedColor() {sorry();}
void XDefaultColormap () {sorry();}
#else
void setupDisplay() {}
void showHeap() { fprintf(stderr, "Sorry, program not linked with -showheap\n"); exit(1); }
#endif
