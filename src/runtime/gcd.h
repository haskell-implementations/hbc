#include "node.h"

extern GcLink *gclink; /* Nodes outside heap */
extern GcLink *oldgclink;
extern FunLink *funlink;
extern FunLink *oldfunlink;

#if 0
#ifdef SLIDE_GC
void mark();
void recmark();
void marklinks();
void flip();
Node *restofgc();
void clearbits();
#endif

#ifdef BAKER_GC
Node *move(); /* returns pointer to next free word */
Node *scan();
Node *followlinks();
void clearlinks();
#endif
#endif
