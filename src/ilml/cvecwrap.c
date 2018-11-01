#include "../runtime/node.h"



/*
** This is the C part of an enormous kludge!
** The calling convention for instance vectors (VVxxx) is to pass the
** method number and dictionary(ies) in registers.  The VVxxx also needs to be
** a "real" function, i.e. a FUN node, since the method call assumes this
** to be evaluated, and being a FUN.
** There is no way for the interactive system to manufacture a new FUN since
** the function descriptor must reside outside the heap.  Well, here's the function
** that does it.  It mallocs(!) space for a FUN and the descriptor and fills them.
** The code for the function is generic, but it accesses the descriptor (another
** monster hack) to get the value of the real function to call.
** NOTE: This memory cannot be reclaimed.
*/
PTR
cvecwrap(f)
PTR f;
{
    PTR res, tag;
    FunInfo *inf;
    extern Tag FUN, TAG;
    extern void /*unw2(), vunw2(),*/ jvecwrap();

    res = (PTR)malloc(sizeof(PTR) + sizeof(Nodefun) + sizeof(FunInfo) + sizeof(PTR) + sizeof(Node21));
    *(PTR*)res = 0;			/* link word */
    res = (PTR)((PTR*)res + 1);
    inf = (FunInfo *)((char *)res + sizeof(Nodefun));
    tag = (PTR)((char *)inf + sizeof(FunInfo));
    *(PTR*)tag = 0;			/* link word */
    tag = (PTR)((PTR*)tag + 1);
    res->nodefun.tag = &FUN;
    res->nodefun.fun = inf;
    inf->arity = 2;
    inf->node = res;
    inf->unw = 0;
    inf->vunw = 0;
    inf->jmpcode = jvecwrap;
    inf->callcode = 0;
    inf->stingy = 0;
    inf->hprofinfo = 0;
    inf->spare = 0;
    inf->link.next = 0;
    inf->link.nrefs = 1;
    inf->link.refs[0] = tag;
    tag->tag = &TAG;
    tag->node21.val0 = 0;
    tag->node21.ptr0 = f;
    return res;
}
