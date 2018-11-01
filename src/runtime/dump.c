#include <stdio.h>
#include "runtime.h"
#include "vars.h"
#include "funs.h"

extern VPTR garbvp;  /* Vp when gc started */
extern VPTR garbvp0; /* Vp when print was first called */


#define MAXDUMP	10000		/* never dump more nodes than this */
int maxdump = MAXDUMP;
int dumpdepth = 5;
int Gno = 1;

extern Tag      AP_1, VAP_1, ZAP_1, HOLE_1, STRING_1, STRINGN_1, INPUTD_1, INPUT_1, 
                STRING_F, STRINGN_F, INPUTD_F, INPUT_F, 
                AP, APG, VAP, VAPG, ZAP, HOLE, STRING, STRINGN, INPUTD, INPUT, INDIR, 
                CAP,  FUN, 
      	        INT, SFLOAT, DFLOAT, BIGNUM, CHAR, PAIR, PAIR0, PAIR1, PAIR2, PAIR3, PAIR4, TAG, TAG0, 
		VEK, DVEK,
                MARKED, MOVED, GCRET, GSRET,
                CAP_1,
      	        INT_1, SFLOAT_1, DFLOAT_1, BIGNUM_1, CHAR_1,
                PAIR_1, PAIR0_1, PAIR1_1, PAIR2_1, PAIR3_1, PAIR4_1, TAG_1, TAG0_1, 
		VEK_1, DVEK_1;

void dumpfuninfo PROTO((FunInfo *, int));

char *
tagname(p)
PTR p;
{
	if(p->tag == &INT_1) return "INT_1";
	if(p->tag == &SFLOAT_1) return "SFLOAT_1";
	if(p->tag == &DFLOAT_1) return "DFLOAT_1";
	if(p->tag == &BIGNUM_1) return "BIGNUM_1";
	if(p->tag == &CHAR_1) return "CHAR_1";
	if(p->tag == &TAG_1) return "TAG_1";
	if(p->tag == &TAG0_1) return "TAG0_1";
	if(p->tag == &PAIR_1) return "PAIR_1";
	if(p->tag == &PAIR0_1) return "PAIR0_1";
	if(p->tag == &PAIR1_1) return "PAIR1_1";
	if(p->tag == &PAIR2_1) return "PAIR2_1";
	if(p->tag == &PAIR3_1) return "PAIR3_1";
	if(p->tag == &PAIR4_1) return "PAIR4_1";
	if(p->tag == &VEK_1) return "VEK_1";
	if(p->tag == &DVEK_1) return "DVEK_1";
	if(p->tag == &HOLE_1) return "HOLE_1";
	if(p->tag == &ZAP_1) return "ZAP_1";
	if(p->tag == &STRING_1) return "STRING_1";
	if(p->tag == &STRINGN_1) return "STRINGN_1";
	if(p->tag == &INPUT_1) return "INPUT_1";
	if(p->tag == &INPUTD_1) return "INPUTD_1";
	if(p->tag == &AP_1) return "AP_1";
	if(p->tag == &CAP_1) return "CAP_1";
	if(p->tag == &VAP_1) return "VAP_1";

	if(p->tag == &INT) return "INT";
	if(p->tag == &SFLOAT) return "SFLOAT";
	if(p->tag == &DFLOAT) return "DFLOAT";
	if(p->tag == &BIGNUM) return "BIGNUM";
	if(p->tag == &CHAR) return "CHAR";
	if(p->tag == &TAG) return "TAG";
	if(p->tag == &TAG0) return "TAG0";
	if(p->tag == &PAIR) return "PAIR";
	if(p->tag == &PAIR0) return "PAIR0";
	if(p->tag == &PAIR1) return "PAIR1";
	if(p->tag == &PAIR2) return "PAIR2";
	if(p->tag == &PAIR3) return "PAIR3";
	if(p->tag == &PAIR4) return "PAIR4";
	if(p->tag == &VEK) return "VEK";
	if(p->tag == &DVEK) return "DVEK";
	if(p->tag == &HOLE) return "HOLE";
	if(p->tag == &ZAP) return "ZAP";
	if(p->tag == &STRING) return "STRING";
	if(p->tag == &STRINGN) return "STRINGN";
	if(p->tag == &INPUT) return "INPUT";
	if(p->tag == &INPUTD) return "INPUTD";
	if(p->tag == &AP) return "AP";
	if(p->tag == &APG) return "APG";
	if(p->tag == &CAP) return "CAP";
	if(p->tag == &FUN) return "FUN";
	if(p->tag == &VAP) return "VAP";
	if(p->tag == &VAPG) return "VAPG";

	if(p->tag == &STRING_F) return "STRING_F";
	if(p->tag == &STRINGN_F) return "STRINGN_F";
	if(p->tag == &INPUT_F) return "INPUT_F";
	if(p->tag == &INPUTD_F) return "INPUTD_F";

	if(p->tag == &MARKED) return "MARKED";
	if(p->tag == &MOVED) return "MOVED";
	if(p->tag == &GCRET) return "GCRET";
	if(p->tag == &GSRET) return "GSRET";
	if(p->tag == &INDIR) return "INDIR";
	return "function";
}

void
dumpgraph(p, d)
PTR p;
int d;
{
    static int dcount;

    /* To prevent megabyte dumps */
    if (++dcount > maxdump) {
	fprintf(stderr, "Too many cells dumped.\n");
	finish(1);
    }
    fprintf(stderr, "<%lx %d>", (UInt)p, 
	    p < (PTR)startheap ? 	 0 : 
	    p > (PTR)endheap   ?    	 0 :
	    p < (PTR)(starthp)          ?1 :
	    2
	    );
    if (p == 0 || p > (PTR)endheap+10000) {
	fprintf(stderr, "Bogus addr!");
    } else if(d == 0){
	fprintf(stderr, "(...)");
    } else if(p->tag == &INT || p->tag == &INT_1){
	fprintf(stderr, "(%s %ld)", tagname(p), INTOF(p));
    } else if(p->tag == &DFLOAT || p->tag == &DFLOAT_1){
#if IntSize == 8
	union u { double d; Int i; } u;
	u.i = p->node20.val0;
#else
	union u { double d; int i[2]; } u;
	u.i[0] = p->node30.val0;
	u.i[1] = p->node30.val1;
#endif
	fprintf(stderr, "(%s %10.7e)", tagname(p), u.d);
    } else if(p->tag == &SFLOAT || p->tag == &SFLOAT_1){
	union u { float d; int i; } u;
	u.i = p->node20.val0;
	fprintf(stderr, "(%s %10.7e)", tagname(p), (double)u.d);
    } else if(p->tag == &BIGNUM || p->tag == &BIGNUM_1){
	fprintf(stderr, "(%s ",tagname(p));
	dumpgraph(p->node11.ptr0, d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &CHAR || p->tag == &CHAR_1){
	fprintf(stderr, "(%s %c)", tagname(p), (int)INTOF(p));
    } else if(p->tag == &TAG || p->tag == &TAG_1){
	fprintf(stderr, "(%s %ld ", tagname(p), p->node21.val0);
	dumpgraph(p->node21.ptr0, d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &TAG0 || p->tag == &TAG0_1){
	fprintf(stderr, "(%s %ld)", tagname(p), p->node20.val0);
    } else if(p->tag == &PAIR || p->tag == &PAIR_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &PAIR0 || p->tag == &PAIR0_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &PAIR1 || p->tag == &PAIR1_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &PAIR2 || p->tag == &PAIR2_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &PAIR3 || p->tag == &PAIR3_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &PAIR4 || p->tag == &PAIR4_1){
	fprintf(stderr, "(%s ", tagname(p));
	dumpgraph(HDOF(p), d-1);
	dumpgraph(TLOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &AP || p->tag == &AP_1 || p->tag == &APG || p->tag == &CAP || p->tag == &CAP_1){
	fprintf(stderr,  "(%s ", tagname(p));
	dumpgraph(FUNOF(p), d-1);
	dumpgraph(ARGOF(p), d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &INDIR){
	fprintf(stderr,  "(INDIR ");
	dumpgraph(p->node11.ptr0, d-1);
	fprintf(stderr, ")");
    } else if(p->tag == &FUN){
	if (Gflag > 1) {
	    fprintf(stderr,  "(FUN %lx ", (UInt)p->nodefun.fun);
	    dumpfuninfo(p->nodefun.fun, d);
	    fprintf(stderr, ")");
	} else {
	    fprintf(stderr,  "(FUN %lx)", (UInt)p->nodefun.fun);
	}
    } else if(p->tag == &VAP || p->tag == &VAP_1 || p->tag == &VAPG ){
	int i, m;
	m = p->nodevap.fun->arity;
	fprintf(stderr,  "(%s %lx %d ",tagname(p),(UInt)p->nodevap.fun, m);
	if (Gflag > 1) {
	    dumpfuninfo(p->nodevap.fun, d);
	}
	for(i = 0; i < m; i++){
	    dumpgraph(p->nodevap.ptr[i], d-1);
	}
	fprintf(stderr, ")");
    } else if(p->tag == &STRING || p->tag == &STRING_F || p->tag == &STRING_1){
	fprintf(stderr, "(%s \"%s\")",tagname(p), STRINGOF(p));
    } else if(p->tag == &STRINGN || p->tag == &STRINGN_F || p->tag == &STRINGN_1){
        int i = STRINGNOF(p);
	char *s = STRINGOF(p);
	fprintf(stderr, "(%s %d \"",tagname(p), i);
	while(i--)
	  putc(*s++,stderr);
	fprintf(stderr, "\")");
    } else if(p->tag == &VEK || p->tag == &VEK_1){
	int i;
	fprintf(stderr, "(%s %d ", tagname(p), p->nodevek.size);
	for(i = 0; i < p->nodevek.size; i++){
	    dumpgraph(p->nodevek.ptr[i], d-1);
	}
	fprintf(stderr, ")");
    } else if(p->tag == &DVEK || p->tag == &DVEK_1){
	int i, m = p->nodedvek.size;
	fprintf(stderr, "(%s %d ", tagname(p), m);
	for(i = 0; i < m; i++)
	    fprintf(stderr, "%08x ", (UInt)p->nodedvek.val[i]);
	fprintf(stderr, ")");
    } else if(p->tag == &MARKED){
	fprintf(stderr, "(MARKED) ");
    } else if(p->tag == &MOVED){
	fprintf(stderr, "(MOVED %lx) ", (UInt)p->nodemvd.new);
    } else if(p->tag == &HOLE || p->tag == &HOLE_1){
	fprintf(stderr, "(%s)", tagname(p));
    } else if(p->tag == &ZAP || p->tag == &ZAP_1){
	fprintf(stderr, "(%s %lx", tagname(p), (UInt)p->nodezap.fun);
	if (Gflag > 1)
	    dumpfuninfo(p->nodezap.fun, d);
	fprintf(stderr, ")");
    } else if(p->tag == &INPUT || p->tag == &INPUT_F || p->tag == &INPUT_1){
	fprintf(stderr, "(%s %lx %lx) ", tagname(p), (UInt)p->node30.val0, (UInt)p->node30.val1);
    } else if(p->tag == &INPUTD || p->tag == &INPUTD_F || p->tag == &INPUTD_1){
	fprintf(stderr, "(%s %lx) ", tagname(p), (UInt)p->node30.val0);
    } else if(p->tag == &GCRET){
	fprintf(stderr, "(GCRET %lx) ", (UInt)p->nodemvd.new);
    } else if(p->tag == &GSRET){
	fprintf(stderr, "(GSRET %lx) ", (UInt)p->nodemvd.new);
    } else {
	fprintf(stderr, "(BADTAG %lx)", (UInt)p->tag);
    }
#undef p		
}

void
dumpstack(msg)
char *msg;
{
    PTR *p;

    fprintf(stderr, "\n%s (depth = %ld):\nvp=%lx",msg, bos-ep, (UInt)garbvp);
    fprintf(stderr, "\nPointer stack dump:\n\n");
    for(p = ep; p < bos; p++){
	fprintf(stderr, "%10lx:  ", (UInt)p);
	dumpgraph(*p, dumpdepth);
	fprintf(stderr, "\n");
    }
}

void
dumpfuninfo(p, n)
FunInfo *p;
int n;
{
    int i, k;

    if (n > 0) {
	n--;
	fprintf(stderr, "%s=[%ld %lx %lx %ld (%ld ", lookup((UInt)p), p->arity, (UInt)p->node,
		(UInt)p->jmpcode, (UInt)p->link.next, p->link.nrefs);
	k = p->link.nrefs;
	for(i = 0; i < k; i++) {
	    dumpgraph(p->link.refs[i], n);
	    fprintf(stderr, ",");
	}
        fprintf(stderr, ")]");
    } else
	fprintf(stderr, "...");
}


#if 0
void
dumpvtop(v)
PTR v;
{
    extern void debstop();
    Int *data = v[0], *addr=v[1];
    if (!Gflag)
	return;
    fprintf(stderr, "vtop=%lx is %lx %lx", (UInt)v, (UInt)addr, (UInt)data);
    fprintf(stderr, " %lx\n", *data);
    if (chkaddr == (PTR)addr)
	debstop();
}
#endif
