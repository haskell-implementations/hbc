#ifdef HPROFILE
#include "runtime.h"
#include "vars.h"
#include "sample.h"
#include "../mcode/limit.h"
#include "../mcode/magic.h"

double sampleinterval = STARTINTERVAL;
int gengcmark = 1;

HPnodefwd *nodelist2;		/* linked-list of nodes of size 8  bytes   */
HPnodefwd *nodelist3;		/* linked-list of nodes of size 12 bytes   */
HPnodefwd *nodelistvek;		/* linked-list of vek nodes 		   */
HPnodefwd *nodelistvap;		/* linked-list of vap nodes 		   */

extern int run_gc_hooks PROTO((int));
int inres PROTO((char*, struct restrlist *));

#define DEBUG

#ifdef DEBUG
#define STATIC
#else
#define STATIC	static
#endif


STATIC void CreateNodeLists PROTO((void));
STATIC void DestroyNodeLists PROTO((void));
STATIC void SampleBegin PROTO((double));
STATIC void SampleEnd PROTO((double));
STATIC void FillProfileTable PROTO((void));
STATIC void PrintProfileTable PROTO((void));
STATIC void EmptyProfileTable PROTO((void));
STATIC void AddEntry PROTO((struct hprofinfo *, int, HPnodefwd *));
STATIC void OutputSpecial PROTO((FILE *, char *));

extern newheapsize PROTO((int, int));
extern void samplegc();
extern int iswhite PROTO((int));

/*
 *	This code is adapted from the LML garbage collector. It combines 
 *	graph sampling and garbage collection in an attempt to lower the 
 *	cost of sampling.
 */
STATIC void
sgcstk()
{
    extern int nfunlink;
#ifdef OLDNEXTRESP
    extern PTR nextresp;			/* used in Dialogue I/O */
#endif
    PTR dummy;
    extern Tag VEK;
    int nelem = bos - ep;

    nfunlink = 0;
    if(ep < stack+2) {
	fprintf(stderr, "\nPointer stack overflow (pushed %d to many)\n",(VPTR)stack-(VPTR)ep);
	finish(1);
    }

    run_gc_hooks(0);

    /* Make stack into one huge vector node and do GC on that */
    *--ep = (PTR)nelem;
    *--ep = (PTR)&VEK;
    ep[-1] = 0; /* This is a gclink word */
    samplegc(ep, &dummy);
    
    ep += 2;
    if (stack[0]) {
	fprintf(stderr, "Stack overrun after GC.\n");
	exit(1);
    }
    if (Gflag) {
	fprintf(stderr, "nfunlink = %d\n", nfunlink);
    }

    run_gc_hooks(1);

#ifdef OLDNEXTRESP
    /* Relocate nextresp if necessary */
    if (nextresp) {
	extern Tag MOVED;
	if (nextresp->tag == &MOVED) {
	    nextresp = nextresp->nodemvd.new;
	}
    }
#endif
}

void
DoSample()
{
    double time = milliseconds / 1000.0;
    static int nsamples = 0;

    if (!noautosample) {
	/* Adjust sampling interval as time goes */
	if (++nsamples >= SAMPLEMAX) {
	    /* double the interval */
	    sampleinterval *= 2;
	    nsamples /= 2;
	}
    }

    if (debug) fprintf(stderr, "start DoSample\n");
    doinggc++;
    if (intrflag) {
	intrflag = 0;
	ehp = realehp;
	cleansig();
	failintr();
    }
  
    if (hp > realehp+HCLAIM) {
	/* The heap has been overrun */
	fprintf(stderr, "Error: heap overrun: %lx %lx %lx\n", (UInt)hp, (UInt)ehp, (UInt)realehp);
	finish(1);
    }

    /* Switch heaps. */
    { VPTR p = nxtheap; nxtheap = curheap; curheap = p;}
  
    CreateNodeLists();
    SampleBegin(time);

    hp = curheap;
    sgcstk();
  
    /* Find out how much graph was moved. */
    heapsize = newheapsize(hp-curheap, heapsize);
    starthp = hp;
  
    realehp = ehp = curheap + heapsize - HCLAIM;
  
    FillProfileTable();
    PrintProfileTable();
    EmptyProfileTable();

    SampleEnd(time);
    DestroyNodeLists();

    filegcfinish();
    
    if(hp+Minleft > curheap+(endheap-startheap)/2) {
	noheapleft();
    }

    if (intrflag) {
	intrflag = 0;
	cleansig();
	failintr();
    }
    sampleflag = 0;
    doinggc = 0;
    if (debug) fprintf(stderr, "end DoSample\n");
}

STATIC void
CreateNodeLists()
{
    nodelist2 = 0;
    nodelist3 = 0;
    nodelistvek = 0;
    nodelistvap = 0;
}

STATIC void 
DestroyNodeLists()
{
    /* nothing now */
}


STATIC void SampleBegin(clock)
double clock;
{
    fprintf(hpfile, "BEGIN_SAMPLE %.2f\n", clock);
    fflush(hpfile);
}

STATIC void SampleEnd(clock)
double clock;
{
    fprintf(hpfile, "END_SAMPLE %.2f\n", clock);
    fflush(hpfile);
}


/* 
 *	Profiling information is stored in a hash table with chaining.
 */
struct entry {
    int    count;			/* bytes for this collection */
    struct entry* next;
    char*  name;
};

#define PROFILE_TABLE_SIZE  1023 

static struct entry *profiletable[PROFILE_TABLE_SIZE];

static int Hash(s)
char *s;
{
    int r;

    for (r = 0; *s; s++)
        r += *s;

    return r % PROFILE_TABLE_SIZE;
}

STATIC char * copynstring PROTO((char *, int));

STATIC char*
copynstring(p, len)
char *p;
int len;
{
    char *r;
    r = (char *)xmalloc(len + 1);
    strncpy(r, p, len);
    r[len] = 0;
/*fprintf(stderr, "copynstr '%s' %d '%s'\n", p, len, r);*/
    return r;
}

/*
 *	Get space for a new entry in the profiling table. Initialise
 *	it, and return a pointer to the new entry.
 */
STATIC struct entry* 
MakeEntry(name, count)
char* name;
int count;
{
    int h;
    struct entry* e;
 
    h = Hash(name);
    e = (struct entry*)xmalloc(sizeof(struct entry));
    e->count = count;
    e->name = name;
    return e;
}

STATIC void 
FillProfileTable()
{
    HPnodefwd *p;
    extern Tag BIGNUM;

    for(p = nodelist2; p; p = p->next) {
	AddEntry(p->forward->n2.hpinf, p->forward->n2.n11.tag == &BIGNUM ? 4+p->forward->n2.n11.ptr0->nodedvek.size : 2, p);
    }
    for(p = nodelist3; p; p = p->next) {
	AddEntry(p->forward->n3.hpinf, 3, p);
    }
    for(p = nodelistvek; p; p = p->next) {
	Nodevek *node = &p->forward->nvek;
	int size = node->size;
	if (size == 0)
	    fprintf(stderr, "Size 0 VEK!\n"), size = 1;
	AddEntry((struct hprofinfo*)node->ptr[size], 2+size, p);
    }
    for(p = nodelistvap; p; p = p->next) {
	Nodevap *node = &p->forward->nvap;
	int size = node->fun->arity;
	if (size == 0)
	    fprintf(stderr, "Size 0 VAP!\n"), size = 1;
	AddEntry((struct hprofinfo*)node->ptr[size], 2+size, p);
    }
}

STATIC void 
EmptyProfileTable()
{
    int i;
    struct entry* e;
 
    for (i = 0; i < PROFILE_TABLE_SIZE; i++) {
        for (e = profiletable[i]; e; e = e->next) {
            e->count = 0;
        }
    }
}
 
STATIC void 
PrintProfileTable()
{
    int i;
    struct entry* e;
 
    for (i = 0; i < PROFILE_TABLE_SIZE; i++) {
        for (e = profiletable[i]; e; e = e->next) {
	    if (e->count > 0) {
    	        fprintf(hpfile, "  ");
		OutputSpecial(hpfile, e->name);
    	        fprintf(hpfile, " %d\n", 4 * (e->count));
	    }
        }   
    }
    fflush(hpfile);
}
 
extern int hashon;

STATIC void 
AddEntry(mpc, size, glob)
struct hprofinfo *mpc;
int size;
HPnodefwd *glob;
{
    char* name;
    int h;
    struct entry* e;

    if (!mpc || mpc->hp_magic != HEAPMAGIC) {
	fprintf(stderr, "Bad heap profile data encountered %lx %lx %d!=%d\n", (UInt)glob, (UInt)mpc, mpc?mpc->hp_magic:0, HEAPMAGIC);
#if DUMP
	dumpgraph((PTR)glob->forward, 4);
	fprintf(stderr, "\n");
#endif
	finish(1);
    }
    if (someresflag) {
	if (grpresflag && !inres(mpc->hp_info[GROUP], grpres)) return;
	if (modresflag && !inres(mpc->hp_info[MODULE], modres)) return;
	if (proresflag && !inres(mpc->hp_info[PRODUCER], prores)) return;
	if (conresflag && !inres(mpc->hp_info[CONSTRUCTION], conres)) return;
	if (typresflag && !inres(mpc->hp_info[TYPE], typres)) return;
    }
    name = mpc->hp_info[hashon];
    h = Hash(name);
    for (e = profiletable[h]; e; e = e->next) {
        if (strcmp(e->name, name) == 0) {
	    break;
        }
    }
    if (e) {
       e->count += size;
    } else {
	if (*name == '\0') {
	    fprintf(stderr, "Strange heap profile entry: '%s' '%s' '%s' '%s' '%s'\n",
		    mpc->hp_info[GROUP], mpc->hp_info[MODULE], mpc->hp_info[PRODUCER], 
		    mpc->hp_info[CONSTRUCTION], mpc->hp_info[TYPE]);
	}
	e = MakeEntry(name, size); 
	e->next = profiletable[h];
	profiletable[h] = e;
    }
}

/****************************************************************************
 *                                                                          *
 *		New stuff for dealing with restrictions                     *
 *								            *
 ****************************************************************************/

void InputSpecial PROTO((char *));

STATIC void
getres(s, res)
char* s;
struct restrlist **res;
{
    int ignorecomma;
    struct restrlist *rl;
    char *p;

    if (*s == '{') s++;
    for (; *s && *s != '}'; ) {
        while (iswhite(*s)) 
	    s++;
	for (p = s, ignorecomma = 0; *s && *s != '}' && (ignorecomma || *s != ','); s++) {
	    if (*s == '(') ignorecomma++;
	    if (*s == ')') ignorecomma--;
	}
	p = copynstring(p, s-p);
	InputSpecial(p);
	rl = (struct restrlist*)xmalloc(sizeof(struct restrlist));
	rl->name = p;
	rl->next = *res;
	*res = rl;
	if (*s == ',') s++;
    }
}

void
putres(fp, res)
FILE* fp;
struct restrlist *res;
{
    struct restrlist *p;

    if (!res) 
	return;	/* don't print empty restrictions */

    fprintf(fp, "{");
    for (p = res; p; p = p->next) {
	OutputSpecial(fp, p->name);
	if (p->next)
	    fprintf(fp, ",");
    }
    fprintf(fp, "}");
}

int 
inres(name, res)
char* name;
struct restrlist *res;
{
    struct restrlist *p;

    for(p = res; p; p = p->next)
	if (strcmp(name, p->name) == 0)
	    return 1;
    return 0;
}

/*
 *	Input and output of special operators
 */

static int member();

STATIC void OutputSpecial(fp,op)
FILE* fp;
char* op;
{
    if (member(op[0], "+-*/@.:~|&=><^%?!")) {
	fprintf(fp, "(%s)", op);
    } else if (op[0] == 'P' && op[1] == '#') {
	switch (op[2]) {
        case '2' :
            fprintf(fp, "(,)");
	    break;
        case '3' :
            fprintf(fp, "(,,)");
	    break;
        case '4' :
            fprintf(fp, "(,,,)");
	    break;
	case '5':
            fprintf(fp, "(,,,,)");
            break;
        case '6' :
            fprintf(fp, "(,,,,,)"); 
            break;
        case '7' :
            fprintf(fp, "(,,,,,,)"); 
            break;
        case '8' :
            fprintf(fp, "(,,,,,,,)"); 
            break;
        case '9' :
            fprintf(fp, "(,,,,,,,,)"); 
            break;
        default  :
            fprintf(fp, "%s", op);
	    break;
        }
    } else {
	while(*op) {
	    if (*op == ' ')
		putc('_', fp);
	    else
		putc(*op, fp);
	    op++;
	}
    }
}

void InputSpecial(op)
char* op;
{ 
    if (op[0] != '(') return;

    if (strcmp(op,"(,)") == 0) {
	sprintf(op, "P#2");
    } else if (strcmp(op,"(,,)") == 0) {
	sprintf(op, "P#3");
    } else if (strcmp(op,"(,,,)") == 0) {
	sprintf(op, "P#4");
    } else if (strcmp(op,"(,,,,)") == 0) {
	sprintf(op, "P#5");
    } else if (strcmp(op,"(,,,,,)") == 0) {
	sprintf(op, "P#6");
    } else if (strcmp(op,"(,,,,,,)") == 0) {
	sprintf(op, "P#7");
    } else if (strcmp(op,"(,,,,,,,)") == 0) {
	sprintf(op, "P#8");
    } else if (strcmp(op,"(,,,,,,,,)") == 0) {
	sprintf(op, "P#9");
    } else {	/* discard brackets */
	int i;

	for (i = 1; op[i] != ')' && op[i] != '\0'; i++) {
	    op[i-1] = op[i];
	}
	op[i-1] = '\0';
   }
}


static int member(c,cs)
char c; char* cs;
{
    for (; *cs; cs++) {
	if (c == *cs) return 1;
    }

    return 0;
}

int
decodehparg(ap)
char *ap;
{
    static struct {
	int hash;
	struct restrlist **res;
	int *flag;
    } restrs[] = { { GROUP,        &grpres, &grpresflag },
		   { MODULE,       &modres, &modresflag },
		   { PRODUCER,     &prores, &proresflag },
		   { CONSTRUCTION, &conres, &conresflag },
		   { TYPE,         &typres, &typresflag },
		   };
    char *p = "gmpct";
    int i = strchr(p, *ap++) - p;

    someresflag++;
    if (!profiling) {
	profiling++;
	hashon = restrs[i].hash;
    }
    if (*ap) {
	(*restrs[i].flag)++; 
	getres(ap, restrs[i].res);
	return 1;
    } else
	return 0;
}
#else
/* Just to stop compiler warnings */
int _dummy_sample = 1;
#endif
