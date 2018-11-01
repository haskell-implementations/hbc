#ifndef sym_defined
#define sym_defined
typedef enum {
	antiquote,
	gramterm,
	gramintterm,
	gramidterm,
	gramsymterm,
	expterm,
	expintterm,
	expidterm,
	expsymterm,
	nonterminal,
	list1,
	list0
} Tsym;

typedef struct { Tsym tag; } *sym;

/* Compatibility defines */
extern Tsym tsym();

#endif
extern sym mkantiquote();
extern tree *Rgexpr();
#define gexpr(xyzxyz) (*Rgexpr(xyzxyz))

extern sym mkgramterm();
extern char *Rgcharacter();
#define gcharacter(xyzxyz) (*Rgcharacter(xyzxyz))

extern sym mkgramintterm();
extern int *Rgcint();
#define gcint(xyzxyz) (*Rgcint(xyzxyz))

extern sym mkgramidterm();
extern id *Rgcid();
#define gcid(xyzxyz) (*Rgcid(xyzxyz))

extern sym mkgramsymterm();
extern id *Rgcsym();
#define gcsym(xyzxyz) (*Rgcsym(xyzxyz))

extern sym mkexpterm();
extern char *Rgcharacter2();
#define gcharacter2(xyzxyz) (*Rgcharacter2(xyzxyz))

extern sym mkexpintterm();
extern int *Rgcint2();
#define gcint2(xyzxyz) (*Rgcint2(xyzxyz))

extern sym mkexpidterm();
extern id *Rgcid2();
#define gcid2(xyzxyz) (*Rgcid2(xyzxyz))

extern sym mkexpsymterm();
extern id *Rgcsym2();
#define gcsym2(xyzxyz) (*Rgcsym2(xyzxyz))

extern sym mknonterminal();
extern ttype *Rgtype();
#define gtype(xyzxyz) (*Rgtype(xyzxyz))

extern sym mklist1();
extern ttype *Rgtypel1();
#define gtypel1(xyzxyz) (*Rgtypel1(xyzxyz))
extern list *Rgterms1();
#define gterms1(xyzxyz) (*Rgterms1(xyzxyz))

extern sym mklist0();
extern ttype *Rgtypel0();
#define gtypel0(xyzxyz) (*Rgtypel0(xyzxyz))
extern list *Rgterms0();
#define gterms0(xyzxyz) (*Rgterms0(xyzxyz))

