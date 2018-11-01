#ifndef finfot_defined
#define finfot_defined
typedef enum {
	nofinfo,
	finfo,
	hfinfo
} Tfinfot;

typedef struct { Tfinfot tag; } *finfot;

/* Compatibility defines */
extern Tfinfot tfinfot();

#endif
extern finfot mknofinfo();

extern finfot mkfinfo();
extern id *Rfi1();
#define fi1(xyzxyz) (*Rfi1(xyzxyz))
extern id *Rfi2();
#define fi2(xyzxyz) (*Rfi2(xyzxyz))
extern int *Rfi3();
#define fi3(xyzxyz) (*Rfi3(xyzxyz))

extern finfot mkhfinfo();
extern list *Rpfinline();
#define pfinline(xyzxyz) (*Rpfinline(xyzxyz))
extern id *Rpfstrict();
#define pfstrict(xyzxyz) (*Rpfstrict(xyzxyz))
extern list *Rpfentry();
#define pfentry(xyzxyz) (*Rpfentry(xyzxyz))
extern int *Rpfarity();
#define pfarity(xyzxyz) (*Rpfarity(xyzxyz))
extern int *Rpffrsize();
#define pffrsize(xyzxyz) (*Rpffrsize(xyzxyz))
extern list *Rpfinsts();
#define pfinsts(xyzxyz) (*Rpfinsts(xyzxyz))
extern int *Rpfevaled();
#define pfevaled(xyzxyz) (*Rpfevaled(xyzxyz))

