#ifndef expidt_defined
#define expidt_defined
typedef enum {
	expid,
	expdd,
	exppdd,
	expl
} Texpidt;

typedef struct { Texpidt tag; } *expidt;

/* Compatibility defines */
extern Texpidt texpidt();

#endif
extern expidt mkexpid();
extern id *Rgexpid();
#define gexpid(xyzxyz) (*Rgexpid(xyzxyz))

extern expidt mkexpdd();
extern id *Rgexpdd();
#define gexpdd(xyzxyz) (*Rgexpdd(xyzxyz))

extern expidt mkexppdd();
extern id *Rgexppdd();
#define gexppdd(xyzxyz) (*Rgexppdd(xyzxyz))

extern expidt mkexpl();
extern id *Rgexplid();
#define gexplid(xyzxyz) (*Rgexplid(xyzxyz))
extern list *Rgexpll();
#define gexpll(xyzxyz) (*Rgexpll(xyzxyz))

