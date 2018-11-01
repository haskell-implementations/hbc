#ifndef impidt_defined
#define impidt_defined
typedef enum {
	impid,
	imptype,
	impeqtype,
	impisotype,
	impview,
	impimport,
	impsyn,
	impclass,
	impinst,
	impids,
	impctype
} Timpidt;

typedef struct { Timpidt tag; } *impidt;

/* Compatibility defines */
extern Timpidt timpidt();

#endif
extern impidt mkimpid();
extern id *Rgimpid();
#define gimpid(xyzxyz) (*Rgimpid(xyzxyz))
extern ttype *Rgimptype();
#define gimptype(xyzxyz) (*Rgimptype(xyzxyz))
extern finfot *Rgimpfinfo();
#define gimpfinfo(xyzxyz) (*Rgimpfinfo(xyzxyz))

extern impidt mkimptype();
extern ttype *Rgimptypet();
#define gimptypet(xyzxyz) (*Rgimptypet(xyzxyz))
extern list *Rgimpder();
#define gimpder(xyzxyz) (*Rgimpder(xyzxyz))
extern list *Rgimptypi();
#define gimptypi(xyzxyz) (*Rgimptypi(xyzxyz))

extern impidt mkimpeqtype();
extern ttype *Rgimpeqtype();
#define gimpeqtype(xyzxyz) (*Rgimpeqtype(xyzxyz))
extern list *Rgimpeqcon();
#define gimpeqcon(xyzxyz) (*Rgimpeqcon(xyzxyz))
extern list *Rgimpeqder();
#define gimpeqder(xyzxyz) (*Rgimpeqder(xyzxyz))

extern impidt mkimpisotype();
extern ttype *Rgimpisotype();
#define gimpisotype(xyzxyz) (*Rgimpisotype(xyzxyz))
extern list *Rgimpisocon();
#define gimpisocon(xyzxyz) (*Rgimpisocon(xyzxyz))
extern list *Rgimpisoder();
#define gimpisoder(xyzxyz) (*Rgimpisoder(xyzxyz))

extern impidt mkimpview();
extern ttype *Rgimpviewtype();
#define gimpviewtype(xyzxyz) (*Rgimpviewtype(xyzxyz))
extern ttype *Rgimpviewof();
#define gimpviewof(xyzxyz) (*Rgimpviewof(xyzxyz))
extern list *Rgimpviewcon();
#define gimpviewcon(xyzxyz) (*Rgimpviewcon(xyzxyz))

extern impidt mkimpimport();
extern id *Rgimpimpmodid();
#define gimpimpmodid(xyzxyz) (*Rgimpimpmodid(xyzxyz))
extern list *Rgimpimpexp();
#define gimpimpexp(xyzxyz) (*Rgimpimpexp(xyzxyz))
extern list *Rgimpimpren();
#define gimpimpren(xyzxyz) (*Rgimpimpren(xyzxyz))

extern impidt mkimpsyn();
extern ttype *Rgimpsynsrc();
#define gimpsynsrc(xyzxyz) (*Rgimpsynsrc(xyzxyz))
extern ttype *Rgimpsyndst();
#define gimpsyndst(xyzxyz) (*Rgimpsyndst(xyzxyz))

extern impidt mkimpclass();
extern ttype *Rgimpclasst();
#define gimpclasst(xyzxyz) (*Rgimpclasst(xyzxyz))
extern list *Rgimpclassd();
#define gimpclassd(xyzxyz) (*Rgimpclassd(xyzxyz))
extern list *Rgimpclasss();
#define gimpclasss(xyzxyz) (*Rgimpclasss(xyzxyz))

extern impidt mkimpinst();
extern ttype *Rgimpinstt();
#define gimpinstt(xyzxyz) (*Rgimpinstt(xyzxyz))
extern int *Rgimpinstd();
#define gimpinstd(xyzxyz) (*Rgimpinstd(xyzxyz))
extern id *Rgimpinstm();
#define gimpinstm(xyzxyz) (*Rgimpinstm(xyzxyz))
extern list *Rgimpinsts();
#define gimpinsts(xyzxyz) (*Rgimpinsts(xyzxyz))

extern impidt mkimpids();
extern list *Rgimpids();
#define gimpids(xyzxyz) (*Rgimpids(xyzxyz))
extern ttype *Rgimptypes();
#define gimptypes(xyzxyz) (*Rgimptypes(xyzxyz))
extern finfot *Rgimpfinfos();
#define gimpfinfos(xyzxyz) (*Rgimpfinfos(xyzxyz))

extern impidt mkimpctype();
extern ttype *Rgimpcttype();
#define gimpcttype(xyzxyz) (*Rgimpcttype(xyzxyz))
extern list *Rgimpctprod();
#define gimpctprod(xyzxyz) (*Rgimpctprod(xyzxyz))

