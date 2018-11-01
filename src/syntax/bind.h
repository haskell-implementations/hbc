#ifndef binding_defined
#define binding_defined
typedef enum {
	tbind,
	pbind,
	gbind,
	abind,
	rbind,
	lbind,
	ebind,
	ibind,
	cbind,
	nbind,
	ubind,
	sbind,
	specbind,
	specinstbind,
	xbind,
	vbind
} Tbinding;

typedef struct { Tbinding tag; } *binding;

/* Compatibility defines */
extern Tbinding tbinding();

#endif
extern binding mktbind();
extern ttype *Rgtbindid();
#define gtbindid(xyzxyz) (*Rgtbindid(xyzxyz))
extern list *Rgtbindc();
#define gtbindc(xyzxyz) (*Rgtbindc(xyzxyz))
extern list *Rgtbindd();
#define gtbindd(xyzxyz) (*Rgtbindd(xyzxyz))

extern binding mkpbind();
extern list *Rgpbindl();
#define gpbindl(xyzxyz) (*Rgpbindl(xyzxyz))

extern binding mkgbind();
extern ttype *Rgtcbindid();
#define gtcbindid(xyzxyz) (*Rgtcbindid(xyzxyz))
extern list *Rgtcbindc();
#define gtcbindc(xyzxyz) (*Rgtcbindc(xyzxyz))

extern binding mkabind();
extern binding *Rgabindfst();
#define gabindfst(xyzxyz) (*Rgabindfst(xyzxyz))
extern binding *Rgabindsnd();
#define gabindsnd(xyzxyz) (*Rgabindsnd(xyzxyz))

extern binding mkrbind();
extern binding *Rgrbind();
#define grbind(xyzxyz) (*Rgrbind(xyzxyz))

extern binding mklbind();
extern binding *Rglbindfst();
#define glbindfst(xyzxyz) (*Rglbindfst(xyzxyz))
extern binding *Rglbindsnd();
#define glbindsnd(xyzxyz) (*Rglbindsnd(xyzxyz))

extern binding mkebind();
extern ttype *Rgebindid();
#define gebindid(xyzxyz) (*Rgebindid(xyzxyz))
extern ttype *Rgebindt();
#define gebindt(xyzxyz) (*Rgebindt(xyzxyz))

extern binding mkibind();
extern ttype *Rgitype();
#define gitype(xyzxyz) (*Rgitype(xyzxyz))
extern binding *Rgibindb();
#define gibindb(xyzxyz) (*Rgibindb(xyzxyz))

extern binding mkcbind();
extern ttype *Rgcbindt();
#define gcbindt(xyzxyz) (*Rgcbindt(xyzxyz))
extern binding *Rgcbindb();
#define gcbindb(xyzxyz) (*Rgcbindb(xyzxyz))

extern binding mknbind();

extern binding mkubind();
extern list *Rguids();
#define guids(xyzxyz) (*Rguids(xyzxyz))

extern binding mksbind();
extern list *Rgbsids();
#define gbsids(xyzxyz) (*Rgbsids(xyzxyz))
extern ttype *Rgbstype();
#define gbstype(xyzxyz) (*Rgbstype(xyzxyz))

extern binding mkspecbind();
extern id *Rgspecid();
#define gspecid(xyzxyz) (*Rgspecid(xyzxyz))
extern list *Rgspectypes();
#define gspectypes(xyzxyz) (*Rgspectypes(xyzxyz))

extern binding mkspecinstbind();
extern ttype *Rgspecinst();
#define gspecinst(xyzxyz) (*Rgspecinst(xyzxyz))

extern binding mkxbind();
extern ttype *Rgxbindid();
#define gxbindid(xyzxyz) (*Rgxbindid(xyzxyz))
extern list *Rgxbindc();
#define gxbindc(xyzxyz) (*Rgxbindc(xyzxyz))
extern list *Rgxbindd();
#define gxbindd(xyzxyz) (*Rgxbindd(xyzxyz))

extern binding mkvbind();
extern ttype *Rgvbindid();
#define gvbindid(xyzxyz) (*Rgvbindid(xyzxyz))
extern ttype *Rgvbindt();
#define gvbindt(xyzxyz) (*Rgvbindt(xyzxyz))
extern list *Rgvbindc();
#define gvbindc(xyzxyz) (*Rgvbindc(xyzxyz))
extern binding *Rgvbindb();
#define gvbindb(xyzxyz) (*Rgvbindb(xyzxyz))

