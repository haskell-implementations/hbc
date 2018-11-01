#ifndef tree_defined
#define tree_defined
typedef enum {
	module,
	ident,
	integer,
	charr,
	string,
	floatt,
	bignum,
	ratnum,
	tuple,
	ap,
	lam,
	cexpr,
	letv,
	casee,
	par,
	as,
	condp,
	lazyp,
	restr,
	eannot,
	listf,
	listg,
	hmodule,
	wherev,
	doexp,
	record
} Ttree;

typedef struct { Ttree tag; } *tree;

/* Compatibility defines */
extern Ttree ttree();

#endif
extern tree mkmodule();
extern list *Rgimplist();
#define gimplist(xyzxyz) (*Rgimplist(xyzxyz))
extern list *Rgexplist();
#define gexplist(xyzxyz) (*Rgexplist(xyzxyz))
extern list *Rgmodlist();
#define gmodlist(xyzxyz) (*Rgmodlist(xyzxyz))

extern tree mkident();
extern id *Rgident();
#define gident(xyzxyz) (*Rgident(xyzxyz))

extern tree mkinteger();
extern int *Rginteger();
#define ginteger(xyzxyz) (*Rginteger(xyzxyz))

extern tree mkcharr();
extern id *Rgchar();
#define gchar(xyzxyz) (*Rgchar(xyzxyz))

extern tree mkstring();
extern id *Rgstring();
#define gstring(xyzxyz) (*Rgstring(xyzxyz))

extern tree mkfloatt();
extern double *Rgfloat();
#define gfloat(xyzxyz) (*Rgfloat(xyzxyz))

extern tree mkbignum();
extern id *Rgbignum();
#define gbignum(xyzxyz) (*Rgbignum(xyzxyz))

extern tree mkratnum();
extern id *Rgratnum();
#define gratnum(xyzxyz) (*Rgratnum(xyzxyz))

extern tree mktuple();
extern list *Rgtuplelist();
#define gtuplelist(xyzxyz) (*Rgtuplelist(xyzxyz))

extern tree mkap();
extern tree *Rgfun();
#define gfun(xyzxyz) (*Rgfun(xyzxyz))
extern tree *Rgarg();
#define garg(xyzxyz) (*Rgarg(xyzxyz))

extern tree mklam();
extern tree *Rglamid();
#define glamid(xyzxyz) (*Rglamid(xyzxyz))
extern tree *Rglamexpr();
#define glamexpr(xyzxyz) (*Rglamexpr(xyzxyz))

extern tree mkcexpr();
extern list *Rgsymlist();
#define gsymlist(xyzxyz) (*Rgsymlist(xyzxyz))

extern tree mkletv();
extern list *Rgletvdeflist();
#define gletvdeflist(xyzxyz) (*Rgletvdeflist(xyzxyz))
extern tree *Rgletvexpr();
#define gletvexpr(xyzxyz) (*Rgletvexpr(xyzxyz))

extern tree mkcasee();
extern tree *Rgcaseexpr();
#define gcaseexpr(xyzxyz) (*Rgcaseexpr(xyzxyz))
extern list *Rgcasebody();
#define gcasebody(xyzxyz) (*Rgcasebody(xyzxyz))

extern tree mkpar();
extern tree *Rgpare();
#define gpare(xyzxyz) (*Rgpare(xyzxyz))

extern tree mkas();
extern id *Rgasid();
#define gasid(xyzxyz) (*Rgasid(xyzxyz))
extern tree *Rgase();
#define gase(xyzxyz) (*Rgase(xyzxyz))

extern tree mkcondp();
extern tree *Rgcondpp();
#define gcondpp(xyzxyz) (*Rgcondpp(xyzxyz))
extern tree *Rgcondpe();
#define gcondpe(xyzxyz) (*Rgcondpe(xyzxyz))

extern tree mklazyp();
extern tree *Rglazyp();
#define glazyp(xyzxyz) (*Rglazyp(xyzxyz))

extern tree mkrestr();
extern tree *Rgrestre();
#define grestre(xyzxyz) (*Rgrestre(xyzxyz))
extern ttype *Rgrestrt();
#define grestrt(xyzxyz) (*Rgrestrt(xyzxyz))

extern tree mkeannot();
extern tree *Rgeannote();
#define geannote(xyzxyz) (*Rgeannote(xyzxyz))
extern id *Rgeannota();
#define geannota(xyzxyz) (*Rgeannota(xyzxyz))

extern tree mklistf();
extern int *Rglistt();
#define glistt(xyzxyz) (*Rglistt(xyzxyz))
extern list *Rglistf();
#define glistf(xyzxyz) (*Rglistf(xyzxyz))

extern tree mklistg();
extern tree *Rglgg();
#define glgg(xyzxyz) (*Rglgg(xyzxyz))
extern list *Rglgq();
#define glgq(xyzxyz) (*Rglgq(xyzxyz))

extern tree mkhmodule();
extern id *Rghmodid();
#define ghmodid(xyzxyz) (*Rghmodid(xyzxyz))
extern list *Rghexp();
#define ghexp(xyzxyz) (*Rghexp(xyzxyz))
extern list *Rghimp();
#define ghimp(xyzxyz) (*Rghimp(xyzxyz))
extern list *Rghfix();
#define ghfix(xyzxyz) (*Rghfix(xyzxyz))
extern list *Rghbind();
#define ghbind(xyzxyz) (*Rghbind(xyzxyz))

extern tree mkwherev();
extern list *Rgwges();
#define gwges(xyzxyz) (*Rgwges(xyzxyz))
extern list *Rgwdefs();
#define gwdefs(xyzxyz) (*Rgwdefs(xyzxyz))

extern tree mkdoexp();
extern list *Rgstmt();
#define gstmt(xyzxyz) (*Rgstmt(xyzxyz))

extern tree mkrecord();
extern tree *Rgrecid();
#define grecid(xyzxyz) (*Rgrecid(xyzxyz))
extern list *Rgrecfields();
#define grecfields(xyzxyz) (*Rgrecfields(xyzxyz))

