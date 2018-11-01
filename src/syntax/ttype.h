#ifndef ttype_defined
#define ttype_defined
typedef enum {
	tname,
	tvar,
	tstrict,
	tcontext,
	tsels,
	tap,
	tnamek
} Tttype;

typedef struct { Tttype tag; } *ttype;

/* Compatibility defines */
extern Tttype tttype();

#endif
extern ttype mktname();
extern id *Rgtypeid();
#define gtypeid(xyzxyz) (*Rgtypeid(xyzxyz))
extern list *Rgtypel();
#define gtypel(xyzxyz) (*Rgtypel(xyzxyz))

extern ttype mktvar();
extern int *Rgtvar();
#define gtvar(xyzxyz) (*Rgtvar(xyzxyz))

extern ttype mktstrict();
extern ttype *Rgtstrict();
#define gtstrict(xyzxyz) (*Rgtstrict(xyzxyz))

extern ttype mktcontext();
extern list *Rgcontexts();
#define gcontexts(xyzxyz) (*Rgcontexts(xyzxyz))
extern ttype *Rgctype();
#define gctype(xyzxyz) (*Rgctype(xyzxyz))

extern ttype mktsels();
extern list *Rgselidstys();
#define gselidstys(xyzxyz) (*Rgselidstys(xyzxyz))

extern ttype mktap();
extern ttype *Rgtapvar();
#define gtapvar(xyzxyz) (*Rgtapvar(xyzxyz))
extern list *Rgtaptypel();
#define gtaptypel(xyzxyz) (*Rgtaptypel(xyzxyz))

extern ttype mktnamek();
extern id *Rgtypeidk();
#define gtypeidk(xyzxyz) (*Rgtypeidk(xyzxyz))
extern kind *Rgkindk();
#define gkindk(xyzxyz) (*Rgkindk(xyzxyz))
extern list *Rgtypelk();
#define gtypelk(xyzxyz) (*Rgtypelk(xyzxyz))

