#ifndef impstuff_defined
#define impstuff_defined
typedef enum {
	import,
	ispec,
	irename,
	ifix,
	inone,
	isome,
	interface,
	itypinfo
} Timpstuff;

typedef struct { Timpstuff tag; } *impstuff;

/* Compatibility defines */
extern Timpstuff timpstuff();

#endif
extern impstuff mkimport();
extern id *Rgimodid();
#define gimodid(xyzxyz) (*Rgimodid(xyzxyz))
extern list *Rgiimps();
#define giimps(xyzxyz) (*Rgiimps(xyzxyz))
extern list *Rgifixes();
#define gifixes(xyzxyz) (*Rgifixes(xyzxyz))
extern list *Rgients();
#define gients(xyzxyz) (*Rgients(xyzxyz))
extern impstuff *Rgispec();
#define gispec(xyzxyz) (*Rgispec(xyzxyz))
extern list *Rgirename();
#define girename(xyzxyz) (*Rgirename(xyzxyz))
extern int *Rg3qual();
#define g3qual(xyzxyz) (*Rg3qual(xyzxyz))
extern list *Rg3as();
#define g3as(xyzxyz) (*Rg3as(xyzxyz))

extern impstuff mkispec();
extern int *Rgiexpose();
#define giexpose(xyzxyz) (*Rgiexpose(xyzxyz))
extern list *Rgiids();
#define giids(xyzxyz) (*Rgiids(xyzxyz))

extern impstuff mkirename();
extern id *Rgirensrc();
#define girensrc(xyzxyz) (*Rgirensrc(xyzxyz))
extern id *Rgirendst();
#define girendst(xyzxyz) (*Rgirendst(xyzxyz))

extern impstuff mkifix();
extern list *Rgifixids();
#define gifixids(xyzxyz) (*Rgifixids(xyzxyz))
extern int *Rgifixass();
#define gifixass(xyzxyz) (*Rgifixass(xyzxyz))
extern int *Rgifixprec();
#define gifixprec(xyzxyz) (*Rgifixprec(xyzxyz))

extern impstuff mkinone();

extern impstuff mkisome();
extern list *Rgisome();
#define gisome(xyzxyz) (*Rgisome(xyzxyz))

extern impstuff mkinterface();
extern id *Rgiimodid();
#define giimodid(xyzxyz) (*Rgiimodid(xyzxyz))
extern list *Rgiiimps();
#define giiimps(xyzxyz) (*Rgiiimps(xyzxyz))
extern list *Rgiifixes();
#define giifixes(xyzxyz) (*Rgiifixes(xyzxyz))
extern list *Rgiients();
#define giients(xyzxyz) (*Rgiients(xyzxyz))

extern impstuff mkitypinfo();
extern int *Rgincon();
#define gincon(xyzxyz) (*Rgincon(xyzxyz))
extern int *Rgiflat();
#define giflat(xyzxyz) (*Rgiflat(xyzxyz))

