#ifndef atype_defined
#define atype_defined
typedef enum {
	atc
} Tatype;

typedef struct { Tatype tag; } *atype;

/* Compatibility defines */
extern Tatype tatype();

#endif
extern atype mkatc();
extern id *Rgatcid();
#define gatcid(xyzxyz) (*Rgatcid(xyzxyz))
extern list *Rgatcctx();
#define gatcctx(xyzxyz) (*Rgatcctx(xyzxyz))
extern list *Rgatctypel();
#define gatctypel(xyzxyz) (*Rgatctypel(xyzxyz))

