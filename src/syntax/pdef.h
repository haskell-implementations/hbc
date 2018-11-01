#ifndef pbinding_defined
#define pbinding_defined
typedef enum {
	ppat
} Tpbinding;

typedef struct { Tpbinding tag; } *pbinding;

/* Compatibility defines */
extern Tpbinding tpbinding();

#endif
extern pbinding mkppat();
extern tree *Rgppat();
#define gppat(xyzxyz) (*Rgppat(xyzxyz))
extern tree *Rgpexpr();
#define gpexpr(xyzxyz) (*Rgpexpr(xyzxyz))

