#ifndef qual_defined
#define qual_defined
typedef enum {
	qgen,
	qfilter,
	qlet
} Tqual;

typedef struct { Tqual tag; } *qual;

/* Compatibility defines */
extern Tqual tqual();

#endif
extern qual mkqgen();
extern tree *Rgqgenpat();
#define gqgenpat(xyzxyz) (*Rgqgenpat(xyzxyz))
extern tree *Rgqgenexp();
#define gqgenexp(xyzxyz) (*Rgqgenexp(xyzxyz))

extern qual mkqfilter();
extern tree *Rgqfilter();
#define gqfilter(xyzxyz) (*Rgqfilter(xyzxyz))

extern qual mkqlet();
extern binding *Rgqbinding();
#define gqbinding(xyzxyz) (*Rgqbinding(xyzxyz))

