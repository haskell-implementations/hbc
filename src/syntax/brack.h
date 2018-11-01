#ifndef brack_defined
#define brack_defined
typedef enum {
	normal,
	forget
} Tbrack;

typedef struct { Tbrack tag; } *brack;

/* Compatibility defines */
extern Tbrack tbrack();

#endif
extern brack mknormal();
extern list *Rgconstrn();
#define gconstrn(xyzxyz) (*Rgconstrn(xyzxyz))
extern int *Rgprecn();
#define gprecn(xyzxyz) (*Rgprecn(xyzxyz))
extern assoc *Rgassn();
#define gassn(xyzxyz) (*Rgassn(xyzxyz))

extern brack mkforget();
extern list *Rgconstrf();
#define gconstrf(xyzxyz) (*Rgconstrf(xyzxyz))
extern int *Rgprecf();
#define gprecf(xyzxyz) (*Rgprecf(xyzxyz))
extern assoc *Rgassf();
#define gassf(xyzxyz) (*Rgassf(xyzxyz))

