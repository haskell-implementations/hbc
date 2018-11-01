#ifndef list_defined
#define list_defined
typedef enum {
	lcons,
	lnil
} Tlist;

typedef struct { Tlist tag; } *list;

/* Compatibility defines */
extern Tlist tlist();

#endif
extern list mklcons();
extern list *Rlhd();
#define lhd(xyzxyz) (*Rlhd(xyzxyz))
extern list *Rltl();
#define ltl(xyzxyz) (*Rltl(xyzxyz))

extern list mklnil();

