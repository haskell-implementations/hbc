#ifndef pair_defined
#define pair_defined
typedef enum {
	ppair
} Tpair;

typedef struct { Tpair tag; } *pair;

/* Compatibility defines */
extern Tpair tpair();

#endif
extern pair mkppair();
extern voidptr *Rpfst();
#define pfst(xyzxyz) (*Rpfst(xyzxyz))
extern voidptr *Rpsnd();
#define psnd(xyzxyz) (*Rpsnd(xyzxyz))

