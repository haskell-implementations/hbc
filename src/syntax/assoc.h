#ifndef assoc_defined
#define assoc_defined
typedef enum {
	leftassoc,
	rightassoc,
	nonassoc,
	bothassoc
} Tassoc;

typedef struct { Tassoc tag; } *assoc;

/* Compatibility defines */
extern Tassoc tassoc();

#endif
extern assoc mkleftassoc();

extern assoc mkrightassoc();

extern assoc mknonassoc();

extern assoc mkbothassoc();

