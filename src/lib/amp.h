#include "gmp.h"
#include "gmp-impl.h"
#include "longlong.h"

typedef struct amp_struct {
    long int tag;
    long int alloclen;
    long int size;
    unsigned long int d[1];
} AMP;

#if __STDC__
#define OP1 (AMP*)
#define OP2 (AMP*, AMP*)
#define OP3 (AMP*, AMP*, AMP*)
#define OPI (signed long int)
#define ROP1 (AMP**, AMP*)
#define ROP2 (AMP**, AMP*, AMP*)
#else
#define OP1 ()
#define OP2 ()
#define OP3 ()
#define OPI ()
#define ROP1 ()
#define ROP2 ()
#endif
AMP *amp_add OP2, *amp_sub OP2, *amp_mul OP2, *amp_div OP2, *amp_mod OP2, *amp_divmod ROP2, *amp_neg OP1, 
    *amp_from_int OPI, *amp_powm OP3, *amp_gcd OP2,
    *amp_and OP2, *amp_ior OP2, *amp_sqrt ROP1;
AMP *amp_alloc();
int amp_cmp(), amp_to_int();
void amp_str();

/*#define abs(x) ((x) < 0 ? -(x) : (x))*/

#define BASE 4294967296.0

#define AMPOF(p) ((AMP*)(p)->node11.ptr0)

