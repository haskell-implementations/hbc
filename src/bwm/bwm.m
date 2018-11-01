module
#ifdef BWM
#include "bwm1.t"
#include "bwm2.t"
#include "bwm3.t"
#include "bwm4.t"
#include "bwm5.t"
#include "bwm6.t"
#endif
export bwm;
rec bwm u e = 
#ifdef BWM
    let e1 = bwmfillcase e in
    let bm1 = bwmconv e1 in
    let bm2 = bwmstk bm1 in
    let bc  = bwmcode bm2 in
    let bc' = bwmfix bc in
    let bo  = bwmcoder bc' in
    show_Bmodule bm2@"\n"@
    show_Bwms bc'@
    show_BwmCs bo@
    "-------cut-------\n"@
    prcode bo
#else
    fail "not compiled with -DBWM"
#endif
end
