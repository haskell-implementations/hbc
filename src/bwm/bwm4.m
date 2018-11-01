module
#ifdef BWM
#include "../expr/id.t"
#include "../expr/constr_t.t"
#include "../expr/constrfun.t"
#include "../funnos.h"
#include "bwm2.t"
export bwmcode, Bwm, Bmode, show_Bwms;
rec type Bwm =
    BUILD      (List Bmode) +
    BUILDN     (List Bmode) +
    UPDATE Int (List Bmode) +
    PUSH       (List Bmode) +
    CASE       (List Bmode) (List (List Bwm)) +
    FUN    Id  Int (List Bwm) +
    SMALL      (List Bwm)   +
    CODE       BwmC         +
    ALU	       Int (List Bmode)

and type Bmode =
    Bstack Int +
    Bheap  Int +
    Blit   Id  +
    Bcon   Int Int Int +
    Bint   Int +
    Bnoop      +
    Bunused    +
    Balures

and show_Bmode (Bstack k) = "%"@itos k
||  show_Bmode (Bheap  k) = "#"@itos k
||  show_Bmode (Blit   i) = "$"@prid i
||  show_Bmode (Bcon i k _) = "$"@itos i@"_"@itos k
||  show_Bmode (Bint   i) = "$"@itos i
||  show_Bmode (Bnoop   ) = "_"
||  show_Bmode (Bunused ) = "?"
||  show_Bmode (Balures ) = "alures"

and show_Bwm (BUILD  bs)   = "\tBUILD\t\t" @mix (map show_Bmode bs) ", "@"\n"
||  show_Bwm (BUILDN bs)   = "\tBUILDN\t\t" @mix (map show_Bmode bs) ", "@"\n"
||  show_Bwm (UPDATE i bs) = "\tUPDATE\t"  @itos i@"\t"@mix (map show_Bmode bs) ", "@"\n"
||  show_Bwm (PUSH   bs)   = "\tPUSH\t\t"  @mix (map show_Bmode bs) ", "@"\n"
||  show_Bwm (CASE   bs xs)= "\tCASE\t\t"  @mix (map show_Bmode bs) ", " @ ", $JTAB\n"@
                             conc (map2 (\bs.\n. itos n@":\n"@show_Bwms bs) xs (from 0)) @ "\tENDCASE\n"
||  show_Bwm (FUN i k bs)  = "\n"@prid i@":\t"@itos k@"\n"@show_Bwms bs
||  show_Bwm (SMALL bs)    = "# combine\n"@show_Bwms bs
||  show_Bwm (CODE  _)     = "# BC\n"
||  show_Bwm (ALU i bs)    = "\tALU\t"@itos i@"\t"@mix (map show_Bmode bs) ", "@"\n"
and show_Bwms bs = concmap show_Bwm bs

and bwmcode dss = concmap (map bwmfun) dss
and bwmfun (f, is, e) = FUN (bvar2id f) (length is) (hadj 0 (bcode (length is) e))
and bcode d (Blet ies e) = concmap bcap ies @ bcode d e
||  bcode d (Bcase e ts cies) = (if null ts then [] else [PUSH (map bv ts)]) @ cASE (bms e) (map (bcase (d+length ts)) cies)
||  bcode d b  = [UPDATE d (bms b)]

and cASE [Balures] [bs] = PUSH [Balures] . bs
||  cASE bs xss = [CASE bs xss]

and bcase d (c, is, e) = bcode (d+length is+1) e

and bcap (v, (Bapply _ [Bapply i es])) =
    /* We need to do arithmetic */
    if length es = 2 then
	[ALU (id_no (bvar2id i)) (map bm es) ] @ bldalu v
    else if id_no (bvar2id i) = Fneg then
	[ALU Fsub [Bint 0; bm (hd es)] ] @ bldalu v
    else
	fail "unary"
||  bcap (_, b) = [BUILD (bms b)]
and bldalu (Bvalures) = []
||  bldalu _ = [ BUILD [Balures] ]
and bms (Bapply v es) = bv v.map bm es
||  bms (Bvar v)      = [bv v]
||  bms e             = fail ("No match in bms "@show_Bexpr e)
and bm (Bvar v) = bv v
||  bm _ = trace "special bm" (Bstack 0)
and bv (Bvstack k _) = Bstack k
||  bv (Bvheap  k _) = Bheap  k
||  bv (Bvglob    i) = Blit   i
||  bv (Bvalures   ) = Balures
||  bv (Bvconstr i j c) = 
    case constrtype c in
	Gint : Bint i
    ||  Gchar: Bint i
    ||  Gtype: Bcon i j (nconstrs c)
    ||  _    : fail "Unimplemented BWM type"
    end
and hadj _ [] = []
||  hadj n (b.bs) = adj n b.hadj (hinc n b) bs
and hinc n (BUILD _) = n+1
||  hinc n _         = n
and adj n (BUILD    bs) = BUILD    (map (adjm n) bs)
||  adj n (UPDATE i bs) = UPDATE i (map (adjm n) bs)
||  adj n (PUSH     bs) = PUSH     (map (adjm n) bs)
||  adj n (CASE  bs xs) = CASE     (map (adjm n) bs) (map (hadj 0) xs)
||  adj _ i           = i
and adjm n (Bheap k) = Bheap (n-k)
||  adjm _ b         = b
#else
export ;
dummy=0
#endif
end
