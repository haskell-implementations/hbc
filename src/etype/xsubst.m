module
#include "../expr/types_t.t"
#include "../expr/ttype_t.t"
#include "../expr/ttype.t"
#include "../expr/id.t"
#include "../misc/flags.t"
#include "../type/subst_t.t"
#include "../type/subst.t"
#include "../type/unify.t"
export instsubstTR, prsubst, TRvbind, addTRs, TRCgs, prvb, instCgsTR, appCgsT ;
rec instsubstTR T (ok _ al s) = 
        let hack (x as (a,mktvar b)) = if a < b 
                                       then (b,mktvar a) 
   				       else x
         || hack x = x
        and chass (mkassert i vs) = mkassert i (map (\a.
						 let mktvar n = T (mktvar a) 
						 in n) vs)
	 || chass err = err
	in 
	   ok []
	      (map chass al)
	      (map (\(a,t).let mktvar n = T (mktvar a) in  hack (n,T t)) s)

and TRvbind V vb = mapsnd (TRtype V) vb

and addTRs idTs S = reduce addTR S idTs 

and prsubst s = prTR s

and TRCgs s (mkcnt t)          = mkcnt (TRtype s t)  
||  TRCgs s (mkct c)           = mkct c 
||  TRCgs s (mkctint c)        = mkctint c 
||  TRCgs s (mkctid c)         = mkctid c 
||  TRCgs s (mkctsym c)        = mkctsym c
||  TRCgs s (mklist1 t cgss n) = mklist1 (TRtype s t) (map (TRCgs s) cgss) n
||  TRCgs s (mklist0 t cgss)   = mklist0 (TRtype s t) (map (TRCgs s) cgss) 
||  TRCgs s (mklistend n b)    = mklistend n b 

and instCgsTR T s (mkcnt t)          = mkcnt (TRtype s (T t))
||  instCgsTR T s (mkct c)           = mkct c
||  instCgsTR T s (mkctint c)        = mkctint c
||  instCgsTR T s (mkctid c)         = mkctid c
||  instCgsTR T s (mkctsym c)        = mkctsym c
||  instCgsTR T s (mklist1 t cgss n) = mklist1 (TRtype s (T t)) 
                                               (map (instCgsTR T s) cgss) 
     					       n
||  instCgsTR T s (mklist0 t cgss)   = mklist0 (TRtype s (T t))
 				               (map (instCgsTR T s) cgss) 
||  instCgsTR T s (mklistend n b)    = mklistend n b 

and appCgsT T (mkcnt t)          = mkcnt (T t)
||  appCgsT T (mkct c)           = mkct c
||  appCgsT T (mkctint c)        = mkctint c
||  appCgsT T (mkctid c)         = mkctid c
||  appCgsT T (mkctsym c)        = mkctsym c
||  appCgsT T (mklist1 t cgss n) = mklist1 (T t) 
                                           (map (appCgsT T) cgss) 
     					   n
||  appCgsT T (mklist0 t cgss)   = mklist0 (T t)
 				           (map (appCgsT T) cgss) 
||  appCgsT T (mklistend n b)    = mklistend n b 


and prvb1 (id,t) = "("@prid id @ "," @ prttype t @ ")"
and prvb vb = "[" @ concmap prvb1 vb @ "]"

end
