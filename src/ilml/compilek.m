module
#include "cexpr.t"
#include "cons.t"
#include "imisc.t"
export compk;
-- Compile constructor application
rec type Pairs = Pair0 Univ Univ + Pair1 Univ Univ + Pair2 Univ Univ + Pair3 Univ Univ + Pair4 Univ Univ
and type Trips = Trip0 Univ Univ Univ + Trip1 Univ Univ Univ + Trip2 Univ Univ Univ + Trip3 Univ Univ Univ + Trip4 Univ Univ Univ
/*#define UNIV(x) ((x){# NOTCHK #})*/

and compk :: Cexpr -> List Univ -> Univ
and compk (e as Konstr k m bs) es = if Or bs then comp e (strictify bs es) else comp e es
and comp :: Cexpr -> List Univ -> Univ
and comp (Konstr k 0 _)     [] = ktag0 k
||  comp (Konstr k 1 _)    [e] = ktag k e
||  comp (Konstr 0 2 _)  [x;y] = (Pair0 x y){# NOTCHK #}
||  comp (Konstr 1 2 _)  [x;y] = (Pair1 x y){# NOTCHK #}
||  comp (Konstr 2 2 _)  [x;y] = (Pair2 x y){# NOTCHK #}
||  comp (Konstr 3 2 _)  [x;y] = (Pair3 x y){# NOTCHK #}
||  comp (Konstr 4 2 _)  [x;y] = (Pair4 x y){# NOTCHK #}
||  comp (Konstr k 2 _)  [x;y] = (ktpair k) x y
||  comp (Konstr 0 3 _)  [x;y;z] = (Trip0 x y z){# NOTCHK #}
||  comp (Konstr 1 3 _)  [x;y;z] = (Trip1 x y z){# NOTCHK #}
||  comp (Konstr 2 3 _)  [x;y;z] = (Trip2 x y z){# NOTCHK #}
||  comp (Konstr 3 3 _)  [x;y;z] = (Trip3 x y z){# NOTCHK #}
||  comp (Konstr 4 3 _)  [x;y;z] = (Trip4 x y z){# NOTCHK #}
||  comp (Konstr k 3 _)  [x;y;z] = (kppair k) x y z
||  comp (Konstr k m _)  es =
    if k < 5 then
	doap (kpvek k m) es
    else
	doap (ktvek k m) es
-- kpvek&ktvek are variadic functions and require AP nodes in the right places to work
and doap :: Univ -> List Univ -> Univ
and doap e [] = e
||  doap f (x.xs) = doap ((f x){# NOTCHK #}) xs

and strictify :: List Bool -> List Univ -> List Univ
and strictify [] [] = []
||  strictify (false.bs) (x.xs) = 
    let xs' = strictify bs xs in        seq xs' (x.xs')
||  strictify (true.bs) (x.xs) =
    let xs' = strictify bs xs in seq x (seq xs' (x.xs'))
end
