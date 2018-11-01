module
#include "Gcodedef_t.t"

export Gseq, Gseql, Gi, Gu;
rec
    Gseq  []   t = ([],t)
||  Gseq (f.l) t = 
    	let (c1,t1) = f t  in
    	let (c2,t2) = Gseq l t1 
	in (c1@c2, t2)
and Gseql  []   t = ([],t)
||  Gseql (f.l) t = 
    	let (c1,t1) = f t  in
    	let (c2,t2) = Gseql l t1 
	in (c1.c2, t2)
and Gi c t = (c,t)
and Gu x f (Label(n)) = f (map (\i.Label i)(count n (n+x-1))) (Label(n+x))
end
