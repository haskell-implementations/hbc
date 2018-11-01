module
#include "id_t.t"
#include "id.t"
export itnil, itadd1, itlookupdef, itlookup, itmake;
#ifndef USELIST
-- New array version
rec type Idtab *a = idtab (LArray (List (Id # *a))) Int
and np n = array 0 (n-1) (\x.x) []
and itnil h = idtab (np h) h
and itadd1 (idtab a h) (i as mkid n _ _ _) v = idtab (array 0 (h-1) conc ((n%h, [(i,v)]) . [(i,a?i);; i<-[lowerbound a..upperbound a]])) h
and itlookupdef (idtab l h) (i as mkid n _ _ _) d = look (l ? (n%h)) n d
and itlookup l i = itlookupdef l i (fail ("itlookup "@idtostr i))
and itmake h l = idtab (array 0 (h-1) (\x.x) (map (\(d as (mkid n _ _ _, _)).(n%h, d)) l)) h
and look [] _ d = d
||  look ((mkid m _ _ _, v).is) n d = if m = n then v else look is n d
#else
#define HASH 9
rec type Idtab *a = idtab (List (List (Id # *a)))
and np = rept HASH []
and itnil _ = idtab np
and itadd1 (idtab l) (i as mkid n _ _ _) v = idtab (add (n%HASH) (i,v) l)
and itlookupdef (idtab l) (i as mkid n _ _ _) d = look (select (n%HASH+1) l) n d
and itlookup l i = itlookupdef l i (fail ("itlookup "@idtostr i))
and itmake _ l = idtab (reduce (\(d as (mkid n _ _ _, _)).\ps. add (n%HASH) d ps) np l)

and add 0 p (x.xs) = (p.x).xs
||  add n p (x.xs) = x.add (n-1) p xs
and look [] _ d = d
||  look ((mkid m _ _ _, v).is) n d = if m = n then v else look is n d
#endif
end
