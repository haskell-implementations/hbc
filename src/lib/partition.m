/*
**	partition:	partition a list with a predicate
**
*/
module
export	partition;
--    partition f l = (filter f l, filter (not o f) l)
rec partition f [] = ([], [])
||  partition f (x.xs) =
    let (ys, ns) = partition f xs in
    if f x then (x.ys, ns) else (ys, x.ns)
end
