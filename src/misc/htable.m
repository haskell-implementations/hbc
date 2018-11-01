module
export makehtbl, lookuphtbl;

-- String hashing stuff, move to own module later.
#ifndef USELIST
-- New array version
#define HASH 55
#else
#define HASH 11
#endif

#ifndef USELIST
#define GET(p,i) ((p) ? (i))
#else
#define GET(p,i) (select ((i)+1) (p))
#endif

rec makehtbl xys = hashl xys
and lookuphtbl tab v d = hlookfor v (GET(tab,hash v)) d
and hlookfor v [] d = d
||  hlookfor v ((s,y).r) d = if v=s then y else hlookfor v r d

and hash [] = 0			-- Should never occur
||  hash [c1] = ord c1 % HASH
||  hash [c1;c2] = ord c2 % HASH
||  hash [c1;c2;c3] = (ord c2 + ord c3) % HASH
||  hash [c1;c2;c3;c4] = (ord c2 + ord c3 + ord c4) % HASH
||  hash [c1;c2;c3;c4;c5] = (ord c2 + ord c3 + ord c4 + ord c5) % HASH
||  hash (c1.c2.c3.c4.c5.c6._) = (ord c2 + ord c3 + ord c4 + ord c5 + ord c6) % HASH
#ifndef USELIST
and hashl xys = array 0 (HASH-1) (\x.x) (map (\ (xy as (s,_)).(hash s, xy)) xys)
#else
and hashl xys = reduce (\(xy as (s,_)).\ps. adde (hash s) ps xy) (rept HASH []) is
and adde 0 (x.xs) d = (d.x).xs
||  adde n (x.xs) d = x.adde (n-1) xs d
#endif

end
