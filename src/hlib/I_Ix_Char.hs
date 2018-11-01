module I_Ix_Char() where
enchr s i e = if s > e then [] else chr s : enchr (s+i) i e

instance Ix Char where
	range (l,h) = enchr (ord l) 1 (ord h)
	index (l,h) i = ord i - ord l
	inRange (l,h) i = l <= i && i <= h
