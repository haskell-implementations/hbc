module PreludeX() where
#define chr {-:"P_chr":-}
#define ord {-:"P_ord":-}
instance Enum Char where
	toEnum i = chr i
	fromEnum i = ord i
	enumFrom x		= enchr (ord x) 1 255
	enumFromThen x y	= if y >= x then enchr (ord x) (ord y - ord x) 255 else enchrr (ord x) (ord y - ord x) 0
	enumFromTo x y		= enchr (ord x) 1 (ord y)
	enumFromThenTo x y z	= if y >= x then enchr (ord x) (ord y - ord x) (ord z) else enchrr (ord x) (ord y - ord x) (ord z)

enchr s i e = if s > e then [] else chr s : enchr (s+i) i e
enchrr s i e = if s < e then [] else chr s : enchrr (s+i) i e
