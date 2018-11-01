module I_Enum_Char() where
instance Enum Char where
	enumFrom x		= enchr (ord x) 1 255
	enumFromThen x y	= if y >= x then enchr (ord x) (ord y - ord x) 255 else enchrr (ord x) (ord y - ord x) 0
	enumFromTo x y		= enchr (ord x) 1 (ord y)
	enumFromThenTo x y z	= if y >= x then enchr (ord x) (ord y - ord x) (ord z) else enchrr (ord x) (ord y - ord x) (ord z)

enchr s i e = if s > e then [] else chr s : enchr (s+i) i e
enchrr s i e = if s < e then [] else chr s : enchrr (s+i) i e
