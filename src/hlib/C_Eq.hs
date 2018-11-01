module C_Eq(Eq(..)) where
class Eq a where
	(==), (/=) :: a -> a -> Bool
	x /= y = if x == y then False else True
