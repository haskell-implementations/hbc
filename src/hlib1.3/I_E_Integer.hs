module PreludeX() where
instance Enum Integer where
	toEnum i = toInteger i
	fromEnum i = fromInteger i
	enumFrom n = from n
	enumFromThen n m = fromi n (m - n)
	enumFromTo n m = irange n m
	enumFromThenTo n m p = if m > n then iranges n (m-n) p else irranges n (m-n) p

irange :: Integer->Integer->[Integer]
irange l h = if l > h then [] else l : irange (l+1) h
from :: Integer -> [Integer]
from n = n : from (n+1)
fromi :: Integer -> Integer -> [Integer]
fromi n i = n : fromi (n+i) i
iranges :: Integer->Integer->Integer->[Integer]
iranges l i h = if l > h then [] else l : iranges (l+i) i h
irranges :: Integer->Integer->Integer->[Integer]
irranges l i h = if l < h then [] else l : irranges (l+i) i h

