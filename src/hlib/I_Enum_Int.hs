module I_Enum_Int() where
instance Enum Int where
	enumFrom n = from n
	enumFromThen n m = fromi n (m - n)
	enumFromTo n m = irange n m
	enumFromThenTo n m p = if m > n then iranges n (m-n) p else irranges n (m-n) p

irange :: Int->Int->[Int]
irange l h = if l > h then [] else l : irange (l+1) h
from :: Int -> [Int]
from n = n : from (n+1)
fromi :: Int -> Int -> [Int]
fromi n i = n : fromi (n+i) i
iranges :: Int->Int->Int->[Int]
iranges l i h = if l > h then [] else l : iranges (l+i) i h
irranges :: Int->Int->Int->[Int]
irranges l i h = if l < h then [] else l : irranges (l+i) i h
