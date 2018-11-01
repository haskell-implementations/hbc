module PreludeX() where
instance Enum Int where
	toEnum i = i
	fromEnum i = i
	enumFrom n = enumFromTo n maxBound
	enumFromThen n m = enumFromThenTo n m (if m > n then maxBound else minBound)
	enumFromTo n m = irange n m
	enumFromThenTo n m p = if m > n then iranges n (m-n) p else irranges n (m-n) p

irange :: Int->Int->[Int]
irange l h = if l > h then [] else l : irange (l+1) h
iranges :: Int->Int->Int->[Int]
iranges l i h = if l > h then [] else l : iranges (l+i) i h
irranges :: Int->Int->Int->[Int]
irranges l i h = if l < h then [] else l : irranges (l+i) i h
