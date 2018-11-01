module PreludeX(Enum(..)) where
class  Enum a  where
    toEnum		:: Int -> a
    fromEnum		:: a -> Int
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

--    enumFrom n		=  map toEnum (from (fromEnum n))
--    enumFromThen n' m'	=  let n = fromEnum n'; m = fromEnum m' in map toEnum (fromi n (m - n))
    enumFromTo n m	=  map toEnum (irange (fromEnum n) (fromEnum m))
    enumFromThenTo n' m' p'
			=  let n = fromEnum n'; m = fromEnum m'; p = fromEnum p' in 
			   if m > n then map toEnum (iranges n (m-n) p) else map toEnum (irranges n (m-n) p)

{-
from :: Int -> [Int]
from n = n : from (n+1)
fromi :: Int -> Int -> [Int]
fromi n i = n : fromi (n+i) i
-}
irange :: Int->Int->[Int]
irange l h = if l > h then [] else l : irange (l+1) h
iranges :: Int->Int->Int->[Int]
iranges l i h = if l > h then [] else l : iranges (l+i) i h
irranges :: Int->Int->Int->[Int]
irranges l i h = if l < h then [] else l : irranges (l+i) i h
