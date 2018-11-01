module C_Enum(Enum(..)) where
class  (Ord a) => Enum a	where
    enumFrom		:: a -> [a]		-- [n..]
    enumFromThen	:: a -> a -> [a]	-- [n,n'..]
    enumFromTo		:: a -> a -> [a]	-- [n..m]
    enumFromThenTo	:: a -> a -> a -> [a]	-- [n,n'..m]

-- SLOW
    enumFromTo n m	=  takeWhile (<= m) (enumFrom n)
    enumFromThenTo n n' m
			=  takeWhile (if n' >= n then (<= m) else (>= m))
				     (enumFromThen n n')
