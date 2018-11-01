module PreludeX() where
instance  Enum Float where
	fromEnum x = truncate x
	toEnum x = fromInt x
	enumFrom x = enumFromBy x 1.0
	enumFromThen x y = enumFromBy x (y - x)
	enumFromTo x y = takeWhile (<= y) (enumFrom x)
	enumFromThenTo n n' m = takeWhile (if n' >= n then (<= m) else (>= m))
				          (enumFromThen n n')

enumFromBy::Float->Float->[Float]
enumFromBy n k		=  n : enumFromBy (n+k) k
