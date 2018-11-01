module I_Float() where
instance  Enum Float where
	enumFrom x = enumFromBy x 1.0
	enumFromThen x y = enumFromBy x (y - x)

enumFromBy::Float->Float->[Float]
enumFromBy n k		=  n : enumFromBy (n+k) k
