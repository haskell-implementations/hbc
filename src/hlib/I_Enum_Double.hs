module I_Double() where
instance  Enum Double where
	enumFrom x = enumFromBy x 1.0
	enumFromThen x y = enumFromBy x (y - x)

enumFromBy::Double->Double->[Double]
enumFromBy n k		=  n : enumFromBy (n+k) k
