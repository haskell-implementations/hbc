module PreludeX where
maximum		:: (Ord a) => [a] -> a
maximum	[x]	= x
maximum (x:xs)	= max x (maximum xs)
