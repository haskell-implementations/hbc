module PreludeX where
minimum		:: (Ord a) => [a] -> a
minimum	[x]	= x
minimum (x:xs)	= min x (minimum xs)
