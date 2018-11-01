module PreludeX where
scanl1			:: (a -> a -> a) -> [a] -> [a]
scanl1 _ []		=  error "PreludeList.scanl1: empty list"
scanl1 f (x:xs)		=  scanl f x xs
