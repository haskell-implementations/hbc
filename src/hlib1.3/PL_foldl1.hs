module PreludeX where
foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 _ []		=  error "PreludeList.foldl1: empty list"
foldl1 f (x:xs)		=  foldl f x xs
