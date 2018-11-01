module PreludeX where
foldr1			:: (a -> a -> a) -> [a] -> a
foldr1 _ []		=  error "PreludeList.foldr1: empty list"
foldr1 f [x]		=  x
foldr1 f (x:xs)		=  f x (foldr1 f xs)
