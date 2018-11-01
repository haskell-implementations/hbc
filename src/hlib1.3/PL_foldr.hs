module PreludeX where
foldr			:: (a -> b -> b) -> b -> [a] -> b
foldr f z []		=  z
foldr f z (x:xs)	=  f x (foldr f z xs)
