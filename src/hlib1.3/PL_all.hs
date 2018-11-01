module PreludeX where
all		:: (a -> Bool) -> [a] -> Bool
all p []	=  True
all p (x:xs)	=  p x && all p xs
