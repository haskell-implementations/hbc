module PreludeX where
any		:: (a -> Bool) -> [a] -> Bool
any p []	= False
any p (x:xs)	= p x || any p xs
