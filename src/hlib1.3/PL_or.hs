module PreludeX where
or		:: [Bool] -> Bool
or []		=  False
or (x:xs)	=  x || or xs
