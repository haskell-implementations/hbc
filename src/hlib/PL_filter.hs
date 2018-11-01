module P_List_filter  where
filter		:: (a -> Bool) -> [a] -> [a]
filter p []	= []
filter p (x:xs)	= if p x then x:filter p xs else filter p xs
