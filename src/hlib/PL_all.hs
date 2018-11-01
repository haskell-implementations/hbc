module P_List_all  where
all		:: (a -> Bool) -> [a] -> Bool
all p []	=  True
all p (x:xs)	=  p x && all p xs
