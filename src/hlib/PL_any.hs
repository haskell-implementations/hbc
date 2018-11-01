module P_List_any  where
any		:: (a -> Bool) -> [a] -> Bool
any p []	= False
any p (x:xs)	= p x || any p xs
