module P_List_or  where
or		:: [Bool] -> Bool
or []		=  False
or (x:xs)	=  x || or xs
