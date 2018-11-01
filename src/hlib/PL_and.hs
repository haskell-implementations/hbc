module P_List_and  where
and		:: [Bool] -> Bool
and []		=  True
and (x:xs)	=  x && and xs
