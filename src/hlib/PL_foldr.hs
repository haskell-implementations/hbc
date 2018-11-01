module P_List_foldr  where
foldr			:: (a -> b -> b) -> b -> [a] -> b
foldr f z []		=  z
foldr f z (x:xs)	=  f x (foldr f z xs)
