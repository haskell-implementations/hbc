module P_List_foldl1  where
foldl1			:: (a -> a -> a) -> [a] -> a
foldl1 _ []		=  error "Prelude.foldl1: empty list"
foldl1 f (x:xs)		=  foldl f x xs
