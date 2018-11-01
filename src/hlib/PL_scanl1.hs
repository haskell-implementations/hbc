module P_List_scanl1  where
scanl1			:: (a -> a -> a) -> [a] -> [a]
scanl1 _ []		=  error "Prelude.scanl1: empty list"
scanl1 f (x:xs)		=  scanl f x xs
