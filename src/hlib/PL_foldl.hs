module P_List_foldl  where
foldl			:: (a -> b -> a) -> a -> [b] -> a
foldl f z []		=  z
foldl f z (x:xs)	=  foldl f (f z x) xs
