module P_List_map  where
map			:: (a -> b) -> [a] -> [b]
map f []		=  []
map f (x:xs)		=  f x : map f xs
