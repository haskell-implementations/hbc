module P_List_iterate  where
iterate			:: (a -> a) -> a -> [a]
iterate f x		=  x : iterate f (f x)
