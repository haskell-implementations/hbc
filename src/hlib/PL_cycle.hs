module P_List_cycle  where
cycle			:: [a] -> [a]
cycle xs		=  xs' where xs' = xs ++ xs'
