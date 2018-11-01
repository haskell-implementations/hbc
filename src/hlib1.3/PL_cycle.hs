module PreludeX where
cycle			:: [a] -> [a]
cycle xs		=  xs' where xs' = xs ++ xs'
