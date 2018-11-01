module PreludeX where
replicate		:: Int -> a -> [a]
replicate n x		= if n <= 0 then [] else x : replicate (n-1) x
