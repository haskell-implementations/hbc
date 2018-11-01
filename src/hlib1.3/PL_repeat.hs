module PreludeX where
repeat			:: a -> [a]
repeat x		=  xs where xs = x:xs
