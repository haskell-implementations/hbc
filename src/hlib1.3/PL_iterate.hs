module PreludeX where
iterate			:: (a -> a) -> a -> [a]
iterate f x		=  x : iterate f (f x)
